#!/usr/bin/env tclsh


# Get a list of all of the USB devices on a system, along with their
# associated hidraw or serial (tty) devices.
namespace eval USBList {
   # root-dir of directory of usb devices
   variable SYSPATH    "/sys/bus/usb/devices"
   # root-dir of directory of hidraw devices (not necessarily all usb)
   variable SYS_HIDRAW "/sys/class/hidraw"
   # root-dir of directory of tty devices (not necessarily all usb)
   variable SYS_TTY    "/sys/class/tty"
   # default structure with all keys and empty values
   variable BLANKDEV [dict create "idVendor" ""  "idProduct" ""  "manufacturer" "" \
      "product" ""   "busnum" ""  "devnum" ""  "devices" ""   "firmware" ""  "error" "" ]
   # list of attributes to read from /sys/... device path
   variable ATTRS    {idVendor idProduct manufacturer product busnum devnum}

   # read and return contents of file (minus final "\n")
   # if any error then return "" instead.
   proc readfile {path} {
      try {
         set fp [open $path "r"]; set data [read $fp]; close $fp
         return [string trimright $data "\n"]
      } on error {msg o} { return "" }
   }

   # retrieve device information from /sys/... device path
   proc device_info {dirname} {
      variable BLANKDEV; variable ATTRS

      set device $BLANKDEV
      foreach attr $ATTRS {
         set val [readfile [file join $dirname $attr] ]
         if {$attr in {idVendor idProduct} && $val eq ""} { return "" }
         dict set device $attr $val
      }
      return $device
   }

   proc get_usb_devices {} {
      # Scan a well-known Linux hierarchy in /sys and try to find
      # all of the relevant USB devices on a system.
      variable SYS_HIDRAW; variable SYS_TTY; variable SYSPATH
      set all_devs {}; set result {}

      # First get a list of devices for the relevant classes:
      foreach dir [list $SYS_HIDRAW $SYS_TTY] {
         foreach dev [glob -nocomplain -directory $dir -types {l} "*"] {
            set tgt [file readlink $dev]; # read symlink
            lappend all_devs [file normalize [file join $dir $tgt]]
         }
      }

      # Then iterate the list of usb devices and pick relevant dev names
      # from list obtained above.
      foreach path [glob -directory $SYSPATH -types {d} -nocomplain {[0-9]*}] {
         set path [file normalize [file join $SYSPATH [file readlink $path]]]
         if {[ dict size [ set devinfo [device_info $path] ] ]} {
            set devices [lsearch -glob -all -inline $all_devs "$path*"]
            set devtails [lmap d $devices { file tail $d }]
            set busdev "[dict get $devinfo busnum]_[dict get $devinfo devnum]"
            dict set devinfo "devices" $devtails
            lappend result $path $busdev $devinfo
         }
      }
      return [lsort -index 1 -stride 3 $result]
   }
}

# Read temperature and/or humidity information from a specified USB device.
namespace eval USBRead {
   variable ABBREVIATIONS {
      "IT" "internal temperature"     "IH" "internal humidity"
      "ET" "external temperature"     "EH" "external humidity"
   }
   variable KNOWN_HIDRAW_FIRMWARES {
      "TEMPerF1.2"       { "IT"  2 256.0 }
      "TEMPerF1.4"       { "IT"  2 256.0 }
      "TEMPer1F1."       { "IT"  2 256.0 }
      "TEMPerGold_V3.1"  { "IT"  2 100.0 }
      "TEMPerGold_V3.3"  { "IT"  2 100.0 }
      "TEMPerGold_V3.4"  { "IT"  2 100.0 }
      "TEMPerGold_V3.5"  { "IT"  2 100.0 }
      "TEMPerX_V3.1"     { "IT"  2 100.0  "IH"  4 100.0  "ET" 10 100.0  "EH" 12 100.0 }
      "TEMPerX_V3.3"     { "IT"  2 100.0  "IH"  4 100.0  "ET" 10 100.0  "EH" 12 100.0 }
      "TEMPer2_M12_V1.3" { "IT"  2 256.0  "ET"  4 256.0 }
      "TEMPer2_V3.7"     { "IT"  2 100.0  "ET" 10 100.0 }
      "TEMPer2_V3.9"     { "IT"  2 100.0  "ET" 10 100.0 }
      "TEMPerHUM_V3.9"   { "IT"  2 100.0  "IH"  4 100.0  "ET" 10 100.0 }
      "TEMPer1F_H1V1.5F" { "IT"  2 {t {-46.85 + 175.72 * ($t<<2) / 65536}}
                           "IH"  4 {h {-6 + 125.0 * ($h<<4) / 65536}} }
      "TEMPer2_V4.1"     { "IT"  2 100.0  "ET" 10 100.0 }
      "TEMPer1F_V3.9"    { "IT"  2 100.0 }
   }

   variable QUERY_FIRMWARE     [binary format "H*"  "0186ff0100000000" ]
   variable QUERY_TEMP_HUM     [binary format "H*"  "0180330100000000" ]

   variable forced_firmware    ""

   proc set_forced_firmware {ffw} {
      variable KNOWN_HIDRAW_FIRMWARES; variable forced_firmware $ffw
      if {![dict exists $KNOWN_HIDRAW_FIRMWARES $forced_firmware]} {
         puts [format "Unknown Firmware \"%s\"" $forced_firmware]
         puts "Known: [join [dict keys $KNOWN_HIDRAW_FIRMWARES] ", "]"
         exit 1
      }
   }

   proc read_hidraw_firmware {fd {verbose false}} {
      variable QUERY_FIRMWARE

      # Quote from original temper.py:
      # Sometimes we don't get all of the expected information from the
      # device.  We'll retry a few times and hope for the best.
      # See: https://github.com/urwen/temper/issues/9
      foreach i [lrepeat 10 ""] {
         puts -nonewline $fd $QUERY_FIRMWARE; flush $fd
        
         set firmware ""
         while {true} {
            after 100; set data [read $fd 8]
            if {$data eq ""} { break }
            append firmware $data
         }
        
         if {$firmware eq ""} {
            close $fd
            throw {TEMPER FIRMWARE READ} "Cannot read device firmware identifier"
         }
        
         if {[string length $firmware] > 8 } { break }
      }
        
      return $firmware
   }

   # special lambda-expression. e.g.: t {$t/256.0} - take body as an expression
   proc exprlambda {scale} { lreplace $scale 1 1 [list expr [lindex $scale 1]] }
  
   proc read_hidraw {device verbose} {
      variable KNOWN_HIDRAW_FIRMWARES; variable ABBREVIATIONS
      variable QUERY_TEMP_HUM; variable forced_firmware
      # Using the Linux hidraw device, send the special commands and receive the
      # raw data. Then call 'parse_bytes' based on the firmware version to provide
      # temperature and humidity information.
      #
      # A dictionary of temperature and humidity info is returned.

      set path [file join "/dev" $device]
      set fd [open $path "r+b"]
      fconfigure $fd -blocking off
  
      if {$forced_firmware ne ""} {
         set firmware $forced_firmware; set sfirm $firmware
      } else {
         set firmware [read_hidraw_firmware $fd $verbose]
         set sfirm [encoding convertfrom iso8859-1 $firmware]
      }
      binary scan $firmware H* hfirm
  
      if {$verbose} {
         puts [format "Firmware value: %s \"%s\"" \
            $hfirm $sfirm]
      }
  
      # Get temperature/humidity
      puts -nonewline $fd $QUERY_TEMP_HUM; flush $fd

      set bytes ""
      while {true} {
         after 100; set data [read $fd 8]
         if {$data eq ""} { break }
         append bytes $data
      }
      close $fd; binary scan $bytes H* hbytes

      if {$verbose} {
         puts [format "Data value: %s" $hbytes]
      }
  
      set info [dict create]
      dict set info "firmware" $sfirm
      if {$verbose} {
         dict set info "hex_firmware" $hfirm
         dict set info "hex_data" $hbytes
      }

      set found 0
      foreach {kfirm kdevinfo} $KNOWN_HIDRAW_FIRMWARES {
         set tfirm [string range $sfirm 0 [string length $kfirm]-1]
         if {$kfirm eq $tfirm} {
            foreach {attr pos scale} $kdevinfo {
               set attr [dict get $ABBREVIATIONS $attr]
               binary scan $bytes "@${pos} Su" value ;# big-endian 16bit integer
               if {$value != 0x4e20} { 
                  if {[string is double -strict $scale]} {
                     set value [expr {$value / $scale}]
                  } elseif {[string is list $scale] && [llength $scale]==2} {
                     if {$verbose} { puts [format "Raw $attr: 0x%4x" $value] }
                     set value [apply [exprlambda $scale] $value]
                  } else {
                     puts stderr "Internal error: bad scale in firmware-description for $sfirm"
                     exit 1
                  }
               }
               dict set info $attr $value
            }
            set found 1; break
         }
      }
  
      if { ! $found } {
         dict set info "error" [format "Unknown firmware \"%s\": %s" $sfirm $hfirm]
      }

      return $info
   }
  
   proc read_serial {device} {

      # Using the Linux serial device, send the special commands and receive the
      # text data, which is parsed directly in this method.
  
      # A dictionary of device info (like that returned by USBList) combined with
      # temperature and humidity info is returned.

      set path [file join "/dev" $device]
      set fd [open $path "r+b"]
      fconfigure $fd -mode 9600,8,n,1 -blocking off
      #s.xonoff = False
      #s.rtscts = False
      #s.dsrdtr = False
  
      # Send the "Version" command and save the reply.
      puts $fd "Version"; flush $fd
      set firmware [encoding convertfrom iso8859-1 [gets $fd]]
  
      # Send the "ReadTemp" command and save the reply.
      puts $fd "ReadTemp"; flush $fd
      set    reply [encoding convertfrom iso8859-1 [gets $fd]]\n
      append reply [encoding convertfrom iso8859-1 [gets $fd]]\n
      close $fd
  
      set info [dict create "firmware" $firmware ]
      if {[regexp {Temp-Inner:([0-9.]*).*?, ?([0-9.]*)} $reply _ g1 g2 ]} {
         dict set info "internal temperature" [expr {double($g1)}]
         dict set info "internal humidity"    [expr {double($g2)}]
      } else {
         dict set info "internal temperature" "-"
         dict set info "internal humidity"    "-"
      }
      if {[regexp {Temp-Outer:([0-9.]*).*?, ?([0-9.]*)} $reply _ g1 g2]} {
         dict set info "external temperature" [expr {double($g1)}]
         dict set info "external humidity" [expr {double($g2)}]
      } else {
         dict set info "external temperature" "-"
         dict set info "external humidity"    "-"
      }
      return $info
   }
  
   proc readdev {device verbose} {
      # Read the firmware version, temperature, and humidity from the device and
      # return a dictionary containing these data.

      # Use the last device found
      if {[string match "hidraw*" $device]} {
         return [read_hidraw $device $verbose]
      } elseif {[string match "tty*" $device]} {
         return [read_serial $device $verbose]
      }
      return [dict create "error" "No usable hid/tty devices available"]
   }
}

namespace eval Temper {
   variable KNOWN {
      "0c45:7401" "0c45:7402" 
      "413d:2107" 
      "1a86:5523" "1a86:e025"
      "3553:a001"
   }

   proc init {} {
      variable usb_devices [USBList::get_usb_devices]
      variable forced_vendor_id ""  forced_product_id ""
   }

   proc is_known_id {idVendor idProduct} {
      variable forced_vendor_id; variable forced_product_id; variable KNOWN

      # Returns True if the idVendor and idProduct are valid.
      if {$forced_vendor_id ne "" && $forced_product_id ne ""} {
         return [expr {$forced_vendor_id eq $idVendor && $forced_product_id eq $idProduct}]
      }
      set vidpid "$idVendor:$idProduct"
      return [expr {$vidpid in $KNOWN}]
   }

   proc listdevs {{use_json false}} {
      variable usb_devices
      # Print out a list all of the USB devices on the system. If 'use_json' is
      # true, then JSON formatting will be used.

      if {$use_json} {
         #print(json.dumps(self.usb_devices, indent=4))
         puts stderr "TODO: json not yet implemented"
         return
      }
  
      # original sorting by:
      # key=lambda x: x[1]['busnum'] * 1000 + x[1]['devnum']):
      foreach {path busdev info} $usb_devices {
         dict with info {
            puts [format "Bus %03d Dev %03d %s:%s %s %s %s" \
               $busnum $devnum $idVendor $idProduct \
               [expr {[is_known_id $idVendor $idProduct] ? "*" : " "}] \
               $product [list $devices] \
            ]
         }
      }
   }

   proc readdevs {{verbose false}} {
      variable usb_devices
      # Read all of the known devices on the system and return a list of
      # dictionaries which contain the device information, firmware information,
      # and environmental information obtained. If there is an error, then the
      # 'error' field in the dictionary will contain a string explaining the
      # error.

      set results {}
      foreach {path busdev info} $usb_devices {
         dict with info {
            if {![is_known_id $idVendor $idProduct]} { continue }
            if {[llength $devices] == 0} { continue }
            set devinfo [USBRead::readdev [lindex $devices end] $verbose]
            lappend results [dict merge $info $devinfo]
         }
      }
      return $results
   }

   proc add_temperature {name info} {
      # Helper method to add the temperature to a string in both Celsius and
      # Fahrenheit. If no sensor data is available, then '- -' will be returned.

      if { ! [dict exists $info $name] } { return "- -" }

      set degC [dict get $info $name]
      set degF [expr {$degC * 1.8 + 32.0}]
      return [format {%.2fC %.2fF} $degC $degF]
   }

   proc add_humidity {name info} {
      # Helper method to add the humidity to a string. If no sensor data is
      # available, then '-' will be returned.
   
      if { ! [dict exists $info $name] } { return "- -" }
      return [format {%d%%} [expr {int([dict get $info $name])}]]
   }
   
   proc print {results {use_json false}} {
      # Print out a list of all of the known USB sensor devices on the system.
      # If 'use_json' is True, then JSON formatting will be used.
      
      if {$use_json} {
         set json ""
         append json "\["; set c ""
         foreach dev $results {
            append json "$c\n   \{"; set C ""
            dict for {k v} $dev {
               append json [format "$C\n      %s: %s"  "\"$k\"" "\"$v\""]
               set C ","
            }
            append json "\n   \}"; set c ","
         }
         append json "\n\]"
         puts $json
         return
      }
      
      foreach {info} $results {
         dict with info {
            set res [format "Bus %03d Dev %03d %s:%s %s" \
               $busnum $devnum $idVendor $idProduct $firmware ]

            if {$error ne ""} {
               append res [format { Error: %s} $error]
            } else {
               append res " " [add_temperature "internal temperature" $info]
               append res " " [add_humidity    "internal humidity"    $info]
               append res " " [add_temperature "external temperature" $info]
               append res " " [add_humidity    "external humidity"    $info]
            }
         }
         puts $res
      }
   }
   
   proc main {argv} {
      # An example 'main' entry point that can be used to make temper.py a
      # standalone program.
      
      set listdevs false; set use_json false; set verbose false
      for {set idx 0} {$idx < [llength $argv]} {incr idx} {
         set arg [lindex $argv $idx]
         switch -exact -- $arg {
            "-l" - "--list" { set listdevs true }
            "-j" - "--json"        { set use_json true }
            "-v" - "--verbose"     { set verbose  true }
            "-f" - "--force"       {
               incr idx; set arg [lindex $argv $idx]
               lassign [split $arg ":"] vendor_id product_id
               variable forced_vendor_id  $vendor_id
               variable forced_product_id $product_id
            }
            "-F" - "--firmware"       {
               incr idx; set arg [lindex $argv $idx]
               USBRead::set_forced_firmware $arg
            }
            "-h" - "--help" {
               puts "usage: $::argv0 \[options ...\]"
               puts "-l or --list          : List all USB devices"
               puts "-j or --json          : Provide output as JSON"
               puts "-f or --force Vid:Pid : Force the use of the hex id; ignore other ids"
               puts "-F or --firmware \"TEMPer...\" : Force given firmware instead of querying"
               puts "-v or --verbose       : Output binary data from thermometer"
               puts "-h or --help          : Output this help text"
               exit 1
            }
            default { puts stderr "Unknown argument: '$arg'" }
         }
      }

      if {$listdevs} {
         listdevs $use_json
         return 0
      }
      
      # By default, output the temperature and humidity for all known sensors.
      set results [readdevs $verbose]
      print $results $use_json
      return 0
   }
}

Temper::init
set rc [Temper::main $argv]
exit $rc

