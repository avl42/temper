#!/usr/bin/env tclsh

# Modify these Variables defined below to add support for new devices:
#    KNOWN_VID_PID          list of known vendor:product ids in "%04x:%04x" format
#    KNOWN_HIDRAW_FIRMWARES list of known firmware strings and their handling.
# other variables are rather unlikely to ever require changes.


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
   variable BLANKDEV   {
      "idVendor" ""  "idProduct" ""  "manufacturer" ""  "product" ""
      "busnum"   ""  "devnum"    ""  "devices"      ""
   }
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
         if {$attr in {"idVendor" "idProduct"} && $val eq ""} { return "" }
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
            dict set devinfo "devices" [lsort -dict $devtails]
            lappend result $path $busdev $devinfo
         }
      }
      return [lsort -index 1 -stride 3 $result]
   }
}

# Read temperature and/or humidity information from a specified USB device.
namespace eval USBRead {

   # This structure describes each recognized firmware, and how to extract the data.
   #
   # In Tcl terms, the structure is a "dict" that maps each firmware name
   # to a "list", and each list has a triplet of elements for each "sensor"
   # in the device.
   #
   # Each triplet consist of:
   #    1) a description of the sensor,
   #    2) the byte-offset into the device's returned bytes where the sensor value
   #       is encoded, and
   #    3) either:
   #         a double-value by which the 16bit value needs to be divided,
   #       or:
   #         a special lambda expression specifying a variable name and
   #         an expression (using that variable) to calculate the real-world
   #         value from the encoded 16bit value .
   #
   # Firmware strings are truncated to the length of each key before comparison.
   # This is so because it was done so in the original python temper.py
   # and I don't have knowledge of the full untruncated firmware strings.
   #
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

   # Abbreviations can be used for sensor names in KNOWN_HIDRAW_FIRMWARES
   # to reduce redundancy:
   variable ABBREVIATIONS {
      "IT" "internal temperature"     "IH" "internal humidity"
      "ET" "external temperature"     "EH" "external humidity"
   }

   # hidraw query "curses":
   variable QUERY_FIRMWARE     [binary format "H*"  "0186ff0100000000" ]
   variable QUERY_TEMP_HUM     [binary format "H*"  "0180330100000000" ]

   # can be set from command line:
   variable forced_firmware    ""

   proc set_forced_firmware {sfirm} {
      variable KNOWN_HIDRAW_FIRMWARES; variable forced_firmware $sfirm
      set found 0
      foreach {kfirm kdevinfo} $KNOWN_HIDRAW_FIRMWARES {
         set tfirm [string range $sfirm 0 [string length $kfirm]-1]
         if {$kfirm eq $tfirm} { set found 1; break }
      }
      if { ! $found } {
         puts [format "Unknown Firmware \"%s\"" $forced_firmware]
         puts "Known are: [join [dict keys $KNOWN_HIDRAW_FIRMWARES] ", "]"
         exit 1
      }
   }

   # special lambda-expression. e.g.: t {$t/256.0} - take body as an expression
   proc exprlambda {lambda} { lreplace $lambda 1 1 [list expr [lindex $lambda 1]] }

   # Read a response (firmware or data) from the hidraw
   proc read_hidraw_bytes {fd} {
      set result ""
      while {true} {
         after 100; set data [read $fd 16]
         if {$data eq ""} { break }
         append result $data
      }
      return $result
   }

   proc read_hidraw_firmware {fd verbose} {
      variable QUERY_FIRMWARE

      # Quote from original temper.py:
      # Sometimes we don't get all of the expected information from the
      # device.  We'll retry a few times and hope for the best.
      # See: https://github.com/urwen/temper/issues/9
      foreach i [lrepeat 10 ""] {
         puts -nonewline $fd $QUERY_FIRMWARE; flush $fd

         set firmware [string trimright [read_hidraw_bytes $fd] " "]
         if {$firmware eq ""} {
            close $fd; throw {TEMPER FIRMWARE READ} "Cannot read device firmware identifier"
         }

         if {[string length $firmware] > 8 } { break };# looks good
      }

      return $firmware
   }

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
      }; binary scan $firmware H* hfirm

      if {$verbose} {
         puts [format "Firmware value: %s \"%s\"" $hfirm $sfirm]
      }

      # Get temperature/humidity
      puts -nonewline $fd $QUERY_TEMP_HUM; flush $fd

      set bytes [read_hidraw_bytes $fd]
      binary scan $bytes H* hbytes

      close $fd

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
               if {[dict exists $ABBREVIATIONS $attr]} {
                  set attr [dict get $ABBREVIATIONS $attr]
               }
               binary scan $bytes "@${pos} Su" value ;# big-endian 16bit integer
               if {$value != 0x4e20} {
                  if {[string is double -strict $scale]} {
                     set value [expr {$value / $scale}]
                  } elseif {[string is list $scale] && [llength $scale] in {2 3}} {
                     if {$verbose} { puts [format "Raw $attr: 0x%4x" $value] }
                     set value [apply [exprlambda $scale] $value]
                  } else {
                     puts stderr "Internal error: bad scale \"$scale\" in firmware-description for \"$sfirm\""
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
      set fd [open $path "r+"]
      fconfigure $fd -mode 9600,8,n,1 -blocking off

      # Send the "Version" command and save the reply.
      puts $fd "Version"; flush $fd
      set firmware [encoding convertfrom iso8859-1 [gets $fd]]

      # Send the "ReadTemp" command and save the reply.
      puts $fd "ReadTemp"; flush $fd
      set    reply [encoding convertfrom iso8859-1 [gets $fd]]\n
      append reply [encoding convertfrom iso8859-1 [gets $fd]]\n
      close $fd

      set info [dict create "firmware" $firmware ]

      set IT "internal temperature"; set IH "internal humidity"
      set ET "internal temperature"; set EH "internal humidity"

      dict set info $IT "-"; dict set info $IH "-"
      if {[regexp {Temp-Inner:([0-9.]*).*?, ?([0-9.]*)} $reply _ g1 g2 ]} {
         if {[string is double $g1]} { dict set info $IT [expr {double($g1)}] }
         if {[string is double $g2]} { dict set info $IH [expr {double($g2)}] }
      }
      dict set info $ET "-"; dict set info $EH "-"
      if {[regexp {Temp-Outer:([0-9.]*).*?, ?([0-9.]*)} $reply _ g1 g2]} {
         if {[string is double $g1]} { dict set info $ET [expr {double($g1)}] }
         if {[string is double $g2]} { dict set info $EH [expr {double($g2)}] }
      }
      return $info
   }

   proc readdev {device verbose} {
      # use apt strategy for each type of device:
      if {[string match "hidraw*" $device]} {
         return [read_hidraw $device $verbose]
      } elseif {[string match "tty*" $device]} {
         return [read_serial $device $verbose]
      }
      return [dict create "error" "No usable hid/tty devices available"]
   }
}

namespace eval Temper {
   # List of known VendorId:ProductId pairs.
   # Line-breaks don't matter technically, but keep all products of a
   #   particular vendor in a line - for human readers.
   variable KNOWN_VID_PID {
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
      variable forced_vendor_id; variable forced_product_id; variable KNOWN_VID_PID

      # Returns True if the idVendor and idProduct are valid.
      if {$forced_vendor_id ne "" && $forced_product_id ne ""} {
         return [expr {$forced_vendor_id eq $idVendor && $forced_product_id eq $idProduct}]
      } else {
         set vidpid "$idVendor:$idProduct"
         return [expr {$vidpid in $KNOWN_VID_PID}]
      }
   }

   proc print_json {devices} {
      set json "\["; set c ""
      foreach dev $devices {
         append json "$c\n   \{"; set C ""
         dict for {k v} $dev {
            append json [format "$C\n      \"%s\": \"%s\""  $k $v]; set C ","
         }
         append json "\n   \}"; set c ","
      }
      append json "\n\]"
      puts $json
   }

   proc listdevs {use_json} {
      variable usb_devices

      if {$use_json} {
         print_json [lmap {_ _ dev} $usb_devices { set dev }]
      } else {
         foreach {path busdev info} $usb_devices {
            dict with info {
               set check [expr {[is_known_id $idVendor $idProduct] ? "*" : " "}]
               puts [format "Bus %03d Dev %03d %s:%s %s %s %s" \
                  $busnum $devnum $idVendor $idProduct $check $product [list $devices] ]
            }
         }
      }
   }

   proc readdevs {verbose} {
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
      return [format {%.2f°C %.2f°F} $degC $degF]
   }

   proc add_humidity {name info} {
      # Helper method to add the humidity to a string. If no sensor data is
      # available, then '-' will be returned.

      if { ! [dict exists $info $name] } { return "-" }
      return [format {%d%%} [expr {int([dict get $info $name])}]]
   }

   proc printdevs {results use_json} {
      # Print out a list of all of the known USB sensor devices on the system.
      # If 'use_json' is True, then JSON formatting will be used.

      if {$use_json} {
         print_json $results
         return
      }

      foreach {info} $results {
         dict with info {
            set res [format "Bus %03d Dev %03d %s:%s \"%s\"" \
               $busnum $devnum $idVendor $idProduct $firmware ]

            if {[info exists error]} {
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
            "-l" - "--list"     { set listdevs true }
            "-j" - "--json"     { set use_json true }
            "-v" - "--verbose"  { set verbose  true }
            "-f" - "--force"    {
               incr idx; set arg [lindex $argv $idx]
               lassign [split $arg ":"] vendor_id product_id
               variable forced_vendor_id  $vendor_id
               variable forced_product_id $product_id
            }
            "-F" - "--firmware" {
               incr idx; set arg [lindex $argv $idx]
               USBRead::set_forced_firmware $arg
            }
            "-h" - "--help"     {
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
      } else {
         printdevs [readdevs $verbose] $use_json
      }
      return 0
   }
}

Temper::init
set rc [Temper::main $argv]
exit $rc

