# temper.tcl

This is a fork of the temper.py project with a translation to Tcl

It still contains the temper.py to help me with merging upstream
changes, but I removed the docker and other python-specific stuff.

## Design

There are several open source software projects that support these sensors,
sometimes including complicated monitoring and graphing software. Unlike,
these projects, the goal of this project is to simply read data from the
sensors and do nothing else, given the following constraints:
* must work under Linux,
* must work with plain Tcl (without requiring extra packages or extensions).

# udev - let udev make devices read-/writable for us without "sudo"

File "etc--udev--rules.d--24-TEMPer.rules" can be copied to
 /etc/udev/rules.d/24-TEMPer.rules  (root required for that, ONCE)
Then, plugging the device ought to make the device nodes accessible
to all users in group "plugdev".

Note: udev-rules sometimes fail for devices already plugged at boot time.
If that happens, then unplug and replug the sensor.

# Devices

The USB temperature and temperature/humidity sensors sold by PCsensor are
widely available from the parent site
(http://pcsensor.com/usb-temperature-humidity.html), from Amazon, and from
EBay.

## Supported Devices

I own only one device from PCsensors. It is supported by temper.tcl.
The author of original temper.py stated that he owns 5 such devices.
I expect that my script supports the same devices as original temper.py.

The rest of this paragraph is from original temper.py page:

In the following table "I" means the sensor is internal to the USB stick and
"E" means the sensor is on a cable that is plugged into the USB stick.

Product     |    Id     |  Firmware        | Temp | Hum | Notes
------------|-----------|------------------|------|-----|---------------
TEMPer      | 0c45:7401 | TEMPerF1.2       | I    |     | Metal
TEMPer      | 0c45:7401 | TEMPerF1.4       | I    |     | Metal
TEMPer      | 413d:2107 | TEMPerGold_V3.1  | I    |     | Metal
TEMPer      | 1a86:e025 | TEMPerGold_V3.3  | I    |     | Metal
TEMPer      | 1a86:e025 | TEMPerGold_V3.4  | I    |     | Metal
TEMPer      | 3553:a001 | TEMPerGold_V3.5  | I    |     | Metal
TEMPerHUM   | 413d:2107 | TEMPerX_V3.1     | I    | I   | White plastic
TEMPerHUM   | 1a86:e025 | TEMPerHUM_3.9    |      | I   | White plastic with blue button
TEMPerHUM   | 0c45:7402 | TEMPer1F_H1V1.5F | I    | I   | White plastic with blue button
TEMPer2     | 413d:2107 | TEMPerX_V3.3     | I,E  |     | White plastic
TEMPer2     | 1a86:e025 | TEMPer2_V3.7     | I,E  |     | White plastic with red button
TEMPer2     | 1a86:e025 | TEMPer2_V3.9     | I,E  |     | White plastic with red button
TEMPer2     | 1a86:e025 | TEMPer2_M12_V1.3 | I,E  |     | White plastic with red button
TEMPer2     | 3553:a001 | TEMPer2_V4.1     | I,E  |     | White plastic with red button
TEMPer1F    | 413d:2107 | TEMPerX_V3.3     | E    |     | White plastic
TEMPer1F    | 1a86:e025 | TEMPer1F_V3.9    | E    |     | White plastic with pink button
TEMPerX232  | 1a86:5523 | TEMPerX232_V2.0  | I,E  | I   | White plastic
TEMPer1V1.1 | 0c45:7401 | TEMPer1F1.1Per1F | E    |     | Metal

The 1a86:5523 device may identify as 413d:2107 depending on button presses,
but it cannot be used successfully when in that mode.

If you try other software that uses libusb, the hidraw device may be
disconnected. In this case, remove and re-insert the USB stick.

The TEMPer1F has only an external sensor, but it is not possible to detect
that it is external, so it is reported as an internal temperature.

### TEMPer

This is a metal USB stick marked "TEMPer" with thermometer logo on one side,
and "TEMPer" on the other side. The end opposite the USB connector has a screw
hole. There is *no* humidity detector, but it appears water proof and I have
submerged mine momentarily in ice water and in boiling water.

### TEMPerHUM

This is a white plastic USB stick marked "TEMPerHUM", "-40C - +85C", and
"0-100%RH"; with *blue button* marked "TXT". On the reverse, "PCsensor". This
model does *not* have a jack on the end.

When the button is pressed the red LED will blink as messages of the following
style are sent (the temperature line repeats every second).

```
www.pcsensor.com
temperx v3.1
caps lock:on/off/++
num lock:off/on/--
type:inner-h2
inner-temperinner-humidityinterval
32.73 [c]36.82 [%rh]1s
```
When the button is pressed again, the LED will either be off or be solid red.
This is the mode that temper.py uses.

### TEMPer2

physical description: White plastic USB stick marked "TEMPer2",
"-40C - +125C"; with red button marked "TXT". On the reverse, "PCsensor".
This model has a jack for an external sensor on the end.

notes: When the button is pressed, the red LED will blink as messages
of the following form are sent (the temperature line repeats every
second).

Without an external sensor:
```
www.pcsensor.com
temperx v3.3
caps lock:on/off/++
num lock:off/on/--
type:inner-tx
inner-tempinterval
27.93 [c]1s
```

With an external sensor:
```
www.pcsensor.com
temperx v3.3
caps lock:on/off/++
num lock:off/on/--
type:inner-tx;outer-tx
inner-tempintervalinterval
27.18 [c]29.62 [c]1s
```

This program uses the mode where the LED is either off or solid red.

### TEMPer1F

White plastic USB stick marked "TEMPer1F", "-40C - +125C"; with pink button
marked "TXT'. On the reverse, "PCsensor". This model has a jack for an
external sensor and does *not* have an internal sensor.

When the button is pressed, the red LED will blink as messages
of the following form are sent (the temperature line repeats every 1
second).

Without the probe inserted:
```
www.pcsensor.com
temperx v3.3
caps lock:on/off/++
num lock:off/on/--
type:unknown
1s
```

With the probe inserted:
```
www.pcsensor.com
temperx v3.3
caps lock:on/off/++
num lock:off/on/--
type:outer-tx
outer-tempinterval
24.93 [c]1s
```

This program uses the mode where the LED is either off or solid red.

### TEMPerX232

White plastic USB stick marked "TEMPerX232", "0-100%RH",
and "-40 - +85C"; with a *green button* marked "press". On the reverse,
"PCsensor". On the end opposite the USB connector, there is a jack for an
external temperature sensor (which I do not have and did not try).

When the button is pressed and held down until the red LED is solid, a blue
LED will flash every second. In this mode, the USB vendor:product changes to
413d:2107, but only one HID device is available, and protocol sent to the
hidraw device is rejected with an error.

When the LED is flashing blue, and the button is pressed momentarily,
the following are sent (the temperature line repeats every second).

```
www.PCsensor.com
TEMPerX232-V2.0
type:inner-H2
inner-temperinner-humidityinterval
30.48 [C]40.19 [%RH]1
```

When the button is pressed and held down until the red LED is solid, a green
LED will flash every second. This is the mode temper.py uses. In this mode, if
"Help" is sent to the serial device, the following will be sent back:

```
   >>PCsensor<<
Welcome to use TEMPerX232!
Firmware Version:TEMPerX232_V2.0
The command is:
    ReadTemp                     -->read temperature,temp_value = sensor_value + calibration
    ReadCalib                    -->read calibration
    SetCalib-type:xx.x,xx.x>     -->set calibration, xx.x(-10.0~+10.0)
    EraseFlash                   -->erase calibration
    Version                      -->read firmware version
    ReadType                     -->read the sensor type
    ReadAlert-Temp               -->read temp alert value
    SetTempUpperAlert-type:xx.xx>-->set temp upper alert value,xx.xx(-40.00~+85.00)
    SetTempLowerAlert-type:xx.xx>-->set temp lower alert value,xx.xx(-40.00~+85.00)
    ReadAlert-Hum                -->read hum alert value
    SetHumUpperAlert-type:xx.xx> -->set hum upper alert value,xx.xx(00.00~99.99)
    SetHumLowerAlert-type:xx.xx> -->set hum lower alert value,xx.xx(00.00~99.99)
    SetMode-Temp:x>              -->set tempmode, x(0~1)
    ReadMode-Temp                -->read tempmode
    Help                         -->command help
    ?                            -->command help
The COM configuration is:
    Mode:       ASCII
    Baud Rate:  9600bps
    Data Bit:   8
    Parity Bit: None
    Stop Bit:   1
SHENZHEN RDing Tech CO.,LTD
www.PCsensor.com
```

This is the mode that temper.py uses. I was not successful getting this device
to respond to any commands sent via the HID device.

I initially had trouble getting a reply to ReadTemp when using a terminal
program (e.g., cu), but the example in the temper.py works without any
problems, perhaps because no newline is sent after the command.

## Example Command Output

### Help

```
$ ./temper.tcl -h
usage: ./temper.tcl [options ...]
-l or --list          : List all USB devices
-j or --json          : Provide output as JSON
-f or --force Vid:Pid : Force the use of the hex id; ignore other ids
-F or --firmware "TEMPer..." : Force given firmware instead of querying
-v or --verbose       : Output binary data from thermometer
-h or --help          : Output this help text
```

### List Devices

It lists all usb-devices, but only the supported devices have that `*` mark.

```
$ ./temper.tcl -l
...
Bus 004 Dev 003 2109:0817   USB3.0 Hub              {}
Bus 005 Dev 033 3553:a001 * TEMPerGold {hidraw8 hidraw9}
```

The list of devices follows Tcl syntax rather than python syntax, which
means, that if there were only *one* device (e.g. just "hidraw8"), then the
curly-braces would disappear.

### Temperature

```
$ ./temper.tcl
Bus 005 Dev 033 3553:a001 "TEMPerGold_V3.5" 26.18°C 79.12°F - - - -
```

The tcl-version puts the firmware name in quotes, and adds a degree (°) symbol
compared to temper.py .

```
$ ./temper.tcl --json
[
   {
      "idVendor": "3553",
      "idProduct": "a001",
      "manufacturer": "PCsensor",
      "product": "TEMPerGold",
      "busnum": "5",
      "devnum": "33",
      "devices": "hidraw8 hidraw9",
      "firmware": "TEMPerGold_V3.5",
      "internal temperature": "26.18"
   }
]
```

It differs from temper.py in that idVendor and idProduct are in hex format,
and all values are strings.

Similar JSON output can be generated with the --list option.

