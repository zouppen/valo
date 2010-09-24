Valo
====

Haskell modules for controlling DMX lights via ENTTEC USB DMX Pro.
Contains also an UDP server for receiving color codes.

For information about UDP server, see README.udp.

Requirements
------------

In Ubuntu and Debian, these are package names you can install directly
with apt-get:

libghc6-time-dev libghc6-network-dev

"Raw" usage
-----------

Usage for 2 lights connected to DMX address 1 and 6 and DMX controller
is at default location (in Linux):

$ ghci DMX.hs LightSource.hs
> :m +LightSource
> bus <- createBus "/dev/serial/by-id/usb-ENTTEC_DMX_USB_PRO_ENR35XBU-if00-port0"
> let eka = Light 1 bus
> let toka = Light 6 bus
> setLight eka (Color 255 255 255)
> sendDMX bus
> setLight eka (Color 255 255 0)
> sendDMX bus
> setLight toka (Color 0 255 0)
> sendDMX bus
> setLight toka (Color 127 255 0)
> setLight eka (Color 255 255 255)
> sendDMX bus

Blinking lights
---------------

With Experiments.hs and Timing.hs there is funky DMX light replacement
of HTML blink tag. :-D To blink two lights in turn at 2.1
blinks-per-second, you can run:

> bus <- myBus
> sequenceFps 2.1 $ switchActions bus 
> closeBus bus

Troubleshooting:

If your lamps do not react to commands, you can try switching lamps on
by dumping one DMX test "frame" directly to the device:

$ cat test_lamps.dat >/dev/serial/by-id/usb-ENTTEC_DMX_USB_PRO_ENR35XBU-if00-port0"

Compiling:
----------

To compile UDP light server as a static binary:

ghc --make UDPCommander.hs

If you want smaller binary and you have all the "dyn" libraries, you can use:

ghc --make -dynamic UDPCommander.hs
