module Experiments where

import DMX
import Timing
import LightSource

onOffColors = cycle [Color 255 255 255,Color 0 0 0]
onOffActions light = map (setLightInstantly light) onOffColors

switchAction bus (on,off) = do
  setLight (Light on bus) (Color 255 255 255)
  setLight (Light off bus) (Color 0 0 0)
  sendDMX bus

switchActions bus = cycle $ map (switchAction bus) [(1,6),(6,1)]

-- My own configuration

myBus = createBus "/dev/serial/by-id/usb-ENTTEC_DMX_USB_PRO_ENR35XBU-if00-port0"

myLight = Light 1

