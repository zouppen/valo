Introduction
============

Valo packet protocol is used to transfer messages unidirectionally
from clients to concentrator. Concentrator may pass messages forward
to actual sinks using this protocol.

The protocol is intended to be transmitted over UDP protocol. If you
need to transmit Valo packets over serial line, we are planning
sligthly modified version.

All reference code in this documentation is written in Haskell but should be
self-explanatory to any protocol programmer.

TODO more motivation, comparison to different protocols like DMX,
Artnet, etc.

The following Haskell modules are used:

> import Control.Monad (ap,when)
> import Data.Attoparsec.ByteString as A
> import Data.Bits
> import Data.Word
> import Data.Text.Encoding
> import Data.Text (Text)

Data types
==========

Data types are defined as Enumerations and Named Fields of
Haskell. Your implementation is free to use any kind of data
structure, like JSON or native C structs.

A datagram may contain multiple frames. Frame definition:

> data Frame  =  Tag Text
>             |  Light Integer Colour
>                deriving (Show)

Lights have colour information associated which is colourspace dependent. Here is the definition:

> data Colour  =  RGB Word8 Word8 Word8
>              |  HSV Word8 Word8 Word8
>                 deriving (Show)

Protocol definition
===================

We start by defining natural number and then proceed into the actual protocol definition.

Natural number
--------------

One of the key data types of Valo is unbounded natural number. It is
derived from the spirit of UTF-8 while being simpler to implement and
somewhat more space efficient.

Let's define natural number recursively. We shift the accumulator left
by 7 bits and OR it with a 8-bit word we read from the stream,
ignoring the MSB of that byte. If the read byte has MSB set, then we
recursively ask for next byte. Otherwise we stop. 

> natural' :: Integer -> Parser Integer
> natural' acc = do
> 	byte <- anyWord8
>	let value = acc `shiftL` 7 .|. fromIntegral byte `clearBit` 7
> 	if byte `testBit` 7
> 		then  natural'  (value+1)  -- Recurse
> 		else  return    value      -- Stop

Because the accumulator is redundant when called from outside, a
shorthand function is defined:

> natural :: Parser Integer
> natural = natural' 0

If your platform of choice makes it unconvenient to handle unbounded
numbers the natural number may be restricted to the range of [0,127]. That is
not an issue if there is no use for higher indices.

> naturalInJava :: Parser Integer
> naturalInJava = do
>	value <- anyWord8
>	when (value > 127) (fail "This implementation is a bit broken")
>	return (fromIntegral value)

Datagram
--------

Datagram starts with version indicator. The version 0 is reserved to
the legacy implementation of Valo and version 1 is the current
version. In this paper the old version is ignored completely. If you
want to support it, you may look the sources at
\url{https://github.com/zouppen/valo}.

Version byte is followed by actual frames. Minimum number of frames
per packet is one and maximum number of frames is limited by UDP
datagram maximum length. The formal definition is:

> datagram :: Parser [Frame]
> datagram = do
> 	word8 1      <?> "Only protocol version 1 is supported"
> 	many1 frame  <?> "At least one frame must be submitted"

Frame contains actual command for a single device or distinct
controllable unit like light fixture, smoke machine, or similar. The
definition is trivial:

> frame :: Parser Frame
> frame = do
>	a <- natural
>	case a of
>		0 -> tag
>		1 -> light
>		_ -> fail "Unsupported frame type"

Tag
---

Tag is a frame which appends any string to log messages. It is useful
not only in debugging but also in plotting user activity. Keep in mind
this should not ever used in any kind of accounting. There's no
cryptography involved.

Tag contains UTF-8 encoded string and it is terminated with 0x00 like
strings in C programming languge.

> tag :: Parser Frame
> tag = do
> 	bytes <- A.takeWhile (/= 0)
>	word8 0
> 	case decodeUtf8' bytes of
>		Left e   -> fail (show e)
>		Right a  -> return (Tag a)

Light
-----

There is wide variety of lightning which falls into this
category. Spot lights with colour disc, LED bars, dimmer-controlled
spot lights, etc. Light colour can be expressed in variety of formats
but there is always some kind of native colourspace the hardware
supports.

The server should therefore support conversion from a colourspace to
another if the client device is not capable of doing such
conversions. Algorithms of doing that are not part of this
specification, but there are plenty of literature and Web sources
covering that issue.

A light has ID and colour. The frame starts with ID, is followed by
colourspace type and actual colour definition which is type dependent.

> light :: Parser Frame
> light = do
>	id      <- natural
>	kind    <- natural
>	colour  <- case kind of
>		0 -> rgb
>		1 -> hsv
>		_ -> fail "Unknown light type"
>	return (Light id colour)

RGB colourspace is probably the most common type of representing
colours. It is composed of three 8-bit channels representing red,
green and blue channels, respectively. Most LED fixtures, monitors,
and human eyes have this kind of internal structure.

> rgb :: Parser Colour
> rgb = return RGB `ap` anyWord8 `ap` anyWord8 `ap` anyWord8

HSV stands for Hue, Saturation and Value. It is an alternative way of
representing colours and is native in some (old) fixtures. However
this is probably the most natural way for a human to comprehend
colours.

> hsv :: Parser Colour
> hsv = return HSV `ap` anyWord8 `ap` anyWord8 `ap` anyWord8

Instrutions for colourspace manipulation guidance.

Asynchronous serial protocol
============================

Motivation: Arduino

TODO
