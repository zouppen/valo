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

The following Haskell modules are used:

> import Data.Attoparsec.ByteString as A
> import Data.Bits
> import Data.Word
> import Data.Text.Encoding
> import Data.Text (Text)

Data types
==========

Data types are defined as Enumerations or Named Fields of
Haskell. Your implementation is free to use any kind of data
structure, like JSON or native C structs.

> data Frame = Tag Text
>            | Light
>            deriving (Show)

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
>	let value = acc `shiftL` 7 .|. ((fromIntegral byte) `clearBit` 7)
> 	if byte `testBit` 7
> 		then  natural'  (value+1)  -- Recurse
> 		else  return    value      -- Stop

Because the accumulator is redundant when called from outside, a
shorthand function is defined:

> natural :: Parser Integer
> natural = natural' 0

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
> frame = choice [tag,light]

Tag
---

Tag is a frame which appends any string to log messages. It is useful
not only in debugging but also in plotting user activity. Keep in mind
this shouldn't ever used in any kind of accounting. There's no
cryptography involved.

Tag starts with value of 0x00 and UTF-8 encoded string follows. The
tag is terminated with 0x00 like strings in C programming languge.

> tag :: Parser Frame
> tag = do
> 	word8 0
> 	bytes <- A.takeWhile (/= 0)
>	word8 0
> 	case decodeUtf8' bytes of
>		Left e   -> fail (show e)
>		Right a  -> return (Tag a)

Light
-----

Light TODO.

> light :: Parser Frame
> light = undefined
