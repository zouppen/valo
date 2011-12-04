\documentclass{article}
%include lhs2TeX.fmt
%include lhs2TeX.sty
\usepackage{hyperref}
\begin{document}
\title{Valo datagram protocol}
\author{Joel Lehtonen and Esa-Matti Suuronen}
\date{November 2011}
\maketitle

\section{Introduction}

Valo packet protocol is used to transfer messages unidirectionally
from clients to concentrator. Concentrator may pass messages forward
to actual sinks using this protocol.

The protocol is intended to be transmitted over UDP protocol. If you
need to transmit Valo packets over serial line, we are planning
sligthly modified version.

All reference code in this documentation is written in Haskell but should be
self-explanatory to any protocol programmer.

The following Haskell modules are used:

> import Data.Attoparsec.ByteString
> import Data.Bits
> import Data.Word

\section{Protocol definition}

We start by defining natural number and then proceed into the actual protocol definition.

\subsection{Natural number}

One of the key data types of Valo is unbounded natural number. It is
derived from the spirit of UTF-8 while being simpler to implement and
somewhat more space efficient.

Let's define natural number recursively. We shift the accumulator left
by 7 bits and OR it with a 8-bit word we read from the stream,
ignoring the MSB of that byte. If the read byte has MSB set, then we
recursively ask for next byte. Otherwise we stop. As said in source code:

> natural' :: Integer -> Parser Integer
> natural' acc = do
> 	byte <- anyWord8
>	let value = acc `shiftL` 7 .|. ((fromIntegral byte) `clearBit` 7)
> 	if byte `testBit` 7
> 		then  natural'  (value+1)  -- Recurse. Summing 
> 		else  return    value      -- Stop

Because the accumulator is redundant when called from outside, a shorthand function is defined:

> natural :: Parser Integer
> natural = natural' 0

\subsection{Datagram}

Datagram starts with version indicator. The version 0 is reserved to the legacy implementation of Valo and version 1 is the current version. In this paper the old version is ignored completely. If you want to support it, you may look the sources at \url{https://github.com/zouppen/valo}.

\end{document}
