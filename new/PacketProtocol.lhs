\documentclass[11pt,a4paper,oneside]{report}
\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      basicstyle=\small\ttfamily,
      flexiblecolumns=false,
      basewidth={0.5em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2 
               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1               
    }
\begin{document}
\title{Valo packet protocol}
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

\section{Protocol definition}

The following snippets are written in Haskell but should be
self-explanatory to any protocol programmer.

One of the key data types of Valo is unbounded natural number. It is
derived from the spirit of UTF-8 while being simpler to implement and
somewhat more space efficient.

\begin{code}
natural :: Parser Integer
natural = 13
\end{code}

Uhhahhei!

\end{document}

