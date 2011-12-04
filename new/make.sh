#!/bin/bash -eu
lhs2TeX PacketProtocol.lhs >PacketProtocol.tex
pdflatex PacketProtocol.tex
pandoc -f latex+lhs -t html PacketProtocol.tex -o PacketProtocol.html

