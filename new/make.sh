#!/bin/bash -eu
GEN="pandoc -s -f markdown+lhs PacketProtocol.md"
$GEN -t html -o specs.html
$GEN -t latex -o specs.tex
# Actual source for GHC
$GEN -t latex+lhs -o PacketProtocol.lhs
# And the PDF
pdflatex PacketProtocol.tex


