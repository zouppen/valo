#!/bin/bash -eu
GEN="pandoc -N -s -S -f markdown+lhs PacketProtocol.lhs"
$GEN -t html -5 -o specs.html
$GEN -t latex -o specs.tex
# Actual source for GHC
$GEN -t latex+lhs -H header.tex -B body.tex -o specs.lhs
# Alternative tex and PDF
lhs2TeX --poly specs.lhs --output=specs-new.tex
#pdflatex specs.tex
pdflatex specs-new.tex
# Cleanup
rm -f specs.lhs specs.aux specs.log specs.out specs.tex specs-new.aux specs-new.log specs-new.out specs-new.tex specs-new.ptb
