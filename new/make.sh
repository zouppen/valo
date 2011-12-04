#!/bin/bash -eu
lhs2TeX PacketProtocol.lhs >PacketProtocol.tex
pdflatex PacketProtocol.tex
