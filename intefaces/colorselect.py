#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# A derivate work of
# http://www.eurion.net/python-snippets/snippet/Color Select.html

import pygtk
pygtk.require('2.0')
import gtk
import socket
import struct
import sys

addr = ("10.0.0.1", 9909)
UDPSock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

def set_light(i, r,g,b):
    command_out = struct.pack('>LLBBB',0,i,r,g,b)
    UDPSock.sendto(command_out, addr)

class ColorSelectionExample:
    # Color changed handler
    def color_changed_cb(self, widget):

        # Get current color
        color = self.colorseldlg.colorsel.get_current_color()

        # Setting light number 0.
        set_light(0,
                  color.red >> 8,
                  color.green >> 8,
                  color.blue >> 8)

    def __init__(self):
        # Create color selection dialog
        self.colorseldlg = gtk.ColorSelectionDialog(
            "Select a color for your room")

        # Get the ColorSelection widget
        colorsel = self.colorseldlg.colorsel

        colorsel.set_has_palette(True)

        # Connect to the "color_changed" signal
        colorsel.connect("color_changed", self.color_changed_cb)
        # Show the dialog, ignore response code
        response = self.colorseldlg.run()

        self.colorseldlg.hide()


def main():
    return 0

if __name__ == "__main__":
    ColorSelectionExample()
    main()
