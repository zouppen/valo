=============================
UDP light controlling support
=============================

Valo has support for listening to UDP port 9909 for incoming light
commands. The support is quite hard-wired but working. :-)


Terminology
===========

You can do well without understanding these terms. Just look to
example section.

Word8
    Unsigned 8 bit integer. Is usually an unsigned char in C.

Word32be
    Unsigned 32 bit integer in big endian format. In C this can
    be got with htonl() from unsigned int.


Server
======

To start Valo server, just run:

$ ghci UDPCommander.hs
> lightServer path_to_device

You can also compile the server for standalone use.
To compile UDP light server as a static binary:

ghc --make UDPCommander.hs

If you want smaller binary and you have all the "dyn" libraries, you can use:

ghc --make -dynamic UDPCommander.hs

Then, to run standalone server, invoke:

$ UDPCommander [device]

You can omit device path if you want to use the default of ENTTEC DMX USB PRO.


Packet format
=============

A packet is composed of zero or more commands. All commands in one
packet are executed in one pass, immediately after the whole packet is
parsed.

A command is composed of command type and command data. Depending of
command type, different data section is expected. Packet type field is
32 bit unsigned big-endian integer. Multiple commands in one packet
are concatenated with no padding.

Supported commands and their codes are following:

0. 8-bit precision RGB light source (DMX LED PAR or similar)

Support for more types are added as the software becomes more feature
rich.

8-bit precision RGB light source
--------------------------------

This command is composed of command type, light source ID and
intensities of three colour components: red, green and blue.

Common type of hardware is LED PAR connected to the computer via DMX
bus.

+-----------+-------------+-------------------------+
| Data type | Range       | Meaning                 |
+===========+=============+=========================+
| Word32be  | 0           | Command type. Always 0. |
+-----------+-------------+-------------------------+
| Word32be  | 0 .. 2^32-1 | Light source ID         |
+-----------+-------------+-------------------------+
| Word8     | 0 .. 255    | Intensity of red        |
+-----------+-------------+-------------------------+
| Word8     | 0 .. 255    | Intensity of green      |
+-----------+-------------+-------------------------+
| Word8     | 0 .. 255    | Intensity of blue       |
+-----------+-------------+-------------------------+


Examples
========

To put the first lamp white, you need to send the following data. Data
is given in hexadecimal format for clarity:

  00 00 00 00 00 00 00 00 FF FF FF

To put second lamp yellow:

  00 00 00 00 00 00 00 01 FF FF 00

To put do the preceding two operations in a single packet:

  00 00 00 00 00 00 00 00 FF FF FF 00 00 00 00 00 00 00 01 FF FF 00

Issuing multiple commands in a single packet is recommended because it
quarantees that light switching is atomic (or atomic enough for human
eye).


Testing
=======

Before your own implementation is ready, you are free to try sending
data with netcat to see how it works:

nc -w 0 -u server_address 9909 <udp-example/0-orchid.dat
nc -w 0 -u server_address 9909 <udp-example/1-yellow.dat

Or, if you want to splash both in a single packet:

nc -w 0 -u server_address 9909 <udp-example/0-1-combo.dat

Now your house is filled with funky colors. \o/


Limitations
===========

Due to DMX limitations, feasible update frequencies are about 20
commands per seconds. If you send commands too fast, no real DMX
action is performed until the next DMX update pass (or whatever it is
called). If your lights look a bit glitchy, try to increase update
interval.

DMX channels are hard-coded in UDPCommander.hs.

On my hardware, first lamp is assumed to be in DMX address of
1 and the second is at address 6. These assumptions are done in
UDPCommander.hs. This is a temporary hack.

Feel free to rewrite my code to support configuration files. :-)


Security
========

None. Use firewalls for port 9909 if you want to protect
yourself. This can be improved if there is any need.
