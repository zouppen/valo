UDP light controlling support
=============================

Valo has support for listening to UDP port 9909 for incoming light
commands. The support is quite hard-wired but working. :-)

Server
------

To start Valo server, just run:

$ ghci UDPCommander.hs
> lightServer

Packet format
-------------

Packets are composed of four unsigned 8-bit integers. The first one
contains the lamp number and successive three values contain
brightness data for red, green and blue, respectively.

To put the first lamp white, you need to send the following data. Data
is given in hexadecimal format for clarity:

  00 FF FF FF

To put second lamp yellow:

  01 FF FF 00

First lamp is assumed to be in DMX address of 1 and the second is at
address 6. The assumptions are done in UDPCommander.hs.

Remember: Only one command per packet is allowed. This may change in
future but now it works this way.

Testing
-------

Before your own implementation is ready, you are free to try sending
data with netcat to see how it works:

nc -w 0 -u server_address 9909 <udp-example/0-orchid.dat
nc -w 0 -u server_address 9909 <udp-example/1-yellow.dat

Now your house is filled with funky colors. \o/

Limitations
-----------

Due to DMX limitations, feasible update frequencies are about 20
commands per seconds. If you send commands too fast, no real DMX
action is performed until the next DMX update pass (or whatever it is
called). If your lights look a bit glitchy, try to increase update
interval.

DMX channels are hard-coded in UDPCommander.hs. Feel free to rewrite
my code to support configuration files. :-)

Security
--------

None. Use firewalls for port 9909 if you want to protect
yourself. This can be improved if there is any need.
