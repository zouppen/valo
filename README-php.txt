AJAX interface
==============

Configuration
-------------

1) Copy or link php/ subdirectory to your public WWW directory.
2) In php/ directory: cp config.php.example config.php
3) Edit config.php to suit your needs.

That's it!

Usage
-----

HTTP request is formed of the following GET parameters:

 i = index of the light source, either 0 or 1 on my hardware.
 r = red component channel, 0-255
 g = green ...
 b = blue ...

To test this, navigate to your PHP script with your browser. You can
try the following (remember to change server name and resource path):

http://www.example.com/lights.php?i=0&r=255&g=0&b=120

You can use XMLHttpRequest to change colors in your funky Web 2.0
application! Even wget works, too :-)

Please note: This implementation lacks support for multiple commands
in one request. Feel free to improve PHP script. Well, for any serious
light control, use UDP. :-)
