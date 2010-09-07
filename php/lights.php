<?php

require("config.php");

if (!(isset($_GET['i']) &&
      isset($_GET['r']) &&
      isset($_GET['g']) && 
      isset($_GET['b']) ))
{
	echo "Parameter error. Use HTTP GET parameters i, r, g and b!";
	exit(1);
}

// Pack is byte order aware. Cool.
$bytes = pack("NNCCC",0,$_GET['i'],
	      $_GET['r'],$_GET['g'],$_GET['b']);

send_frame($bytes, $light_host, $light_port);
echo "ok.";

/**
 * Sends DMX color command to Valo lightServer.
 *
 * See also:
 * http://iki.fi/zouppen/repo/valo.git/
 *
 * @param string $command Packet data
 * @param string $host    Target host
 * @param int    $host    Target port (udp)
 */
function send_frame($buf, $host, $port=9909) {
    $socket = socket_create(AF_INET, SOCK_DGRAM, SOL_UDP);
    socket_sendto($socket, $buf, strlen($buf), 0, $host, $port);
    socket_close($socket);
}
?>
