#!/usr/bin/env python 
# UDP Echo Server - Chapter 3 - udpechoserver.py 
import socket, traceback, time 

host = '127.0.0.1'                               # Bind to all interfaces 
port = 51423 

s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM) 
s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1) 
s.bind((host, port)) 

while 1: 
    try: 
        message, address = s.recvfrom(8192) 
        print "Got message '%s' from %s" % (message, address)

        # Echo it back 
        s.sendto(message, address) 
        print "Sent response to '%s'" % (address,)

    except (KeyboardInterrupt, SystemExit): 
        raise 
    except: 
        traceback.print_exc() 
