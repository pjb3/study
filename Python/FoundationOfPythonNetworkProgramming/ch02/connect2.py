#!/usr/bin/env python 
# Revised Connection Example - Chapter 2 - connect2.py 
# I'm hoping there are at least 4 connect examples in this chapter,
# so we can have a connect4.py

import socket 

print "Creating socket...", 
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM) 
print "done." 

print "Looking up port number...", 
port = socket.getservbyname('http', 'tcp') 
print "done." 

print "Connecting to remote host on port %d..." % port, 
s.connect(("www.google.com", port)) 
print "done." 
