#!/usr/bin/env ruby
require 'socket'

host = '127.0.0.1'
port = 51423

socket = UDPSocket.new
socket.bind(host, port)
loop do
  message, address = socket.recvfrom(8192)
  puts "Got message '#{message}' from '#{address[3]}'"
  
  socket.send(message 0, address[3], address[1])
  puts "Sent message to #{address[3]}:#{address[1]}"
end
