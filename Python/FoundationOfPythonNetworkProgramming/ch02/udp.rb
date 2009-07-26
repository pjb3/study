#!/usr/bin/env ruby
require 'socket'

socket = UDPSocket.new
socket.connect ARGV[0], ARGV[1]

puts "Enter data to transmit: "
data = STDIN.gets.chomp

socket.send data, 0
puts "Looking for replies; press Ctrl-C or Ctrl-Break to stop." 

loop do
  buf = socket.recvfrom(2048)
  puts buf.first
end