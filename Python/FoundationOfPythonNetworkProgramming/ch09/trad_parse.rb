#!/usr/bin/env ruby 
# Traditional Message Parsing -- Chapter 9 
# trad_parse.rb
 
require 'rubygems'
require 'tmail'

msg = TMail::Mail.load(ARGV[1] || 'message.txt')
 
puts " *** Headers in message: " 
msg.each_header do |header, value|
  puts "#{header}:\n  #{value}"
end

abort "This program cannot handle MIME multipart messages; exiting." if msg.multipart?

puts "-" * 78 
if s = msg.header_string('subject')
  puts "Subject: #{s}" 
  puts "-" * 78 
end

puts "Message Body:\n#{msg.body}"
