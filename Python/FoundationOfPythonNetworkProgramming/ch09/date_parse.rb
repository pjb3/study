#!/usr/bin/env ruby 
# Traditional Message Parsing -- Chapter 9 
# trad_parse.rb
 
require 'rubygems'
require 'tmail'

msg = TMail::Mail.load(ARGV[1] || 'message.txt')

puts "The message was sent at #{msg.date.strftime('%A, %B %d %Y at %I:%M %p')}"