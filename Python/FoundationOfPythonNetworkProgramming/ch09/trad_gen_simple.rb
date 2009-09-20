#!/usr/bin/env ruby
# Traditional Message Generation, Simple -- Chapter 9 
# trad_gen_simple.rb 

require 'rubygems'
require 'tmail'

message = %Q{Hello, 

This is a test message from Chapter 9.  I hope you enjoy it! 

-- Anonymous}

msg = TMail::Mail.new
msg.to = 'recipient@example.com' 
msg.from = 'Test Sender <sender@example.com>' 
msg.subject = 'Test Message, Chapter 9' 
msg.body = message

puts msg.to_s
