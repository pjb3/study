#!/usr/bin/env ruby
# Traditional Message Generation with Date and Message-ID -- Chapter 9 
# trad_gen_newhdrs.prb

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
msg.date = Time.now
msg.message_id = TMail.new_message_id

puts msg.to_s
