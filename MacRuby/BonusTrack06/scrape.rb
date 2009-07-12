framework "Cocoa"
url = NSURL.URLWithString("http://macruby.org")
s = String.stringWithContentsOfURL(url, encoding: NSUTF8StringEncoding, error: nil)
puts s