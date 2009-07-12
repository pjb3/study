class TwittRbDelegate
  
  attr_accessor :credentials_window, :main_window
  attr_accessor :username_field, :password_field
  attr_accessor :username, :password
  
  def applicationDidFinishLaunching(notification)
    NSApp.beginSheet(credentials_window,
      modalForWindow: main_window,
      modalDelegate: nil,
      didEndSelector: nil,
      contextInfo: nil)
  end
  
  def submitCredentials(sender)
    self.username = username_field.stringValue
    self.password = password_field.stringValue
    NSApp.endSheet(credentials_window)
    credentials_window.orderOut(sender)
    NSLog "I have a #{username} as a username"
    NSLog "I have a password length of #{password.length}"
    url = NSURL.URLWithString "http://twitter.com/statuses/friends_timeline.xml"
    request = NSURLRequest.requestWithURL(url)
    @connection = NSURLConnection.connectionWithRequest(request, delegate: self)
  end
  
  def hideCredentials(sender)
    NSLog "Cancelled twitter credentials"
    NSApp.endSheet(credentials_window)
    credentials_window.orderOut(sender)      
  end
  
  def connection(connection, didReceiveResponse: response)
    NSLog "Got a response #{response.statusCode}"
  end
  
  def connection(connection, didReceiveData: data)
    @receivedData ||= NSMutableData.new
    @receivedData.appendData(data)
  end
  
  def connectionDidFinishLoading(connection)
    doc = NSXMLDocument.alloc.initWithData(@receivedData,
      options: NSXMLDocumentValidate,
      error: nil)
      
    updates = doc.rootElement.nodesForXPath('status', error: nil);
    NSLog "Received #{updates.count} updates from Twitter"  
  end
  
  def connection(conn, didReceiveAuthenticationChallenge: challenge)
    if challenge.previousFailureCount == 0 && !challenge.proposedCredential
      creds = NSURLCredential.credentialWithUser(username,
        password: password,
        persistence: NSURLCredentialPersistenceNone)
      challenge.sender.useCredential(creds,
        forAuthenticationChallenge: challenge)
    else
      NSLog "Credentials failed. :-("
    end
  end
  
end

