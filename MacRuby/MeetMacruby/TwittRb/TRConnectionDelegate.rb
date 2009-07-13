class TRConnectionDelegate

  def initialize(parent, &block)
    @parent = parent
    @block = block
  end

  def connection(connection, didReceiveResponse:response)
    case response.statusCode
    when 401
      @block.call("Invalid username and password")
    when (400..500)
      @block.call("Unable to complete your request")
    end
  end

  def connection(connection, didReceiveData:data)
    @receivedData ||= NSMutableData.new
    @receivedData.appendData(data)
  end

  def connection(conn, didFailWithError:error)
    @parent.send_button.enabled = true
    @parent.status_label.stringValue = "Error sending update"
  end

  def connectionDidFinishLoading(connection)
    doc = NSXMLDocument.alloc.initWithData(@receivedData,
      options: NSXMLDocumentValidate,
      error: nil)
    
    if doc
      statuses = doc.nodesForXPath("*/status|status", error: nil)
      tweets = statuses.map do |s|
        {
          :user => s.nodesForXPath('user/name', error: nil).first.stringValue,
          :tweet => s.nodesForXPath('text', error: nil).first.stringValue
        }
      end
      @block.call(tweets)
    else
      @block.call("Invalid response")
    end
    
  end

end
