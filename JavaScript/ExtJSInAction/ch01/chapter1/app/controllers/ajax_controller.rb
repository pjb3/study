class AjaxController < ApplicationController
  def index
    render :layout => 'extjs'
  end
  
  def page1
    sleep 1 
    @date = Time.now.strftime("%m/%d/%Y")
    @time = Time.now.strftime("%l:%M:%S %p")
    @server = request.host
  end
  
  def page2
    sleep 1
  end
  
end