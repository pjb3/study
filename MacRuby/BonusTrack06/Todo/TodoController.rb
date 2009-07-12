#
#  TodoController.rb
#  Todo
#
#  Created by Paul Barry on 7/12/09.
#  Copyright (c) 2009 __MyCompanyName__. All rights reserved.
#

class TodoController < NSWindowController
  
  attr_accessor :tableView
  attr_accessor :textField
  attr_accessor :removeButton 
  
  def addItem(sender)
    @items << @textField.stringValue
    @textField.stringValue = ""
    @tableView.reloadData
  end
  
  def removeItem(sender)
    @items.delete_at(@tableView.selectedRow)
    @tableView.reloadData
    @tableView.deselectAll(self)
    @removeButton.enabled = false
  end
  
  def tableViewSelectionDidChange(notification)
    @removeButton.enabled = true
  end
  
  def awakeFromNib
    @items = ["Install MacRuby", "Explore macirb"]
  end
  
  def numberOfRowsInTableView(table)
    @items ? @items.size : 0
  end
  
  def tableView(table, objectValueForTableColumn: column, row: row)
    @items[row]
  end
  
end
