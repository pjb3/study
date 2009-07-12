#
#  TodoController.rb
#  Todo
#
#  Created by Paul Barry on 7/12/09.
#  Copyright (c) 2009 __MyCompanyName__. All rights reserved.
#

class TodoController < NSWindowController

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
