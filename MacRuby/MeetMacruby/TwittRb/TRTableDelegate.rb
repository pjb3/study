#
#  TRTableDelegate.rb
#  TwittRb
#
#  Created by Paul Barry on 7/12/09.
#  Copyright (c) 2009 __MyCompanyName__. All rights reserved.
#

class TRTableDelegate

  attr_accessor :parent
  
  def initialize
    NSLog "Creating TRTableDelegate"
  end
  
  def parent=(p)
    NSLog "parent is #{p.description}"
    @parent = p
  end
  
  def numberOfRowsInTableView(tableView)
    parent.updates.count
  end

  def tableView(tableView, objectValueForTableColumn: column, row: row)
    parent.updates[row].valueForKey(column.identifier) if row < parent.updates.length - 1
  end

end
