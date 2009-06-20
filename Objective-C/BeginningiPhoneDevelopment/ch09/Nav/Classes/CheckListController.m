//
//  CheckListController.m
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "CheckListController.h"

@implementation CheckListController
@synthesize list, lastIndexPath;

- (void) dealloc {
  [list release];
  [lastIndexPath release];
  [super dealloc];
}

- (void) viewDidLoad {
  self.list = [[NSArray alloc] initWithObjects:@"Who Hash", 
               @"Bubba Gump Shrimp Étouffée", @"Who Pudding", @"Scooby Snacks", 
               @"Everlasting Gobstopper", @"Green Eggs and Ham", @"Soylent Green", 
               @"Hard Tack", @"Lembas Bread",  @"Roast Beast", @"Blancmange", nil]; 
  [super viewDidLoad];  
}

#pragma mark -
#pragma mark Table Data Source Methods
- (NSInteger) tableView:(UITableView *)tableView 
    numberOfRowsInSection:(NSInteger)section { 
  return [list count]; 
}

- (UITableViewCell *)tableView:(UITableView *)tableView 
         cellForRowAtIndexPath:(NSIndexPath *)indexPath { 
  static NSString *cellId = @"CheckMarkCellIdentifier"; 
  UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier: cellId]; 
  
  if (cell == nil) { 
    cell = [[[UITableViewCell alloc] initWithFrame: CGRectZero 
                                   reuseIdentifier: cellId] autorelease]; 
  } 
  
  NSUInteger row = [indexPath row]; 
  NSUInteger oldRow = [lastIndexPath row];

  cell.text = [list objectAtIndex: row]; 
  cell.accessoryType = (row == oldRow && lastIndexPath != nil) ? 
    UITableViewCellAccessoryCheckmark :  
    UITableViewCellAccessoryNone; 
  
  return cell;
}
  
#pragma mark - 
#pragma mark Table Delegate Methods 
- (void) tableView:(UITableView *)tableView 
    didSelectRowAtIndexPath:(NSIndexPath *) indexPath { 
  
  int newRow = [indexPath row];
  int oldRow = (lastIndexPath != nil) ? [lastIndexPath row] : -1; 
  
  if (newRow != oldRow) { 
    UITableViewCell *newCell = [tableView cellForRowAtIndexPath: indexPath]; 
    newCell.accessoryType = UITableViewCellAccessoryCheckmark; 

    UITableViewCell *oldCell = [tableView cellForRowAtIndexPath: lastIndexPath]; 
    oldCell.accessoryType = UITableViewCellAccessoryNone; 
      
    lastIndexPath = indexPath; 
  } 
  [tableView deselectRowAtIndexPath: indexPath animated:YES]; 
}

@end
