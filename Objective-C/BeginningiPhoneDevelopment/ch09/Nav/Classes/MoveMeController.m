//
//  MoveMeController.m
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "MoveMeController.h"

@implementation MoveMeController
@synthesize list;

- (IBAction) toggleMove {
  [self.tableView setEditing: !self.tableView.editing animated: YES];
}

- (void)viewDidLoad { 
  self.list = [[NSMutableArray alloc] initWithObjects: 
               @"Eeny", @"Meeny", @"Miney", @"Moe", @"Catch", @"A", 
               @"Tiger", @"By", @"The", @"Toe", nil]; 

  self.navigationItem.rightBarButtonItem = [[UIBarButtonItem alloc] 
                                            initWithTitle: @"Move" 
                                                    style: UIBarButtonItemStyleBordered 
                                                   target: self 
                                                   action: @selector(toggleMove)]; 
  [super viewDidLoad]; 
} 

- (void)dealloc {
  [list release];
  [super dealloc];
}

#pragma mark - 
#pragma mark Table Data Source Methods 
- (NSInteger) tableView:(UITableView *)tableView 
   numberOfRowsInSection:(NSInteger)section { 
  return [list count]; 
} 

- (UITableViewCell *) tableView:(UITableView *)tableView 
          cellForRowAtIndexPath:(NSIndexPath *)indexPath { 
  static NSString *cellId = @"MoveMeCellIdentifier"; 
  UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier: cellId]; 
  if (cell == nil) { 
    cell = [[[UITableViewCell alloc] initWithFrame: CGRectZero 
                                   reuseIdentifier: cellId] autorelease]; 
    cell.showsReorderControl = YES; 
  } 
  cell.text = [list objectAtIndex: [indexPath row]]; 
  return cell; 
}

- (UITableViewCellEditingStyle) tableView:(UITableView *)tableView 
            editingStyleForRowAtIndexPath:(NSIndexPath *)indexPath { 
  return UITableViewCellEditingStyleNone; 
}

- (BOOL)tableView:(UITableView *)tableView 
    canMoveRowAtIndexPath:(NSIndexPath *)indexPath { 
  return YES; 
} 

- (void) tableView:(UITableView *)tableView 
moveRowAtIndexPath:(NSIndexPath *)fromIndexPath 
       toIndexPath:(NSIndexPath *)toIndexPath { 
  NSUInteger fromRow = [fromIndexPath row]; 
  NSUInteger toRow = [toIndexPath row]; 
  id object = [[list objectAtIndex:fromRow] retain]; 
  [list removeObjectAtIndex:fromRow]; 
  [list insertObject:object atIndex:toRow]; 
  [object release]; 
}

@end
