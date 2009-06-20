//
//  RowControlsController.m
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "RowControlsController.h"

@implementation RowControlsController
@synthesize list;

- (void)viewDidLoad { 
  self.list = [[NSArray alloc]  initWithObjects:@"R2- D2", 
               @"C3PO",  @"Tik- Tok", @"Robby", @"Rosie", @"Uniblab", 
               @"Bender", @"Marvin", @"Lt. Commander Data", 
               @"Evil Brother Lore", @"Optimus Prime", @"Tobor", @"HAL", 
               @"Orgasmatron", nil]; 
  [super viewDidLoad]; 
}

- (void) dealloc {
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
  static NSString *cellId = @"RowControlsControllerCellId"; 
  UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier: cellId]; 
    if (cell == nil) { 
    cell = [[[UITableViewCell alloc] initWithFrame: CGRectZero 
                                   reuseIdentifier: cellId] autorelease]; 
    UISwitch *switchView = [[UISwitch alloc] init]; 
    cell.accessoryView = switchView; 
    [switchView release]; 
  } 
  NSString *rowTitle = [list objectAtIndex: [indexPath row]]; 
  cell.text = rowTitle; 
  return cell; 
} 

#pragma mark - 
#pragma mark Table Delegate Methods 
- (void) tableView:(UITableView *)tableView 
    didSelectRowAtIndexPath:(NSIndexPath *)indexPath { 
  NSUInteger row = [indexPath row]; 
  UITableViewCell *cell = [tableView cellForRowAtIndexPath: indexPath]; 
  UISwitch *switchView = (UISwitch *)cell.accessoryView;
  NSString *baseString = @"%@ %@."; 
  NSString *onString = (switchView.on) ? @"IS on" : @"IS NOT on"; 
  NSString *robot = [list objectAtIndex: row]; 
  NSString *messageString = [NSString stringWithFormat:baseString, robot, onString];
  UIAlertView *alert = [[UIAlertView alloc] 
                            initWithTitle: @"Row Selected." 
                                  message: messageString 
                                 delegate: nil 
                        cancelButtonTitle: @"Thanks!" 
                        otherButtonTitles: nil]; 
  [alert show]; 
  [alert release];
}

@end
