//
//  DisclosureButtonController.m
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "DisclosureButtonController.h"
#import "DisclosureDetailController.h"

@implementation DisclosureButtonController
@synthesize list;

- (void) viewDidLoad {
  self.list = [[NSArray alloc] initWithObjects: @"Toy Story",
    @"A Bug's Life", @"Toy Story 2", @"Monsters, Inc.",
    @"Finding Nemo", @"The Incredibles", @"Cars",
    @"Ratatouille", @"WALL-E", @"Up", nil];
  [super viewDidLoad];
}

- (void) dealloc {
  [list release];
  [super dealloc];
}

#pragma mark -
#pragma mark Table Data Source Methods
- (NSInteger)tableView:(UITableView *)tableView 
    numberOfRowsInSection:(NSInteger)section { 
  return [list count]; 
} 

- (UITableViewCell *)tableView:(UITableView *)tableView 
         cellForRowAtIndexPath:(NSIndexPath *)indexPath { 
  static NSString *cellId = @"DisclosureButtonCellIdentifier"; 
  
  UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier: cellId];
  
  if(cell == nil) { 
    cell = [[[UITableViewCell alloc] initWithFrame: CGRectZero 
                                   reuseIdentifier: cellId] autorelease]; 
  } 
  cell.text = [list objectAtIndex: [indexPath row]]; 
  
  return cell; 
}

#pragma mark - 
#pragma mark Table Delegate Methods 
- (UITableViewCellAccessoryType)tableView:(UITableView *)tableView 
         accessoryTypeForRowWithIndexPath:(NSIndexPath *)indexPath { 
  return UITableViewCellAccessoryDetailDisclosureButton; 
} 

- (void)tableView:(UITableView *)tableView 
    didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
  UIAlertView *alert = [[UIAlertView alloc] initWithTitle: @"Hey, do you see the disclosure button?" 
                                                  message: @"If you're trying to drill down, touch that instead" 
                                                 delegate: nil 
                                        cancelButtonTitle: @"Won't happen again" 
                                        otherButtonTitles: nil]; 
  [alert show]; 
  [alert release]; 
}

- (void)tableView:(UITableView *)tableView 
    accessoryButtonTappedForRowWithIndexPath:(NSIndexPath *)indexPath { 

  if (childController == nil) {
    childController = [[DisclosureDetailController alloc] 
                       initWithNibName:@"DisclosureDetail" bundle:nil];    
  }
  childController.title = @"Disclosure Button Pressed";
  
  NSString *selectedMovie = [list objectAtIndex: [indexPath row]]; 
  NSString *detailMessage  = [[NSString alloc] 
                              initWithFormat:@"You pressed the disclosure button for %@.", 
                              selectedMovie];
  
  childController.message = detailMessage; 
  childController.title = selectedMovie; 
  [detailMessage release]; 

  [self.navigationController pushViewController: childController animated: YES];

} 

@end
