//
//  StatesViewController.m
//  States
//
//  Created by Paul Barry on 4/22/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "StatesViewController.h"

@implementation StatesViewController

@synthesize states;

- (void) viewDidLoad {
  [super viewDidLoad];
  [self loadStates];
}

- (void) loadStates {
  //Override in subclasses
}

#pragma mark -
#pragma mark Table View Methods

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 1;
}


// Customize the number of rows in the table view.
- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return states.count;
}


// Customize the appearance of table view cells.
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    
  static NSString *CellIdentifier = @"Cell";
  
  UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
  if (cell == nil) {
      cell = [[[UITableViewCell alloc] initWithFrame:CGRectZero reuseIdentifier:CellIdentifier] autorelease];
  }
  
  cell.text = [[states objectAtIndex:indexPath.row] name];

  return cell;
}


#pragma mark -

- (void)dealloc {
    [super dealloc];
}

@end

