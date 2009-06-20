//
//  DeleteMeController.m
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "DeleteMeController.h"

@implementation DeleteMeController
@synthesize list;

-(IBAction)toggleEdit:(id)sender { 
  [self.tableView setEditing:!self.tableView.editing animated:YES]; 
}

- (void)viewDidLoad { 
  NSString *path = [[NSBundle mainBundle] pathForResource:@"computers" ofType:@"plist"]; 

  self.list = [[NSMutableArray alloc] initWithContentsOfFile:path]; 

  self.navigationItem.rightBarButtonItem = [[UIBarButtonItem alloc] 
                                            initWithTitle: @"Delete" 
                                                    style: UIBarButtonItemStyleBordered 
                                                   target: self 
                                                   action: @selector(toggleEdit:)];
  [super viewDidLoad]; 
}

- (void)dealloc {
  [list release];
  [super dealloc];
}

#pragma mark - 
#pragma mark Table Data Source Methods 
- (NSInteger) tableView:(UITableView *) tableView 
    numberOfRowsInSection:(NSInteger)section { 
  return [list count]; 
} 

- (UITableViewCell *) tableView:(UITableView *) tableView 
          cellForRowAtIndexPath:(NSIndexPath *) indexPath { 
  static NSString *cellId = @"DeleteMeCellIdentifier"; 
  UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier: cellId]; 
  
  if (cell == nil) { 
    cell = [[[UITableViewCell alloc] initWithFrame: CGRectZero 
                                   reuseIdentifier: cellId] autorelease];
  } 

  cell.text = [self.list objectAtIndex: [indexPath row]]; 
  return cell; 
}

#pragma mark - 
#pragma mark Table View Data Source Methods 
- (void) tableView:(UITableView *)tableView 
commitEditingStyle:(UITableViewCellEditingStyle) editingStyle 
forRowAtIndexPath:(NSIndexPath *)indexPath { 
  NSUInteger row = [indexPath row]; 
  [self.list removeObjectAtIndex:row]; 
  [tableView deleteRowsAtIndexPaths: [NSArray arrayWithObject: indexPath] 
                   withRowAnimation: UITableViewRowAnimationFade]; 
}

@end
