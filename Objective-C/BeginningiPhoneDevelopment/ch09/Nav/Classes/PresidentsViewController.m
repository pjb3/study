//
//  PresidentsViewController.m
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "PresidentsViewController.h"
#import "PresidentDetailController.h"
#import "President.h"

@implementation PresidentsViewController
@synthesize list;

- (void)dealloc {
  [list release];
  [super dealloc];
}

- (void)viewDidLoad { 
  NSString *path = [[NSBundle mainBundle] pathForResource:@"Presidents" ofType:@"plist"]; 
  NSData *data = [[NSData alloc] initWithContentsOfFile: path];
  NSKeyedUnarchiver *unarchiver = [[NSKeyedUnarchiver alloc] initForReadingWithData: data]; 
  self.list = [unarchiver decodeObjectForKey:@"Presidents"]; 
  [unarchiver finishDecoding]; 
  [unarchiver release]; 
  [data release]; 
  [super viewDidLoad]; 
} 

#pragma mark - 
#pragma mark Table Data Source Methods 
- (NSInteger) tableView:(UITableView *)tableView 
    numberOfRowsInSection:(NSInteger)section { 
  return [list count]; 
} 

- (UITableViewCell *) tableView:(UITableView *)tableView 
          cellForRowAtIndexPath:(NSIndexPath *)indexPath { 
  static NSString *cellId = @"PresidentListCellIdentifier"; 
  UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier: cellId]; 
  if (cell == nil) { 
    cell = [[[UITableViewCell alloc] initWithFrame: CGRectZero 
                                   reuseIdentifier: cellId] autorelease]; 
  } 
  President *president = [self.list objectAtIndex: [indexPath row]]; 
  cell.text = president.name; 
  return cell; 
} 

#pragma mark - 
#pragma mark Table Delegate Methods 
- (void)tableView:(UITableView *)tableView 
didSelectRowAtIndexPath:(NSIndexPath *)indexPath { 
  President *president = [self.list objectAtIndex: [indexPath row]]; 
  PresidentDetailController *childController = 
    [[PresidentDetailController alloc] initWithStyle:UITableViewStyleGrouped]; 
  childController.title = president.name; 
  childController.president = president; 
  [self.navigationController pushViewController: childController animated: YES];
  [childController release]; 
}

@end
