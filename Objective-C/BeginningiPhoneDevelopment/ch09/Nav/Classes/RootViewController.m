//
//  RootViewController.m
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "RootViewController.h"
#import "SecondLevelViewController.h"
#import "DisclosureButtonController.h"
#import "CheckListController.h"
#import "RowControlsController.h"
#import "NavAppDelegate.h"

@implementation RootViewController
@synthesize controllers;

- (void) addController:(NSString *)controllerClassName
             withTitle:(NSString *)title
          andImageName:(NSString *)imageName
               toArray:(NSMutableArray *)array {
  SecondLevelViewController *controller = 
    [[NSClassFromString(controllerClassName) alloc] initWithStyle: UITableViewStyleGrouped];
  controller.title = title;
  controller.rowImage = [UIImage imageNamed: imageName];
  [array addObject: controller];
  [controller release];
}

- (void) viewDidLoad {
  self.title = @"Root Level";
  NSMutableArray *array = [NSMutableArray new];
  
  [self addController: @"DisclosureButtonController"
            withTitle: @"Disclosure Buttons"
         andImageName: @"disclosureButtonControllerIcon.png"
              toArray: array];

  [self addController: @"CheckListController"
            withTitle: @"Check One"
         andImageName: @"checkmarkControllerIcon.png"
              toArray: array];
  
  [self addController: @"RowControlsController"
            withTitle: @"Row Controls"
         andImageName: @"rowControlsIcon.png"
              toArray: array];
  
  [self addController: @"MoveMeController"
            withTitle: @"Move Me"
         andImageName: @"moveMeIcon.png"
              toArray: array];  

  [self addController: @"DeleteMeController"
            withTitle: @"Delete Me"
         andImageName: @"deleteMeIcon.png"
              toArray: array];    

  [self addController: @"PresidentsViewController"
            withTitle: @"Detail Edit"
         andImageName: @"detailEditIcon.png"
              toArray: array]; 
  
  self.controllers = [NSArray arrayWithArray: array];
  [array release];
  [super viewDidLoad];
}

- (void) dealloc {
  [controllers release];
  [super dealloc];
}

#pragma mark -
#pragma mark Table Data Source Methods
- (NSInteger) tableView:(UITableView *)tableView
    numberOfRowsInSection:(NSInteger)section {
  return [self.controllers count];
}

- (UITableViewCell *) tableView:(UITableView *)tableView
          cellForRowAtIndexPath:(NSIndexPath *)indexPath {
  
  static NSString *cellId = @"RootViewControllerCell";
  
  UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier: cellId];
  if(cell == nil) {
    cell = [[[UITableViewCell alloc] initWithFrame: CGRectZero
                                   reuseIdentifier: cellId] autorelease];
  }
  
  // Configure the cell
  SecondLevelViewController *controller =
    [controllers objectAtIndex: [indexPath row]];
  cell.text = controller.title;
  cell.image = controller.rowImage;
  
  return cell;
}

#pragma mark -
#pragma mark Table View Delegate Methods
- (UITableViewCellAccessoryType) tableView:(UITableView *)tableView
            accessoryTypeForRowAtIndexPath:(NSIndexPath *)indexPath {
  return UITableViewCellAccessoryDisclosureIndicator;
}

- (void)tableView:(UITableView *)tableView 
    didSelectRowAtIndexPath:(NSIndexPath *)indexPath { 
  SecondLevelViewController *nextController = [self.controllers objectAtIndex: [indexPath row]]; 
  [self.navigationController pushViewController:nextController animated: YES];
} 

@end
