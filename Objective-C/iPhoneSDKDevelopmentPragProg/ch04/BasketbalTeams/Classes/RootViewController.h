//
//  RootViewController.h
//  BasketbalTeams
//
//  Created by Paul Barry on 4/20/09.
//  Copyright BrowserMedia 2009. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface RootViewController : UITableViewController {
  NSMutableArray *teams;
  UIBarButtonItem *addButtonItem;
  IBOutlet UITableViewCell *teamCell;
}

@property (nonatomic, retain) NSMutableArray *teams;
@property (nonatomic, retain) UIBarButtonItem *addButtonItem;

- (IBAction) addButtonWasPressed;
- (void) addTeamNamed:(NSString *)teamName;

@end
