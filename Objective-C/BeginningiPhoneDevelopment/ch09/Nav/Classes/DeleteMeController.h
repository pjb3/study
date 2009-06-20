//
//  DeleteMeController.h
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "SecondLevelViewController.h"

@interface DeleteMeController : SecondLevelViewController 
    <UITableViewDelegate, UITableViewDataSource> {
  NSMutableArray *list;
}

@property (nonatomic, retain) NSMutableArray *list;

- (IBAction) toggleEdit:(id)sender;

@end
