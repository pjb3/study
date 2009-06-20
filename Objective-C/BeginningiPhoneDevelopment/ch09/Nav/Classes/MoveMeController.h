//
//  MoveMeController.h
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "SecondLevelViewController.h"

@interface MoveMeController : SecondLevelViewController 
    <UITableViewDelegate, UITableViewDataSource> {
  NSMutableArray *list;
}

@property (nonatomic, retain) NSMutableArray *list;

- (IBAction) toggleMove;

@end
