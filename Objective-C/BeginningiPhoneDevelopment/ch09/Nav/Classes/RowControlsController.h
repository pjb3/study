//
//  RowControlsController.h
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "SecondLevelViewController.h"

@interface RowControlsController : SecondLevelViewController 
    <UITableViewDelegate, UITableViewDataSource> {
  NSArray *list;
}

@property (nonatomic, retain) NSArray *list;

@end
