//
//  DisclosureButtonController.h
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "SecondLevelViewController.h"

@class DisclosureDetailController;

@interface DisclosureButtonController : SecondLevelViewController 
    <UITableViewDelegate, UITableViewDataSource> {
  NSArray *list;
  DisclosureDetailController *childController;
}

@property (nonatomic, retain) NSArray *list;

@end
