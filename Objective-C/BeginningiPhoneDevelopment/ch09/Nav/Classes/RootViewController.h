//
//  RootViewController.h
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface RootViewController : UITableViewController 
    <UITableViewDelegate, UITableViewDataSource> {
  NSArray *controllers;
}

@property (nonatomic, retain) NSArray *controllers;

@end
