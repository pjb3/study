//
//  StatesViewController.h
//  States
//
//  Created by Paul Barry on 4/22/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import <UIKit/UIKit.h>


@interface StatesViewController : UITableViewController {
  NSArray *states;
}

@property (nonatomic, retain) NSArray *states;

- (void) loadStates;

@end
