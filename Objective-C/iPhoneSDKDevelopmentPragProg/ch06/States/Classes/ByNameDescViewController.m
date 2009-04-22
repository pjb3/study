//
//  ByNameDescViewController.m
//  States
//
//  Created by Paul Barry on 4/22/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "ByNameDescViewController.h"
#import "State.h"

@implementation ByNameDescViewController

- (void) loadStates {
  self.states = [State allStatesSortedByProperty: @"name" ascending: NO];
}

@end
