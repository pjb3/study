//
//  ByNameViewController.m
//  States
//
//  Created by Paul Barry on 4/22/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "ByNameViewController.h"
#import "State.h"

@implementation ByNameViewController

- (void) loadStates {
  self.states = [State allStatesSortedByProperty: @"name" ascending: YES];
}

@end