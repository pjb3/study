//
//  ByAreaDescViewController.m
//  States
//
//  Created by Paul Barry on 4/22/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "ByAreaDescViewController.h"
#import "State.h"

@implementation ByAreaDescViewController

- (void) loadStates {
  self.states = [State allStatesSortedByProperty: @"area" ascending: NO];
}

@end
