//
//  ByPopulationViewController.m
//  States
//
//  Created by Paul Barry on 4/22/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "ByPopulationViewController.h"
#import "State.h"

@implementation ByPopulationViewController

- (void) loadStates {
  self.states = [State allStatesSortedByProperty: @"population" ascending: YES];
}

@end

