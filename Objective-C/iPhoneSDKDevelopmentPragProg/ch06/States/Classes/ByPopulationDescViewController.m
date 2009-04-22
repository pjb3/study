//
//  ByPopulationDescViewController.m
//  States
//
//  Created by Paul Barry on 4/22/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "ByPopulationDescViewController.h"
#import "State.h"

@implementation ByPopulationDescViewController

- (void) loadStates {
  self.states = [State allStatesSortedByProperty: @"population" ascending: NO];
}

@end