//
//  ByAreaViewController.m
//  States
//
//  Created by Paul Barry on 4/22/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "ByAreaViewController.h"
#import "State.h"

@implementation ByAreaViewController

- (void) viewDidLoad {
  [super viewDidLoad];
  self.states = [State allStatesByArea];
}

@end
