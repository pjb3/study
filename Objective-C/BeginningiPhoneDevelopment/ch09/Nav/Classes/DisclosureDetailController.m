//
//  DisclosureDetailController.m
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "DisclosureDetailController.h"

@implementation DisclosureDetailController
@synthesize label, message;

- (void) viewWillAppear:(BOOL)animated {
  label.text = message;
  [super viewWillAppear: animated];
}

- (void)dealloc {
  [label release];
  [message release];
  [super dealloc];
}


@end
