//
//  Foo.m
//  RandomApp
//
//  Created by Paul Barry on 3/8/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "Foo.h"

@implementation Foo

- (IBAction)generate:(id)sender {
  // Generate a number between 1 and 100 inclusive
  int generated;
  generated = (random() % 100) + 1;
  
  NSLog(@"generated = %d", generated);
  
  // Ask the text field to change what it is displaying
  [textField setIntValue:generated];
}

- (IBAction)seed:(id)sender {
  // Seed the random number generator with the time
  srandom(time(NULL));
  [textField setStringValue:@"Generator seeded"];
}

- (void)awakeFromNib {
  NSCalendarDate *now;
  now = [NSCalendarDate calendarDate];
  [textField setObjectValue:now];
}

@end
