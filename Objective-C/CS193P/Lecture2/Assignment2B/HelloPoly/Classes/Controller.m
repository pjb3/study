//
//  Controller.m
//  HelloPoly
//
//  Created by Paul Barry on 8/20/09.
//  Copyright 2009 GroupSite.com. All rights reserved.
//

#import "Controller.h" 
@implementation Controller 

- (void) updateInterface {
	if(polygonShape.numberOfSides >= polygonShape.maximumNumberOfSides) {
		increaseButton.enabled = NO;
	} else {
		increaseButton.enabled = YES;
	}
	if(polygonShape.numberOfSides <= polygonShape.minimumNumberOfSides) {
		decreaseButton.enabled = NO;
	} else {
		decreaseButton.enabled = YES;
	}
	numberOfSidesLabel.text = [NSString stringWithFormat: @"%d", polygonShape.numberOfSides];
}

- (void) awakeFromNib {
	polygonShape.minimumNumberOfSides = 3;
	polygonShape.maximumNumberOfSides = 12;
	polygonShape.numberOfSides = numberOfSidesLabel.text.integerValue;
	NSLog(@"My polygon: %@", polygonShape);
}

- (IBAction)increase { 
	polygonShape.numberOfSides++;
	[self updateInterface];
} 
- (IBAction)decrease { 
    polygonShape.numberOfSides--;
	[self updateInterface];
} 
@end