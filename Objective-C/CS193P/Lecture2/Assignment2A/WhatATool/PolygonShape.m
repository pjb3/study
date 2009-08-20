//
//  PolygonShape.m
//  WhatATool
//
//  Created by Paul Barry on 8/19/09.
//  Copyright 2009 GroupSite.com. All rights reserved.
//

#import "PolygonShape.h"

#define MIN_SIDES 2
#define MAX_SIDES 12

#define DEFAULT_SIDES 5
#define DEFAULT_MIN_SIDES 3
#define DEFAULT_MAX_SIDES 10

@implementation PolygonShape

@synthesize numberOfSides, minimumNumberOfSides, maximumNumberOfSides;

- (id)initWithNumberOfSides:(int)sides minimumNumberOfSides:(int)min 
	   maximumNumberOfSides:(int)max {
	if(self = [super init]) {
		self.minimumNumberOfSides = min;
		self.maximumNumberOfSides = max;
		self.numberOfSides = sides;		
	}
	return self;
}

- (id)init {
	return [self initWithNumberOfSides: DEFAULT_SIDES  
				  minimumNumberOfSides: DEFAULT_MIN_SIDES
				  maximumNumberOfSides: DEFAULT_MAX_SIDES];
}

- (void) setMinimumNumberOfSides:(int) theMinimumNumberOfSides {
	if(theMinimumNumberOfSides > MIN_SIDES) {
		minimumNumberOfSides = theMinimumNumberOfSides;
	} else {
		NSLog(@"The minimum number of sides must be greater than %d", MIN_SIDES);
	}
}

- (void) setMaximumNumberOfSides:(int) theMaximumNumberOfSides {
	if(theMaximumNumberOfSides <= MAX_SIDES) {
		maximumNumberOfSides = theMaximumNumberOfSides;
	} else {
		NSLog(@"he maximum number of sides must be less than or equal to %d", MAX_SIDES);
	}
}

- (void) setNumberOfSides:(int)theNumberOfSides {
	if(theNumberOfSides < self.minimumNumberOfSides) {
		NSLog(@"Invalid number of sides: %d is less than the minimum of %d allowed", 
			  theNumberOfSides, self.minimumNumberOfSides);
	} else if(theNumberOfSides > self.maximumNumberOfSides) {
		NSLog(@"Invalid number of sides: %d is greater than the maximum of %d allowed", 
			  theNumberOfSides, self.maximumNumberOfSides);		
	} else {
		numberOfSides = theNumberOfSides;
	}
	
}

- (float) angleInDegrees {
	return (180.0f * (self.numberOfSides - 2)) / self.numberOfSides;
}

- (float) angleInRadians {
	return self.angleInDegrees * M_PI / 180.0f;
}

- (NSString *) name {
	switch (numberOfSides) {
		case 3: return @"Triangle";
		case 4: return @"Quadrilateral";
		case 5: return @"Pentagon";
		case 6: return @"Hexagon";
		case 7: return @"Heptagon";
		case 8: return @"Octagon";
		case 9: return @"Nonagon";
		case 10: return @"Decagon";
		case 11: return @"Hendecagon";
		case 12: return @"Dodecagon";
		default: return @"Polygon";
	}
}

- (NSString *) description {
	return [NSString stringWithFormat: @"Hello, I am a %d-sided polygon (aka a %@) with angles of %f degrees (%f radians).", 
		self.numberOfSides,
		self.name,
		self.angleInDegrees,
		self.angleInRadians];
}

- (void) dealloc {
	NSLog(@"Deallocing");
	[super dealloc];
}

@end
