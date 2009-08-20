//
//  PolygonShape.h
//  WhatATool
//
//  Created by Paul Barry on 8/19/09.
//  Copyright 2009 GroupSite.com. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface PolygonShape : NSObject {
	int numberOfSides;
	int minimumNumberOfSides;
	int maximumNumberOfSides;
}

@property int numberOfSides;
@property int minimumNumberOfSides;
@property int maximumNumberOfSides;
@property (readonly) float angleInDegrees;
@property (readonly) float angleInRadians;
@property (readonly) NSString *name;

- (id)initWithNumberOfSides:(int)sides minimumNumberOfSides:(int)min 
	   maximumNumberOfSides:(int)max; 

- (id)init;

@end
