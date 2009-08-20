//
//  Controller.h
//  HelloPoly
//
//  Created by Paul Barry on 8/20/09.
//  Copyright 2009 GroupSite.com. All rights reserved.
//

#import <UIKit/UIKit.h> 
#import <Foundation/Foundation.h> 
#import "PolygonShape.h"

@interface Controller : NSObject
{ 
    IBOutlet UIButton *decreaseButton; 
    IBOutlet UIButton *increaseButton; 
    IBOutlet UILabel *numberOfSidesLabel; 
	IBOutlet PolygonShape *polygonShape;
} 
- (IBAction)decrease; 
- (IBAction)increase; 
@end 
