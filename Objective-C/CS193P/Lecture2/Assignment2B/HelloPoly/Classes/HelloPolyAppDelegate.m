//
//  HelloPolyAppDelegate.m
//  HelloPoly
//
//  Created by Paul Barry on 8/20/09.
//  Copyright GroupSite.com 2009. All rights reserved.
//

#import "HelloPolyAppDelegate.h"

@implementation HelloPolyAppDelegate

@synthesize window;


- (void)applicationDidFinishLaunching:(UIApplication *)application {    

    // Override point for customization after application launch
    [window makeKeyAndVisible];
}


- (void)dealloc {
    [window release];
    [super dealloc];
}


@end
