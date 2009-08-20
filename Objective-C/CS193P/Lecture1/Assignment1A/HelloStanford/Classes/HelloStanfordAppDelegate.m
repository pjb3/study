//
//  HelloStanfordAppDelegate.m
//  HelloStanford
//
//  Created by Paul Barry on 8/19/09.
//  Copyright GroupSite.com 2009. All rights reserved.
//

#import "HelloStanfordAppDelegate.h"

@implementation HelloStanfordAppDelegate

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
