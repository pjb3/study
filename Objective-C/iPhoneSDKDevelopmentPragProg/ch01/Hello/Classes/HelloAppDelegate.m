//
//  HelloAppDelegate.m
//  Hello
//
//  Created by Paul Barry on 4/20/09.
//  Copyright BrowserMedia 2009. All rights reserved.
//

#import "HelloAppDelegate.h"
#import "HelloViewController.h"

@implementation HelloAppDelegate

@synthesize window;
@synthesize viewController;


- (void)applicationDidFinishLaunching:(UIApplication *)application {    
    
    // Override point for customization after app launch    
    [window addSubview:viewController.view];
    [window makeKeyAndVisible];
}


- (void)dealloc {
    [viewController release];
    [window release];
    [super dealloc];
}


@end
