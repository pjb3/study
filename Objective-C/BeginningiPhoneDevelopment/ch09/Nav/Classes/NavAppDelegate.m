//
//  NavAppDelegate.m
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright BrowserMedia 2009. All rights reserved.
//

#import "NavAppDelegate.h"

@implementation NavAppDelegate

@synthesize window;
@synthesize navController;

- (void)applicationDidFinishLaunching:(UIApplication *)application {    
  [window addSubview: navController.view];
  [window makeKeyAndVisible];
}

- (void)dealloc {
  [navController release];
  [window release];
  [super dealloc];
}

@end
