//
//  DVDCaseAppDelegate.m
//  DVDCase
//
//  Created by Paul Barry on 4/22/09.
//  Copyright BrowserMedia 2009. All rights reserved.
//

#import "DVDCaseAppDelegate.h"
#import "RootViewController.h"


@implementation DVDCaseAppDelegate

@synthesize window;
@synthesize navigationController;


- (void)applicationDidFinishLaunching:(UIApplication *)application {
	
	// Configure and show the window
	[window addSubview:[navigationController view]];
	[window makeKeyAndVisible];
}


- (void)applicationWillTerminate:(UIApplication *)application {
	// Save data if appropriate
}


- (void)dealloc {
	[navigationController release];
	[window release];
	[super dealloc];
}

@end
