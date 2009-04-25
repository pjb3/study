//
//  FilesystemExplorerAppDelegate.m
//  FilesystemExplorer
//
//  Created by Paul Barry on 4/24/09.
//  Copyright BrowserMedia 2009. All rights reserved.
//

#import "FilesystemExplorerAppDelegate.h"
#import "DirectoryViewController.h"

@implementation FilesystemExplorerAppDelegate

@synthesize window;
@synthesize navigationController;
@synthesize directoryViewController;

- (void)applicationDidFinishLaunching:(UIApplication *)application {
  directoryViewController.directoryPath = NSHomeDirectory();
	
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
