//
//  NewsAppDelegate.m
//  News
//
//  Created by Paul Barry on 12/9/09.
//  Copyright __MyCompanyName__ 2009. All rights reserved.
//

#import "NewsAppDelegate.h"
#import "Helpers/TFAppHelpers.h"

@implementation NewsAppDelegate

@synthesize window;
@synthesize tabBarController;


- (void)applicationDidFinishLaunching:(UIApplication *)application {
    
  // Add the tab bar controller's current view as a subview of the window
  [window addSubview:tabBarController.view];

}


/*
// Optional UITabBarControllerDelegate method
- (void)tabBarController:(UITabBarController *)tabBarController didSelectViewController:(UIViewController *)viewController {
}
*/

/*
// Optional UITabBarControllerDelegate method
- (void)tabBarController:(UITabBarController *)tabBarController didEndCustomizingViewControllers:(NSArray *)viewControllers changed:(BOOL)changed {
}
*/


- (void)dealloc {
    [tabBarController release];
    [window release];
    [super dealloc];
}

@end

