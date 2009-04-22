//
//  StatesAppDelegate.m
//  States
//
//  Created by Paul Barry on 4/22/09.
//  Copyright BrowserMedia 2009. All rights reserved.
//

#import "StatesAppDelegate.h"
#import "State.h"

@implementation StatesAppDelegate

@synthesize window;
@synthesize tabBarController;

- (void)applicationDidFinishLaunching:(UIApplication *)application {
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

