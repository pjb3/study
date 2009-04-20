//
//  HelloUserAppDelegate.h
//  HelloUser
//
//  Created by Paul Barry on 4/20/09.
//  Copyright BrowserMedia 2009. All rights reserved.
//

#import <UIKit/UIKit.h>

@class HelloUserViewController;

@interface HelloUserAppDelegate : NSObject <UIApplicationDelegate> {
    UIWindow *window;
    HelloUserViewController *viewController;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) IBOutlet HelloUserViewController *viewController;

@end

