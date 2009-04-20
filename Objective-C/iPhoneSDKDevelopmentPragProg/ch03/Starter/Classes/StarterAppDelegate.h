//
//  StarterAppDelegate.h
//  Starter
//
//  Created by Paul Barry on 4/20/09.
//  Copyright BrowserMedia 2009. All rights reserved.
//

#import <UIKit/UIKit.h>

@class StarterViewController;

@interface StarterAppDelegate : NSObject <UIApplicationDelegate> {
    UIWindow *window;
    StarterViewController *viewController;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) IBOutlet StarterViewController *viewController;

@end

