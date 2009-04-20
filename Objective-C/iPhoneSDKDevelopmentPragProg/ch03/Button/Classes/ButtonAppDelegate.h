//
//  ButtonAppDelegate.h
//  Button
//
//  Created by Paul Barry on 4/20/09.
//  Copyright BrowserMedia 2009. All rights reserved.
//

#import <UIKit/UIKit.h>

@class ButtonViewController;

@interface ButtonAppDelegate : NSObject <UIApplicationDelegate> {
    UIWindow *window;
    ButtonViewController *viewController;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) IBOutlet ButtonViewController *viewController;

@end

