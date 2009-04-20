//
//  BasketbalTeamsAppDelegate.h
//  BasketbalTeams
//
//  Created by Paul Barry on 4/20/09.
//  Copyright BrowserMedia 2009. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface BasketbalTeamsAppDelegate : NSObject <UIApplicationDelegate> {
    
    UIWindow *window;
    UINavigationController *navigationController;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) IBOutlet UINavigationController *navigationController;

@end

