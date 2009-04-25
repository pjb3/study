//
//  FilesystemExplorerAppDelegate.h
//  FilesystemExplorer
//
//  Created by Paul Barry on 4/24/09.
//  Copyright BrowserMedia 2009. All rights reserved.
//

#import <UIKit/UIKit.h>

@class DirectoryViewController;

@interface FilesystemExplorerAppDelegate : NSObject <UIApplicationDelegate> {
    
  UIWindow *window;
  UINavigationController *navigationController;
  DirectoryViewController *directoryViewController;  
  
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) IBOutlet UINavigationController *navigationController;
@property (nonatomic, retain) IBOutlet DirectoryViewController *directoryViewController;

@end

