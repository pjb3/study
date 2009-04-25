//
//  CreateDirectoryViewController.h
//  FilesystemExplorer
//
//  Created by Paul Barry on 4/24/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import <UIKit/UIKit.h>

@class DirectoryViewController;

@interface CreateDirectoryViewController : UIViewController <UITextFieldDelegate> {
	NSString *parentDirectoryPath;
	DirectoryViewController *directoryViewController;  
  IBOutlet UITextField *directoryNameField;
  NSString *newDirectoryPath;
}

@property (nonatomic, retain) NSString *parentDirectoryPath;
@property (nonatomic, retain) DirectoryViewController *directoryViewController;

- (IBAction) save;
- (BOOL) validate;

@end
