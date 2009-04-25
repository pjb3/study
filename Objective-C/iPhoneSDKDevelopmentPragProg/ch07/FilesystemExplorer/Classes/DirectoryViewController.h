//
//  RootViewController.h
//  FilesystemExplorer
//
//  Created by Paul Barry on 4/24/09.
//  Copyright BrowserMedia 2009. All rights reserved.
//

#import <UIKit/UIKit.h>


@interface DirectoryViewController : UITableViewController <UIActionSheetDelegate> {
  
  NSString *directoryPath;
  NSArray *directoryContents;
  
}

@property (nonatomic, retain) NSString *directoryPath;

- (void) showAddOptions;
- (void) createNewFile;
- (void) createNewDirectory;
- (void) loadDirectoryContents;

@end
