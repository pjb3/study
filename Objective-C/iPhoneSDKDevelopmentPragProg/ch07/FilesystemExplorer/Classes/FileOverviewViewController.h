//
//  FileOverviewViewController.h
//  FilesystemExplorer
//
//  Created by Paul Barry on 4/24/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface FileOverviewViewController : UIViewController {
  NSString *filePath;
  IBOutlet UILabel *fileNameLabel;
  IBOutlet UILabel *fileSizeLabel;
  IBOutlet UILabel *fileCreatedLabel;
  IBOutlet UILabel *fileModifiedLabel;
}

@property (nonatomic, retain) NSString *filePath;

- (void) updateFileOverview;
- (IBAction) readFileContents;

@end
