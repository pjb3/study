//
//  FileContentsViewController.h
//  FilesystemExplorer
//
//  Created by Paul Barry on 4/24/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import <UIKit/UIKit.h>


@interface FileContentsViewController : UIViewController {
  NSString *filePath;
	IBOutlet UILabel *fileNameLabel;
	IBOutlet UILabel *fileSizeLabel;
	IBOutlet UITextView *fileContentsTextView;  
}

@property (nonatomic, retain) NSString *filePath;

@end
