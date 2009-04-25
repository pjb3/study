//
//  FileOverviewViewController.m
//  FilesystemExplorer
//
//  Created by Paul Barry on 4/24/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "FileOverviewViewController.h"
#import "FileContentsViewController.h"

@implementation FileOverviewViewController

-(NSString*) filePath {
	return filePath;
}

-(void) setFilePath: (NSString*) path {
	if (filePath != NULL) {
    [filePath release];
  }		
	filePath = [path retain];
	[self updateFileOverview];
}

-(void) updateFileOverview {
	if(self.filePath != NULL) {
		NSString *fileName = [self.filePath lastPathComponent];
		fileNameLabel.text = fileName;
    self.title = fileName;
    
		NSDateFormatter *dateFormatter = [[NSDateFormatter alloc] init];
		[dateFormatter setDateStyle:NSDateFormatterMediumStyle];
		[dateFormatter setTimeStyle:NSDateFormatterNoStyle];
		
		NSNumberFormatter *numberFormatter =
		[[NSNumberFormatter alloc] init];
		[numberFormatter setPositiveFormat: @"#,##0.## bytes"];
		
		NSDictionary *fileAttributes = [[NSFileManager defaultManager]
      fileAttributesAtPath: self.filePath traverseLink: YES];
    
		NSDate *creationDate = (NSDate*)
      [fileAttributes objectForKey: NSFileCreationDate];
		NSDate *modificationDate = (NSDate*)
      [fileAttributes objectForKey: NSFileModificationDate];
		NSNumber *fileSize = (NSNumber*)
      [fileAttributes objectForKey: NSFileSize];
    
		fileSizeLabel.text = [numberFormatter stringFromNumber: fileSize];
		fileCreatedLabel.text = [dateFormatter stringFromDate: creationDate];
		fileModifiedLabel.text = [dateFormatter stringFromDate: modificationDate];
		
		NSLog (@"file %@\nsize %@\ncreated %@\nmodified %@",
           fileName,
           [numberFormatter stringFromNumber: fileSize], 
           [dateFormatter stringFromDate: creationDate],
           [dateFormatter stringFromDate: modificationDate]);
		
		[numberFormatter release];
		[dateFormatter release];
		
	}
}

- (IBAction) readFileContents {
	FileContentsViewController *fileContentsViewController =
    [[FileContentsViewController alloc]
     initWithNibName: @"FileContentsView" bundle:nil];
	fileContentsViewController.filePath = filePath;
	fileContentsViewController.title = [filePath lastPathComponent];
	[self.navigationController pushViewController:fileContentsViewController animated:YES];
	[fileContentsViewController release];
}

/*
// The designated initializer. Override to perform setup that is required before the view is loaded.
- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil {
    if (self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil]) {
        // Custom initialization
    }
    return self;
}
*/

/*
// Implement loadView to create a view hierarchy programmatically, without using a nib.
- (void)loadView {
}
*/

/*
// Implement viewDidLoad to do additional setup after loading the view, typically from a nib.
- (void)viewDidLoad {
    [super viewDidLoad];
}
*/

/*
// Override to allow orientations other than the default portrait orientation.
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation {
    // Return YES for supported orientations
    return (interfaceOrientation == UIInterfaceOrientationPortrait);
}
*/

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning]; // Releases the view if it doesn't have a superview
    // Release anything that's not essential, such as cached data
}


- (void)dealloc {
    [super dealloc];
}


@end
