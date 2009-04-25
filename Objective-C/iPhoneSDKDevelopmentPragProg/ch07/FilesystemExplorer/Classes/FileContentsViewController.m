//
//  FileContentsViewController.m
//  FilesystemExplorer
//
//  Created by Paul Barry on 4/24/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "FileContentsViewController.h"


@implementation FileContentsViewController

@synthesize filePath;

-(void) appendTextToView: (NSString*) textToAppend {
    fileContentsTextView.text = [NSString stringWithFormat:
                                 @"%@%@", fileContentsTextView.text, textToAppend];    
}

- (void) setUpAsynchronousContentLoad {
	NSLog(@"Setting up stream to load: %@", filePath);
  
	NSInputStream *inputStream =
    [[NSInputStream alloc] initWithFileAtPath: filePath];
  
	[inputStream setDelegate: self];
	[inputStream scheduleInRunLoop: [NSRunLoop currentRunLoop]
                         forMode: NSDefaultRunLoopMode];
	[inputStream open];
	[inputStream release];
}

// delegate method for handling callbacks while reading stream, called by run lop
- (void)stream:(NSStream *)theStream handleEvent:(NSStreamEvent)streamEvent {
	NSInputStream *inputStream = (NSInputStream*) theStream;
	switch (streamEvent) {
		case NSStreamEventHasBytesAvailable: {
			NSInteger maxLength = 128;
			uint8_t readBuffer [maxLength];
			NSInteger bytesRead =
			[inputStream read: readBuffer maxLength:maxLength];
			if (bytesRead > 0) {
				NSString *bufferString = [[NSString alloc]
                                  initWithBytesNoCopy: readBuffer
                                  length: bytesRead
                                  encoding: NSASCIIStringEncoding
                                  freeWhenDone: NO];							   
				[self appendTextToView: bufferString];
				[bufferString release];
			}
			break;
		}
		case NSStreamEventErrorOccurred: {
			NSError* error = [theStream streamError];
			if (error != NULL) {
				UIAlertView *errorAlert = [[UIAlertView alloc]
                                   initWithTitle: [error localizedDescription]
                                   message: [error localizedFailureReason]
                                   delegate:nil
                                   cancelButtonTitle:@"OK"
                                   otherButtonTitles:nil];
				[errorAlert show];
				[errorAlert release];
			}
			
			[inputStream removeFromRunLoop: [NSRunLoop currentRunLoop] 
                             forMode: NSDefaultRunLoopMode];
			[theStream close];
		}
		case NSStreamEventEndEncountered: {
			[inputStream removeFromRunLoop: [NSRunLoop currentRunLoop] 
                             forMode: NSDefaultRunLoopMode];
			[theStream close];
		}
	} 
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

// Implement viewDidLoad to do additional setup after loading the view, typically from a nib.
- (void)viewDidLoad {
  [super viewDidLoad];
  
	// set fields with name and size
	if(self.filePath != nil) {
		NSString *fileName = [self.filePath lastPathComponent];
		fileNameLabel.text = fileName;
		
		NSNumberFormatter *numberFormatter = [[NSNumberFormatter alloc] init];
      [numberFormatter setPositiveFormat: @"#,##0.## bytes"];
		
    NSNumber *fileSize = [[[NSFileManager defaultManager] 
                           fileAttributesAtPath: self.filePath traverseLink: YES]
                          objectForKey: NSFileSize];
		
		fileSizeLabel.text = [NSString stringWithFormat: @"%@ bytes", fileSize];
    [numberFormatter release];
	}
	
  fileContentsTextView.text = @"";
  
	[self setUpAsynchronousContentLoad];  
}


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
