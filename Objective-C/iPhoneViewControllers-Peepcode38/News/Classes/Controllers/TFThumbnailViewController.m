//
//  TFThumbnailViewController.m
//  News
//
//  Created by Paul Barry on 12/9/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "TFThumbnailViewController.h"
#import "TFWebViewController.h"

@interface TFThumbnailViewController (PrivateMethods)
- (void) loadData;
@end

@implementation TFThumbnailViewController

@synthesize newsItems;

- (void)loadView {
	TFThumbnailView *thumbnailView = [[TFThumbnailView alloc] initWithFrame:[[UIScreen mainScreen] applicationFrame]
																	delegate:self];
	self.view = thumbnailView;
	[thumbnailView release];
}

- (void) viewDidAppear:(BOOL)animated {
	[self loadData];
	[super viewDidAppear:animated];
}

- (void) loadData {
	if(newsItems == nil) {
		[TFNewsItem loadRecentWithDelegate:self];
	} else {
		[self receivedNewsItems:newsItems];
	}
}

#pragma mark Delegate Methods

- (void)receivedNewsItems:(NSArray *)theNewsItems {
	self.newsItems = theNewsItems;
	[(TFThumbnailView *)self.view updateContent:newsItems];
}

- (void)didClickOnItem:(id<TFThumbnailViewDataItem>)item {
	TFWebViewController *wvc = [[TFWebViewController alloc] initWithNewsItem:item];
	[self.navigationController pushViewController:wvc animated:YES];
	[wvc release];
}

/*
// Override to allow orientations other than the default portrait orientation.
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation {
    // Return YES for supported orientations
    return (interfaceOrientation == UIInterfaceOrientationPortrait);
}
*/

- (void)didReceiveMemoryWarning {
	// Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
	
	// Release any cached data, images, etc that aren't in use.
}

- (void)viewDidUnload {
	// Release any retained subviews of the main view.
	// e.g. self.myOutlet = nil;
}


- (void)dealloc {
	[newsItems release];
	[super dealloc];
}


@end
