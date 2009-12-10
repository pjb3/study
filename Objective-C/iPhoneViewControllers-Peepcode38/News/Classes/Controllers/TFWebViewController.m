//
//  TFWebViewControleler.m
//  News
//
//  Created by Paul Barry on 12/9/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "TFWebViewController.h"
#import "TFNewsItem.h"

@implementation TFWebViewController

@synthesize webView, newsItem;

- (id)initWithNewsItem:(TFNewsItem *)theNewsItem {
  if(![super initWithNibName:@"TFWebViewController" bundle:nil]) {
    return nil;
  }
  
  self.newsItem = theNewsItem;
  self.title = newsItem.title;
  return self;
}

- (void)viewDidLoad {
  if(newsItem) {
    [self goToURL:newsItem.url];
  }
    
  [super viewDidLoad];
}

- (void)goToURL:(NSURL *)aURL {
  NSURLRequest *request = [NSURLRequest requestWithURL:aURL];
  [webView loadRequest:request];
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
  [webView release];
  [newsItem release];
  [super dealloc];
}

@end
