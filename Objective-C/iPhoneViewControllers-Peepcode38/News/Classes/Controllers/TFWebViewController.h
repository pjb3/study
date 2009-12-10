//
//  TFWebViewControleler.h
//  News
//
//  Created by Paul Barry on 12/9/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>

@class TFNewsItem;

@interface TFWebViewController : UIViewController {
	IBOutlet UIWebView *webView;
	IBOutlet TFNewsItem *newsItem;
}

@property (nonatomic, retain) UIWebView *webView;
@property (nonatomic, retain) TFNewsItem *newsItem;

- (id)initWithNewsItem:(TFNewsItem *)theNewsItem;
- (void)goToURL:(NSURL *)aURL;

@end
