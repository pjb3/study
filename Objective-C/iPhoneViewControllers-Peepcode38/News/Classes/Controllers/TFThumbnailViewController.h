//
//  TFThumbnailViewController.h
//  News
//
//  Created by Paul Barry on 12/9/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "TFThumbnailView.h"
#import "TFNewsItem.h"

@interface TFThumbnailViewController : UIViewController <TFNewsItemDelegate, TFThumbnailViewDelegate> {
	NSArray *newsItems;
}

@property (nonatomic, retain) NSArray *newsItems;

@end
