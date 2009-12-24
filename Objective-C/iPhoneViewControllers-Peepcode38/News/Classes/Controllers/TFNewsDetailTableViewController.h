//
//  TFNewsDetailTableViewController.h
//  News
//
//  Created by Paul Barry on 12/22/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>

@class TFNewsItem;

@interface TFNewsDetailTableViewController : UITableViewController {
  TFNewsItem *newsItem;
}

@property (nonatomic, retain) TFNewsItem *newsItem;

- (id)initWithNewsItem:(TFNewsItem *)theNewsItem;

@end
