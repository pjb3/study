//
//  TFNewsListTableViewController.h
//  News
//
//  Created by Paul Barry on 12/22/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "TFNewsItem.h"

@interface TFNewsListTableViewController : UITableViewController <TFNewsItemDelegate> {
  NSArray *newsItems;
}

@property (nonatomic, retain) NSArray *newsItems;

@end
