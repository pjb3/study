//
//  TFNewsItem.h
//  PeepCodeNews
//
//  Created by Geoffrey Grosenbach on 8/15/09.
//  Copyright 2009 Topfunky Corporation. All rights reserved.
//

#import <Foundation/Foundation.h>
#include <HTTPRiot/HTTPRiot.h> 
#include "TFThumbnailView.h"

@protocol TFNewsItemDelegate <NSObject>
- (void)receivedNewsItems:(NSArray *)theNewsItems;
@end


@interface TFNewsItem : HRRestModel <TFThumbnailViewDataItem> {
  NSString *title;
  NSString *text;
  NSURL    *url;
  NSString *fromUser;
  NSString *screenshotPath;
  NSURL    *screenshotURL;
  
  UIImage  *thumbnailImage;
  UIImage  *image;
  
  NSURLConnection *connection;
  NSMutableData *receivedData;
}

@property (nonatomic, retain) NSString *title;
@property (nonatomic, retain) NSString *text;
@property (nonatomic, retain) NSURL *url;
@property (nonatomic, retain) NSString *fromUser;
@property (nonatomic, retain) NSString *screenshotPath;
@property (nonatomic, retain) NSURL *screenshotURL;

@property (nonatomic, retain) UIImage *thumbnailImage;
@property (nonatomic, retain) UIImage *image;

@property (nonatomic, retain) NSMutableData *receivedData;

+ (void)loadRecentWithDelegate:(id)aDelegate;

@end
