//
//  TFThumbnailView.h
//  PeepCodeNews
//
//  Created by Geoffrey Grosenbach on 8/18/09.
//  Copyright 2009 Topfunky Corporation. All rights reserved.
//

#import <UIKit/UIKit.h>

@protocol TFThumbnailViewDataItem <NSObject>
- (UIImage *)image;
@end

@protocol TFThumbnailViewDelegate <NSObject>
- (void)didClickOnItem:(id<TFThumbnailViewDataItem>)item;
@end

@interface TFThumbnailView : UIView {
  id delegate;
  NSArray *content;
  NSMutableArray *buttons;
  UIScrollView *scrollView;
  UIView *scrollContentView;
}

@property (nonatomic, retain) UIView *scrollContentView;
@property (nonatomic, retain) NSArray *content;
@property (nonatomic, retain) NSMutableArray *buttons;
@property (nonatomic, retain) UIScrollView *scrollView;

- (void)updateContent:(NSArray *)newContent;
- (id)initWithFrame:(CGRect)frame delegate:(id)aDelegate;

@end


