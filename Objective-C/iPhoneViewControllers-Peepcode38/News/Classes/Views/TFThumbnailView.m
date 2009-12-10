//
//  TFThumbnailView.m
//  PeepCodeNews
//
//  Created by Geoffrey Grosenbach on 8/18/09.
//  Copyright 2009 Topfunky Corporation. All rights reserved.
//

#import "TFThumbnailView.h"
#import "TFAppHelpers.h"

#define TFThumbnailViewColumnCount 2.0f

@interface TFThumbnailView (PrivateMethods)
- (void)removeExistingButtons;
- (void)removeContentObservers;
- (void)rebuildButtons;
- (void)clickedButton:(id)sender;
@end

@implementation TFThumbnailView

@synthesize scrollContentView;
@synthesize scrollView;
@synthesize content;
@synthesize buttons;

- (id)initWithFrame:(CGRect)frame delegate:(id)aDelegate
{
  if (self = [super initWithFrame:frame]) {
    delegate = aDelegate;
    
    CGRect frameAtOrigin = CGRectMake(0.0f, 0.0f, frame.size.width, frame.size.height);
    scrollView = [[UIScrollView alloc] initWithFrame:frameAtOrigin];
    scrollView.autoresizingMask = UIViewAutoresizingFlexibleHeight|UIViewAutoresizingFlexibleWidth;
    scrollView.autoresizesSubviews = YES;
    
    scrollContentView = [[UIView alloc] initWithFrame:frameAtOrigin];
    [scrollView addSubview:scrollContentView];
    
    [self addSubview:scrollView];
    self.autoresizesSubviews = YES;
    self.autoresizingMask = UIViewAutoresizingFlexibleHeight|UIViewAutoresizingFlexibleWidth;
  }
  return self;
}

- (void)updateContent:(NSArray *)newContent
{
  if (content != newContent) {
    [self removeContentObservers];
    self.content = newContent;
    for (id dataItem in content)
    {
      [dataItem addObserver:self forKeyPath:@"image" options:0 context:@"imageChanged"];
    }

    [self removeExistingButtons];
    [self rebuildButtons];
  }
}

- (void)removeExistingButtons
{
  [buttons release];
  for (UIView *child in scrollContentView.subviews) {
    if ([child isKindOfClass:[UIButton class]]) {
      [child removeFromSuperview];
    }
  }
}


- (void)removeContentObservers
{
  for (id dataItem in content)
  {
    [dataItem removeObserver:self forKeyPath:@"image"];
  }  
}

- (void)rebuildButtons
{
  self.buttons = [NSMutableArray array];

  CGFloat buttonWidth = (self.frame.size.width) / TFThumbnailViewColumnCount;
  CGFloat buttonHeight = buttonWidth * (3.0f/2.0f);

  NSUInteger rows         = ceil([content count] / TFThumbnailViewColumnCount);
  CGFloat contentHeight   = rows * buttonHeight;
  CGRect expandedFrame    = CGRectMake(0.0f, 0.0f, self.frame.size.width, contentHeight);
  scrollContentView.frame = expandedFrame;

  for (NSUInteger i=0; i < [content count]; i++) {
    id <TFThumbnailViewDataItem> dataItem = [content objectAtIndex:i];

    NSUInteger column = (i % (NSInteger)TFThumbnailViewColumnCount);
    NSUInteger row    = (i / TFThumbnailViewColumnCount);

    CGRect buttonFrame = CGRectMake((column * buttonWidth), (row * buttonHeight),
                                    buttonWidth, buttonHeight);

    UIButton *button = [[UIButton alloc] initWithFrame:buttonFrame];
    [button setBackgroundImage:dataItem.image forState:UIControlStateNormal];
    [button addTarget:self action:@selector(clickedButton:) forControlEvents:UIControlEventTouchUpInside];
    button.tag = i;

    [scrollContentView addSubview:button];
    [buttons addObject:button];
  }
  [scrollView setContentSize:scrollContentView.frame.size];
}

- (void)observeValueForKeyPath:(NSString *)keyPath
                      ofObject:(id)object
                        change:(NSDictionary *)change
                       context:(void *)context
{
  if ([keyPath isEqualToString:@"image"])
    {
      NSUInteger itemIndex                  = [content indexOfObject:object];
      UIButton *button                      = [buttons objectAtIndex:itemIndex];
      id <TFThumbnailViewDataItem> dataItem = [content objectAtIndex:itemIndex];
      [button setBackgroundImage:dataItem.image
                        forState:UIControlStateNormal];
    }
}

- (void)clickedButton:(id)sender
{
  if ([delegate respondsToSelector:@selector(didClickOnItem:)])
    {
      id <TFThumbnailViewDataItem> item = [content objectAtIndex:[sender tag]];
      [delegate didClickOnItem:item];
    }
}

- (void)dealloc {
  [self removeContentObservers];
  [content release];
  [buttons release];
  [scrollView release];
  [scrollContentView release];
  [super dealloc];
}


@end


