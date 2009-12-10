//
//  TFParagraphCell.m
//  PeepCodeNews
//
//  Created by Geoffrey Grosenbach on 8/18/09.
//  Copyright 2009 Topfunky Corporation. All rights reserved.
//

#import "TFParagraphCell.h"
#import "../Helpers/TFAppHelpers.h"

#define TFParagraphCellTextFont [UIFont systemFontOfSize:[UIFont systemFontSize]]
#define TFParagraphCellTextLeftMargin 85.0f
#define TFParagraphCellMargin 10.0f

@implementation TFParagraphCell

+ (id)cellWithReuseIdentifier:(NSString *)reuseIdentifier
{
  return [[[self alloc] initWithStyle:UITableViewCellStyleValue2 
                     reuseIdentifier:reuseIdentifier] autorelease];
}

// Runs a calculation only, does not set any values.
+ (CGFloat)heightForCellInTable:(UITableView *)aTableView 
                       withText:(NSString *)theText
{
  CGFloat cellWidth = aTableView.frame.size.width;

  if (aTableView.style == UITableViewStyleGrouped)
    cellWidth -= 20.0f;

  // NOTE: FLT_MAX is a large float. Returned height will be less.
  CGSize targetSize = CGSizeMake(cellWidth - TFParagraphCellTextLeftMargin - TFParagraphCellMargin, 
                                 FLT_MAX);
  CGSize cellSize = [theText sizeWithFont:TFParagraphCellTextFont 
                        constrainedToSize:targetSize
                            lineBreakMode:UILineBreakModeWordWrap];
  // NOTE: 1.0f for the bottom pixel line on the row
  cellSize.height += (2.0f * TFParagraphCellMargin) + 1.0f;
  return cellSize.height;
}

- (void)layoutSubviews
{
  [super layoutSubviews];
    
  CGRect labelFrame      = self.contentView.frame;
  labelFrame.size.width  = self.contentView.frame.size.width - TFParagraphCellTextLeftMargin - TFParagraphCellMargin;
  labelFrame.size.height = self.contentView.frame.size.height - (2.0f * TFParagraphCellMargin);
  labelFrame.origin.x    = TFParagraphCellTextLeftMargin;
  labelFrame.origin.y    = TFParagraphCellMargin;
  
  self.detailTextLabel.frame = labelFrame;
  self.detailTextLabel.font = TFParagraphCellTextFont;
  self.detailTextLabel.lineBreakMode = UILineBreakModeWordWrap;
  self.detailTextLabel.numberOfLines = 0;
}

- (void)dealloc {
  [super dealloc];
}

@end
