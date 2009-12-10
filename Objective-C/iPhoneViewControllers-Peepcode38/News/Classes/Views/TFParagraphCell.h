//
//  TFParagraphCell.h
//  PeepCodeNews
//
//  Created by Geoffrey Grosenbach on 8/18/09.
//  Copyright 2009 Topfunky Corporation. All rights reserved.
//

#import <UIKit/UIKit.h>


@interface TFParagraphCell : UITableViewCell {
}

+ (id)cellWithReuseIdentifier:(NSString *)reuseIdentifier;

+ (CGFloat)heightForCellInTable:(UITableView *)aTableView withText:(NSString *)theText;

@end
