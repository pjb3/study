//
//  UIImage+INResizeImageAllocator.h
//  Sartorialist
//
//  Created by Geoffrey Grosenbach on 2/17/09.
//  Copyright 2009 Topfunky Corporation. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface UIImage (INResizeImageAllocator)
+ (UIImage*)imageWithImage:(UIImage*)image scaledToSize:(CGSize)newSize;
- (UIImage*)scaleImageToSize:(CGSize)newSize;
@end
