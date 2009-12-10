//
//  UIImage+INResizeImageAllocator.m
//
//  Modified from the Three20 project. 
//  http://github.com/joehewitt/three20/tree/master
//
//  NOTE: Not thread safe. Call on main thread only.

#import "UIImage+INResizeImageAllocator.h"


@implementation UIImage (INResizeImageAllocator)
+ (UIImage*)imageWithImage:(UIImage*)image scaledToSize:(CGSize)targetSize
{
  // Aspect Fill, modified from Three20 project
  CGSize imageSize = image.size;
  if (imageSize.height < imageSize.width) {
    imageSize.width = (imageSize.width/imageSize.height) * targetSize.height;
    imageSize.height = targetSize.height;
  } else {
    imageSize.height = (imageSize.height/imageSize.width) * targetSize.width;
    imageSize.width = targetSize.width;
  }
  CGRect rect = CGRectMake(0.0f, 0.0f,
                           imageSize.width, imageSize.height);
  
  UIImage *newImage;
  UIGraphicsBeginImageContext(targetSize);
  [image drawInRect:CGRectMake(rect.origin.x, rect.origin.y, rect.size.width, rect.size.height)];
  newImage = UIGraphicsGetImageFromCurrentImageContext();
  UIGraphicsEndImageContext();
  return newImage;
}

- (UIImage*)scaleImageToSize:(CGSize)newSize
{
  return [UIImage imageWithImage:self scaledToSize:newSize];
}

@end
