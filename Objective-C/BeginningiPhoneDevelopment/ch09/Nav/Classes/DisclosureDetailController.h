//
//  DisclosureDetailController.h
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface DisclosureDetailController : UIViewController {
  IBOutlet UILabel *label;
  NSString *message;
}

@property (nonatomic, retain) UILabel *label;
@property (nonatomic, retain) NSString *message;

@end
