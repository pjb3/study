//
//  DVDCabinetController.h
//  DVDCase
//
//  Created by Paul Barry on 4/22/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import <UIKit/UIKit.h>


@interface DVDCabinetController : UITableViewController {
  NSDictionary *data;
  NSString *key;
}

@property (nonatomic, retain) NSString *key;
@property (nonatomic, retain) NSDictionary *data;

@end
