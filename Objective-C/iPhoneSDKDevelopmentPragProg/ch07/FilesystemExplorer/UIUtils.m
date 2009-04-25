//
//  UIUtils.m
//  FilesystemExplorer
//
//  Created by Paul Barry on 4/25/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "UIUtils.h"

void alert(NSString *title, NSString *message) {
  UIAlertView *alert =
  [[UIAlertView alloc] initWithTitle: title
                             message: message
                            delegate: nil
                   cancelButtonTitle: @"OK"
                   otherButtonTitles: nil];
  [alert show];
  [alert release];  
}