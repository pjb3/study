//
//  Foo.h
//  RandomApp
//
//  Created by Paul Barry on 3/8/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface Foo : NSObject {
  IBOutlet NSTextField *textField;
}
- (IBAction)seed:(id)sender;
- (IBAction)generate:(id)sender;

@end
