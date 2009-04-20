//
//  EditorViewController.h
//  Starter
//
//  Created by Paul Barry on 4/20/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import <UIKit/UIKit.h>


@interface EditorViewController : UIViewController <UITextFieldDelegate> {
  IBOutlet UITextField *messageInputField;
}

@property (nonatomic, retain) UITextField *messageInputField;

@end
