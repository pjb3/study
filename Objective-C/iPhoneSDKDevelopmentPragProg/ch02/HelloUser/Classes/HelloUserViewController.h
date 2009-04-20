//
//  HelloUserViewController.h
//  HelloUser
//
//  Created by Paul Barry on 4/20/09.
//  Copyright BrowserMedia 2009. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface HelloUserViewController : UIViewController <UITextFieldDelegate> {

  IBOutlet UILabel* helloLabel;
  IBOutlet UITextField* nameField;
  
}

@property (nonatomic, retain) UILabel* helloLabel;
@property (nonatomic, retain) UITextField* nameField;

- (IBAction) sayHello: (id) sender;

@end

