//
//  StarterViewController.h
//  Starter
//
//  Created by Paul Barry on 4/20/09.
//  Copyright BrowserMedia 2009. All rights reserved.
//

#import <UIKit/UIKit.h>

@class EditorViewController;

@interface StarterViewController : UIViewController {
  IBOutlet UILabel *messageLabel;
  IBOutlet EditorViewController *editorViewController;
}

- (IBAction) edit;

@end

