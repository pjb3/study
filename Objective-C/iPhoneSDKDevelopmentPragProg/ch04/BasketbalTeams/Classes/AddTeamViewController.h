//
//  AddTeamViewController.h
//  BasketbalTeams
//
//  Created by Paul Barry on 4/20/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "RootViewController.h"

@interface AddTeamViewController : UIViewController <UITextFieldDelegate> {
  IBOutlet UITextField *teamTextField;
  RootViewController *rootViewController;
}

@property (nonatomic, retain) UITextField *teamTextField;
@property (nonatomic, retain) RootViewController *rootViewController;

@end
