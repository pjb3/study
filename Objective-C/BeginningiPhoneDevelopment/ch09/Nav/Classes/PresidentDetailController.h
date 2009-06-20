//
//  PresidentDetailController.h
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#define kNumberOfEditableRows        4 
#define kNameRowIndex                0 
#define kFromYearRowIndex            1 
#define kToYearRowIndex              2 
#define kPartyIndex                  3 
#define kLabelTag                    4096

#import <UIKit/UIKit.h>

@class President;

@interface PresidentDetailController : UITableViewController 
    <UITableViewDelegate, UITableViewDataSource, UITextFieldDelegate> {
  President *president; 
  NSArray *fieldLabels; 
  NSMutableDictionary *tempValues; 
  UITextField *textFieldBeingEdited; 
}

@property (nonatomic, retain) President *president; 
@property (nonatomic, retain) NSArray *fieldLabels; 
@property (nonatomic, retain) NSMutableDictionary *tempValues; 
@property (nonatomic, retain) UITextField *textFieldBeingEdited; 

- (IBAction)cancel:(id)sender; 
- (IBAction)save:(id)sender; 
- (IBAction)textFieldDone:(id)sender;

@end
