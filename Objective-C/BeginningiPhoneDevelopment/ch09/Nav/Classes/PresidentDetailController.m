//
//  PresidentDetailController.m
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "PresidentDetailController.h"
#import "President.h"

@implementation PresidentDetailController
@synthesize president, fieldLabels, tempValues, textFieldBeingEdited;

- (void)dealloc {
  [textFieldBeingEdited release]; 
  [tempValues release]; 
  [president release]; 
  [fieldLabels release];  
  [super dealloc];
}

#pragma mark - 
- (IBAction) cancel:(id)sender{ 
  [self.navigationController popViewControllerAnimated: YES];
} 

- (IBAction) save:(id)sender { 
  if (textFieldBeingEdited != nil) { 
    NSNumber *tagAsNum = [[NSNumber alloc] initWithInt:textFieldBeingEdited.tag]; 
    [tempValues setObject:textFieldBeingEdited.text forKey: tagAsNum]; 
    [tagAsNum release];
  } 
  for (NSNumber *key in [tempValues allKeys]) { 
    switch ([key intValue]) { 
      case kNameRowIndex: 
        president.name = [tempValues objectForKey:key]; 
        break; 
      case kFromYearRowIndex: 
        president.fromYear = [tempValues objectForKey:key]; 
        break; 
      case kToYearRowIndex: 
        president.toYear = [tempValues objectForKey:key]; 
        break; 
      case kPartyIndex: 
        president.party = [tempValues objectForKey:key]; 
      default: 
        break; 
    } 
  } 
  [self.navigationController popViewControllerAnimated: YES];
  NSArray *allControllers = self.navigationController.viewControllers;
  UITableViewController *parent = [allControllers lastObject]; 
  [parent.tableView reloadData]; 
}

-(IBAction)textFieldDone:(id)sender { 
  [sender resignFirstResponder]; 
}

#pragma mark - 
- (void)viewDidLoad { 
  self.fieldLabels = [[NSArray alloc] initWithObjects:@"Name:", @"From:", @"To:", @"Party:", nil]; 
//  self.navigationItem.leftBarButtonItem = [[UIBarButtonItem alloc] 
//                                           initWithTitle: @"Cancel" 
//                                                   style: UIBarButtonItemStylePlain 
//                                                  target: self 
//                                                  action: @selector(cancel:)];

  self.navigationItem.rightBarButtonItem = [[UIBarButtonItem alloc] 
                                            initWithTitle: @"Save" 
                                                    style: UIBarButtonItemStyleDone 
                                                   target: self 
                                                   action: @selector(save:)]; 
  self.tempValues = [NSMutableDictionary new]; 
  [super viewDidLoad];  
}

#pragma mark - 
#pragma mark Table Data Source Methods 
- (NSInteger)tableView:(UITableView *)tableView 
    numberOfRowsInSection:(NSInteger)section { 
  return kNumberOfEditableRows; 
} 

- (UITableViewCell *) tableView:(UITableView *)tableView 
          cellForRowAtIndexPath:(NSIndexPath *)indexPath { 
  static NSString *cellId = @"PresidentCellIdentifier"; 
  UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier: cellId]; 
  if (cell == nil) {
    
    cell = [[[UITableViewCell alloc] initWithFrame: CGRectZero 
                                   reuseIdentifier: cellId] autorelease]; 
    
    UILabel *label = [[UILabel alloc] initWithFrame: CGRectMake(10, 10, 75, 25)]; 
    label.textAlignment = UITextAlignmentRight; 
    label.tag = kLabelTag; 
    label.font = [UIFont boldSystemFontOfSize:14]; 
    [cell.contentView addSubview:label]; 
    [label release]; 
    
    UITextField *textField = [[UITextField alloc] initWithFrame: CGRectMake(90, 12, 200, 25)]; 
    textField.clearsOnBeginEditing = NO; 
    [textField setDelegate:self]; 
    textField.returnKeyType = UIReturnKeyDone; 
    [textField addTarget:self 
                  action:@selector(textFieldDone:) 
        forControlEvents:UIControlEventEditingDidEndOnExit]; 
    [cell.contentView addSubview:textField]; 
  }
  
  NSUInteger row = [indexPath row]; 
  UILabel *label = (UILabel *)[cell viewWithTag:kLabelTag]; 
  UITextField *textField = nil; 
  for (UIView *oneView in cell.contentView.subviews) { 
    if ([oneView isMemberOfClass:[UITextField class]]) {
      textField = (UITextField *)oneView;   
    }
  } 
  label.text = [fieldLabels objectAtIndex:row]; 
  NSNumber *rowAsNum = [[NSNumber alloc] initWithInt:row];   
  switch (row) { 
    case kNameRowIndex: 
      if ([[tempValues allKeys] containsObject:rowAsNum]) 
        textField.text = [tempValues objectForKey:rowAsNum]; 
      else 
        textField.text = president.name; 
      break; 
    case kFromYearRowIndex: 
      if ([[tempValues allKeys] containsObject:rowAsNum]) 
        textField.text = [tempValues objectForKey:rowAsNum]; 
      else 
        textField.text = president.fromYear; 
      break; 
    case kToYearRowIndex: 
      if ([[tempValues allKeys] containsObject:rowAsNum]) 
        textField.text = [tempValues objectForKey:rowAsNum]; 
      else 
        textField.text = president.toYear; 
      break; 
    case kPartyIndex: 
      if ([[tempValues allKeys] containsObject:rowAsNum]) 
        textField.text = [tempValues objectForKey:rowAsNum]; 
      else 
        textField.text = president.party; 
    default: 
      break; 
  }
  if (textFieldBeingEdited == textField) {
    textFieldBeingEdited = nil;
  }
  textField.tag = row; 
  [rowAsNum release]; 
  return cell; 
} 

#pragma mark - 
#pragma mark Table Delegate Methods 
- (NSIndexPath *)tableView:(UITableView *)tableView 
    willSelectRowAtIndexPath:(NSIndexPath *)indexPath { 
  return nil; 
} 

#pragma mark Text Field Delegate Methods 
- (void)textFieldDidBeginEditing:(UITextField *)textField { 
  self.textFieldBeingEdited = textField; 
} 

- (void)textFieldDidEndEditing:(UITextField *)textField { 
  NSNumber *tagAsNum = [[NSNumber alloc] initWithInt:textField.tag]; 
  [tempValues setObject:textField.text forKey:tagAsNum]; 
  [tagAsNum release]; 
} 

@end
