//
//  CreateDirectoryViewController.m
//  FilesystemExplorer
//
//  Created by Paul Barry on 4/24/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "CreateDirectoryViewController.h"
#import "DirectoryViewController.h"
#import "FileUtils.h"
#import "UIUtils.h"

@implementation CreateDirectoryViewController

@synthesize parentDirectoryPath, directoryViewController;

- (void) viewDidLoad {
  [super viewDidLoad];
  UIBarButtonItem *saveButton = 
  [[UIBarButtonItem alloc] initWithTitle: @"Save" 
                                   style: UIBarButtonItemStylePlain 
                                  target: self 
                                  action: @selector(save)];
  self.navigationItem.rightBarButtonItem = saveButton;
}

- (BOOL) create {
  if([self validate]) {
    NSLog(@"Creating");
    [directoryNameField resignFirstResponder];
    mkdir(newDirectoryPath);
    [directoryViewController loadDirectoryContents];
    [directoryViewController.tableView reloadData];
    [self.navigationController popViewControllerAnimated: YES];      
    return YES;
  } else {
    NSLog(@"Invalid");
    return NO;
  }
}

- (BOOL) validate {
  NSLog(@"Validating");
  newDirectoryPath = fileJoin(parentDirectoryPath, directoryNameField.text);
  if(!isWriteable(parentDirectoryPath)) {
    alert(@"Not Writeable", @"You cannot write to this directory");
    return NO;
  } else if(fileExists(newDirectoryPath)) {
    alert(@"File Exists", [NSString stringWithFormat: @"A file named '%@' already exists", directoryNameField.text]);
    return NO;
  } else {
    return YES;  
  }  
}

- (IBAction) save {
  NSLog(@"save");
  [self create];
}

- (BOOL) textFieldShouldReturn:(UITextField *)textField {
  NSLog(@"textFieldShouldReturn");
  return [self create];
}


@end
