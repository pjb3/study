//
//  RootViewController.m
//  FilesystemExplorer
//
//  Created by Paul Barry on 4/24/09.
//  Copyright BrowserMedia 2009. All rights reserved.
//

#import "DirectoryViewController.h"
#import "FilesystemExplorerAppDelegate.h"
#import "FileUtils.h"
#import "UIUtils.h"
#import "FileOverviewViewController.h"
#import "CreateDirectoryViewController.h"

@implementation DirectoryViewController

- (void)viewDidLoad {
  [super viewDidLoad];
  UIBarButtonItem *addButton = [[[UIBarButtonItem alloc]
                                 initWithBarButtonSystemItem: UIBarButtonSystemItemAdd
                                 target: self
                                 action: @selector(showAddOptions)] autorelease];
  self.navigationItem.rightBarButtonItem = addButton;
}

#pragma mark -
#pragma mark Directory Methods

- (void) loadDirectoryContents {
  if(directoryContents != nil) {
    [directoryContents release];
  }
  directoryContents = [[NSFileManager defaultManager] directoryContentsAtPath: directoryPath];
  [directoryContents retain];
}

- (NSString *) directoryPath {
  return directoryPath;
}

- (void) setDirectoryPath:(NSString *) path {
  [directoryPath release];
  [path retain];
  directoryPath = path;
  [self loadDirectoryContents];
  self.title = [directoryPath lastPathComponent];
}

#pragma mark -
#pragma mark Add Button Methods

- (void) showAddOptions {
  NSString *sheetTitle = [[NSString alloc]
                          initWithFormat: @"Edit \"%@\"",
                          [directoryPath lastPathComponent]];
  UIActionSheet *actionSheet = [[UIActionSheet alloc]
                                initWithTitle: sheetTitle 
                                delegate: self 
                                cancelButtonTitle: @"Cancel" 
                                destructiveButtonTitle: NULL 
                                otherButtonTitles: @"New File", @"New Directory", NULL];
  [actionSheet showInView: self.view];
  [sheetTitle release];
  [actionSheet release];
}

- (void)actionSheet:(UIActionSheet *)actionSheet clickedButtonAtIndex:(NSInteger)buttonIndex {
	if (buttonIndex == 0) {
    [self createNewFile];
  } else if (buttonIndex == 1) {
    [self createNewDirectory];
  }
}

- (void) createNewFile {
  
}

- (void)createNewDirectory {
	if (!isWriteable(self.directoryPath)) {
    alert(@"Not Permitted:", @"Cannot write to this directory");
		return;
	}
    
	CreateDirectoryViewController *createDirectoryViewController =
    [[CreateDirectoryViewController alloc] initWithNibName: @"CreateDirectoryView" bundle:nil];
	createDirectoryViewController.parentDirectoryPath = directoryPath;
	createDirectoryViewController.directoryViewController = self;
	createDirectoryViewController.title = @"Create Directory";
	[[self navigationController] pushViewController:createDirectoryViewController animated:YES];
	[createDirectoryViewController release];
}

#pragma mark -
#pragma mark Table view methods

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 1;
}


// Customize the number of rows in the table view.
- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return [directoryContents count];
}


// Customize the appearance of table view cells.
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    
    static NSString *CellIdentifier = @"Cell";
    
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
    if (cell == nil) {
        cell = [[[UITableViewCell alloc] initWithFrame:CGRectZero reuseIdentifier:CellIdentifier] autorelease];
    }
    
    cell.text = (NSString *)[directoryContents objectAtIndex: indexPath.row];

    return cell;
}


- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
  NSString *selectedFile = (NSString *)[directoryContents objectAtIndex: indexPath.row];
  NSString *selectedPath = [directoryPath stringByAppendingPathComponent: selectedFile];
  
  NSLog(@"selectedFile: %@", selectedFile);
  NSLog(@"selectedPath: %@", selectedPath);
  
  BOOL isDirectory = isDir(selectedPath);
  NSLog(@"isDir: %@", isDirectory ? @"YES" : @"NO"); 
  
  if(isDirectory) {
    DirectoryViewController *directoryViewController =
      [[DirectoryViewController alloc] 
        initWithNibName: @"DirectoryViewController" bundle:nil];
    [self.navigationController pushViewController: directoryViewController 
                                         animated: YES];
    directoryViewController.directoryPath = selectedPath;
    [directoryViewController release];
  } else {
    FileOverviewViewController *fileOverviewViewController =
      [[FileOverviewViewController alloc]
        initWithNibName: @"FileOverviewView" bundle:nil];
    [self.navigationController pushViewController: fileOverviewViewController 
                                         animated: YES];
    fileOverviewViewController.filePath = selectedPath;
    [fileOverviewViewController release];
  }
}

- (void) tableView: (UITableView *)tableView
commitEditingStyle: (UITableViewCellEditingStyle)editingStyle
 forRowAtIndexPath: (NSIndexPath *)indexPath {
  
  if(editingStyle == UITableViewCellEditingStyleDelete) {
    NSString *selectedFile = (NSString *)[directoryContents objectAtIndex: indexPath.row];
    NSString *selectedPath = fileJoin(directoryPath, selectedFile);
    if(isWriteable(selectedPath)) {
      NSError *error = nil;
      if([[NSFileManager defaultManager] removeItemAtPath: selectedPath error: &error]) {
        [self loadDirectoryContents];
        NSArray *deletedPaths = [NSArray arrayWithObject: indexPath];
        [self.tableView deleteRowsAtIndexPaths: deletedPaths withRowAnimation: YES];
      } else {
        alert(@"Error", [NSString stringWithFormat: @"Error Code: %d", [error code]]);
      }
    } else {
      alert(@"Not Writeable", @"You cannont delete this file");
    }
  }
  
}

@end

