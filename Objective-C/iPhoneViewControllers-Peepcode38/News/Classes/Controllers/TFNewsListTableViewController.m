//
//  TFNewsListTableViewController.m
//  News
//
//  Created by Paul Barry on 12/22/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "TFNewsListTableViewController.h"
#import "TFNewsDetailTableViewController.h"

@interface TFNewsListTableViewController (PrivateMethod)
- (void)loadData;
- (void)prepareCell:(UITableViewCell *)cell forIndexPath:(NSIndexPath *)indexPath;
@end

@implementation TFNewsListTableViewController

@synthesize newsItems;

- (void)viewDidAppear:(BOOL)animated {
  [self loadData];
  [super viewDidAppear:animated];
}

#pragma mark Data loading methods

- (void)loadData {
  if(newsItems == nil) {
    [TFNewsItem loadRecentWithDelegate:self];
  } else {
    [self.tableView reloadData];
  }
}

- (void) receivedNewsItems:(NSArray *)theNewsItems {
  if(newsItems != theNewsItems) {
    for(id newsItem in newsItems) {
      [newsItem removeObserver:self forKeyPath:@"image"];
    }
    self.newsItems = theNewsItems;
    for(id newsItem in newsItems) {
      [newsItem addObserver:self
                 forKeyPath:@"image" 
                    options:0 
                    context:@"imageChanged"];
    }
  }
  [self.tableView reloadData];
  //[activityIndicator stopAnimating];
}

- (void)observeValueForKeyPath:(NSString *)keyPath 
                      ofObject:(id)object 
                        change:(NSDictionary *)change 
                       context:(void *)context {
  if([keyPath isEqualToString:@"image"]) {
    NSUInteger itemIndex = [newsItems indexOfObject:object];
    UITableViewCell *cell = [self.tableView cellForRowAtIndexPath:[NSIndexPath indexPathForRow:itemIndex inSection:0]];
    
    if(cell) {
      cell.imageView.image = [(TFNewsItem *)object thumbnailImage];
    }
  }
}

#pragma mark Table view methods

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
  return 1;
}

// Customize the number of rows in the table view.
- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
  return [newsItems count];
}

// Customize the appearance of table view cells.
- (UITableViewCell *)tableView:(UITableView *)tableView 
         cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    
  static NSString *CellIdentifier = @"Cell";
  
  UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
  if (cell == nil) {
    cell = [[[UITableViewCell alloc] initWithStyle:UITableViewCellStyleSubtitle
                                   reuseIdentifier:CellIdentifier] autorelease];
  }
  
  [self prepareCell:cell forIndexPath:indexPath];

  return cell;
}

- (void)prepareCell:(UITableViewCell *)cell 
       forIndexPath:(NSIndexPath *)indexPath {
  TFNewsItem *newsItem = [newsItems objectAtIndex:indexPath.row];
  cell.textLabel.text = newsItem.title;
  cell.detailTextLabel.text = newsItem.text;
  cell.imageView.image = [newsItem thumbnailImage];
}

- (void)tableView:(UITableView *)tableView 
        didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
  TFNewsItem *newsItem = [newsItems objectAtIndex:indexPath.row];
  TFNewsDetailTableViewController *c = [[TFNewsDetailTableViewController alloc] initWithNewsItem:newsItem];
  [self.navigationController pushViewController:c animated:YES];
  [c release];
}


/*
// Override to support conditional editing of the table view.
- (BOOL)tableView:(UITableView *)tableView canEditRowAtIndexPath:(NSIndexPath *)indexPath {
    // Return NO if you do not want the specified item to be editable.
    return YES;
}
*/


/*
// Override to support editing the table view.
- (void)tableView:(UITableView *)tableView commitEditingStyle:(UITableViewCellEditingStyle)editingStyle forRowAtIndexPath:(NSIndexPath *)indexPath {
    
    if (editingStyle == UITableViewCellEditingStyleDelete) {
        // Delete the row from the data source
        [tableView deleteRowsAtIndexPaths:[NSArray arrayWithObject:indexPath] withRowAnimation:YES];
    }   
    else if (editingStyle == UITableViewCellEditingStyleInsert) {
        // Create a new instance of the appropriate class, insert it into the array, and add a new row to the table view
    }   
}
*/


/*
// Override to support rearranging the table view.
- (void)tableView:(UITableView *)tableView moveRowAtIndexPath:(NSIndexPath *)fromIndexPath toIndexPath:(NSIndexPath *)toIndexPath {
}
*/


/*
// Override to support conditional rearranging of the table view.
- (BOOL)tableView:(UITableView *)tableView canMoveRowAtIndexPath:(NSIndexPath *)indexPath {
    // Return NO if you do not want the item to be re-orderable.
    return YES;
}
*/


- (void)dealloc {
  [newsItems release];
  [super dealloc];
}


@end

