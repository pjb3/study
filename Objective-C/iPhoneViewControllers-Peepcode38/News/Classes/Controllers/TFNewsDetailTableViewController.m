//
//  TFNewsDetailTableViewController.m
//  News
//
//  Created by Paul Barry on 12/22/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "TFNewsDetailTableViewController.h"
#import "TFNewsItem.h"
#import "TFParagraphCell.h"
#import "TFWebViewController.h"

typedef enum {
  TFNewsDetailTitleIndex,
  TFNewsDetailFromUserIndex,
  TFNewsDetailURLIndex,
  TFNewsDetailTextIndex
} TFNewsDetailIndices;

@interface TFNewsDetailTableViewController (PrivateMethods)
- (void)prepareCell:(UITableViewCell *)cell forIndexPath:(NSIndexPath *)indexPath;
@end


@implementation TFNewsDetailTableViewController

@synthesize newsItem;

- (id)initWithNewsItem:(TFNewsItem *)theNewsItem {
  if(![super initWithStyle:UITableViewStyleGrouped]) {
    return nil;
  }
  
  self.newsItem = theNewsItem;
  self.title = newsItem.title;
  
  return self;
}

- (void)viewDidLoad {
  [super viewDidLoad];
  
  UIImage *image = [newsItem image];
  UIImageView *imageView = [[UIImageView alloc] initWithFrame:CGRectMake(0.0f,
                                                                         0.0f,
                                                                         image.size.width,
                                                                         image.size.height)];
  imageView.autoresizingMask = UIViewAutoresizingFlexibleLeftMargin|UIViewAutoresizingFlexibleRightMargin;
  imageView.image = image;
  self.tableView.tableFooterView = imageView;
  [imageView release];
}


/*
- (void)viewWillAppear:(BOOL)animated {
    [super viewWillAppear:animated];
}
*/
/*
- (void)viewDidAppear:(BOOL)animated {
    [super viewDidAppear:animated];
}
*/
/*
- (void)viewWillDisappear:(BOOL)animated {
	[super viewWillDisappear:animated];
}
*/
/*
- (void)viewDidDisappear:(BOOL)animated {
	[super viewDidDisappear:animated];
}
*/

/*
// Override to allow orientations other than the default portrait orientation.
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation {
    // Return YES for supported orientations
    return (interfaceOrientation == UIInterfaceOrientationPortrait);
}
*/

- (void)didReceiveMemoryWarning {
	// Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
	
	// Release any cached data, images, etc that aren't in use.
}

- (void)viewDidUnload {
	// Release any retained subviews of the main view.
	// e.g. self.myOutlet = nil;
}


#pragma mark Table view methods

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 1;
}


// Customize the number of rows in the table view.
- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return 4;
}


// Customize the appearance of table view cells.
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    
  static NSString *CellIdentifier = @"Cell";
  static NSString *ParagraphCellIdentifier = @"TFParagraphCell";
  
  UITableViewCell *cell;
  
  if(indexPath.row == TFNewsDetailTextIndex) {
    cell = [tableView dequeueReusableCellWithIdentifier:ParagraphCellIdentifier];
    if(cell == nil) {
      cell = [TFParagraphCell cellWithReuseIdentifier:ParagraphCellIdentifier];
    }
  } else {
    cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
    if (cell == nil) {
      cell = [[[UITableViewCell alloc] initWithStyle:UITableViewCellStyleValue2 reuseIdentifier:CellIdentifier] autorelease];
    }
  }
  
  [self prepareCell:cell forIndexPath:indexPath];
  return cell;
}


- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
  if(indexPath.row == TFNewsDetailURLIndex) {
    TFWebViewController *c = [[TFWebViewController alloc] initWithNewsItem:newsItem];
    [self.navigationController pushViewController:c animated:YES];
    [c release];    
  }
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

- (CGFloat)tableview:(UITableView *)tableView
           heightForRowAtIndexPath:(NSIndexPath *)indexPath {
  if(indexPath.row == TFNewsDetailTextIndex) {
    return [TFParagraphCell heightForCellInTable:tableView withText:newsItem.text];
  } else {
    return 44.0f;
  }
}

- (void)prepareCell:(UITableViewCell *)cell forIndexPath:(NSIndexPath *)indexPath {
  switch(indexPath.row) {
    case TFNewsDetailTitleIndex:
      cell.textLabel.text = @"title";
      cell.detailTextLabel.text = newsItem.title;
      break;
    case TFNewsDetailTextIndex:
      cell.detailTextLabel.text = newsItem.text;
      break;
    case TFNewsDetailFromUserIndex:
      cell.textLabel.text = @"from user";
      cell.detailTextLabel.text = newsItem.fromUser;
      break;
    case TFNewsDetailURLIndex:
      cell.textLabel.text = @"url";
      cell.detailTextLabel.text = [newsItem.url absoluteString];
      cell.accessoryType = UITableViewCellAccessoryDisclosureIndicator;
      break;
  }
}

- (void)dealloc {
  [newsItem release];
  [super dealloc];
}


@end

