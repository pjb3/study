//
//  RootViewController.m
//  BasketbalTeams
//
//  Created by Paul Barry on 4/20/09.
//  Copyright BrowserMedia 2009. All rights reserved.
//

#import "RootViewController.h"
#import "BasketbalTeamsAppDelegate.h"
#import "AddTeamViewController.h"

@implementation RootViewController

@synthesize teams, addButtonItem;

- (id)initWithCoder:(NSCoder *)coder {
  if(self = [super initWithCoder:coder]) {
    self.teams = [NSMutableArray
                  arrayWithObjects:@"Anaheim Aardvarks",
                                   @"Baltimore Bats",
                                   @"California Caterpillars",
                                   @"Denver Donkeys",
                                  nil];
    self.addButtonItem = [[[UIBarButtonItem alloc]
                          initWithBarButtonSystemItem:UIBarButtonSystemItemAdd 
                            target:self 
                            action:@selector(addButtonWasPressed)] autorelease];
  
  }
  return self;
}

- (void)viewDidLoad {
    [super viewDidLoad];

    // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
    self.navigationItem.leftBarButtonItem = self.editButtonItem;
    self.navigationItem.rightBarButtonItem = self.addButtonItem;
}

- (IBAction) addButtonWasPressed {
  AddTeamViewController *addTeamViewController;
  addTeamViewController = [[AddTeamViewController alloc]
                           initWithNibName:@"AddTeamViewController" 
                                    bundle:nil];
  addTeamViewController.rootViewController = self;
  [self.navigationController pushViewController:addTeamViewController animated:YES];
  [addTeamViewController release];
}

- (void) addTeamNamed:(NSString *)teamName {
  [teams addObject:teamName];
  NSIndexPath *indexPath;
  indexPath = [NSIndexPath indexPathForRow:[teams indexOfObject:teamName]
                                 inSection: 0];
  [self.tableView insertRowsAtIndexPaths:[NSArray arrayWithObjects:indexPath]
                        withRowAnimation:NO];
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
    [super didReceiveMemoryWarning]; // Releases the view if it doesn't have a superview
    // Release anything that's not essential, such as cached data
}

#pragma mark Table view methods

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 1;
}


// Customize the number of rows in the table view.
- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return [teams count];
}


// Customize the appearance of table view cells.
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    
  static NSString *CellIdentifier = @"Team";
  
  UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
  if (cell == nil) {
    NSLog(@"Loading TeamCell.xib...");
    [[NSBundle mainBundle] loadNibNamed:@"TeamCell" owner:self options:nil];
    cell = teamCell;
  }
  
  NSLog(@"Cell is %@", cell);
  
  NSString *teamName = [teams objectAtIndex:indexPath.row];
  
  UILabel *teamNameLabel = (UILabel*) [cell viewWithTag:1];
  teamNameLabel.text = teamName;
  
  UILabel *divisionLabel = (UILabel*) [cell viewWithTag:2];
  divisionLabel.text = 
    ([teamName length] % 2 == 0) ? @"Eastern" : @"Western";
  
  UILabel *recordLabel = (UILabel *) [cell viewWithTag:3];
  recordLabel.text =
    [[[NSString alloc] initWithFormat: @"%d - %d",
     [teamName length], 30 - [teamName length]]
     autorelease];

  return cell;
}


- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    // Navigation logic may go here. Create and push another view controller.
	// AnotherViewController *anotherViewController = [[AnotherViewController alloc] initWithNibName:@"AnotherView" bundle:nil];
	// [self.navigationController pushViewController:anotherViewController];
	// [anotherViewController release];
}


/*
// Override to support conditional editing of the table view.
- (BOOL)tableView:(UITableView *)tableView canEditRowAtIndexPath:(NSIndexPath *)indexPath {
    // Return NO if you do not want the specified item to be editable.
    return YES;
}
*/


// Override to support editing the table view.
- (void)tableView:(UITableView *)tableView commitEditingStyle:(UITableViewCellEditingStyle)editingStyle forRowAtIndexPath:(NSIndexPath *)indexPath {
    [tableView beginUpdates];
    if (editingStyle == UITableViewCellEditingStyleDelete) {
        [teams removeObjectAtIndex:indexPath.row];
        [tableView deleteRowsAtIndexPaths:[NSArray arrayWithObject:indexPath] 
                         withRowAnimation:UITableViewRowAnimationFade];
    }   
    [tableView endUpdates];   
}

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
  [teams release];
  [super dealloc];
}


@end

