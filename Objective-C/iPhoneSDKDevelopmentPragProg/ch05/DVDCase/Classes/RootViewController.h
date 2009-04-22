//
//  RootViewController.h
//  DVDCase
//
//  Created by Paul Barry on 4/22/09.
//  Copyright BrowserMedia 2009. All rights reserved.
//

#import <UIKit/UIKit.h>

@class DVDCabinetController;

@interface RootViewController : UITableViewController {
  DVDCabinetController *cabinetController;
}

@property (nonatomic, retain) IBOutlet DVDCabinetController *cabinetController;

@end
