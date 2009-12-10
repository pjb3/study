//
//  TFAppHelpers.m
//  News
//
//  Created by Paul Barry on 12/9/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "TFAppHelpers.h"

void TFAlertWithMessageAndDelegate(NSString *message, id theDelegate) {
	UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"News App"
													message:message 
												   delegate:theDelegate 
										  cancelButtonTitle:@"OK" 
										  otherButtonTitles:nil];
	[alert show];
	[alert release];
}

void TFAlertWithErrorAndDelegate(NSError *error, id theDelegate) {
	NSString *message = [@"Sorry, " stringByAppendingString:[error localizedDescription]];
	TFAlertWithMessageAndDelegate(message, theDelegate);
}

