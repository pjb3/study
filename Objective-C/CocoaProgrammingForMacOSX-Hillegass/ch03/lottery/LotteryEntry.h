//
//  LotteryEntry.h
//  lottery
//
//  Created by Paul Barry on 3/8/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface LotteryEntry : NSObject {
  NSCalendarDate  *entryDate;
  int firstNumber;
  int secondNumber;
}
- (void)prepareRandomNumbers;
- (void)setEntryDate:(NSCalendarDate *)date;
- (NSCalendarDate *)entryDate;
- (int) firstNumber;
- (int) secondNumber;
@end
