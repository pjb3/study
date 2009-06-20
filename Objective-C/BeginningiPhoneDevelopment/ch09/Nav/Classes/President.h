//
//  President.h
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#define kPresidentNumberKey         @"President" 
#define kPresidentNameKey           @"Name" 
#define kPresidentFromKey           @"FromYear" 
#define kPresidentToKey             @"ToYear" 
#define kPresidentPartyKey          @"Party"

#import <Foundation/Foundation.h>


@interface President : NSObject <NSCoding> {
  NSInteger   number; 
  NSString    *name; 
  NSString    *fromYear; 
  NSString    *toYear; 
  NSString    *party; 
} 
@property NSInteger number; 
@property (nonatomic, retain) NSString *name; 
@property (nonatomic, retain) NSString *fromYear; 
@property (nonatomic, retain) NSString *toYear; 
@property (nonatomic, retain) NSString *party; 

@end
