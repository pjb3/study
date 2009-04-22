//
//  State.h
//  States
//
//  Created by Paul Barry on 4/22/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface State : NSObject {
  NSString *name;
  NSNumber *area;
  NSNumber *population;
}

@property (nonatomic, retain) NSString *name;
@property (nonatomic, retain) NSNumber *area;
@property (nonatomic, retain) NSNumber *population;

#pragma mark -
#pragma mark Factory Methods

+ (State *) stateWithName:(NSString *)theName andArea:(NSNumber *)theArea andPopulation:(NSNumber *)thePopulation;
+ (State *) stateWithDictionary:(NSDictionary *)dict;

#pragma mark -
#pragma mark Finders

+ (NSArray *) allStates;
+ (NSArray *) allStatesSortedByProperty:(NSString *)propertyName ascending:(BOOL)ascending;

#pragma mark -
#pragma mark Initializers

- (State *) initWithName:(NSString *)theName andArea:(NSNumber *)theArea andPopulation:(NSNumber *)thePopulation;
- (State *) initWithDictionary:(NSDictionary *)dict;

@end
