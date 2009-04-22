//
//  State.m
//  States
//
//  Created by Paul Barry on 4/22/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "State.h"

@implementation State

@synthesize name, area, population;

#pragma mark -
#pragma mark Factory Methods

+ (State *) stateWithName:(NSString *)theName andArea:(NSNumber *)theArea andPopulation:(NSNumber *)thePopulation {
  return [[[self alloc] initWithName:theName andArea:theArea andPopulation:thePopulation] autorelease];
}

+ (State *) stateWithDictionary:(NSDictionary *)dict {
  return [[[self alloc] initWithDictionary:dict] autorelease];
}

#pragma mark -
#pragma mark Finders

+ (NSArray *) allStates {
  NSMutableArray *data = [NSMutableArray array];
  
  [data addObject: [State stateWithName: @"California"
                                andArea: [NSNumber numberWithInt:163770]
                          andPopulation: [NSNumber numberWithInt:36553215]]];  

  [data addObject: [State stateWithName: @"Florida"
                                andArea: [NSNumber numberWithInt:65795]
                          andPopulation: [NSNumber numberWithInt:18251243]]];  
  
  [data addObject: [State stateWithName: @"Illinois"
                                andArea: [NSNumber numberWithInt:57918]
                          andPopulation: [NSNumber numberWithInt:12852548]]];    
  
  [data addObject: [State stateWithName: @"New York"
                                andArea: [NSNumber numberWithInt:54555]
                          andPopulation: [NSNumber numberWithInt:19297729]]];  

  [data addObject: [State stateWithName: @"Texas"
                                andArea: [NSNumber numberWithInt:268820]
                          andPopulation: [NSNumber numberWithInt:23904380]]];  
  
  return [NSArray arrayWithArray:data];
}

+ (NSArray *) allStatesSortedByProperty:(NSString *)propertyName ascending:(BOOL)ascending {
  NSSortDescriptor *sorter = [[[NSSortDescriptor alloc] initWithKey:propertyName ascending:ascending] autorelease];
  return [[self allStates] sortedArrayUsingDescriptors: [NSArray arrayWithObject:sorter]];
}

#pragma mark -
#pragma mark Initializers

- (State *) initWithName:(NSString *)theName andArea:(NSNumber *)theArea andPopulation:(NSNumber *)thePopulation {
  if(self = [super init]) {
    self.name = theName;
    self.area = theArea;
    self.population = thePopulation;
  }
  return self;
}

- (State *) initWithDictionary:(NSDictionary *)dict {
  return [self initWithName: [dict objectForKey: @"name"] 
                    andArea: [dict objectForKey: @"area"]
              andPopulation: [dict objectForKey: @"population"]];
}

@end
