//
//  President.m
//  Nav
//
//  Created by Paul Barry on 5/27/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "President.h"

@implementation President
@synthesize number, name, fromYear, toYear, party; 

-(void)dealloc{ 
  [name release]; 
  [fromYear release]; 
  [toYear release]; 
  [party release]; 
  [super dealloc]; 
}

#pragma mark -
#pragma mark NSCoding 
- (void)encodeWithCoder:(NSCoder *)coder { 
  [coder encodeInt:self.number forKey:kPresidentNumberKey]; 
  [coder encodeObject:self.name forKey:kPresidentNameKey]; 
  [coder encodeObject:self.fromYear forKey:kPresidentFromKey]; 
  [coder encodeObject:self.toYear forKey:kPresidentToKey]; 
  [coder encodeObject:self.party forKey:kPresidentPartyKey]; 
} 

- (id)initWithCoder:(NSCoder *)coder { 
  if (self = [super init]) { 
    self.number = [coder decodeIntForKey:kPresidentNumberKey]; 
    self.name = [coder decodeObjectForKey:kPresidentNameKey]; 
    self.fromYear = [coder decodeObjectForKey:kPresidentFromKey]; 
    self.toYear = [coder decodeObjectForKey:kPresidentToKey]; 
    self.party = [coder decodeObjectForKey:kPresidentPartyKey]; 
  } 
  return self; 
}

@end
