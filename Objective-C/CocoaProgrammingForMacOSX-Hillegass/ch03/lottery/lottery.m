#import <Foundation/Foundation.h>

int main (int argc, const char * argv[]) {
  NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

  NSCalendarDate *now = [NSCalendarDate new];
  
  srandom(time(NULL));
  NSMutableArray *array = [NSMutableArray new];
  
  int i;
  for(i = 0; i < 10; i++) {
    
    // I don't like the verbosity of ObjectiveC named parameters
    // because I hate that they are required to be all be specified,
    // in the right order, even if you want to use the obivous default value
    // a much cleaner version of this in Ruby or Python would look like
    // iWeeksFromNow = now.add(:days => i * 7)
    NSCalendarDate *iWeeksFromNow = [now dateByAddingYears:0 
                                                    months:0
                                                      days:(i*7)
                                                     hours:0
                                                   minutes:0
                                                   seconds:0];
    

    
    NSNumber *newNumber = [[NSNumber alloc] initWithInt:(i*3)];
    [array addObject:newNumber];
  }

  for(i = 0; i < 10; i++) {
    NSNumber *numberToPrint = [array objectAtIndex:i];
    NSLog(@"The number at index %d is %@", i, numberToPrint);
  }
  
  [pool drain];
  return 0;
}
