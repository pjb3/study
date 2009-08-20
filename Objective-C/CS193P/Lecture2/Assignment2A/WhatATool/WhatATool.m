#import <Foundation/Foundation.h>
#import "PolygonShape.h"

void printPolygonInfo() {
	NSMutableArray *shapes = [[NSMutableArray alloc] init];
	
	PolygonShape *shape = 
		[[PolygonShape alloc] initWithNumberOfSides: 4
							   minimumNumberOfSides: 3 
							   maximumNumberOfSides: 7];
	[shapes addObject: shape];
	NSLog(@"%@", shape);

	shape = [[PolygonShape alloc] 
			     initWithNumberOfSides: 6
				  minimumNumberOfSides: 6 
				  maximumNumberOfSides: 9];
	[shapes addObject: shape];
	NSLog(@"%@", shape);

	shape = [[PolygonShape alloc] 
			 initWithNumberOfSides: 12
			 minimumNumberOfSides: 9 
			 maximumNumberOfSides: 12];
	[shapes addObject: shape];
	NSLog(@"%@", shape);	
	
	for(shape in shapes) {
		shape.numberOfSides = 10;
		[shape release];
	}
	
	[shapes release];
}

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

	printPolygonInfo();
	
    [pool drain];
    return 0;
}
