#import <Foundation/Foundation.h>

void printPathInfo() {
	NSString *path = @"~";
	NSString *home = [path stringByExpandingTildeInPath];
	
	NSLog(@"My home folder is at '%@'", home);
	
	for(NSString *pathComponent in [home pathComponents]) {
		NSLog(@"%@", pathComponent);
	}
}

void printProcessInfo() {
	NSProcessInfo *processInfo = [NSProcessInfo processInfo];
	NSLog(@"Process Name: '%@' Process ID: '%d'", [processInfo processName], [processInfo processIdentifier]);
}

void printBookmarkInfo() {
	NSDictionary *bookmarks = 
		[NSDictionary dictionaryWithObjectsAndKeys:	
			[NSURL URLWithString: @"http://www.stanford.edu"], @"Stanford University",
			[NSURL URLWithString: @"http://www.apple.com"], @"Apple",
			[NSURL URLWithString: @"http://cs193p.stanford.edu"], @"CS193P",
			[NSURL URLWithString: @"http://itunes.stanford.edu"], @"Stanford on iTunes U",
			[NSURL URLWithString: @"http://stanfordshop.com"], @"Stanford Mall", nil];
	
	for(NSString *key in bookmarks) {
		if([key hasPrefix: @"Stanford"]) {
			NSLog(@"Key: '%@' URL: '%@'", key, [bookmarks objectForKey: key]); 
		}
	}
	
}

void printIntrospectionInfo() {
	NSArray *objects = [NSArray arrayWithObjects: 
						@"FOO", 
						[NSURL URLWithString: @"http://www.stanford.edu"], 
						[NSDictionary new],
						nil];
	
	for(NSObject *object in objects) {
		NSLog(@"Class name: %@", [object class]);
		NSLog(@"Is Member of NSString: %@", [object isMemberOfClass: [NSString class]] ? @"YES" : @"NO");
		NSLog(@"Is Kind of NSString: %@", [object isKindOfClass: [NSString class]] ? @"YES" : @"NO");
		BOOL lowercaseString = [object respondsToSelector:@selector(lowercaseString)];
		NSLog(@"Responds to lowercaseString: %@", lowercaseString ? @"YES" : @"NO");
		if(lowercaseString) {
			NSLog(@"lowercaseString is: %@", [object performSelector:@selector(lowercaseString)]);
		}
	}
}

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
	
	printPathInfo();           // Section 1 
	printProcessInfo();        // Section 2 
	printBookmarkInfo();       // Section 3 
	printIntrospectionInfo();  // Section 4
	
    [pool drain];
    return 0;
}
