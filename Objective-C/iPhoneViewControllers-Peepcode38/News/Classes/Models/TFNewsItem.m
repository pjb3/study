//
//  TFNewsItem.m
//  PeepCodeNews
//
//  Created by Geoffrey Grosenbach on 8/15/09.
//  Copyright 2009 Topfunky Corporation. All rights reserved.
//

#import "TFNewsItem.h"
#import "../Categories/UIImage+INResizeImageAllocator.h"
#import "../Helpers/TFAppHelpers.h"

@interface TFNewsItem (PrivateMethods)
- (void)fetchImageAsynchronously;
- (void)fetchImageSynchronously;
@end

@implementation TFNewsItem

@synthesize title, text, url, fromUser, screenshotPath, screenshotURL;
@synthesize thumbnailImage, image;
@synthesize receivedData;

+ (void)loadRecentWithDelegate:(id)aDelegate
{  
  [self getPath:@"/snaps.json" withOptions:nil object:aDelegate];
}

+ (void)initialize 
{
  [self setDelegate:self];

  // NSString *urlString = [NSString stringWithString:[[[NSBundle mainBundle] infoDictionary] objectForKey:@"TFNewsAPIURL"]];
  NSURL *apiURL = [NSURL URLWithString:@"http://news.peepcode.com"];
  
  [self setBaseURL:apiURL];
}

- (id)initWithDictionary:(NSDictionary *)dict 
{
  if (![super init])
    return nil;
  
  self.title = [dict valueForKeyPath:@"snap.title"];
  self.text  = [dict valueForKeyPath:@"snap.text"];
  self.url   = [NSURL URLWithString:[dict valueForKeyPath:@"snap.url"]];
  self.fromUser       = [dict valueForKeyPath:@"snap.from_user"];
  self.screenshotPath = [dict valueForKeyPath:@"snap.screenshot_path"];
  self.screenshotURL  = [NSURL URLWithString:screenshotPath relativeToURL:[[self class] baseURL]];
  // Populate other fields as needed
  
  return self;
}

#pragma mark Image-related methods

- (UIImage *)image
{
  if (image)
    return image;

  NSInvocationOperation *op = [[NSInvocationOperation alloc] initWithTarget:self 
                                                                   selector:@selector(fetchImageSynchronously) 
                                                                     object:nil];
  [op setQueuePriority:NSOperationQueuePriorityVeryLow];
  [[HROperationQueue sharedOperationQueue] addOperation:op];
  [op release];

  return [UIImage imageNamed:@"default-320x480.png"];
}

- (UIImage *)thumbnailImage
{
  if (thumbnailImage)
    return thumbnailImage;
  
  if (image)
  { 
    self.thumbnailImage = [UIImage imageWithImage:image scaledToSize:CGSizeMake(43.0, 43.0)];
    return thumbnailImage;    
  }

  // Trigger fetch of image
  self.image;

  return [UIImage imageNamed:@"default-43x43.png"];
}

- (void)fetchImageSynchronously
{
  NSData *data = [NSData dataWithContentsOfURL:screenshotURL];
  if (data)
  {
    [self performSelectorOnMainThread:@selector(setImage:) 
                           withObject:[[UIImage alloc] initWithData:data]
                        waitUntilDone:YES];
  } else {
    TFAlertWithMessageAndDelegate(@"unable to load an image.", nil); 
  }
}

- (void)fetchImageAsynchronously
{
  NSURLRequest *theRequest=[NSURLRequest requestWithURL:screenshotURL
                                            cachePolicy:NSURLRequestUseProtocolCachePolicy
                                        timeoutInterval:60.0];
  connection = [[NSURLConnection alloc] initWithRequest:theRequest delegate:self];
  if (connection) {
    self.receivedData = [NSMutableData data];
  } else {
    TFAlertWithMessageAndDelegate(@"An image request could not be made. Please connect to the Internet first.", nil);
  }
}

#pragma mark NSURLConnection Delegate

- (void)connection:(NSURLConnection *)theConnection didReceiveResponse:(NSURLResponse *)response
{
  if ([receivedData respondsToSelector:@selector(receivedData:)])
    [receivedData setLength:0];
}

- (void)connection:(NSURLConnection *)theConnection didReceiveData:(NSData *)data
{
  if ([receivedData respondsToSelector:@selector(appendData:)])
    [receivedData appendData:data];
}

- (void)connection:(NSURLConnection *)theConnection didFailWithError:(NSError *)error
{
  [connection release];
  if (receivedData)
    [receivedData release];
  TFAlertWithErrorAndDelegate(error, nil);
}

- (void)connectionDidFinishLoading:(NSURLConnection *)theConnection
{
  [connection release];

  if (receivedData)
  {
    UIImage *theImage = [[UIImage alloc] initWithData:receivedData];
    self.image = theImage;
    [theImage release];
    if ([receivedData respondsToSelector:@selector(release)])
      [receivedData release];
  }
}

#pragma mark - HRRequestOperation Delegates

+ (void)restConnection:(NSURLConnection *)connection didFailWithError:(NSError *)error object:(id)object {
  // Handle connection errors.  Failures to connect to the server, etc.
  TFAlertWithErrorAndDelegate(error, nil);
}

+ (void)restConnection:(NSURLConnection *)connection didReceiveError:(NSError *)error response:(NSHTTPURLResponse *)response object:(id)object {
  // Handle invalid responses, 404, 500, etc.
  TFAlertWithErrorAndDelegate(error, nil);
}

+ (void)restConnection:(NSURLConnection *)connection didReceiveParseError:(NSError *)error responseBody:(NSString *)string 
{
  // Request was successful, but couldn't parse the data returned by the server. 
  TFAlertWithErrorAndDelegate(error, nil);
}

// Fires off method in delegate: receivedNewsItems:
+ (void)restConnection:(NSURLConnection *)connection didReturnResource:(id)resource object:(id)anObject 
{
  NSArray *resources = [(NSDictionary *)[(NSArray *)resource objectAtIndex:0] objectForKey:@"topfunky"];

  NSMutableArray *newsItems = [[[NSMutableArray alloc] init] autorelease];

  for(id item in resources) {
    [newsItems addObject:[[self alloc] initWithDictionary:item]];
  }
  
  [anObject performSelector:@selector(receivedNewsItems:) withObject:newsItems]; 
}

#pragma mark Dealloc

- (void)dealloc
{
  [title release];
  [text release];
  [image release];
  [thumbnailImage release];
  [url release];
  [screenshotURL release];
  [screenshotPath release];
  [fromUser release];
  [receivedData release];
  [connection release];
  [super dealloc];
}

@end
