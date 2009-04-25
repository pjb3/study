//
//  FileUtils.h
//  FilesystemExplorer
//
//  Created by Paul Barry on 4/24/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import <Foundation/Foundation.h>

NSString * fileJoin(NSString *basePath, NSString *newPath);
BOOL fileExists(NSString *filePath);
BOOL isDir(NSString *path);
BOOL isWriteable(NSString *);
BOOL canCreateFileInDirectory(NSString *fileName, NSString *directoryPath);
BOOL mkdir(NSString *path);