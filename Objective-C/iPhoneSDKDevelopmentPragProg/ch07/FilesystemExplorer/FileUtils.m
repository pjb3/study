//
//  FileUtils.m
//  FilesystemExplorer
//
//  Created by Paul Barry on 4/24/09.
//  Copyright 2009 BrowserMedia. All rights reserved.
//

#import "FileUtils.h"

NSString * fileJoin(NSString *basePath, NSString *newPath) {
  return [basePath stringByAppendingPathComponent: newPath];
}

BOOL fileExists(NSString *filePath) {
  return [[NSFileManager defaultManager] fileExistsAtPath: filePath];
}

BOOL isDir(NSString *path) {
  BOOL isDirectory = NO;
  if([[NSFileManager defaultManager] fileExistsAtPath: path isDirectory: &isDirectory]) {
    return isDirectory ? YES : NO;
  } else {
    return NO;
  }
}

BOOL isWriteable(NSString *path) {
  return [[NSFileManager defaultManager] isWritableFileAtPath: path];
}

BOOL canCreateFileInDirectory(NSString *fileName, NSString *directoryPath) {
  NSString *filePath = fileJoin(directoryPath, fileName);
  return isWriteable(directoryPath) && !fileExists(filePath);
}

BOOL mkdir(NSString *path) {
  return [[NSFileManager defaultManager]
          createDirectoryAtPath: path 
                     attributes: nil];
}