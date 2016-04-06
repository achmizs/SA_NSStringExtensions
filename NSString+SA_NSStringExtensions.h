//
//  NSString+SA_NSStringExtensions.h
//
//	Copyright (c) 2016 Said Achmiz.
//
//	This software is licensed under the MIT license.
//	See the file "LICENSE" for more information.

#import <Foundation/Foundation.h>

@interface NSString (SA_NSStringExtensions)

- (BOOL)containsCharactersInSet:(NSCharacterSet *)characters;
- (BOOL)containsCharactersInString:(NSString *)characters;

- (NSRange)firstWhitespaceAfterRange:(NSRange)aRange;
- (NSRange)firstNonWhitespaceAfterRange:(NSRange)aRange;

- (NSRange)rangeAfterRange:(NSRange)aRange;
- (NSRange)rangeFromEndOfRange:(NSRange)aRange;
- (NSRange)rangeToEndFrom:(NSRange)aRange;

- (NSArray <NSString *> *)componentsSplitByWhitespace;
- (NSArray <NSString *> *)componentsSplitByWhitespaceWithMaxSplits:(NSUInteger)maxSplits;

@end
