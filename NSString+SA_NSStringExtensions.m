//
//  NSString+SA_NSStringExtensions.m
//
//	Copyright (c) 2016 Said Achmiz.
//
//	This software is licensed under the MIT license.
//	See the file "LICENSE" for more information.

#import "NSString+SA_NSStringExtensions.h"

#import "NSRange-Conventional.h"

@implementation NSString (SA_NSStringExtensions)

- (BOOL)containsCharactersInSet:(NSCharacterSet *)characters
{
	NSRange rangeOfCharacters = [self rangeOfCharacterFromSet:characters];
	return rangeOfCharacters.location != NSNotFound;
}

- (BOOL)containsCharactersInString:(NSString *)characters
{
	return [self containsCharactersInSet:[NSCharacterSet characterSetWithCharactersInString:characters]];
}

- (NSRange)firstWhitespaceAfterRange:(NSRange)aRange
{
	NSRange restOfString = NSRangeMake(NSRangeMax(aRange), self.length - NSRangeMax(aRange));
	NSRange firstWhitespace = [self rangeOfCharacterFromSet:[NSCharacterSet whitespaceCharacterSet] options:0 range:restOfString];
	
	return firstWhitespace;
}

- (NSRange)firstNonWhitespaceAfterRange:(NSRange)aRange
{
	NSRange restOfString = NSRangeMake(NSRangeMax(aRange), self.length - NSRangeMax(aRange));
	NSRange firstNonWhitespace = [self rangeOfCharacterFromSet:[[NSCharacterSet whitespaceCharacterSet] invertedSet] options:0 range:restOfString];
	
	return firstNonWhitespace;
}

- (NSRange)rangeAfterRange:(NSRange)aRange
{
	return NSRangeMake(NSRangeMax(aRange), self.length - NSRangeMax(aRange));
}

- (NSRange)rangeFromEndOfRange:(NSRange)aRange
{
	return NSRangeMake(NSRangeMax(aRange) - 1, self.length - NSRangeMax(aRange) + 1);
}

- (NSRange)rangeToEndFrom:(NSRange)aRange
{
	return NSRangeMake(aRange.location, self.length - aRange.location);
}

- (NSArray <NSString *> *)componentsSplitByWhitespace
{
	return [self componentsSplitByWhitespaceWithMaxSplits:NSUIntegerMax];
}

- (NSArray <NSString *> *)componentsSplitByWhitespaceWithMaxSplits:(NSUInteger)maxSplits
{
	if(maxSplits == 0)
	{
		return [NSMutableArray arrayWithObject:self];
	}
	
	__block NSMutableArray <NSString *> *components = [NSMutableArray array];
	
	__block NSUInteger tokenStart;
	__block NSUInteger tokenEnd;
	__block BOOL currentlyInToken = NO;
	__block NSRange tokenRange = NSRangeMake(NSNotFound, 0);
	
	__block NSUInteger splits = 0;
	
	[self enumerateSubstringsInRange:NSRangeMake(0, self.length) 
							 options:NSStringEnumerationByComposedCharacterSequences
						  usingBlock:^(NSString *character, NSRange characterRange, NSRange enclosingRange, BOOL *stop)
	 { 
		 if(currentlyInToken == NO && [character containsCharactersInSet:[[NSCharacterSet whitespaceCharacterSet] invertedSet]])
		 {
			 currentlyInToken = YES;
			 tokenStart = characterRange.location;
		 }
		 else if(currentlyInToken == YES && [character containsCharactersInSet:[NSCharacterSet whitespaceCharacterSet]])
		 {
			 currentlyInToken = NO;
			 tokenEnd = characterRange.location;
			 
			 tokenRange = NSRangeMake(tokenStart, tokenEnd - tokenStart);
			 [components addObject:[self substringWithRange:tokenRange]];
			 splits++;
			 if(splits == maxSplits)
			 {
				 *stop = YES;
				 NSRange lastTokenRange = [self rangeToEndFrom:[self firstNonWhitespaceAfterRange:tokenRange]];
				 if(lastTokenRange.location != NSNotFound)
				 {
					 [components addObject:[self substringWithRange:lastTokenRange]];
				 }
			 }
		 }
	 }];
	
	// If we were in a token when we got to the end, add that last token.
	if(splits < maxSplits && currentlyInToken == YES)
	{
		tokenEnd = self.length;
	   
		tokenRange = NSRangeMake(tokenStart, tokenEnd - tokenStart);
		[components addObject:[self substringWithRange:tokenRange]];
	}
	
	return components;
}

@end
