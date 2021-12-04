//
//  NSString+SA_NSStringExtensions.m
//
//  Copyright 2015-2021 Said Achmiz.
//  See LICENSE and README.md for more info.

#import "NSString+SA_NSStringExtensions.h"

#import "NSIndexSet+SA_NSIndexSetExtensions.h"
#import "NSArray+SA_NSArrayExtensions.h"
#import <CommonCrypto/CommonDigest.h>

static BOOL _SA_NSStringExtensions_RaiseRegularExpressionCreateException = YES;

/***********************************************************/
#pragma mark - SA_NSStringExtensions category implementation
/***********************************************************/

@implementation NSString (SA_NSStringExtensions)

/******************************/
#pragma mark - Class properties
/******************************/

+(void) setSA_NSStringExtensions_RaiseRegularExpressionCreateException:(BOOL)SA_NSStringExtensions_RaiseRegularExpressionCreateException {
	_SA_NSStringExtensions_RaiseRegularExpressionCreateException = SA_NSStringExtensions_RaiseRegularExpressionCreateException;
}

+(BOOL) SA_NSStringExtensions_RaiseRegularExpressionCreateException {
	return _SA_NSStringExtensions_RaiseRegularExpressionCreateException;
}

/*************************************/
#pragma mark - Working with characters
/*************************************/

-(BOOL) containsCharactersInSet:(NSCharacterSet *)characters {
	NSRange rangeOfCharacters = [self rangeOfCharacterFromSet:characters];
	return rangeOfCharacters.location != NSNotFound;
}

-(BOOL) containsCharactersInString:(NSString *)characters {
	return [self containsCharactersInSet:[NSCharacterSet characterSetWithCharactersInString:characters]];
}

-(NSString *) stringByRemovingCharactersInSet:(NSCharacterSet *)characters {
	NSMutableString *workingCopy = [self mutableCopy];

	[workingCopy removeCharactersInSet:characters];
//	NSRange rangeOfCharacters = [workingCopy rangeOfCharacterFromSet:characters];
//	while (rangeOfCharacters.location != NSNotFound) {
//		[workingCopy replaceCharactersInRange:rangeOfCharacters withString:@""];
//		rangeOfCharacters = [workingCopy rangeOfCharacterFromSet:characters];
//	}

	return [workingCopy copy];
}

-(NSString *) stringByRemovingCharactersInString:(NSString *)characters {
	return [self stringByRemovingCharactersInSet:[NSCharacterSet characterSetWithCharactersInString:characters]];
}

/**********************/
#pragma mark - Trimming
/**********************/

-(NSString *) stringByTrimmingToMaxLengthInBytes:(NSUInteger)maxLengthInBytes
								   usingEncoding:(NSStringEncoding)encoding
					withStringEnumerationOptions:(NSStringEnumerationOptions)enumerationOptions
						andStringTrimmingOptions:(SA_NSStringTrimmingOptions)trimmingOptions {
	NSMutableString *workingCopy = [self mutableCopy];

	[workingCopy trimToMaxLengthInBytes:maxLengthInBytes
						  usingEncoding:encoding
		   withStringEnumerationOptions:enumerationOptions
			   andStringTrimmingOptions:trimmingOptions];

	return [workingCopy copy];

//	NSString *trimmedString = self;
//
//	// Trim whitespace.
//	if (trimmingOptions & SA_NSStringTrimming_TrimWhitespace)
//		trimmedString = [trimmedString stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
//
//	// Collapse whitespace.
//	if (trimmingOptions & SA_NSStringTrimming_CollapseWhitespace)
//		trimmedString = [trimmedString stringByReplacingAllOccurrencesOfPattern:@"\\s+"
//																   withTemplate:@" "];
//
//	// Length of the ellipsis suffix, in bytes.
//	NSString *ellipsis = @" …";
//	NSUInteger ellipsisLengthInBytes = [ellipsis lengthOfBytesUsingEncoding:encoding];
//
//	// Trim (leaving space for ellipsis, if necessary).
//	__block NSUInteger cutoffLength = 0;
//	[trimmedString enumerateSubstringsInRange:trimmedString.fullRange
//									  options:(enumerationOptions|NSStringEnumerationSubstringNotRequired)
//								   usingBlock:^(NSString * _Nullable substring,
//												NSRange substringRange,
//												NSRange enclosingRange,
//												BOOL * _Nonnull stop) {
//									   NSUInteger endOfEnclosingRange = NSMaxRange(enclosingRange);
//									   NSUInteger endOfEnclosingRangeInBytes = [[trimmedString substringToIndex:endOfEnclosingRange]
//																				lengthOfBytesUsingEncoding:encoding];
//
//									   // If we need to append ellipsis when trimming...
//									   if (trimmingOptions & SA_NSStringTrimming_AppendEllipsis) {
//										   if (   trimmedString.fullRange.length == endOfEnclosingRange
//											   && endOfEnclosingRangeInBytes <= maxLengthInBytes) {
//											   // Either the ellipsis is not needed, because the string is not cut off...
//											   cutoffLength = endOfEnclosingRange;
//										   } else if (endOfEnclosingRangeInBytes <= (maxLengthInBytes - ellipsisLengthInBytes)) {
//											   // Or there will still be room for the ellipsis after adding this piece...
//											   cutoffLength = endOfEnclosingRange;
//										   } else {
//											   // Or we don’t add this piece.
//											   *stop = YES;
//										   }
//									   } else {
//										   if (endOfEnclosingRangeInBytes <= maxLengthInBytes) {
//											   cutoffLength = endOfEnclosingRange;
//										   } else {
//											   *stop = YES;
//										   }
//									   }
//								   }];
//	NSUInteger lengthBeforeTrimming = trimmedString.length;
//	trimmedString = [trimmedString substringToIndex:cutoffLength];
//
//	// Append ellipsis.
//	if (   trimmingOptions & SA_NSStringTrimming_AppendEllipsis
//		&& cutoffLength < lengthBeforeTrimming
//		&& maxLengthInBytes >= ellipsisLengthInBytes
//		&& (	cutoffLength > 0
//			|| !(trimmingOptions & SA_NSStringTrimming_ElideEllipsisWhenEmpty))
//		) {
//		trimmedString = [trimmedString stringByAppendingString:ellipsis];
//	}
//
//	return trimmedString;
}

+(instancetype) trimmedStringFromComponents:(NSArray <NSDictionary *> *)components
								  maxLength:(NSUInteger)maxLengthInBytes
								   encoding:(NSStringEncoding)encoding
							cleanWhitespace:(BOOL)cleanWhitespace {
	SA_NSStringTrimmingOptions trimmingOptions = (cleanWhitespace
												  ? ( SA_NSStringTrimming_CollapseWhitespace
													 |SA_NSStringTrimming_TrimWhitespace
													 |SA_NSStringTrimming_AppendEllipsis)
												  : SA_NSStringTrimming_AppendEllipsis);

	NSMutableArray <NSDictionary *> *mutableComponents = [components mutableCopy];

	// Get the formatted version of the component
	// (inserting the value into the format string).
	NSString *(^formatComponent)(NSDictionary *) = ^NSString *(NSDictionary *component) {
		return (component[@"appendFormat"] != nil
				? [NSString stringWithFormat:component[@"appendFormat"], component[@"value"]]
				: component[@"value"]);
	};

	// Get the full formatted result.
	NSString *(^formatResult)(NSArray <NSDictionary *> *) = ^NSString *(NSArray <NSDictionary *> *componentsArray) {
		return [componentsArray reduce:^NSString *(NSString *resultSoFar,
												   NSDictionary *component) {
			return [resultSoFar stringByAppendingString:formatComponent(component)];
		} initial:@""];
	};

	// Clean and trim (if need be) each component.
	[mutableComponents enumerateObjectsUsingBlock:^(NSDictionary * _Nonnull component,
													NSUInteger idx,
													BOOL * _Nonnull stop) {
		NSMutableDictionary *adjustedComponent = [component mutableCopy];

		// Clean whitespace.
		if (cleanWhitespace) {
			adjustedComponent[@"value"] = [adjustedComponent[@"value"] stringByReplacingAllOccurrencesOfPatterns:@[ @"^\\s*(.*?)\\s*$",		// Trim whitespace.
																													@"(\\s*\\n\\s*)+",		// Replace newlines with ‘ / ’.
																													@"\\s+"					// Collapse whitespace.
																													]
																								   withTemplates:@[ @"$1",
																													@" / ",
																													@" "
																													]];
		}

		// If component length is individually limited, trim it.
		if (   adjustedComponent[@"limit"] != nil
			&& adjustedComponent[@"trimBy"] != nil) {
			adjustedComponent[@"value"] = [adjustedComponent[@"value"] stringByTrimmingToMaxLengthInBytes:[adjustedComponent[@"limit"] unsignedIntegerValue]
																							usingEncoding:encoding
																			 withStringEnumerationOptions:[adjustedComponent[@"trimBy"] unsignedIntegerValue]
																				 andStringTrimmingOptions:trimmingOptions];
		}

		[mutableComponents replaceObjectAtIndex:idx
									 withObject:adjustedComponent];
	}];

	// Maybe there’s no length limit? If so, don’t trim; just format and return.
	if (maxLengthInBytes == 0)
		return formatResult(mutableComponents);

	// Get the total (formatted) length of all the components.
	NSUInteger (^getTotalLength)(NSArray <NSDictionary *> *) = ^NSUInteger(NSArray <NSDictionary *> *componentsArray) {
		return ((NSNumber *)[componentsArray reduce:^NSNumber *(NSNumber *lengthSoFar,
																NSDictionary *component) {
			return @(lengthSoFar.unsignedIntegerValue + [formatComponent(component) lengthOfBytesUsingEncoding:encoding]);
		} initial:@(0)]).unsignedIntegerValue;
	};

	// The “lowest” priority is actually the highest numeric priority value.
	NSUInteger (^getIndexOfLowestPriorityComponent)(NSArray <NSDictionary *> *) = ^NSUInteger(NSArray <NSDictionary *> *componentsArray) {
		// By default, return the index of the last component.
		__block NSUInteger lowestPriorityComponentIndex = (componentsArray.count - 1);
		[componentsArray enumerateObjectsUsingBlock:^(NSDictionary * _Nonnull component,
													  NSUInteger idx,
													  BOOL * _Nonnull stop) {
			if ([component[@"priority"] unsignedIntegerValue] > [componentsArray[lowestPriorityComponentIndex][@"priority"] unsignedIntegerValue])
				lowestPriorityComponentIndex = idx;
		}];
		return lowestPriorityComponentIndex;
	};

	// Keep trimming until we’re below the max length.
	NSInteger excessLength = (NSInteger)(getTotalLength(mutableComponents) - maxLengthInBytes);
	while (excessLength > 0) {
		NSUInteger lowestPriorityComponentIndex = getIndexOfLowestPriorityComponent(mutableComponents);
		NSDictionary *lowestPriorityComponent = mutableComponents[lowestPriorityComponentIndex];
		NSUInteger lowestPriorityComponentValueLengthInBytes = [lowestPriorityComponent[@"value"] lengthOfBytesUsingEncoding:encoding];

		if (   lowestPriorityComponent[@"trimBy"] == nil
			|| lowestPriorityComponentValueLengthInBytes <= (NSUInteger)excessLength) {
			[mutableComponents removeObjectAtIndex:lowestPriorityComponentIndex];
		} else {
			NSMutableDictionary *adjustedComponent = [lowestPriorityComponent mutableCopy];
			adjustedComponent[@"value"] = [lowestPriorityComponent[@"value"] stringByTrimmingToMaxLengthInBytes:(lowestPriorityComponentValueLengthInBytes - (NSUInteger)excessLength)
																								  usingEncoding:encoding
																				   withStringEnumerationOptions:[lowestPriorityComponent[@"trimBy"] unsignedIntegerValue]
																					   andStringTrimmingOptions:trimmingOptions];
			// Check to make sure we haven’t trimmed all the way to nothing!
			// (Actually this can’t happen because the SA_NSStringTrimming_ElideEllipsisWhenEmpty
			//  flag is not set on the trim call above...)
			if ([adjustedComponent[@"value"] lengthOfBytesUsingEncoding:encoding] == 0) {
				// ... if we have, just remove the component.
				[mutableComponents removeObjectAtIndex:lowestPriorityComponentIndex];
			} else {
				// ... otherwise, update it.
				[mutableComponents replaceObjectAtIndex:lowestPriorityComponentIndex
											 withObject:adjustedComponent];
			}
		}

		excessLength = (NSInteger)(getTotalLength(mutableComponents) - maxLengthInBytes);
	}

	// Trimming is done; return.
	return formatResult(mutableComponents);
}

/****************************************/
#pragma mark - Partitioning by whitespace
/****************************************/

-(NSRange) firstWhitespaceAfterRange:(NSRange)aRange {
	NSRange restOfString = NSMakeRange(NSMaxRange(aRange), self.length - NSMaxRange(aRange));
	NSRange firstWhitespace = [self rangeOfCharacterFromSet:[NSCharacterSet whitespaceCharacterSet] 
													options:(NSStringCompareOptions) 0
													  range:restOfString];
	
	return firstWhitespace;
}

-(NSRange) firstNonWhitespaceAfterRange:(NSRange)aRange {
	NSRange restOfString = NSMakeRange(NSMaxRange(aRange), self.length - NSMaxRange(aRange));
	NSRange firstNonWhitespace = [self rangeOfCharacterFromSet:[[NSCharacterSet whitespaceCharacterSet] invertedSet] 
													   options:(NSStringCompareOptions) 0
														 range:restOfString];
	
	return firstNonWhitespace;
}

-(NSRange) lastWhitespaceBeforeRange:(NSRange)aRange {
	NSRange stringUntilRange = NSMakeRange(0, aRange.location);
	NSRange lastWhitespace = [self rangeOfCharacterFromSet:[NSCharacterSet whitespaceCharacterSet]
												   options:NSBackwardsSearch
													 range:stringUntilRange];
	return lastWhitespace;
}

-(NSRange) lastNonWhitespaceBeforeRange:(NSRange)aRange {
	NSRange stringUntilRange = NSMakeRange(0, aRange.location);
	NSRange lastNonWhitespace = [self rangeOfCharacterFromSet:[[NSCharacterSet whitespaceCharacterSet] invertedSet]
													  options:NSBackwardsSearch
														range:stringUntilRange];
	return lastNonWhitespace;
}

/********************/
#pragma mark - Ranges
/********************/

-(NSRange) rangeAfterRange:(NSRange)aRange {
	return NSMakeRange(NSMaxRange(aRange), self.length - NSMaxRange(aRange));
}

-(NSRange) rangeFromEndOfRange:(NSRange)aRange {
	return NSMakeRange(NSMaxRange(aRange) - 1, self.length - NSMaxRange(aRange) + 1);
}

-(NSRange) rangeToEndFrom:(NSRange)aRange {
	return NSMakeRange(aRange.location, self.length - aRange.location);
}

-(NSRange) startRange {
	return NSMakeRange(0, 0);
}

-(NSRange) fullRange {
	return NSMakeRange(0, self.length);
}

-(NSRange) endRange {
	return NSMakeRange(self.length, 0);
}

/***********************/
#pragma mark - Splitting
/***********************/

-(NSArray <NSString *> *) componentsSplitByWhitespace {
	return [self componentsSplitByWhitespaceWithMaxSplits:NSUIntegerMax
										  dropEmptyString:YES];
}

-(NSArray <NSString *> *) componentsSplitByWhitespaceWithMaxSplits:(NSUInteger)maxSplits {
	return [self componentsSplitByWhitespaceWithMaxSplits:maxSplits
										  dropEmptyString:YES];
}

-(NSArray <NSString *> *) componentsSplitByWhitespaceWithMaxSplits:(NSUInteger)maxSplits
												   dropEmptyString:(BOOL)dropEmptyString {
	// No need to do anything fancy in this case.
	if (maxSplits == 0)
		return @[ self ];

	static NSRegularExpression *regexp;
	static dispatch_once_t onceToken;
	dispatch_once(&onceToken, ^{
		regexp = [@"(?:^|\\S|$)+" regularExpression];
	});

	NSMutableArray <NSString *> *components = [NSMutableArray array];
	[regexp enumerateMatchesInString:self
							 options:(NSMatchingOptions) 0
							   range:self.fullRange
						  usingBlock:^(NSTextCheckingResult * _Nullable result,
									   NSMatchingFlags flags,
									   BOOL * _Nonnull stop) {
							  if (   dropEmptyString
								  && result.range.length == 0) {
								  // Nothing.
							  } else if (components.count < maxSplits) {
								  [components addObject:[self substringWithRange:result.range]];
							  } else {
								  [components addObject:[self substringWithRange:[self rangeToEndFrom:result.range]]];
								  *stop = YES;
							  }
						  }];
	return components;
}

//-(NSArray <NSString *> *) componentsSplitByWhitespaceWithMaxSplits:(NSUInteger)maxSplits {
//	if (maxSplits == 0) {
//		return @[ self ];
//	}
//
//	NSMutableArray <NSString *> *components = [NSMutableArray array];
//	
//	__block NSUInteger tokenStart;
//	__block NSUInteger tokenEnd;
//	__block BOOL currentlyInToken = NO;
//	__block NSRange tokenRange = NSMakeRange(NSNotFound, 0);
//	
//	__block NSUInteger splits = 0;
//	
//	[self enumerateSubstringsInRange:self.fullRange
//							 options:NSStringEnumerationByComposedCharacterSequences
//						  usingBlock:^(NSString *character,
//									   NSRange characterRange,
//									   NSRange enclosingRange,
//									   BOOL *stop) {
//		 if (   currentlyInToken == NO
//			 && [character containsCharactersInSet:[[NSCharacterSet whitespaceCharacterSet] invertedSet]]
//			 ) {
//			 currentlyInToken = YES;
//			 tokenStart = characterRange.location;
//		 } else if (   currentlyInToken == YES
//					&& [character containsCharactersInSet:[NSCharacterSet whitespaceCharacterSet]]
//					) {
//			 currentlyInToken = NO;
//			 tokenEnd = characterRange.location;
//			 
//			 tokenRange = NSMakeRange(tokenStart,
//									  tokenEnd - tokenStart);
//			 [components addObject:[self substringWithRange:tokenRange]];
//			 splits++;
//			 if (splits == maxSplits) {
//				 *stop = YES;
//				 NSRange lastTokenRange = [self rangeToEndFrom:[self firstNonWhitespaceAfterRange:tokenRange]];
//				 if (lastTokenRange.location != NSNotFound) {
//					 [components addObject:[self substringWithRange:lastTokenRange]];
//				 }
//			 }
//		 }
//	 }];
//	
//	// If we were in a token when we got to the end, add that last token.
//	if (   splits < maxSplits
//		&& currentlyInToken == YES) {
//		tokenEnd = self.length;
//	   
//		tokenRange = NSMakeRange(tokenStart,
//								 tokenEnd - tokenStart);
//		[components addObject:[self substringWithRange:tokenRange]];
//	}
//	
//	return components;
//}

-(NSArray <NSString *> *) componentsSeparatedByString:(NSString *)separator
											maxSplits:(NSUInteger)maxSplits {
	NSArray <NSString *> *components = [self componentsSeparatedByString:separator];
	if (maxSplits >= (components.count - 1))
		return components;

	return [[components subarrayWithRange:NSMakeRange(0, maxSplits)]
			arrayByAddingObject:[[components
								  subarrayWithRange:NSMakeRange(maxSplits,
																components.count - maxSplits)]
								 componentsJoinedByString:separator]];
}

-(NSArray <NSString *> *) componentsSeparatedByString:(NSString *)separator
									  dropEmptyString:(BOOL)dropEmptyString {
	NSMutableArray* components = [[self componentsSeparatedByString:separator] mutableCopy];
	if (dropEmptyString == YES)
		[components removeObject:@""];
	return [components copy];
}

/***************************/
#pragma mark - Byte encoding
/***************************/

-(NSUInteger) UTF8length {
	return [self lengthOfBytesUsingEncoding:NSUTF8StringEncoding];
}

-(NSData *) dataAsUTF8 {
	return [self dataUsingEncoding:NSUTF8StringEncoding];
}

+(instancetype) stringWithData:(NSData *)data
					  encoding:(NSStringEncoding)encoding {
	return [[self alloc] initWithData:data
							 encoding:encoding];
}

+(instancetype) stringWithUTF8Data:(NSData *)data {
	return [self stringWithData:data
					   encoding:NSUTF8StringEncoding];
}

/*********************/
#pragma mark - Hashing
/*********************/

-(NSString *) MD5Hash {
	const char *cStr = [self UTF8String];
	unsigned char result[CC_MD5_DIGEST_LENGTH];
	CC_MD5(cStr, (CC_LONG) strlen(cStr), result);

	return [NSString stringWithFormat:
			@"%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X",
			result[0], result[1], result[2], result[3],
			result[4], result[5], result[6], result[7],
			result[8], result[9], result[10], result[11],
			result[12], result[13], result[14], result[15]
];
}

/***********************/
#pragma mark - Sentences
/***********************/

-(NSString *) firstSentence {
	__block NSString *firstSentence;
	[self enumerateSubstringsInRange:self.fullRange
							 options:NSStringEnumerationBySentences
						  usingBlock:^(NSString * _Nullable substring,
									   NSRange substringRange,
									   NSRange enclosingRange,
									   BOOL * _Nonnull stop) {
							  firstSentence = substring;
							  *stop = YES;
						  }];
	return firstSentence;
}

/*********************/
#pragma mark - Padding
/*********************/

-(NSString *) stringLeftPaddedTo:(int)width {
	return [NSString stringWithFormat:@"%*s", width, [self stringByAppendingString:@"\0"].dataAsUTF8.bytes];
}

/****************************************************/
#pragma mark - Regular expression convenience methods
/****************************************************/

/*********************************************/
/* Construct regular expressions from strings.
 *********************************************/

-(NSRegularExpression *) regularExpression {
	return [self regularExpressionWithOptions:(NSRegularExpressionOptions) 0];
}

-(NSRegularExpression *) regularExpressionWithOptions:(NSRegularExpressionOptions)options {
	NSError *error;
	NSRegularExpression *regexp = [NSRegularExpression regularExpressionWithPattern:self
																			options:options
																			  error:&error];
	if (error) {
		if (NSString.SA_NSStringExtensions_RaiseRegularExpressionCreateException == YES)
			[NSException raise:@"SA_NSStringExtensions_RegularExpressionCreateException"
						format:@"%@", error.localizedDescription];

		return nil;
	}

	return regexp;
}

/**********************************************/
/* Get matches for a regular expression object.
 **********************************************/

-(NSArray <NSString *> *) matchesForRegex:(NSRegularExpression *)regex {
	NSMutableArray <NSString *> *matches = [NSMutableArray arrayWithCapacity:regex.numberOfCaptureGroups];
	[regex enumerateMatchesInString:self
							options:(NSMatchingOptions) 0
							  range:self.fullRange
						 usingBlock:^(NSTextCheckingResult * _Nullable result,
									  NSMatchingFlags flags,
									  BOOL *stop) {
							 [NSIndexSet from:0
										  for:result.numberOfRanges
										   do:^(NSUInteger idx) {
								 NSString *resultString = ([result rangeAtIndex:idx].location == NSNotFound
														   ? @""
														   : [self substringWithRange:[result rangeAtIndex:idx]]);
								 [matches addObject:resultString];
							 }];

							 *stop = YES;
						 }];
	return matches;
}

-(NSArray <NSArray <NSString *> *> *) allMatchesForRegex:(NSRegularExpression *)regex {
	NSMutableArray <NSMutableArray <NSString *> *> *matches = [NSMutableArray arrayWithCapacity:regex.numberOfCaptureGroups];
	[NSIndexSet from:0
				 for:regex.numberOfCaptureGroups
				  do:^(NSUInteger idx) {
		[matches addObject:[NSMutableArray array]];
	}];
	[regex enumerateMatchesInString:self
							options:(NSMatchingOptions) 0
							  range:self.fullRange
						 usingBlock:^(NSTextCheckingResult * _Nullable result,
									  NSMatchingFlags flags,
									  BOOL *stop) {
							 [NSIndexSet from:0
										  for:result.numberOfRanges
										   do:^(NSUInteger idx) {
											   NSString *resultString = ([result rangeAtIndex:idx].location == NSNotFound
																		 ? @""
																		 : [self substringWithRange:[result rangeAtIndex:idx]]);
											   [matches[idx] addObject:resultString];
										   }];
						 }];
	return matches;
}

/*************************************************************************/
/* Get matches for a string representing a regular expression (a pattern).
 *************************************************************************/

-(NSArray <NSString *> *) matchesForRegexPattern:(NSString *)pattern {
	return [self matchesForRegex:[pattern regularExpression]];
}

-(NSArray <NSString *> *) matchesForRegexPattern:(NSString *)pattern
										 options:(NSRegularExpressionOptions)options {
	return [self matchesForRegex:[pattern regularExpressionWithOptions:options]];
}

-(NSArray <NSArray <NSString *> *> *) allMatchesForRegexPattern:(NSString *)pattern {
	return [self allMatchesForRegex:[pattern regularExpression]];
}

-(NSArray <NSArray <NSString *> *> *) allMatchesForRegexPattern:(NSString *)pattern
														options:(NSRegularExpressionOptions)options {
	return [self allMatchesForRegex:[pattern regularExpressionWithOptions:options]];
}

/*******************************************************************************/
/* Use a pattern (a string representing a regular expression) to do replacement.
 *******************************************************************************/

-(NSString *) stringByReplacingFirstOccurrenceOfPattern:(NSString *)pattern
										   withTemplate:(NSString *)template {
	return [self stringByReplacingFirstOccurrenceOfPattern:pattern
											  withTemplate:template
								  regularExpressionOptions:(NSRegularExpressionOptions) 0
										   matchingOptions:(NSMatchingOptions) 0];
}

-(NSString *) stringByReplacingFirstOccurrenceOfPattern:(NSString *)pattern
										   withTemplate:(NSString *)template
							   regularExpressionOptions:(NSRegularExpressionOptions)regexpOptions
										matchingOptions:(NSMatchingOptions)matchingOptions {
	NSRegularExpression *regexp = [pattern regularExpressionWithOptions:regexpOptions];
	NSTextCheckingResult *match = [regexp firstMatchInString:self
													 options:matchingOptions
													   range:self.fullRange];
	if (   match
		&& match.range.location != NSNotFound) {
		return [self stringByReplacingCharactersInRange:match.range
											 withString:[regexp replacementStringForResult:match
																				  inString:self
																					offset:0
																				  template:template]];
	} else {
		return self;
	}
}

-(NSString *) stringByReplacingAllOccurrencesOfPattern:(NSString *)pattern
										  withTemplate:(NSString *)template {
	return [self stringByReplacingAllOccurrencesOfPattern:pattern
											 withTemplate:template
								 regularExpressionOptions:(NSRegularExpressionOptions) 0
										  matchingOptions:(NSMatchingOptions) 0];
}

-(NSString *) stringByReplacingAllOccurrencesOfPattern:(NSString *)pattern
										  withTemplate:(NSString *)template
							  regularExpressionOptions:(NSRegularExpressionOptions)regexpOptions
									   matchingOptions:(NSMatchingOptions)matchingOptions {
	return [[pattern regularExpressionWithOptions:regexpOptions] stringByReplacingMatchesInString:self
																						  options:matchingOptions
																							range:self.fullRange
																					 withTemplate:template];
}

-(NSString *) stringByReplacingAllOccurrencesOfPatterns:(NSArray <NSString *> *)patterns
										  withTemplates:(NSArray <NSString *> *)replacements {
	return [self stringByReplacingAllOccurrencesOfPatterns:patterns
											 withTemplates:replacements
								  regularExpressionOptions:(NSRegularExpressionOptions) 0
										   matchingOptions:(NSMatchingOptions) 0];
}

-(NSString *) stringByReplacingAllOccurrencesOfPatterns:(NSArray <NSString *> *)patterns
										  withTemplates:(NSArray <NSString *> *)replacements
							   regularExpressionOptions:(NSRegularExpressionOptions)regexpOptions
										matchingOptions:(NSMatchingOptions)matchingOptions {
	NSMutableString *workingCopy = [self mutableCopy];

	[workingCopy replaceAllOccurrencesOfPatterns:patterns
								   withTemplates:replacements
						regularExpressionOptions:regexpOptions
								 matchingOptions:matchingOptions];

	return [workingCopy copy];
}

@end

/*****************************************************************************/
#pragma mark - SA_NSStringExtensions category implementation (NSMutableString)
/*****************************************************************************/

@implementation NSMutableString (SA_NSStringExtensions)

/*************************************/
#pragma mark - Working with characters
/*************************************/

-(void) removeCharactersInSet:(NSCharacterSet *)characters {
	NSRange rangeOfCharacters = [self rangeOfCharacterFromSet:characters];
	while (rangeOfCharacters.location != NSNotFound) {
		[self replaceCharactersInRange:rangeOfCharacters withString:@""];
		rangeOfCharacters = [self rangeOfCharacterFromSet:characters];
	}
}

-(void) removeCharactersInString:(NSString *)characters {
	[self removeCharactersInSet:[NSCharacterSet characterSetWithCharactersInString:characters]];
}

/**********************/
#pragma mark - Trimming
/**********************/

-(void) trimToMaxLengthInBytes:(NSUInteger)maxLengthInBytes
				 usingEncoding:(NSStringEncoding)encoding
  withStringEnumerationOptions:(NSStringEnumerationOptions)enumerationOptions
	  andStringTrimmingOptions:(SA_NSStringTrimmingOptions)trimmingOptions {
	// Trim whitespace.
	if (trimmingOptions & SA_NSStringTrimming_TrimWhitespace)
		[self replaceAllOccurrencesOfPattern:@"^\\s*(.*?)\\s*$"
								withTemplate:@"$1"];

	// Collapse whitespace.
	if (trimmingOptions & SA_NSStringTrimming_CollapseWhitespace)
		[self replaceAllOccurrencesOfPattern:@"\\s+"
								withTemplate:@" "];

	// Length of the ellipsis suffix, in bytes.
	NSString *ellipsis = @" …";
	NSUInteger ellipsisLengthInBytes = [ellipsis lengthOfBytesUsingEncoding:encoding];

	// Trim (leaving space for ellipsis, if necessary).
	__block NSUInteger cutoffLength = 0;
	[self enumerateSubstringsInRange:self.fullRange
							 options:(enumerationOptions|NSStringEnumerationSubstringNotRequired)
						  usingBlock:^(NSString * _Nullable substring,
												NSRange substringRange,
												NSRange enclosingRange,
												BOOL * _Nonnull stop) {
							  NSUInteger endOfEnclosingRange = NSMaxRange(enclosingRange);
							  NSUInteger endOfEnclosingRangeInBytes = [[self substringToIndex:endOfEnclosingRange] lengthOfBytesUsingEncoding:encoding];

							  // If we need to append ellipsis when trimming...
							  if (trimmingOptions & SA_NSStringTrimming_AppendEllipsis) {
								  if (   self.fullRange.length == endOfEnclosingRange
									  && endOfEnclosingRangeInBytes <= maxLengthInBytes) {
									  // Either the ellipsis is not needed, because the string is not cut off...
									  cutoffLength = endOfEnclosingRange;
								  } else if (endOfEnclosingRangeInBytes <= (maxLengthInBytes - ellipsisLengthInBytes)) {
									  // Or there will still be room for the ellipsis after adding this piece...
									  cutoffLength = endOfEnclosingRange;
								  } else {
									  // Or we don’t add this piece.
									  *stop = YES;
								  }
							  } else {
								  if (endOfEnclosingRangeInBytes <= maxLengthInBytes) {
									  cutoffLength = endOfEnclosingRange;
								  } else {
									  *stop = YES;
								  }
							  }
						  }];
	NSUInteger lengthBeforeTrimming = self.length;
	[self deleteCharactersInRange:NSMakeRange(cutoffLength, self.length - cutoffLength)];

	// Trim whitespace again.
	if (trimmingOptions & SA_NSStringTrimming_TrimWhitespace)
		[self replaceAllOccurrencesOfPattern:@"^\\s*(.*?)\\s*$"
								withTemplate:@"$1"];

	// Append ellipsis.
	if (   trimmingOptions & SA_NSStringTrimming_AppendEllipsis
		&& cutoffLength < lengthBeforeTrimming
		&& maxLengthInBytes >= ellipsisLengthInBytes
		&& (	cutoffLength > 0
			|| !(trimmingOptions & SA_NSStringTrimming_ElideEllipsisWhenEmpty))
		) {
		[self appendString:ellipsis];
	}
}

/*********************/
#pragma mark - Padding
/*********************/

-(void) leftPadTo:(int)width {
	[self setString:[NSString stringWithFormat:@"%*s", width, [self stringByAppendingString:@"\0"].dataAsUTF8.bytes]];
}

/****************************************************/
#pragma mark - Regular expression convenience methods
/****************************************************/

-(void) replaceFirstOccurrenceOfPattern:(NSString *)pattern
						   withTemplate:(NSString *)template {
	[self replaceFirstOccurrenceOfPattern:pattern
							 withTemplate:template
				 regularExpressionOptions:(NSRegularExpressionOptions) 0
						  matchingOptions:(NSMatchingOptions) 0];
}

-(void) replaceFirstOccurrenceOfPattern:(NSString *)pattern
						   withTemplate:(NSString *)template
			   regularExpressionOptions:(NSRegularExpressionOptions)regexpOptions
						matchingOptions:(NSMatchingOptions)matchingOptions {
	NSRegularExpression *regexp = [pattern regularExpressionWithOptions:regexpOptions];
	NSTextCheckingResult *match = [regexp firstMatchInString:self
													 options:matchingOptions
													   range:self.fullRange];
	if (   match
		&& match.range.location != NSNotFound) {
		NSString *replacementString = [regexp replacementStringForResult:match
																inString:self
																  offset:0
																template:template];
		[self replaceCharactersInRange:match.range
							withString:replacementString];
	}
}

-(void) replaceAllOccurrencesOfPattern:(NSString *)pattern
						  withTemplate:(NSString *)template {
	[self replaceAllOccurrencesOfPattern:pattern
							withTemplate:template
				regularExpressionOptions:(NSRegularExpressionOptions) 0
						 matchingOptions:(NSMatchingOptions) 0];
}

-(void) replaceAllOccurrencesOfPattern:(NSString *)pattern
						  withTemplate:(NSString *)template
			  regularExpressionOptions:(NSRegularExpressionOptions)regexpOptions
					   matchingOptions:(NSMatchingOptions)matchingOptions {
	[[pattern regularExpressionWithOptions:regexpOptions] replaceMatchesInString:self
																		 options:matchingOptions
																		   range:self.fullRange
																	withTemplate:template];
}

-(void) replaceAllOccurrencesOfPatterns:(NSArray <NSString *> *)patterns
						  withTemplates:(NSArray <NSString *> *)replacements {
	[self replaceAllOccurrencesOfPatterns:patterns
							withTemplates:replacements
				 regularExpressionOptions:(NSRegularExpressionOptions) 0
						  matchingOptions:(NSMatchingOptions) 0];
}

-(void) replaceAllOccurrencesOfPatterns:(NSArray <NSString *> *)patterns
						  withTemplates:(NSArray <NSString *> *)replacements
			   regularExpressionOptions:(NSRegularExpressionOptions)regexpOptions
						matchingOptions:(NSMatchingOptions)matchingOptions {
	[patterns enumerateObjectsUsingBlock:^(NSString * _Nonnull pattern,
										   NSUInteger idx,
										   BOOL * _Nonnull stop) {
		NSString *replacement = (replacements.count > idx
								 ? replacements[idx]
								 : @"");
		[self replaceAllOccurrencesOfPattern:pattern
								withTemplate:replacement
					regularExpressionOptions:regexpOptions
							 matchingOptions:matchingOptions];
	}];
}

@end
