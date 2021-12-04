//
//  NSString+SA_NSStringExtensions.h
//
//  Copyright 2015-2021 Said Achmiz.
//  See LICENSE and README.md for more info.

#import <Foundation/Foundation.h>

/********************************************************/
#pragma mark - SA_NSStringExtensions category declaration
/********************************************************/

@interface NSString (SA_NSStringExtensions)

/******************************/
#pragma mark - Class properties
/******************************/

@property (class) BOOL SA_NSStringExtensions_RaiseRegularExpressionCreateException;

/*************************************/
#pragma mark - Working with characters
/*************************************/

/*	Testing for occurrences of specified characters.
 */
-(BOOL) containsCharactersInSet:(NSCharacterSet *)characters;
-(BOOL) containsCharactersInString:(NSString *)characters;

/*	Removing occurrences of specified characters.
 */
-(NSString *) stringByRemovingCharactersInSet:(NSCharacterSet *)characters;
-(NSString *) stringByRemovingCharactersInString:(NSString *)characters;

/**********************/
#pragma mark - Trimming
/**********************/

/*	“Smart” trimming.
 */
typedef NS_OPTIONS(NSUInteger, SA_NSStringTrimmingOptions) {
	SA_NSStringTrimming_CollapseWhitespace		= 1 << 0,
	SA_NSStringTrimming_TrimWhitespace			= 1 << 1,
	SA_NSStringTrimming_AppendEllipsis			= 1 << 2,
	SA_NSStringTrimming_ElideEllipsisWhenEmpty	= 1 << 3
};
-(NSString *) stringByTrimmingToMaxLengthInBytes:(NSUInteger)maxLengthInBytes
								   usingEncoding:(NSStringEncoding)encoding
					withStringEnumerationOptions:(NSStringEnumerationOptions)enumerationOptions
						andStringTrimmingOptions:(SA_NSStringTrimmingOptions)trimmingOptions;

/******************************************************************************/
/* Construct trimmed and cleaned string from component dictionary array.
 *
 * Component dictionary keys:
 *
 *  value
 *			The string value of the component.
 *
 *  appendFormat (optional)
 *			A format string, with exactly one format specifier, which must be
 *			‘%@’; this will be replaced with the string value.
 *
 *	priority
 *			Priority. Lower values mean higher priority. Must be an integer.
 *			(Equal-priority components are NOT guaranteed to be trimmed in
 *			 array order.)
 *
 *	limit (optional)
 *			Individual length limit for the given component. If present, the
 *			string value (NOT the full formatted component!) will be trimmed to
 *			the given length limit *before* the components are formatted and
 *			combined.
 *
 *	trimBy (optional)
 *			Wrapped NSStringEnumerationOptions flags value. Specifies the unit
 *			by which to trim (character, word, etc.) Components without a value
 *			for this key are not trimmed, only (if need be) removed entirely.
 *			NOTE: This means that if you specify a value for they ‘limit’ key,
 *			you MUST specify a value for this key also, otherwise the limit
 *			will be ignored!)
 ******************************************************************************/
+(instancetype) trimmedStringFromComponents:(NSArray <NSDictionary *> *)components
								  maxLength:(NSUInteger)maxLengthInBytes
								   encoding:(NSStringEncoding)encoding
							cleanWhitespace:(BOOL)cleanWhitespace;

/****************************************/
#pragma mark - Partitioning by whitespace
/****************************************/

/*	Finding runs of whitespace.
 */
-(NSRange) firstWhitespaceAfterRange:(NSRange)aRange;
-(NSRange) firstNonWhitespaceAfterRange:(NSRange)aRange;
-(NSRange) lastWhitespaceBeforeRange:(NSRange)aRange;
-(NSRange) lastNonWhitespaceBeforeRange:(NSRange)aRange;

/********************/
#pragma mark - Ranges
/********************/

/*	Range manipulation.
 */
-(NSRange) rangeAfterRange:(NSRange)aRange;
-(NSRange) rangeFromEndOfRange:(NSRange)aRange;
-(NSRange) rangeToEndFrom:(NSRange)aRange;

@property (readonly) NSRange startRange;
@property (readonly) NSRange fullRange;
@property (readonly) NSRange endRange;

/***********************/
#pragma mark - Splitting
/***********************/

/*	Splitting by whitespace.
 */
-(NSArray <NSString *> *) componentsSplitByWhitespace;
-(NSArray <NSString *> *) componentsSplitByWhitespaceWithMaxSplits:(NSUInteger)maxSplits;

/*	Splitting by specified delimiter string.
 */
-(NSArray <NSString *> *) componentsSeparatedByString:(NSString *)separator
											maxSplits:(NSUInteger)maxSplits;
-(NSArray <NSString *> *) componentsSeparatedByString:(NSString *)separator
									  dropEmptyString:(BOOL)dropEmptyString;

/***************************/
#pragma mark - Byte encoding
/***************************/

/*	Convenience method; alias for lengthOfBytesUsingEncoding:NSUTF8StringEncoding.
 */
@property (readonly) NSUInteger UTF8length;

/*	Convenience method; alias for dataWithEncoding:NSUTF8StringEncoding.
 */
@property (nonatomic, readonly) NSData *dataAsUTF8;

/*	Convenience method; alias for [[foo alloc] initWithData:data encoding:encoding].
 */
+(instancetype) stringWithData:(NSData *)data
					  encoding:(NSStringEncoding)encoding;

/*	Convenience method; alias for stringWithData:data encoding:NSUTF8StringEncoding.
 */
+(instancetype) stringWithUTF8Data:(NSData *)data;

/*********************/
#pragma mark - Hashing
/*********************/

/*	MD5 hashing.
 */
@property (nonatomic, readonly) NSString *MD5Hash;

/***********************/
#pragma mark - Sentences
/***********************/

/*	Get first sentence.
 */
@property (nonatomic, readonly) NSString *firstSentence;

/*********************/
#pragma mark - Padding
/*********************/

/*	Left-pad.
 */
-(NSString *) stringLeftPaddedTo:(int)width;

/****************************************************/
#pragma mark - Regular expression convenience methods
/****************************************************/

-(NSRegularExpression *) regularExpression;
-(NSRegularExpression *) regularExpressionWithOptions:(NSRegularExpressionOptions)options;

-(NSArray <NSString *> *) matchesForRegex:(NSRegularExpression *)regex;

-(NSArray <NSArray <NSString *> *> *) allMatchesForRegex:(NSRegularExpression *)regex;

-(NSArray <NSString *> *) matchesForRegexPattern:(NSString *)pattern;
-(NSArray <NSString *> *) matchesForRegexPattern:(NSString *)pattern
										 options:(NSRegularExpressionOptions)options;

-(NSArray <NSArray <NSString *> *> *) allMatchesForRegexPattern:(NSString *)pattern;
-(NSArray <NSArray <NSString *> *> *) allMatchesForRegexPattern:(NSString *)pattern
														options:(NSRegularExpressionOptions)options;

-(NSString *) stringByReplacingFirstOccurrenceOfPattern:(NSString *)pattern
										   withTemplate:(NSString *)replacement;

-(NSString *) stringByReplacingFirstOccurrenceOfPattern:(NSString *)pattern
										   withTemplate:(NSString *)replacement
							   regularExpressionOptions:(NSRegularExpressionOptions)regexpOptions
										matchingOptions:(NSMatchingOptions)matchingOptions;

-(NSString *) stringByReplacingAllOccurrencesOfPattern:(NSString *)pattern
										  withTemplate:(NSString *)replacement;

-(NSString *) stringByReplacingAllOccurrencesOfPattern:(NSString *)pattern
										  withTemplate:(NSString *)replacement
							  regularExpressionOptions:(NSRegularExpressionOptions)regexpOptions
									   matchingOptions:(NSMatchingOptions)matchingOptions;

-(NSString *) stringByReplacingAllOccurrencesOfPatterns:(NSArray <NSString *> *)patterns
										  withTemplates:(NSArray <NSString *> *)replacements;

-(NSString *) stringByReplacingAllOccurrencesOfPatterns:(NSArray <NSString *> *)patterns
										  withTemplates:(NSArray <NSString *> *)replacements
							   regularExpressionOptions:(NSRegularExpressionOptions)regexpOptions
										matchingOptions:(NSMatchingOptions)matchingOptions;

@end

/**************************************************************************/
#pragma mark - SA_NSStringExtensions category declaration (NSMutableString)
/**************************************************************************/

@interface NSMutableString (SA_NSStringExtensions)

/*************************************/
#pragma mark - Working with characters
/*************************************/

/*	Removing occurrences of specified characters.
 */
-(void) removeCharactersInSet:(NSCharacterSet *)characters;
-(void) removeCharactersInString:(NSString *)characters;

/**********************/
#pragma mark - Trimming
/**********************/

/*	“Smart” trimming.
 */
-(void) trimToMaxLengthInBytes:(NSUInteger)maxLengthInBytes
				 usingEncoding:(NSStringEncoding)encoding
  withStringEnumerationOptions:(NSStringEnumerationOptions)enumerationOptions
	  andStringTrimmingOptions:(SA_NSStringTrimmingOptions)trimmingOptions;

/*********************/
#pragma mark - Padding
/*********************/

/*	Left-pad.
 */
-(void) leftPadTo:(int)width;

/****************************************************/
#pragma mark - Regular expression convenience methods
/****************************************************/

-(void) replaceFirstOccurrenceOfPattern:(NSString *)pattern
						   withTemplate:(NSString *)replacement;

-(void) replaceFirstOccurrenceOfPattern:(NSString *)pattern
						   withTemplate:(NSString *)replacement
			   regularExpressionOptions:(NSRegularExpressionOptions)regexpOptions
						matchingOptions:(NSMatchingOptions)matchingOptions;

-(void) replaceAllOccurrencesOfPattern:(NSString *)pattern
						  withTemplate:(NSString *)replacement;

-(void) replaceAllOccurrencesOfPattern:(NSString *)pattern
						  withTemplate:(NSString *)replacement
			  regularExpressionOptions:(NSRegularExpressionOptions)regexpOptions
					   matchingOptions:(NSMatchingOptions)matchingOptions;

-(void) replaceAllOccurrencesOfPatterns:(NSArray <NSString *> *)patterns
						  withTemplates:(NSArray <NSString *> *)replacements;

-(void) replaceAllOccurrencesOfPatterns:(NSArray <NSString *> *)patterns
						  withTemplates:(NSArray <NSString *> *)replacements
			   regularExpressionOptions:(NSRegularExpressionOptions)regexpOptions
						matchingOptions:(NSMatchingOptions)matchingOptions;

@end

