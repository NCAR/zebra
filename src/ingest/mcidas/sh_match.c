/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 SH_MATCH.C 5-Apr-95,13:53:38,`DWS' MCPATH phase 2 (5333)                */
/* 2 SH_MATCH.C 10-Apr-95,8:56:12,`RICKK' Included mcidasp.h.                */
/* 3 SH_MATCH.C 9-May-95,18:22:32,`DWS' MCPATH phase 3 (5429)                */
/* 4 SH_MATCH.C 6-Jun-95,15:12:36,`USER' Released                            */
/* 5 SH_MATCH.C 19-Feb-96,16:12:32,`DWS' reglue: modified file               */
/* 6 SH_MATCH.C 20-Feb-96,12:05:32,`USER' Released                           */
/**** McIDAS Revision History *** */

/*
 * Shell-style Wildcard Pattern Matching
 * 
 * Original Author: John Kercheval
 * 
 * Posted to comp.sources.misc as v29i057 on Sun, 5 Apr 1992
 * 
 * Mr. Kercheval says:
 * 
 * "I submit this without copyright and with the clear understanding
 * that this code may be used by anyone, for any reason, with any
 * modifications and without any guarantees, warrantee or statements
 * of usability of any sort."
 */

#include "mcidas.h"
#include "mcidasp.h"

#undef FALSE
#undef TRUE

#define FALSE	0
#define TRUE	1
#define ABORT	2	/* end of search indicator */

static int
regex_match(const char *pattern, const char *text);
static int
regex_match_after_star(const char *pattern, const char *text);

/*
*| Name:
*|	M0sh_match - tests for a /bin/sh-style pattern match
*|
*| Interface:
*|	#include "mcidasp.h"
*|
*|	int
*|	M0sh_match(const char *pattern, const char *text)
*|
*| Input:
*|	pattern - a /bin/sh-style pattern
*|	text    - the string to match
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	 1	- success
*|	 0	- failure
*|
*| Remarks:
*|	Matchs the given pattern string against the text string, and
*|	returns 1 if it matches, 0 otherwise.
*|	
*|	A match means the entire text string is used up in matching.
*|	
*|	In the pattern string:
*|		`*' matches any sequence of characters
*|		`?' matches any character
*|		[SET] matches any character in the specified set,
*|		[!SET] or [^SET] matches any character not in the
*|			specified set.
*|	
*|	A set is composed of characters or ranges; a range looks like
*|	character hyphen character (as in 0-9 or A-Z).
*|	[0-9a-zA-Z_] is the set of characters allowed in C identifiers.
*|	Any other character in the pattern must be matched exactly.
*|	
*|	To suppress the special syntactic significance of any of
*|	`[]*?!^-\', and match the character exactly, precede it with a
*|	`\'.
*|
*| Categories: 
*|	words from the list
*/

/*
 * Implementation note
 *
 * This is a front end to regex_match() which returns only
 * TRUE or FALSE.
 */

int
M0sh_match(const char *pattern, const char *text)
{
	if (!pattern || !text)
		return FALSE;

	return (regex_match(pattern, text) == TRUE) ? TRUE : FALSE;
}

/*
*| Name:
*|	M0sh_pattern - tests for a /bin/sh-style pattern
*|
*| Interface:
*|	#include "mcidasp.h"
*|
*|	int
*|	M0sh_pattern(const char *pattern)
*|
*| Input:
*|	pattern - the string to be tested
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	 0	- failure
*|	 1	- success
*|
*| Remarks:
*|	Returns 1 if the pattern has any special characters which
*|	M0sh_match() interprets.  If it returns 0, you can use strcmp()
*|	to test for matches with the given string, since there is
*|	nothing special for M0sh_match() to do.
*|
*| Categories: 
*|	words from the list
*/


int
M0sh_pattern(const char *pattern)
{
	if (!pattern)
		return FALSE;

	while (*pattern)
	{
		switch (*pattern++)
		{
		case '?':
		case '*':
		case '[':
			return TRUE;
		case '\\':
			return *pattern ? TRUE : FALSE;
		}
	}
	return FALSE;
}

/*\
 *  Match the pattern PATTERN against the string TEXT;
 *  return TRUE (1) if it matches, FALSE (0) otherwise.
 *
 *  A match means the entire string TEXT is used up in matching.
 *
 *  In the pattern string:
 *	`*' matches any sequence of characters
 *	`?' matches any character
 *	[SET] matches any character in the specified set,
 *	[!SET] or [^SET] matches any character not in the specified set.
 *
 *  Note: the standard regex character '+' (one or more) should be
 *	simulated by using "?*" which is equivalant here.
 *
 *  A set is composed of characters or ranges; a range looks like
 *  character hyphen character (as in 0-9 or A-Z).
 *  [0-9a-zA-Z_] is the set of characters allowed in C identifiers.
 *  Any other character in the pattern must be matched exactly.
 *
 *  To suppress the special syntactic significance of any of `[]*?!^-\',
 *  and match the character exactly, precede it with a `\'.
\*/

static int
regex_match(register const char *p, register const char *t)
{
	register char   range_start;	/* start in range */
	register char   range_end;	/* end in range */

	int             invert;	/* is this [..] or [!..] */
	int             member_match;	/* have I matched the [..]
					 * construct? */
	int             loop;	/* should I terminate? */

	for (; *p; p++, t++)
	{

		/*
		 * if this is the end of the text then this is the
		 * end of the match
		 */
		if (!*t)
		{
			return (*p == '*' && *++p == '\0') ? TRUE : ABORT;
		}

		/* determine and react to pattern type */
		switch (*p)
		{

			/* single any character match */
		case '?':
			break;

			/* multiple any character match */
		case '*':
			return regex_match_after_star(p, t);

			/*
			 * [..] construct, single member/exclusion
			 * character match
			 */
		case '[':
			{

				/* move to beginning of range */
				p++;

				/*
				 * check if this is a member match or
				 * exclusion match
				 */
				invert = FALSE;
				if (*p == '!' || *p == '^')
				{
					invert = TRUE;
					p++;
				}

				/*
				 * if closing bracket here or at
				 * range start then we have a
				 * malformed pattern
				 */
				if (*p == ']')
				{
					return ABORT;
				}

				member_match = FALSE;
				loop = TRUE;

				while (loop)
				{

					/*
					 * if end of construct then
					 * loop is done
					 */
					if (*p == ']')
					{
						loop = FALSE;
						continue;
					}

					/*
					 * matching a '!', '^', '-',
					 * '\' or a ']'
					 */
					if (*p == '\\')
					{
						range_start = range_end = *++p;
					}
					else
					{
						range_start = range_end = *p;
					}

					/*
					 * if end of pattern then bad
					 * pattern (Missing ']')
					 */
					if (!range_start)
						return ABORT;

					/* move to next pattern char */
					p++;

					/* check for range bar */
					if (*p == '-')
					{

						/* get the range end */
						range_end = *++p;

						/*
						 * special character
						 * range end
						 */
						if (range_end == '\\')
							range_end = *++p;

						/*
						 * if end of pattern
						 * or construct then
						 * bad pattern
						 */
						if (range_end == '\0' || range_end == ']')
							return ABORT;
					}

					/*
					 * if the text character is
					 * in range then match found.
					 * make sure the range
					 * letters have the proper
					 * relationship to one
					 * another before comparison
					 */
					if (range_start < range_end)
					{
						if (*t >= range_start && *t <= range_end)
						{
							member_match = TRUE;
							loop = FALSE;
						}
					}
					else
					{
						if (*t >= range_end && *t <= range_start)
						{
							member_match = TRUE;
							loop = FALSE;
						}
					}
				}

				/*
				 * if there was a match in an
				 * exclusion set then no match
				 */

				/*
				 * if there was no match in a member
				 * set then no match
				 */
				if ((invert && member_match) ||
				    !(invert || member_match))
					return FALSE;

				/*
				 * if this is not an exclusion then
				 * skip the rest of the [...]
				 * construct that already matched.
				 */
				if (member_match)
				{
					while (*p != ']')
					{

						/*
						 * bad pattern
						 * (Missing ']')
						 */
						if (!*p)
							return ABORT;

						/* skip exact match */
						if (*p == '\\')
						{
							p++;
						}

						/*
						 * move to next
						 * pattern char
						 */
						p++;
					}
				}

				break;
			}

			/*
			 * next character is quoted and must match
			 * exactly
			 */
		case '\\':

			/*
			 * move pattern pointer to quoted char and
			 * fall through
			 */
			p++;

			/* must match this character exactly */
		default:
			if (*p != *t)
				return FALSE;
		}
	}

	/* if end of text not reached then the pattern fails */
	return !*t;
}

/*
 * recursively call regex_match with final segment of PATTERN and of
 * TEXT.
 */

static int
regex_match_after_star(register const char *p, register const char *t)
{
	register int    match;
	register char   nextp;

	/* pass over existing ? and * in pattern */
	while (*p == '?' || *p == '*')
	{

		/* take one char for each ? */
		if (*p == '?')
		{

			/* if end of text then no match */
			if (!*t++)
			{
				return ABORT;
			}
		}

		/* move to next char in pattern */
		p++;
	}

	/* if end of pattern we have matched regardless of text left */
	if (!*p)
	{
		return TRUE;
	}

	/*
	 * get the next character to match which must be a literal or
	 * '['
	 */
	nextp = *p;
	if (nextp == '\\')
		nextp = p[1];

	/* Continue until we run out of text or definite result seen */
	match = FALSE;
	while (match == FALSE)
	{

		/*
		 * a precondition for matching is that the next
		 * character in the pattern match the next character
		 * in the text or that the next pattern is the
		 * beginning of a range.  Increment text pointer as
		 * we go here
		 */
		if (*p == *t || nextp == '[')
		{
			match = regex_match(p, t);
		}

		/* if the end of text is reached then no match */
		if (!*t++)
			match = ABORT;
	}

	/* return result */
	return match;
}
