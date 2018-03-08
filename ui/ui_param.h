/* $Id: ui_param.h,v 1.14 1999-06-25 19:21:01 burghart Exp $ */
/*
 * Basic UI parameters.
 */
# ifndef UI_PARAM_SYMBOLS
# define UI_PARAM_SYMBOLS

# include <stdio.h>
# include <string.h>

/*
 * So far only g++ has the predefined bool type per the final draft 
 * ANSI C++ standard.  Everywhere else, we have to use our own typedef.
 * 
 * We choose "typedef int" rather than "typedef char" because g++'s bool
 * is 4 bytes on all the systems tested.  We must have a size match for 
 * symbols and structures shared between g++ and non-g++ object modules.
 */
# if !(__cplusplus && __GNUC__)
typedef int bool;
# endif

/*
 * Data types of interest.
 */
typedef unsigned char byte;	/* Basic byte variable */

struct date_st
{
  int	ds_yymmdd;	/* Day portion	*/
  int	ds_hhmmss;	/* time portion */
};
typedef struct date_st date;		/* Date in date/time format */

/*
 * Define these if nobody else has already done it.
 */
# ifndef TRUE
# define TRUE 1
# define FALSE 0
# endif
# ifndef NULL
# define NULL 0
# endif

# define ___ 0		/* Null parameter. */

/*
 * The comment character.
 */
# define COMMENT '!'

/*
 * Utilities.
 */
char *getvm ();
# define NEW(type) ((type *) getvm (sizeof (type)))

/*
 * Cheap absolute value.
 */
# define ABS(v) (((v) < 0) ? -(v) : v)

/*
 * I'm not too fond of do...while loops, so I make them look like this:
 */
# define repeat do
# define until(b) while (! (b))

/*
 * Fast copy
 */
# define COPY(from,to,len) memcpy (to, from, len)

/*
 * Conditional def's for arguments for sprintrmt.  
 *
 * 9/93 - all calls to sprintrmt have been removed from ui and replaced
 * with C variable argument functions.  Sprintrmt is retained for
 * backward compatibility only and should not be used for new development.   
 *
 * Usage:	error (fmt, ARGS)
 *		char *fmt;
 *		int ARGS;
 *		{
 *			. . .
 *			sprintrmt (buf, fmt, SPRINTARGS);
 *			. . .
 *		}
 */
# define ARGS a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
# define SPRINTARGS a1, a2, a3, a4, a5, a6, a7, a8, a9, a10

/*
 * System dependent standard input and output units
 */
# define IN_STD "stdin"
# define OUT_STD "stdout"
# define LUN FILE *
long dview (), dopen ();

/*
 * This is (essentially) the maximum length of an input line.
 */
# define MAXLINE 1024

/*
 * The maximum CSAVE length.  This number determines how big the biggest
 * loop can be.
 */
# define NSAVE 400

# endif
