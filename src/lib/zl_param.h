/* $Id: zl_param.h,v 2.5 1999-03-01 02:04:46 burghart Exp $ */
/*
 * Basic UI types needed regardless of whether we're linking with UI.
 */


# ifndef ZL_PARAM_SYMBOLS
# define ZL_PARAM_SYMBOLS
# ifndef UI_PARAM_SYMBOLS	/* otherwise this is all already defined */


/*
 * Data types of interest.
 */
typedef unsigned char byte;	/* Basic byte variable */

/*
 * So far only g++ has the predefined bool type per the final draft 
 * ANSI C++ standard.  Everywhere else, we have to use our own typedef.
 * 
 * We choose "typedef int" rather than "typedef char" because g++'s bool
 * is 4 bytes on all the systems tested.  We must have a size match for 
 * symbols and structures shared between g++ and non-g++ object modules.
 */
# ifdef BOOL_IS_DEAD
# if !(__cplusplus && __GNUC__)
typedef int bool;		/* Boolean variable	*/
# endif
# endif


struct date_st
{
  int	ds_yymmdd;	/* Day portion	*/
  int	ds_hhmmss;	/* time portion */
};
typedef struct date_st date;		/* Date in date/time format */

/*
 * Symbol types.
 */
# define SYMT_FLOAT	0	/* Floating point number	*/
# define SYMT_INT	1	/* Integer			*/
# define SYMT_STRING	2	/* C string type		*/
# define SYMT_DATE	3	/* ROBOT/MDA date format	*/
# define SYMT_BOOL	4	/* Boolean value		*/
# define SYMT_SYMBOL	5	/* Symbol table			*/
# define SYMT_POINTER	6	/* General pointer to somewhere	*/
# define SYMT_UNDEFINED 99	/* Undefined symbol		*/

/*
 * this union type is used to pass symbol values around.
 */
typedef union usy_value
{
	int us_v_int;		/* Integer symbol value		*/
	float	us_v_float;	/* Floating point value		*/
	date	us_v_date;	/* Date value			*/
	char	*us_v_ptr;	/* Everything else		*/
} SValue;

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

/*
 * Utilities.
 */
# define getvm(size) (char *)malloc(size)
# define relvm(space) free((char *)(space))
# define NEW(type) ((type *) getvm (sizeof (type)))
# define zfill(stuff,size) memset((char *)(stuff),0,(int)(size))

/*
 * Cheap absolute value.
 */
# ifndef ABS
# define ABS(v) (((v) < 0) ? -(v) : v)
# endif

# endif /* UI_PARAM_SYMBOLS */
# endif /* ZL_PARAM_SYMBOLS */
