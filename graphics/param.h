/*
 * Various parameters of interest.
 */
# include <graphdev.h>

# define NULL 0

# ifdef  AIXV3
# undef  TRUE
# endif
# define TRUE -1
# define FALSE 0

# define ___ 0
/*
 * For copying data.
 */
# define COPY(to,from,len) slowcopy (len, from, to)
# ifdef VMS
# define memcpy(dest,src,len) { int __LEN__ = (len); \
			lib$movc3 (&__LEN__, src, dest); }
/*
# define memset(dest,v,len) {int __LEN__ = (len); \
			lib$movc5 (&0, 0, v, &__LEN__, dest); }
*/
# endif

/*
 * Conversion from world to device coords.
 */
# define W_TO_DC(p,min,max,res) \
	((int) ((((p) - (min))/((max) - (min)))*(float)(res-1)))

/*
 * I prefer this to do/while...
 */
# define REPEAT do {
# define UNTIL(b) } while (! (b))

/*
 * Declarations of various routines.
 */
char *getvm ();
