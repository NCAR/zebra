/*
 * Various parameters of interest.
 */
# ifndef NULL
#   define NULL 0
# endif

# define TRUE -1
# define FALSE 0

# define ___ 0
/*
 * For copying data.
 */
# define COPY(to,from,len) slowcopy (len, from, to)

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
