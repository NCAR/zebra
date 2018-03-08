/* 2/85 jc */
/*
 * Virtual memory routines.
 */
# include <stdlib.h>


char *
getvm (size)
int size;
/*
 * Return a pointer to a newly allocated chunk of memory of the given size.
 */
{
# if defined (__osf__) || defined (AIXV3)
        return ( (char *) malloc (size));
# else 
	return (malloc (size));
# endif
}



void
relvm (space)
char *space;
/*
 * Return the given chunk of memory, which had better have been obtained
 * from getvm.
 */
{
	free (space);
}
