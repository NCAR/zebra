/* 2/85 jc */
/*
 * Virtual memory routines.
 * For the VAX, these deal directly with the RTL routines
 * and short out the weirdness of malloc () and free ().
 * Otherwise, they just shunt the calls off to malloc () and free ().
 */
# ifdef VMS
#	include <stdio.h>
#	include <ssdef.h>
#	define ALLOC_FLAG	0x25544024
#	define error(s) (((s) & 0x1) == 0)
# endif
# include <malloc.h>


char *
getvm (size)
int size;
/*
 * Return a pointer to a newly allocated chunk of memory of the given size.
 */
{
# ifdef notdef	/* Try to speed things up under sysV */
	static int first = 1;
	if (first)
	{
		first = 0;
		mallopt (M_MXFAST, 80);
	}
# endif
# ifdef VMS
	int status, len;
	int *cp;
/*
 * Attempt to allocate the space.
 */
	len = size + 8;
	status = lib$get_vm (&len, &cp);
	if (error (status))
	{
		errmes (&status);
		c_panic ("Error allocating %d bytes\n", len);
		exit (status);
	}
/*
 * We store the length of the chunk in the second longword before the 
 * actual space returned, and hope dearly that nobody trashes it.
 */
	cp[0] = len;
	cp[1] = ALLOC_FLAG;
	return (cp + 2);
# else

# if defined (__osf__) || defined (AIXV3)
        return ( (char *) malloc (size));
# else 
	return (malloc (size));
# endif

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
# ifdef VMS
	int status, len;
/*
 * Check for our flag.
 */
	space -= 2;
	if (space[1] != ALLOC_FLAG)
		c_panic ("Bad memory chunk, l: %x, fl: %x", space[0], space[1]);
	len = *space;
/*
 * Return the storage.
 */
	status = lib$free_vm (&len, &space);
	if (error (status))
	{
		printf ("Error returning storage\n");
		exit (status);
	}
# else
	free (space);
# endif
}
