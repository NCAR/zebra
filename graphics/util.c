/* 5/87 jc */
/*
 * Utility routines.
 */

void
slowcopy (len, from, to)
int len;
char *from, *to;
/*
 * Copy this data over.  This routine is used for arrays that are too
 * big for LIB$MOVC3, or (conceivably) on systems with no such routine.
 */
{
	for (; len > 0; len--)
		*to++ = *from++;
}



# ifdef NO_MEMSET

memset (dest, ch, len)
char *dest, ch;
int len;
/*
 * Fake the C memset for now.
 */
{
	while (len-- > 0)
		*dest++ = ch;
}

# endif
