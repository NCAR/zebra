/* 5/87 jc */
/*
 * Utility routines.
 */


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
# ifdef VMS
	int cl;
	while (len > 0)
	{
		cl = (len > 65535) ? 65535 : len;
		len -= cl;
		lib$movc5 (&0, 0, &ch, &cl, dest);
		dest += cl;
	}
# else
	while (len-- > 0)
		*dest++ = ch;
# endif
}

# endif
