/*
 * Various user interface utilities.
 */
# include <string.h>
# include <ctype.h>



char *
zapcase (string)
char * string;
/*
 * Convert this string to lower case, in place.  The return value is the
 * string itself.
 */
{
	char *s = string;

	while (*s)
	{
		if (isupper (*s))
			*s = tolower (*s);
		s++;
	}
	return (string);
}




void
zfill (stuff, size)
char *stuff;
int size;
/*
 * Zero fill this area.
 */
{
# ifdef notdef
	for (; size > 0; size--)
		*stuff++ = 0;
# endif
	memset (stuff, 0, size);
}


void
uiu_slowcopy (len, from, to)
char	*from, *to;
int	len;
/*
 * Copy this data over.  This routine is used for arrays that are too
 * big for LIB$MOVC3, or (conceivably) on systems with no such routine.
 */
{
# ifdef notdef
	for (; len > 0; len--)
		*to++ = *from++;
# endif
	memcpy (to, from, len);
}
