/*
 * Various user interface utilities.
 */
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





zfill (stuff, size)
char *stuff;
int size;
/*
 * Zero fill this area.
 */
{
	for (; size > 0; size--)
		*stuff++ = 0;
}


uiu_slowcopy (len, from, to)
char	*from, *to;
int	len;
/*
 * Copy this data over.  This routine is used for arrays that are too
 * big for LIB$MOVC3, or (conceivably) on systems with no such routine.
 */
{
	for (; len > 0; len--)
		*to++ = *from++;
}
