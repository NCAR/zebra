/*
 * Basic parsing stuff.
 */
static char *rcsid = "$Id: Parse.c,v 2.0 1991-07-18 23:07:02 corbet Exp $";

# include "defs.h"

int
CommaParse (string, substrings)
char	*string, **substrings;
/*
 * Parse comma-separated names from 'string' by putting NULLs in place of
 * the commas, and putting pointers to the beginning of each name into the
 * 'substrings' array.  Return the number of substrings in the string.
 */
{
	int	i = 0, nsubs = 0;

	while (TRUE)
	{
	/*
	 * Skip leading white space
	 */
		while (string[i] == ' ' || string[i] == '\t')
			i++;

		if (string[i] == '\0')
			break;
	/*
	 * We're at the beginning of a substring
	 */
		substrings[nsubs++] = string + i;
	/*
	 * Skip characters until we hit a comma or the end of 'string'
	 */
		while (string[i] != ',' && string[i] != '\0')
			i++;
	/*
	 * Replace a comma with a NULL or quit if we are at the end
	 */
		if (string[i] == ',')
			string[i++] = '\0';
		else
			break;
	}

	return (nsubs);
}



