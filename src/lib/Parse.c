/*
 * Basic parsing stuff.
 */
static char *rcsid = "$Id: Parse.c,v 2.2 1993-10-07 16:06:28 corbet Exp $";
/*		Copyright (C) 1987,88,89,90,91 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

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
	return (ParseLine (string, substrings, ','));
}




int
ParseLine (string, substrings, delim)
char *string, **substrings, delim;
/*
 * Parse up this line using delim, which probably should ought not to be
 * white space.
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
		while (string[i] != delim && string[i] != '\0')
			i++;
	/*
	 * Replace a comma with a NULL or quit if we are at the end
	 */
		if (string[i] == delim)
			string[i++] = '\0';
		else
			break;
	}

	return (nsubs);
}



