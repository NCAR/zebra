/*		Copyright (C) 1987,88,89,90,91,92,93 by UCAR
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

# include <unistd.h>
# include "defs.h"


int
FindFile (file, spath, dest)
char *file, *spath, *dest;
/*
 * Try to find the given file by applying components from the path.
 */
{
	char *path, *delim;
	char *strchr ();
/*
 * Try to find the file as given.
 */
	if (! access (file, F_OK))
	{
		strcpy (dest, file);
		return (TRUE);
	}
/*
 * Oops.  No such luck.  Now we try the search path
 * and see if that works any better.
 */
	for (delim = path = spath; delim; path = delim + 1)
	{
	/*
	 * Build up a new program name from the next path entry.
	 */
		if ((delim = strchr (path, ',')) != 0)
		{
			strncpy (dest, path, delim - path);
			dest[delim - path] = '\0';
		}
		else
			strcpy (dest, path);
		strcat (dest, "/");
		strcat (dest, file);
	/*
	 * See if this one exists.
	 */
		if (! access (dest, F_OK))
			return (TRUE);
	} 	
/*
 * Nope.
 */
 	return (FALSE);
}
