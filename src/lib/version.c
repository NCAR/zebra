/*
 * Module to hold single copies of the version and copyright strings for
 * an application, to make them accessible through ident, strings, or
 * what, and through library routines.  The strings are referenced by
 * the version.h header file, which is included by defs.h.
 */

#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "defs.h"

RCSID("$Id: version.c,v 2.4 2002-02-12 23:36:22 granger Exp $")

char V_buffer[512];
/*
 * Hand edit these lines until it can be done automatically.  One possibility
 * is using the RCS state field, but even that requires running a command
 * manually.  Perhaps an explicit version tag script which tags with CVS as
 * well as updating the ChangeLog and this file.
 */
static const char _zl_version_id1[] = 
"@(#)$ZebraVersion: 5.0beta-ExportDate $";

static const char _zl_version_id2[] = 
"@(#)$ZebraVersion: Research Data Program, NCAR $";

static const char _zl_version_id3[] = 
"@(#)$Copyright: University Corporation for Atmospheric Research, 1987-2000 $";

const char *V_format (buf, a, b, c, d)
char *buf;
const char *a;
const char *b;
const char *c;
const char *d;
/*
 * Format up to four id strings onto consecutive lines of a string buffer.
 * At least the first must the non-NULL, the rest can be NULL.  ID strings
 * must contain a '$' near the beginning and near the end.
 */
{
	sprintf (buf, "%s", strchr(a, '$') + 1);
	if (b)
		sprintf (strrchr(buf,'$'), "\n%s", strchr(b,'$') + 1);
	if (c)
		sprintf (strrchr(buf,'$'), "\n%s", strchr(c,'$') + 1);
	if (d)
		sprintf (strrchr(buf,'$'), "\n%s", strchr(d,'$') + 1);
	sprintf (strrchr(buf,'$'), "\n");
	return (buf);
}


const char *V_version()
{
	static char buf[128];
	return (V_format (buf, _zl_version_id1, _zl_version_id2,
			  _zl_version_id3, NULL));
}


/*
 * Copyright stuff, so that it also gets linked into every program.
 */
static const char *Copyright = 
"		Copyright (C) 1987-1996 by UCAR\n\
	University Corporation for Atmospheric Research\n\
		   All rights reserved\n\
\n\
No part of this work covered by the copyrights herein may be reproduced\n\
or used in any form or by any means -- graphic, electronic, or mechanical,\n\
including photocopying, recording, taping, or information storage and\n\
retrieval systems -- without permission of the copyright owner.\n\
\n\
This software and any accompanying written materials are provided \"as is\"\n\
without warranty of any kind.  UCAR expressly disclaims all warranties of\n\
any kind, either express or implied, including but not limited to the\n\
implied warranties of merchantibility and fitness for a particular purpose.\n\
UCAR does not indemnify any infringement of copyright, patent, or trademark\n\
through use or modification of this software.  UCAR does not provide \n\
maintenance or updates for its software.\n";


const char *V_copyright()
{
	return (Copyright);
}

