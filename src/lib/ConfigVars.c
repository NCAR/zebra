/*
 * Set up configuration variables so that they are available at the UI level.
 */
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
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
# include <string.h>

# include <config.h>
# include "defs.h"
# include "message.h"
# include "setup.h"

RCSID("$Id: ConfigVars.c,v 1.12 2000-04-06 19:40:57 burghart Exp $")

/*
 * Keep the directories around for queries.
 */
char Basedir[DIRLENGTH], Bindir[DIRLENGTH], Libdir[DIRLENGTH];
char Projdir[DIRLENGTH], Datadir[DIRLENGTH];


void
InitDirVariables ()
/*
 * Initialize the relocatable directory variables, if not yet done.
 */
{
	static int done = FALSE;
	char *envbase;
	char *envp;

	if (done)
		return;
	envbase = getenv ("ZEB_TOPDIR");
/*
 * If no base directory in the environment, use the compile-time defaults.
 */
	if (! envbase)
	{
		strcpy (Basedir, BASEDIR);
		strcpy (Libdir, LIBDIR);
		strcpy (Bindir, BINDIR);
	}
/*
 * Otherwise build the names using the environment value.
 */
	else
	{
		strcpy (Basedir, envbase);
		sprintf (Bindir, "%s/bin", envbase);
		sprintf (Libdir, "%s/lib", envbase);
	}
/*
 * Use DATA_DIR from the environment, else use the default
 */
	if ((envp = getenv ("DATA_DIR")) != NULL)
		strcpy (Datadir, envp);
	else
		strcpy (Datadir, DATADIR);
/*
 * Try to figure out a project directory.  If they haven't given anything
 * explicit, just take the current directory and hope for the best.  If
 * the project envariable is an absolute path, take it as is.
 */
	if ((envbase = getenv ("ZEB_PROJDIR")))
	{
		strcpy (Projdir, envbase);
	}
	else if ((envbase = getenv ("ZEB_PROJECT")) && envbase[0] == '/')
	{
		strcpy (Projdir, envbase);
	}
	else if (envbase)
	{
		sprintf (Projdir, "%s/project/%s", Basedir, envbase);
		if (access (Projdir, R_OK | W_OK | X_OK) < 0)
		{
			sprintf (Projdir, "%s/%s", Basedir, envbase);
			if (access (Projdir, R_OK | W_OK | X_OK) < 0)
			{
				strcpy (Projdir, ".");
			}
		}
        }
	else
		strcpy (Projdir, ".");
	done = TRUE;
}



char *
GetBaseDir ()
{
	InitDirVariables ();
	return (Basedir);
}


char *
GetLibDir ()
{
	InitDirVariables ();
	return (Libdir);
}


char *
GetBinDir ()
{
	InitDirVariables ();
	return (Bindir);
}


char *
GetProjDir ()
{
	InitDirVariables ();
	return (Projdir);
}


char *
GetDataDir ()
{
	InitDirVariables ();
	return (Datadir);
}

