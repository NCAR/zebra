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
# include <config.h>
# include <ui_symbol.h>
# include "defs.h"

MAKE_RCSID("$Id: ConfigVars.c,v 1.6 1994-05-24 02:37:38 granger Exp $")

/*
 * Keep the directories around for queries.
 */
# define DIRLENGTH 80
static char Basedir[DIRLENGTH], Bindir[DIRLENGTH], Libdir[DIRLENGTH];
static char Projdir[DIRLENGTH];




static void
InitDirVariables ()
/*
 * Initialize the relocatable directory variables, if not yet done.
 */
{
	static int done = FALSE;
	char *getenv ();
	char *envbase;

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
 * Try to figure out a project directory.  If they haven't given anything
 * explicit, just take the current directory and hope for the best.  If
 * the project envariable is an absolute path, take it as is.
 */
	if ((envbase = getenv ("ZEB_PROJECT")) && envbase[0] == '/')
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




void
SetupConfigVariables ()
/*
 * Set up the configuration variables.  You must have called ui_init first.
 */
{
	stbl vtable = usy_g_stbl ("ui$variable_table");
	SValue v;
	
	InitDirVariables ();
/*
 * Now set the variables.
 */
	v.us_v_ptr = Basedir;
	usy_s_symbol (vtable, "c$basedir", SYMT_STRING, &v);
	v.us_v_ptr = Libdir;
	usy_s_symbol (vtable, "c$libdir", SYMT_STRING, &v);
	v.us_v_ptr = Bindir;
	usy_s_symbol (vtable, "c$bindir", SYMT_STRING, &v);
	v.us_v_ptr = Projdir;
	usy_s_symbol (vtable, "c$projdir", SYMT_STRING, &v);
/*
 * Data dir is separate.  RDSS doesn't change, I don't think.
 */
 	if ((v.us_v_ptr = getenv ("DATA_DIR")) == 0)
		v.us_v_ptr = DATADIR;
	usy_s_symbol (vtable, "c$datadir", SYMT_STRING, &v);
	v.us_v_ptr = RDSSDIR;
	usy_s_symbol (vtable, "c$rdssdir", SYMT_STRING, &v);
/*
 * Lastly, make our message handler name available
 */
	v.us_v_ptr = (char *) msg_myname ();
	usy_s_symbol (vtable, "c$msgname", SYMT_STRING, &v);
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
