static char *rcsid = "$Id: dm_pd.c,v 2.5 1994-11-19 00:28:45 burghart Exp $";
/*
 * Plot description related routines.
 */
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
# include <stdio.h>
# include <string.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <sys/param.h>
# include <sys/file.h>
# include <fcntl.h>
# include <dirent.h>

# include <ui_symbol.h>
# include <ui_error.h>
# include <pd.h>
#ifdef SVR4
# include <unistd.h>
#endif



plot_description
pdread (file)
char *file;
/*
 * Load in a single plot description from a file.
 */
{
	int fd;
	raw_plot_description rpd;
	char *malloc ();
/*
 * Open the file, and find out how long it is.
 */
	if ((fd = open (file, O_RDONLY)) < 0)
		ui_error ("Unable to open '%s'\n", file);
#if defined(SVR4)
	rpd.rp_len = lseek (fd, 0, SEEK_END);
	(void) lseek (fd, 0, SEEK_SET);
#else
	rpd.rp_len = lseek (fd, 0, L_XTND);
	(void) lseek (fd, 0, L_SET);
#endif
/*
 * Just pull it in.
 */
	rpd.rp_data = malloc (rpd.rp_len + 1);
	rpd.rp_data[rpd.rp_len] = '\0';
	if (read (fd, rpd.rp_data, rpd.rp_len) < rpd.rp_len)
		ui_warning ("Read incomplete...");
	close (fd);
/*
 * Compile it.
 */
	return (pd_Load (&rpd));
}




pdload (file, name)
char *file, *name;
/*
 * Load and name a plot description from a file.
 */
{
	pda_StorePD (pdread (file), name);
}



void
pdwrite (pd, path)
plot_description pd;
char *path;
{
	raw_plot_description *rpd;
	FILE *out = NULL;
	char *line, *next;
	struct stat buf;
	char filename[512];
	char pdname[128];

	if (path)
	{
		/*
		 * If the filename parameter is actually a directory, create 
		 * the filename ourselves in the specified directory.
		 */
		if (stat (path, &buf) == 0 && S_ISDIR(buf.st_mode))
		{
			if (! pd_Retrieve (pd, "global", "pd-name", 
					   pdname, SYMT_STRING))
			{
				ui_error ("pdwrite: pd has no name");
				return;
			}
			sprintf (filename, "%s/%s.pd", path, pdname);
		}
		else
		{
			strcpy (filename, path);
		}
		out = fopen (filename, "w");
		if (! out)
		{
			ui_error ("%s: could not open file %s",
				  "pdwrite", filename);
			return;
		}
	}
	/*
	 * Print a line at a time else ui_printf() chokes.
	 */
	rpd = pd_Unload (pd);
	for (line = rpd->rp_data; line != NULL; line = next)
	{
		next = strchr (line, '\n');
		if (next)
			*next++ = '\0';
		if (out)
			fprintf (out, "%s\n", line);
		else
			ui_printf ("%s\n", line);
	}
	pd_RPDRelease (rpd);
	if (out)
		fclose (out);
}




pddir (dir)
char *dir;
/*
 * Pull in all PD files from a directory.
 */
{
	DIR *dp = opendir (dir);
	struct dirent *ent;
	char *dot, *strrchr (), *getcwd (), wd[MAXPATHLEN];
/*
 * Make sure we have a directory.
 */
	if (! dp)
		ui_error ("Unknown directory: '%s'", dir);
/*
 * Move there.
 */
#if defined(SVR4) || defined(SYSV)
	getcwd (wd,(size_t)(MAXPATHLEN+1));
#else
	getwd (wd);
#endif
	chdir (dir);
/*
 * Now read through it.
 */
	while (ent = readdir (dp))
	{
		if (! (dot = strrchr (ent->d_name, '.')) || strcmp(dot, ".pd"))
			continue;
		pddirfile (ent->d_name);
	}
/*
 * Clean up.
 */
	closedir (dp);
	chdir (wd);
}





pddirfile (file)
char *file;
/*
 * Pull in this file.
 */
{
	plot_description pd;
	char name[200];
# ifdef titan
	file -= 2;		/* AAAAAAAAAAARRRRRGGGHHH!!!!!!!!! */
# endif
/*
 * Pull in the file.
 */
	ERRORCATCH
		pd = pdread (file);
	ON_ERROR
		return;
	ENDCATCH
/*
 * Look first for a name attribute.  If that fails, name it after the file
 * with the ".pd" removed.
 */
	if (! pd_Retrieve (pd, "global", "pd-name", name, SYMT_STRING) &&
	    ! pd_Retrieve (pd, "defaults", "pd-name", name, SYMT_STRING))
	{
		char *dot, *strrchr ();

		strcpy (name, file);
		if (dot = strrchr (file, '.'))
			*dot = '\0';
	}
/*
 * Stash away the PD.
 */
	pda_StorePD (pd, name);
}
