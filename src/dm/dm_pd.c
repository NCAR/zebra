/* $Id: dm_pd.c,v 1.1 1990-04-26 16:23:43 corbet Exp $ */
/*
 * Plot description related routines.
 */
# include <sys/types.h>
# include <sys/param.h>
# include <sys/file.h>
# include <fcntl.h>
# include <dirent.h>

# include <ui_symbol.h>
# include <ui_error.h>
# include <pd.h>




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
	rpd.rp_len = lseek (fd, 0, L_XTND);
	(void) lseek (fd, 0, L_SET);
/*
 * Just pull it in.
 */
	rpd.rp_data = malloc (rpd.rp_len);
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
	getwd (wd);
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
	ui_printf ("Loading file %s...", file);
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
	ui_printf ("PD %s\n", name);
	pda_StorePD (pd, name);
}
