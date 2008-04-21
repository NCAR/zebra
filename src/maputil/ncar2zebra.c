/*
 * Extract map points for a user-specified area from the NCAR Graphics 
 * map file /locallib/ezmapdata and write them in a format suitable for
 * use in robot et al.
 */

# include <stdio.h>
# include <string.h>
# include <stdlib.h>

# define MAXPTS	1024

/*
 * Limits for a group of points
 */
float	Minlat, Maxlat, Minlon, Maxlon;

/*
 * User limits
 */
float	Usr_minlat, Usr_maxlat, Usr_minlon, Usr_maxlon;

/*
 * U.S. map dataset
 */
# define NCARMAP	"/local/lib/ncarg/database/ezmapdata"
FILE	*MapFile;




/*
 * Scarfed from RDSS util fixdir.c to avoid needing the rdss util library.
 */
static void
fixdir_t (env, def, file, dest, type)
char *env, *def, *file, *dest, *type;
/*
 * Translate an environment variable, and qualify the
 * given FILE by the result.  If there is no translation for
 * ENV, the DEF name, if non-null, will be used.
 * The result is put into DEST.
 * Also, if a file lacks a type string, add it.
 */
{
	char *trans, *slash;
/*
 * First of all, look at the file name.  If it starts with a slash,
 * we simply take it as it is.
 */
	if (file[0] == '/')
		strcpy (dest, file);
/*
 * If the environment variable translates, use it.
 */
 	else if (trans = getenv (env))
	{
		strcpy (dest, trans);
		strcat (dest, "/");
		strcat (dest, file);
	}
/*
 * Failing that, copy the def if it exists, then put in the file.
 */
 	else if (def)
	{
		strcpy (dest, def);
		strcat (dest, "/");
		strcat (dest, file);
	}
	else
		strcpy (dest, file);
/*
 * Look for an extension, in a rather simple sort of way.
 */
 	if ((slash = strrchr (dest, '/')) == 0)
		slash = dest;
	if (! strchr (slash, '.'))
		strcat (dest, type);
}



main ()
{
	FILE	*projmap;
	char	proj[40], fname[80];
	int	ihdr[5], npts, i, ok_rec, rtype;
	float	fhdr[4], pts[MAXPTS];
/*
 * Get the project name and open a file 'project'.map
 */
	printf ("Enter project name: ");
	fgets (proj, sizeof (proj), stdin);

        fixdir_t ("", ".", proj, fname, ".map");

	projmap = fopen (fname, "w");
/*
 * Open our main dataset
 */
	MapFile = fopen (NCARMAP, "r");

	if (! MapFile)
	{
		fprintf (stderr, "Unable to open file '%s'\n", NCARMAP);
		exit (1);
	}
/*
 * Get the user limits
 */
	printf ("Enter the longitude of the west edge: ");
	scanf ("%f", &Usr_minlon);
	printf ("                           east edge: ");
	scanf ("%f", &Usr_maxlon);
	printf ("\nEnter the latitude of the south edge: ");
	scanf ("%f", &Usr_minlat);
	printf ("                          north edge: ");
	scanf ("%f", &Usr_maxlat);
/*
 * Loop through the dataset and copy records which fit within
 * the user limits
 */
	for (;;)
	{
	/*
	 * Get the header stuff for the next group of points
	 * (The first 4 bytes aren't really part of the header, they're
	 * the record length written by FORTRAN I/O)
	 */
		if (fread (ihdr, sizeof (int), 5, MapFile) < 5)
			break;

		npts = ihdr[1];
		rtype = ihdr[2];

		if (npts > MAXPTS)
		{
			printf ("MAXPTS too small!!\n");
			exit (1);
		}

		if (fread (fhdr, sizeof (float), 4, MapFile) < 4)
			break;

		Maxlat = fhdr[0];
		Minlat = fhdr[1];
		Maxlon = fhdr[2];
		Minlon = fhdr[3];		
	/*
	 * Read the points
	 */
		if (fread (pts, sizeof (float), npts, MapFile) < npts)
		{
			printf ("Couldn't read %d points!", npts);
			break;
		}
	/*
	 * Trailing FORTRAN record length
	 */
		fread (ihdr, sizeof (int), 1, MapFile);
	/*
	 * Only use record type 2, which includes continental outlines, 
	 * international outlines, and U.S. state outlines
	 */
		if (rtype < 1 || rtype > 4)
			printf ("Weird rtype %d\n", rtype);
		if (rtype == 2)
			continue;
	/*
	 * Check if this group has points within the user limits
	 * and move on if we can't use it
	 */
		ok_rec = ((Minlon >= Usr_minlon && Minlon <= Usr_maxlon) ||
			(Maxlon >= Usr_minlon && Maxlon <= Usr_maxlon)) &&
			((Minlat >= Usr_minlat && Minlat <= Usr_maxlat) ||
			(Maxlat >= Usr_minlat && Maxlat <= Usr_maxlat));

		if (! ok_rec)
			continue;
	/*
	 * Write the header
	 */
		fprintf (projmap, "%4d%10.3f%10.3f%10.3f%10.3f", npts,
			Maxlat, Minlat, Maxlon, Minlon);
	/*
	 * Write out the points
	 */
		for (i = 0; i < npts; i += 2)
		{
			if ((i % 8) == 0)
				fprintf (projmap, "\n");

			fprintf (projmap, "%10.3f%10.3f", pts[i], pts[i+1]);
		}

		fprintf (projmap, "\n");
	}
}
