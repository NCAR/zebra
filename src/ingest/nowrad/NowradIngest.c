/*
 * Consume NOWRAD images into the data store.
 */
/*		Copyright (C) 1992 by UCAR
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

static char *rcsid = 
   "$Id: NowradIngest.c,v 1.1 1992-09-11 06:34:16 granger Exp $";

# include <copyright.h>
# include <unistd.h>
# include <errno.h>
# include <math.h>
# include <stdio.h>
# include <dirent.h>
# include <defs.h>
# include <message.h>
# include <timer.h>
# include <DataStore.h>


/*
 * Our platform
 */
PlatformId	Plat;

/*
 * The image and the grid to which we're mapping it
 */
unsigned char	*Image, *Grid;
int	Nx, Ny, GridX = 600, GridY = 400;

/*
 * Image unpacking info
 */
int	Nbytes, Prefixlen, Linelen, Xres, Yres;

/*
 * Image limits in line/elem and lat/lon coordinates
 */
int	Minelem, Maxelem, Minline, Maxline;
float	Minlon, Maxlon, Minlat, Maxlat;

/*
 * Scaling for 2 byte values
 */
int	Scale_2b = 80;

/*
 * Useful stuff
 */
# define BETWEEN(x,lower,upper)	(((x)-(lower))*((x)-(upper)) <= 0)
# define DEG_TO_RAD(x)	((x)*0.017453292)
# define RAD_TO_DEG(x)	((x)*57.29577951)
# define DEG_TO_KM(x)	((x)*111.3238367) /* on a great circle */
# define BADVAL	-999.0

int	Mdays[] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

/*
 * The directory to ingest from
 */
char	*DirName;

/*
 * The temporary file to use if we have to uncompress a file
 */
# define TEMPFILE	"/tmp/nowrad"

/*
 * The symbol table and filename for the list of already ingested files
 */
stbl	DoneTable;
# define DONEFILES	"DoneFiles"
char	Listfile[100];

/*
 * Prototypes
 */
# ifdef __STDC__
	static void	swapfour (int *, int);
	static void	ll_limits (void);
	static unsigned char	imageval (int, int);
	static int	MDispatcher (struct message *);
	static void	DoDirectory (void);
	static void	CheckForNew (void);
	static void	MakeDoneList (void);
	static int	DoFile (char *, int *, int *);
# else
	static void	swapfour ();
	static void	ll_limits ();
	static unsigned char	imageval ();
	static int	MDispatcher ();
	static void	DoDirectory ();
	static void	CheckForNew ();
	static void	MakeDoneList ();
	static int	DoFile ();
# endif




main (argc, argv)
int argc;
char **argv;
{
/*
 * Checking
 */
	if (argc != 2)
	{
		printf ("Usage: %s <directory>\n", argv[0]);
		exit (1);
	}

	DirName = argv[1];
/*
 * Initialize UI symbol stuff and connect to the message handler
 */
	usy_init ();
	msg_connect (MDispatcher, "NowradIngest");
/*
 * DS initialization
 */
	ds_Initialize ();
	if ((Plat = ds_LookupPlatform ("nowrad")) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform 'nowrad'");
		return (FALSE);
	}
/*
 * Build the list of done files
 */
# ifdef notdef
	sprintf (Listfile, "%s/%s", DirName, DONEFILES);
	MakeDoneList ();
# endif
/*
 * Set up a periodic timer, then wait for things to happen
 */
	tl_AddRelativeEvent (CheckForNew, 0, 0, 60 * INCFRAC);
	msg_await ();
}




static void
MakeDoneList ()
/*
 * Build the list of already done files
 */
{
	char	fname[100];
	SValue	v;
	FILE	*fp;
/*
 * Open up the list file
 */
	DoneTable = usy_c_stbl ("DoneTable");
	if ((fp = fopen (Listfile, "r")) == NULL)
		return;
/*
 * Go through and put each filename into the symbol table
 */
	while (fgets (fname, 100, fp))
	{
	/*
	 * Remove the trailing <CR>
	 */
		fname[strlen (fname) - 1] = '\0';
	/*
	 * Add this file to the symbol table
	 */
		v.us_v_int = 1;
		usy_s_symbol (DoneTable, fname, SYMT_INT, &v);
		msg_ELog (EF_INFO, "Added '%s' to done list", fname);
	}
/*
 * All done
 */
	fclose (fp);
}




static void
DoDirectory ()
/*
 * Deal with un-ingested files in the named directory
 */
{
	DIR	*dirp;
	FILE	*fp;
	int	type, date, time;
	SValue	v;
	char	fullname[100];
	struct dirent	*entry;
/*
 * Open up the directory
 */
	if ((dirp = opendir (DirName)) == NULL)
	{
		msg_ELog (EF_EMERGENCY, 
			"Unable to open nowrad directory '%s'", DirName);
		exit (1);
	}
/*
 * Open the done list file
 */
	fp = fopen (Listfile, "a");
/*
 * Loop through the entries
 */
	while ((entry = readdir (dirp)) != NULL)
	{
		char	*fname = entry->d_name;

		sprintf (fullname, "%s/%s", DirName, fname);
	/*
	 * Don't do ., .., or DoneFiles
	 */
		if (! strcmp (fname, ".") || ! strcmp (fname, "..") ||
			! strcmp (fname, DONEFILES))
			continue;
	/*
	 * Bail out if this one's been done
	 */
		if (usy_g_symbol (DoneTable, entry->d_name, &type, &v))
		{
			msg_ELog (EF_DEBUG, "Already did '%s'", fullname);
			continue;
		}
	/*
	 * Ingest this file
	 */
		msg_ELog (EF_DEBUG, "Doing '%s'", fullname);
		if (! DoFile (fullname, &date, &time))
			continue;
	/*
	 * Add this filename to the done list symbol table and file
	 */
		v.us_v_int = 1;
		usy_s_symbol (DoneTable, entry->d_name, SYMT_INT, &v);

		fprintf (fp, "%s\n", entry->d_name);
	}
/*
 * Close the directory and the list file
 */
	closedir (dirp);
	fclose (fp);
}




static void
CheckForNew ()
/*
 * See if we have a new "mdradar.Z" file and deal with it 
 */
{
	int	date, time;
	char	newname[100], fullname[100];

	sprintf (fullname, "%s/mdradar.Z", DirName);
	if (access (fullname, F_OK) != 0)
	{
		msg_ELog (EF_DEBUG, "No mdradar file this time");
		return;
	}

	if (DoFile (fullname, &date, &time))
	{
		sprintf (newname, "%s/%d%04d.fl.Z", DirName, date, time / 100);
		if (rename (fullname, newname) < 0)
			msg_ELog (EF_PROBLEM, "Error %d renaming '%s' to '%s'",
				errno, fullname, newname);
	}
}




static int
DoFile (fname, rdate, rtime)
char	*fname;
int	*rdate, *rtime;
/*
 * Ingest the named NOWRAD file.  Return TRUE and the date and time of the
 * image if we're successful.
 */
{
	FILE	*infile;
	int	*header, *nav_cod;
	int	i, j, line, elem, status, year, month, day, stuff[128];
	int	one = 1, two = 2, use_temp;
	float	dummy, fline, felem, lat, lon, latstep, lonstep;
	time	t;
	char	fldname[5], *c, lasttwo[3];
	RGrid	rg;
	DataObject	dobj;
	ScaleInfo	scale;
	Location	loc;
/*
 * Uncompress the file if the last two characters of the 
 * filename are ".Z"
 */
	strcpy (lasttwo, fname + strlen(fname) - 2);
	if (strcmp (lasttwo, ".Z") == 0)
	{
		sprintf (stuff, "cat %s | uncompress -c > %s", fname, 
			TEMPFILE);
			use_temp = TRUE;
		system (stuff);
	}
/*
 * Open the input file
 */
	if (use_temp)
	{
	/*
	 * Open the file and unlink now, and the file will go away 
	 * automatically when we're done
	 */
		infile = fopen (TEMPFILE, "r");
		unlink (TEMPFILE);
	}
	else
		infile = fopen (fname, "r");

	if (infile == 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening file '%s'\n", errno,
			fname);
		return (FALSE);
	}
/*
 * Read the 256 byte "area directory" header and the 512 byte
 * navigation codicil and swap bytes around in each one.  
 * NOTE: We don't swap in those portions which contain text
 */
	header = (int *) malloc (64 * sizeof (int));
	fread ((void *) header, 4, 64, infile);
	swapfour (header, 20);
	swapfour (header + 32, 19);

	nav_cod = (int *) malloc (128 * sizeof (int));
	fread ((void *) nav_cod, 4, 128, infile);
	swapfour (nav_cod + 1, 39);
/*
 * Verify that this is a PS (polar stereographic) image
 */
	if (strncmp (nav_cod, "PS  ", 4))
	{
		char	imtype[4];

		strncpy (imtype, nav_cod, 4);
		imtype[4] = '\0';

		msg_ELog (EF_PROBLEM, "'%s' contains a '%s' image, not PS",
			fname, imtype);
		fclose (infile);
		return (FALSE);
	}
/*
 * Image size (Nx x Ny)
 */
	Ny = header[8];
	Nx = header[9];
/*
 * Resolution (# of satellite units per image unit)
 */
	Yres = header[11];
	Xres = header[12];
/*
 * Image size (Nx x Ny), bytes per element and prefix length
 */
	Ny = header[8];
	Nx = header[9];
	Nbytes = header[10];
	Prefixlen = header[14];

	Linelen = Nx * Nbytes + Prefixlen;
/*
 * Field name from header word 51 (convert to lower case and remove spaces)
 */
	strncpy (fldname, header + 51, 4);
	fldname[4] = '\0';

	for (i = 0; i < 4; i++)
	{
		c = fldname + i;
		if (*c == ' ')
			*c = '\0';
		else
			*c = tolower (*c);
	}
/*
 * KLUGE: change "wsi " to "visr" for now, since the manually digitized radar
 * data we were getting used "visr"
 */
	if (! strcmp (fldname, "wsi "))
		strcpy (fldname, "visr");
/*
 * Read the image data
 */
	Image = (unsigned char *) malloc (Linelen * Ny);
	fread ((void *) Image, 1, Linelen * Ny, infile);
/*
 * We're done with the file
 */
	fclose (infile);
/*
 * Element and line limits
 */
	Minline = header[5];
	Maxline = Minline + (Ny - 1) * Yres;

	Minelem = header[6];
	Maxelem = Minelem + (Nx - 1) * Xres;
/*
 * Initialize the navigation stuff
 */
	status = nvxini_ (&one, nav_cod);
	if (status < 0)
	{
		msg_ELog (EF_PROBLEM, 
			"Bad navigation initialization for file '%s'", fname);
		return (FALSE);
	}

	status = nvxini_ (&two, "LL  ");
	if (status < 0)
	{
		msg_ELog (EF_PROBLEM, 
			"Can't use lan/lon coords for file '%s'", fname);
		return (FALSE);
	}
/*
 * Get the lat/lon limits
 * (NOTE: W longitudes are positive here, because that's what nvxeas
 * expects)
 */
	ll_limits ();
/*
 * Find the lat/lon steps
 */
	latstep = (Maxlat - Minlat) / (GridY - 1);
	lonstep = (Maxlon - Minlon) / (GridX - 1);
/*
 * Allocate the grid
 */
	Grid = (unsigned char *) malloc (GridX * GridY * sizeof (char));
/*
 * Loop through the points in the grid
 */
	for (j = 0; j < GridY; j++)
	{
		if (! ((j+1) % 20))
			msg_ELog (EF_DEBUG, "%s: line %d of %d", fname,
				j + 1, GridY);

		lat = Maxlat - j * latstep;

		for (i = 0; i < GridX; i++)
		{
		/*
		 * Translate lat/lon into line and element in the image
		 */
			lon = Maxlon - i * lonstep;
			status = nvxeas_ (&lat, &lon, &dummy, &fline, &felem,
				&dummy);

			line = (int)(fline + 0.5);
			elem = (int)(felem + 0.5);
		/*
		 * Assign this grid point
		 */
			if (status == 0)
				Grid[GridX * j + i] = imageval (line, elem);
			else
				Grid[GridX * j + i] = 0;
		}
	}
/*
 * Static data object initialization.
 */
	dobj.do_id = Plat;
	dobj.do_org = OrgImage;
	dobj.do_npoint = 1;
	dobj.do_aloc = ALLOC (Location);
	dobj.do_nfield = 1;
	dobj.do_fields[0] = fldname;
	dobj.do_flags = 0;
/*
 * Date and time (We have to convert from Julian date.  I hate that.)
 */
	year = header[3] / 1000;
	if ((year % 4) == 0)
		Mdays[2] = 29;	/* February has 29 days in leap years */

	day = header[3] % 1000;
	month = 1;
	while (day > Mdays[month])
		day -= Mdays[month++];

	t.ds_yymmdd = *rdate = year * 10000 + month * 100 + day;
	t.ds_hhmmss = *rtime = header[4];
/*
 * Fill in the rest of our data object.
 */
	dobj.do_begin = dobj.do_end = t;
	dobj.do_times = &t;

	dobj.do_badval = BADVAL;
/*
 * laziness KLUGE: the origin latitude is fixed for now
 */
	rg.rg_Xspacing = DEG_TO_KM (lonstep) * cos (DEG_TO_RAD (39.71));
	rg.rg_Yspacing = DEG_TO_KM (latstep);
	rg.rg_Zspacing = 0.0;

	rg.rg_nX = GridX;
	rg.rg_nY = GridY;
	rg.rg_nZ = 0;

	dobj.do_desc.d_img.ri_rg = &rg;

	scale.s_Scale = (Nbytes == 1) ? 1.0 : (float) Scale_2b;
	scale.s_Offset = 0.0;
	dobj.do_desc.d_img.ri_scale = &scale;

	loc.l_lat = Minlat;
	loc.l_lon = -Maxlon;
	loc.l_alt = 0.000;
	dobj.do_aloc = &loc;

	dobj.do_data[0] = (float *) Grid;
	msg_ELog (EF_INFO, "Nowrad image %dx%d deg, %.1f/%.1f", GridX, GridY,
		GridX * lonstep, GridY * latstep);
/*
 * Send it
 */
	ds_PutData (&dobj, TRUE);
	return (TRUE);
}




void
swapfour (array, count)
int	*array, count;
/*
 * Swap byte order (0123 -> 3210) for 'count' longwords in 'array'
 */
{
	int	i;
	char	*bytes, swapped[4];

	for (i = 0; i < count; i++)
	{
		bytes = (char *) &(array[i]);
		swapped[0] = bytes[3];
		swapped[1] = bytes[2];
		swapped[2] = bytes[1];
		swapped[3] = bytes[0];
		memcpy (bytes, swapped, 4);
	}
}




void
ll_limits ()
{
	float	fline, felem, dummy, lat, lon;
/*
 * Find the lat/lon limits based on the corners of the image
 * (NOTE: nvxsae returns W longitudes as positive, but that's OK since
 *  the numbers should only used for calls to nvxeas, which expects
 *  just that)
 */
	Minlat = Minlon = 999.0;
	Maxlat = Maxlon = -999.0;

	fline = Minline; felem = Minelem;
	nvxsae_ (&fline, &felem, &dummy, &lat, &lon, &dummy);
	Minlat = lat < Minlat ? lat : Minlat;
	Minlon = lon < Minlon ? lon : Minlon;
	Maxlat = lat > Maxlat ? lat : Maxlat;
	Maxlon = lon > Maxlon ? lon : Maxlon;

	fline = Minline; felem = Maxelem;
	nvxsae_ (&fline, &felem, &dummy, &lat, &lon, &dummy);
	Minlat = lat < Minlat ? lat : Minlat;
	Minlon = lon < Minlon ? lon : Minlon;
	Maxlat = lat > Maxlat ? lat : Maxlat;
	Maxlon = lon > Maxlon ? lon : Maxlon;

	fline = Maxline; felem = Minelem;
	nvxsae_ (&fline, &felem, &dummy, &lat, &lon, &dummy);
	Minlat = lat < Minlat ? lat : Minlat;
	Minlon = lon < Minlon ? lon : Minlon;
	Maxlat = lat > Maxlat ? lat : Maxlat;
	Maxlon = lon > Maxlon ? lon : Maxlon;

	fline = Maxline; felem = Maxelem;
	nvxsae_ (&fline, &felem, &dummy, &lat, &lon, &dummy);
	Minlat = lat < Minlat ? lat : Minlat;
	Minlon = lon < Minlon ? lon : Minlon;
	Maxlat = lat > Maxlat ? lat : Maxlat;
	Maxlon = lon > Maxlon ? lon : Maxlon;
}




static unsigned char
imageval (line, elem)
int	line, elem;
/*
 * Return the image value associated with satellite line/elem coordinates
 */
{
	int	image_x, image_y, pos;
	unsigned int	val;

	if (BETWEEN(line, Minline, Maxline) && BETWEEN(elem, Minelem, Maxelem))
	{
	/*
	 * Translate from satellite coordinates to image coordinates
	 */
		image_x = (int)((float)(elem - Minelem) / Xres + 0.5);
		image_y = (int)((float)(line - Minline) / Yres + 0.5);
	/*
	 * Find the offset into the image
	 */
		pos = image_y * Linelen + Prefixlen + image_x * Nbytes;
	/*
	 * Unpack the appropriate number of bytes
	 */
		switch (Nbytes)
		{
		    case 1:
			return (Image[pos]);
			break;
		    case 2:
		/*
		 * 2 bytes per element.
		 */
			val = Image[pos + 1] << 8 | Image[pos];
			val /= Scale_2b;
			if (val < 256)
				return ((unsigned char)(val));
			else
			{
				msg_ELog (EF_PROBLEM, 
					"BUG: scaled value %d too big", val);
				return (0);
			}
			break;
		    default:
			msg_ELog (EF_EMERGENCY, 
				"Exiting.  Cannot handle %d byte values.", 
				Nbytes);
			exit (1);
		}
	}
	else
		return (0);
}




static int
MDispatcher (msg)
struct message *msg;
/*
 * Deal with a message.
 */
{
	struct mh_template *tmpl = (struct mh_template *) msg->m_data;

	switch (msg->m_proto)
	{
	   case MT_MESSAGE:
		if (tmpl->mh_type == MH_DIE)
			exit (1);
		break;
	}
	return (0);
}   	
