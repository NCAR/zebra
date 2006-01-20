/*
 * General utilities for accessing GOES AREA files shared among satellite
 * ingestors.
 */


# include <unistd.h>
# include <stdio.h>
# include <string.h>
# include <errno.h>
# include <math.h>
# include <stdio.h>
# include <dirent.h>

# include <defs.h>
# include <message.h>

# include "Area.h"

RCSID("$Id: Area.c,v 1.10 2006-01-20 17:46:14 burghart Exp $")


static int Mdays[] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};


static void GetFileTime (AreaFile *f);
static void swapfour (int *array, int count);



static int
SeekAreaDirectory (AreaFile *f)
/*
 * Seek to the beginning of the area directory.  If necessary, skip any
 * lead-in junk and remember the lead-in for future seeks.
 */
{
	char in[8];
	int *zero = (int *)in;
	int *head = (int *)(in+4);
	int area = 0;
/*
 * If we've found it once before, go to it.
 */
	if (f->area >= 0)
	{
		fseek (f->stream, f->area, 0);
		return (0);
	}
	fseek (f->stream, 0, 0);
	if (fread (in, 1, 8, f->stream) < 8)
	{
		msg_ELog (EF_PROBLEM, "could not read beginning of file");
		return (-1);
	}
	while ((*zero != 0) || (*head != 0x04000000 && *head != 0x4))
	{
		int i, c;
		for (i = 0; i < 7; i++)
			in[i] = in[i+1];
		if ((c = fgetc (f->stream)) == EOF)
		{
			msg_ELog (EF_PROBLEM, "no area directory found");
			return (-1);
		}
		in[7] = (char) c;
		++area;
	}
	msg_ELog (EF_DEBUG, "area directory found at offset %d", area);
/*
 * The second word of the area directory should always be 4.  Determine
 * big-endian vs. little-endian using this word, and swap bytes if necessary.
 */
	f->doswap = (*head == 0x04000000);
	f->area = area;
/*
 * Last but not least, back up to the start of the directory.
 */
	return (fseek (f->stream, f->area, SEEK_SET));
}

	

static int
SeekNavBlock (AreaFile *f, AreaImage *area)
{
	int s;
	char nav[4];
	static char *known[] =
	{ "AIRC", "DMSP", "GMSX", "GOES", "GRAF", "GVAR", "LALO", "LAMB", 
	  "MERC", "MOLL", "MSAT", "MSG ", "NOWR", "PS  ", "RADR", "RECT", 
	  "SIN ", "TANC", "TIRO", NULL };
/*
 * Look for a recognizable nav type to indicate the start of the block.
 */
	if ((s = fseek (f->stream, f->area + area->navblock, SEEK_SET)))
		return (s);
	fread (nav, 1, 4, f->stream);
	do {
		int i, c;
		for (i = 0; known[i]; ++i)
		{
			if (strncmp (nav, known[i], 4) == 0)
				break;
		}
		if (known[i])
			break;
		for (i = 0; i < 3; ++i)
			nav[i] = nav[i+1];
		if ((c = fgetc (f->stream)) == EOF)
			return (-1);
		nav[3] = (char) c;
	}
	while (1);
	f->navoff = ftell(f->stream) - 4;
	msg_ELog (EF_DEBUG, "nav block found at offset %d", f->navoff);
	return (fseek (f->stream, -4, SEEK_CUR));
}



static int
SeekDataBlock (AreaFile *f, AreaImage *area)
{
	return (fseek (f->stream, f->area + area->datablock, SEEK_SET));
}



AreaFile *
AddFile (AreaFile *chain, char *fname, char *fld)
/*
 * Add a file to be ingested.  Insert it at the end of the chain and
 * return the pointer to head of the chain, which may have changed.
 */
{
	FILE *stream;
	AreaFile *f;
/*
 * Make sure the file can be opened first, then add it
 */
	if ((stream = fopen (fname, "r")) == NULL)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening file '%s'", errno, 
			  fname);
		return (chain);
	}

	f = (AreaFile *) malloc (sizeof (AreaFile));
	f->stream = stream;
	f->name = (char *) malloc (strlen (fname) + 1);
	strcpy (f->name, fname);
	f->field = NULL;
	f->area = -1;
	f->navoff = -1;
	if (fld)
	{
		f->field = (char *) malloc (strlen (fld) + 1);
		strcpy (f->field, fld);
	}
	f->next = NULL;
	f->prev = NULL;
	if (! chain)
	{
		chain = f;
	}
	else
	{
		AreaFile *c = chain;
		while (c->next)
			c = c->next;
		c->next = f;
		f->prev = c;
	}
	GetFileTime (f);
	return (chain);
}



AreaFile *
RemoveOldFile (AreaFile *chain, AreaFile *f)
/*
 * Remove files with older times from the list
 */
{
	msg_ELog (EF_INFO, "Not ingesting %s due to time mismatch",
		  f->name);
	return (RemoveFile (chain, f));
}



AreaFile *
RemoveFile (AreaFile *chain, AreaFile *f)
{
	free (f->name);
	if (f->field)
		free (f->field);
	if (f->stream)
		fclose (f->stream);
	/*
	 * Move the head of the chain forward if this is the first file.
	 */
	if (f->prev)
		(f->prev)->next = f->next;
	else
		chain = f->next;
	if (f->next)
		(f->next)->prev = f->prev;
	free (f);
	return (chain);
}



void
CloseAreaFile (AreaFile *f)
{
	if (f->stream)
		fclose (f->stream);
	f->stream = NULL;
}



static void
GetFileTime (AreaFile *f)
/*
 * Return the time of the fentry'th file
 */
{
	int	year, month, day, hour, minute, second, header[5];
/*
 * Read the first piece of the area directory and do the appropriate byte
 * swapping.
 */
	SeekAreaDirectory (f);
	fread ((void *) header, 4, 5, f->stream);
	if (f->doswap)
		swapfour (header, 5);
/*
 * Extract the date.
 */
	year = header[3] / 1000;
	if ((year % 4) == 0)
		Mdays[2] = 29;	/* February has 29 days in leap years */

	day = header[3] % 1000;
	month = 1;
	while (day > Mdays[month])
		day -= Mdays[month++];
	Mdays[2] = 28;		/* return to 28 days in case next file
				 * is in a different year */
/*
 * Time
 */
	hour = header[4] / 10000;
	minute = (header[4] / 100) % 100;
	second = header[4] % 100;
/*
 * Build a zebra time out of the pieces and we're done
 */
	TC_ZtAssemble (&f->when, year, month, day, hour, minute, second, 0);
	return;
}






AreaFile *
TimeCheck (AreaFile *chain, ZebTime *t)
/*
 * Check data times in the files and leave only the latest one(s) for ingest.
 * Return the new pointer to the head of the chain.
 */
{
	ZebraTime zt;
	AreaFile *f, *next;
/*
 * Run through the file list, leaving only one(s) with the latest time
 */
	zt.zt_Sec = zt.zt_MicroSec = 0;
	f = chain;
	while (f)
	{
		next = f->next;
		if (f->when.zt_Sec < zt.zt_Sec)
		{
			chain = RemoveOldFile (chain, f);
		}
		else if (f->when.zt_Sec > zt.zt_Sec)
		{
		/*
		 * We have a new most recent time, so any files currently
		 * considered good are good no longer; remove them.
		 */
			AreaFile *old = chain;
			while (old != f)
			{
				chain = RemoveOldFile (chain, old);
				old = old->next;
			}
			zt = f->when;
		}
	/*
	 * So either we're back to one good file, or this file's time equals
	 * the current time and it was left in the list.  Move on to the
	 * next file.
	 */
		f = next;
	}
	if (t)
		*t = zt;
	return (chain);
}



int
CountFiles (AreaFile *chain)
{
	int n = 0;
	while (chain)
	{
		++n;
		chain = chain->next;
	}
	return (n);
}




void
InitGrid (AreaGrid *ag)
{
	ag->gridX = 0;
	ag->gridY = 0;
	ag->kmres = 0.0;
	ag->limits = FALSE;
	ag->origin_lat = NO_ORIGIN;
	ag->reset = TRUE;
	ag->truncate = FALSE;
}



int
SetGrid (AreaGrid *ag, RGrid *rg, Location *loc)
{
	if (! ag->limits)
	{
		msg_ELog (EF_PROBLEM, "Lat/lon limits must be specified!");
		return (0);
	}
	if (ag->gridX && ag->gridY)
	{
		if (ag->kmres != 0.0)
			msg_ELog (EF_INFO, 
				"Gridsize overrides kmResolution setting");
		ag->latstep = (ag->maxlat - ag->minlat) / (ag->gridY - 1);
		ag->lonstep = (ag->maxlon - ag->minlon) / (ag->gridX - 1);
		ag->reset = FALSE;
	}
	else if (ag->kmres != 0.0)
	{
		ag->latstep = ag->lonstep = KM_TO_DEG (ag->kmres);

		ag->gridX = (int)((ag->maxlon - ag->minlon) / ag->lonstep) + 1;
		ag->gridY = (int)((ag->maxlat - ag->minlat) / ag->latstep) + 1;
	/*
	 * Reset grid sizes to zero when finished, since they were zero
	 * when we entered.
	 */
		ag->reset = TRUE;
		ag->maxlon = ag->minlon + ag->lonstep * (ag->gridX - 1);
		ag->maxlat = ag->minlat + ag->latstep * (ag->gridY - 1);
	}
	else
	{
		msg_ELog (EF_PROBLEM, 
			"gridX and gridY or kmResolution must be given");
		return (0);
	}

	if (ag->origin_lat < -90.0 || ag->origin_lat > 90.0)
	{
		msg_ELog (EF_PROBLEM, 
			  "Illegal or unspecified origin latitude: %g",
			  ag->origin_lat);
	}

	msg_ELog (EF_INFO, "Lat. limits: %.2f to %.2f every %.2f",
		  ag->minlat, ag->maxlat, ag->latstep);
	msg_ELog (EF_INFO, "Lon. limits: %.2f to %.2f every %.2f",
		  ag->minlon, ag->maxlon, ag->lonstep);
/*
 * Build the location and rgrid information
 */
	if (loc)
	{
	  loc->l_lat = ag->minlat;
	  loc->l_lon = ag->minlon;
	  loc->l_alt = 0.000;
	}

	if (rg)
	{
	  rg->rg_Xspacing = DEG_TO_KM (ag->lonstep) * 
	    cos (DEG_TO_RAD(ag->origin_lat));
	  rg->rg_Yspacing = DEG_TO_KM (ag->latstep);
	  rg->rg_Zspacing = 0.0;

	  rg->rg_nX = ag->gridX;
	  rg->rg_nY = ag->gridY;
	  rg->rg_nZ = 1;
	}

	return (1);
}



void
ResetGrid (AreaGrid *ag)
{
	if (ag->reset)
	{
		ag->gridX = 0;
		ag->gridY = 0;
	}
}



int
UserLimits (AreaGrid *ag, double minlat, double minlon, 
	    double maxlat, double maxlon)
/*
 * Get user specified lat/lon limits for the grid
 * ...and do some sanity checking...
 */
{
	ag->limits = FALSE;
	ag->minlat = minlat;
	ag->minlon = minlon;
	ag->maxlat = maxlat;
	ag->maxlon = maxlon;
	if ((ag->minlat < ag->maxlat) && (ag->minlon < ag->maxlon))
	{
		/* values are valid, note as much */
		ag->limits = TRUE;
	}
	else
	{
		/* illegal values: tell user */
		msg_ELog(EF_PROBLEM,"limits values illegal: %f %f %f %f",
			 ag->minlat, ag->minlon, ag->maxlat, ag->maxlon);
		ag->limits = FALSE;
	}
	return (ag->limits);
}




void
SetArea (AreaImage *a, int *header)
/*
 * Set an area structure according to the correctly byte-ordered header.
 */
{
	int i;

	a->sss = header[2];
	a->nchans = header[13];
	a->datablock = header[33];
	a->navblock = header[34];
	a->calblock = header[62];
	strncpy (a->caltype, (char *)(header+52), 4);
	a->caltype[4] = '\0';
	strncpy (a->memo, (char *)(header+24), 32);
	a->caltype[32] = '\0';
/*
 * Resolution (# of satellite units per image unit)
 */
	a->yres = header[11];
	a->xres = header[12];
	a->nbytes = header[10];
/*
 * Image size (Nx x Ny), bytes per element and prefix length
 */
	a->ny = header[8];
	a->nx = header[9];
	a->prefixlen = header[14];
	a->linelen = a->nx * a->nbytes + a->prefixlen;
/*
 * Source name from header word 51 (convert to lower case and remove spaces)
 */
	strncpy (a->source, (char *)(header + 51), 4);
	a->source[4] = '\0';
	for (i = 0; i < 4; i++)
	{
		char *c = a->source + i;
		if (*c == ' ')
			*c = '\0';
		else
			*c = tolower (*c);
	}
 	msg_ELog (EF_DEBUG, "Source is '%s'", a->source);
/*
 * Element and line limits
 */
	a->minline = header[5];
	a->maxline = a->minline + (a->ny - 1) * a->yres;

	a->minelem = header[6];
	a->maxelem = a->minelem + (a->nx - 1) * a->xres;
}



void
ReadArea (AreaFile *f, AreaImage *area)
{
	int header[64];

	if (SeekAreaDirectory (f))
	{
		msg_ELog (EF_PROBLEM, "could not seek to area directory");
		exit (1);
	}
	if (fread ((void *) header, 4, 64, f->stream) != 64)
	{
		msg_ELog (EF_PROBLEM, "failed to read 64 words in header");
		exit (1);
	}
	if (header[0] != 0)
	{
		msg_ELog (EF_PROBLEM, "corrupt file: first word should be 0");
	}
	if (f->doswap)
	{
		swapfour (header, 20);
		swapfour (header + 32, 19);
		swapfour (header + 62, 2);
	}
	SetArea (area, header);
}
		


int *
ReadNavCod (AreaFile *f, AreaImage *area, int *nav_cod, char *imtype)
/*
 * Read navigation codicils in nav_cod buffer, swapped as necessary.
 * Set image type in imtype, if non-null.  Return NULL if we fail
 * because of too many nav chunks, else return nav_cod.
 */
{
	static int buf[128 * MAXNAVCHUNKS];
	int i;

	if (! nav_cod)
		nav_cod = buf;
	if (SeekNavBlock (f, area))
	{
		msg_ELog (EF_PROBLEM, "could not seek to nav block at %d",
			  area->navblock);
	}
	for (i = 0; i < MAXNAVCHUNKS; i++)
	{
		int	*cur_loc = nav_cod + 128 * i;
		fread ((void *) cur_loc, 4, 128, f->stream);
		/*
		 * On little-endian systems, we need to byte-swap 
		 * (most of) the data in the navigation codicil.
		 * However, the McIDAS GMSX handler seems to do this
		 * on its own, so we *don't* swap in that case.
		 * Why just that one?  Who knows...
		 */
		if (f->doswap && strncmp((char*)nav_cod, "GMSX", 4))
		{
		  /*
		   * For the first chunk, swap all but the first and last
		   * 4-byte strings.  For the rest of the chunks, swap	
		   * all but the last 4-byte string.
		   */
		  if (i == 0)
		    swapfour (cur_loc + 1, 126);
		  else
		    swapfour (cur_loc, 127);
		}

		/*
		 * Stop here if the last 4 bytes of this chunk are not "MORE"
		 */
		if (strncmp ((char *)(cur_loc + 127), "MORE", 4))
			break;

		if (i == MAXNAVCHUNKS - 1)
		{
			msg_ELog (EF_EMERGENCY, 
				  "navigation codicil too big, max %d!",
				  MAXNAVCHUNKS);
			return (NULL);
		}
	}
	++i;
	msg_ELog (EF_DEBUG, "%d nav codicil chunks (%d bytes)", i, i*4*128);
	if (imtype)
	{
		strncpy (imtype, (char *)nav_cod, 4);
		imtype[4] = '\0';
	}
	return (nav_cod);
}



unsigned char *
ReadAreaImage (AreaFile *f, AreaImage *area)
/*
 * Allocate space for and read the image from the area file.
 * We expect that the header and nav cod's have already been read.
 * The image array should be freed by the caller.
 */
{
	unsigned char *image;
	int imagelen, ngot;
/*
 * Rather than try to figure out what optional sections precede the data
 * block, seek directly to it from the offset in the area directory.
 */
	if (SeekDataBlock (f, area))
	{
		msg_ELog (EF_PROBLEM, "seek to data block failed, error %d",
			  errno);
		return (NULL);
	}
/*
 * Finally, read the image.
 */
	imagelen = area->linelen * area->ny;
	image = (unsigned char *) malloc (imagelen);
	ngot = fread ((void *) image, 1, imagelen, f->stream);
	if (ngot != imagelen)
	{
		if (feof (f->stream))
			msg_ELog (EF_PROBLEM, 
				  "Premature EOF.  Got %d instead of %d bytes",
				  ngot, imagelen);
		else
			msg_ELog (EF_PROBLEM, 
				  "Read error %d.  Got %d instead of %d bytes",
				  errno, ngot, imagelen);
	}
	return (image);
}




static void
swapfour (int *array, int count)
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


