/*
 * Routines which navigate MCIDAS AREA images and must be linked with the
 * FORTRAN modules appropriate to the image type.
 */

# include <unistd.h>
# include <string.h>
# include <errno.h>
# include <math.h>
# include <stdio.h>

# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <ingest.h>

# include "Area.h"
# include "mcidas.h"

RCSID("$Id: AreaIngest.c,v 1.3 1997-06-17 09:36:23 granger Exp $")


#ifdef notdef
/*
 * The McIDAS navigation interface, linked from FORTRAN modules
 * according to whether were navigating GOES or GVAR.
 */
extern int      nvxeas_ FP ((float *lat, float *lon, float *dummy1, 
                             float *line, float *elem, float *dummy2));
extern int      nvxini_ FP ((int *ifunc, int *nav_codicil));
#endif


/*
 * Local private info we keep about specific field mappings.
 */
#define MAXMAPS 10
static struct FieldMap
{
	FieldId fid;
	unsigned char *map;
	ScaleInfo scale;
}
Maps[MAXMAPS];
static int NMaps = 0;



void
SetFieldMap (FieldId fid, ScaleInfo *scale, unsigned char *map)
{
	struct FieldMap *fm = Maps + NMaps++;
	fm->scale = *scale;
	fm->fid = fid;
	fm->map = map;
}



int
GetFieldScale (FieldId fid, ScaleInfo *scale)
/*
 * Return non-zero if we fill the scale for this field, zero otherwise.
 */
{
	int i;

	for (i = 0; i < NMaps; ++i)
	{
		if (Maps[i].fid == fid)
		{
			*scale = Maps[i].scale;
			break;
		}
	}
	return (i < NMaps);
}



unsigned char *
GetFieldMap (FieldId fid)
/*
 * Return a pointer to this fields image value map, else null.
 */
{
	int i;
	unsigned char *map = NULL;

	for (i = 0; i < NMaps; ++i)
	{
		if (Maps[i].fid == fid)
		{
			map = Maps[i].map;
			break;
		}
	}
	return (map);
}




static inline unsigned char
ImageVal (unsigned char *image, AreaImage *area, int line, int elem)
/*
 * Return the image value associated with satellite line/elem coordinates
 */
{
	int	image_x, image_y, pos;

	if (BETWEEN(line, area->minline, area->maxline) && 
	    BETWEEN(elem, area->minelem, area->maxelem))
	{
	/*
	 * Translate from satellite coordinates to image coordinates
	 */
		image_x = (int)
			((float)(elem - area->minelem) / area->xres + 0.5);
		image_y = (int)
			((float)(line - area->minline) / area->yres + 0.5);
	/*
	 * Find the offset into the image.  We add (Nbytes - 1) so that
	 * we end up pointing to the last byte of multi-byte data, since
	 * that's the MSB.
	 */
		pos = image_y * area->linelen + area->prefixlen + 
			image_x * area->nbytes + (area->nbytes - 1);
	/*
	 * Return the byte.
	 */
		return (image[pos]);
	}
	else
		return ((unsigned char) 0xff);
}




DataChunk *
SetupDC (AreaFile *chain, const char *platname)
{
	int nfiles;
	int nfields;
	ScaleInfo *scale;
	FieldId *fid;
	AreaFile *f;
	PlatformId plat;
	int i;
	DataChunk *dc;

	nfields = 0;
	nfiles = CountFiles (chain);
	fid = (FieldId *) malloc (nfiles * sizeof (FieldId));
	scale = (ScaleInfo *) malloc (nfiles * sizeof (ScaleInfo));
/*
 * Build a field list from all of the unique field names.
 */
	f = chain;
	while (f)
	{
		fid[nfields] = F_DeclareField (f->field, "", "");
	/*
	 * Don't add any fields which we already have
	 */
		for (i = 0; i < nfields; ++i)
			if (fid[nfields] == fid[i])
				break;
		if (i >= nfields)
		{
			/* Check for a scale from field maps */
			if (! GetFieldScale (fid[nfields], scale+nfields))
			{
				scale[nfields].s_Scale = 1.0;
				scale[nfields].s_Offset = 0.0;
			}
			++nfields;
		}
		f = f->next;
	}
/*
 * Get our platform
 */
	if ((plat = ds_LookupPlatform ((char *)platname)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform '%s'", (char *)platname);
		return (NULL);
	}
/*
 * Create and initialize a data chunk
 */
	dc = dc_CreateDC (DCC_Image);
	dc->dc_Platform = plat;
	dc_ImgSetup (dc, nfields, fid, scale);
	free (fid);
	free (scale);
	return (dc);
}




int
AreaIngest (AreaFile *chain, AreaGrid *ag,
	    const char *platname, const char *spec)
/*
 * Begin the ingest process
 */
{
	int		i;
	int		ngood, nfiles;
	int		autolimits, autores;
	void		*grid;
	Location	loc;
	RGrid		rg;
	DataChunk	*dc;
	AreaFile 	*f;
/*
 * Make sure we have a platform name
 */
	if (! platname[0])
	{
		msg_ELog (EF_PROBLEM, "No platform specified");
		return (0);
	}
/*
 * Set up an image dc for all the fields in our input files
 */
	dc = SetupDC (chain, platname);
	if (! dc)
		return (0);
/*
 * Build and insert the grids
 */
	ngood = 0;
	nfiles = 0;
	autolimits = (! ag->limits);
	autores = (ag->kmres == 0.0);
	while (chain)
	{
		++nfiles;
		f = chain;

		msg_ELog (EF_INFO, "Reading %s, field %s, %s", f->name,
			  f->field, TC_AscTime (&f->when, TC_Full));
	/*
	 * Figure out grid spacing.  If the user did not specify limits, 
	 * try to find them automatically for each file.  We call
	 * SetAreaLimits() no matter what in case a default origin needs to 
	 * be set.
	 */
		grid = NULL;
		if (autolimits)
		{
			ag->origin_lat = NO_ORIGIN;
			ag->limits = FALSE;
		}
		if (autores)
			ag->kmres = 0.0;
		if (! SetAreaLimits (f, ag))
			grid = NULL;
		else if (SetGrid (ag, &rg, &loc))
			grid = DoFile (f, spec, ag);
	/*
	 * We're done with the file
	 */
		CloseAreaFile (f);
	/*
	 * If we already have a sample at this time, use it.  If we
	 * checked the times above, then the sample will always be 0.
	 */
		if (grid)
		{
			for (i = 0; i < dc_GetNSample(dc); ++i)
			{
				ZebTime stime;

				dc_GetTime (dc, i, &stime);
				if (TC_Eq (stime, f->when))
					break;
			}
			ngood++;
			dc_ImgAddImage (dc, i, F_Lookup(f->field), 
					&loc, &rg, &f->when, grid, 
					ag->gridX * ag->gridY);
			free (grid);
		}
	/*
	 * Free files as we finish with them.
	 */
		chain = RemoveFile (chain, f);
		ResetGrid (ag);
	}
/*
 * Write out the data chunk.  Finally.
 */
	if (ngood > 0)
	{
		ds_Store (dc, TRUE, NULL, 0);
		msg_ELog (EF_INFO, "Successfully ingested %d of %d images",
			  ngood, nfiles);
	}
	else
		msg_ELog (EF_INFO, "Exiting with nothing ingested");
/*
 * Destroy the DataChunk, and reset our file list and possibly grid sizes
 */
	dc_DestroyDC (dc);
	return (1);
}
	


unsigned char *
MapGrid (unsigned char *image, AreaImage *area, const AreaGrid *ag, 
	 unsigned char *map)
/*
 * The returned grid should be freed by the caller.
 */
{
	unsigned char *grid, ival;
	float dummy, fline, felem, lat, lon;
	int i, j, line, elem;
	int status;
	int mapped;	/* count number of points mapped successfully */
	int inrange;	/* count of line/elem pairs in range */
/*
 * Allocate the grid
 */
	grid = (unsigned char *) malloc (ag->gridX*ag->gridY*sizeof (char));
/*
 * Fill in the grid (This is the meat of the program, the rest is more or
 * less incidental.)  (So this is where we'll occasionally check for 
 * messages and get them out of the way.)
 */
	mapped = 0;
	inrange = 0;
  	for (j = 0; j < ag->gridY; j++)
  	{
  		if (! ((j+1) % 20))
 			msg_ELog (EF_DEBUG, " ...line %d of %d, lat %.2f", 
				  j + 1, ag->gridY, lat);
		if (j % 25 == 0 && msg_Connected())
			while (msg_poll(0) != MSG_TIMEOUT);
  
		lat = ag->maxlat - j * ag->latstep;

		for (i = 0; i < ag->gridX; i++)
		{
		/*
		 * Translate lat/lon into line and element in the image
		 * (NOTE: nvxeas expects west longitudes to be positive, hence
		 * the sign change)
		 */
			lon = -(ag->minlon + i * ag->lonstep);
			status = nvxeas (lat, lon, 0, &fline, &felem, 0);
			line = (int)(fline + 0.5);
			elem = (int)(felem + 0.5);
			if ((line >= area->minline) && (line <= area->maxline)
			    && (elem >= area->minelem)
			    && (elem <= area->maxelem))
				++inrange;
		/*
		 * Assign this grid point
		 */
			if (status == 0)
			{
				ival = ImageVal (image, area, line, elem);
				if (map)
					ival = map[ival];
				grid[ag->gridX * j + i] = ival;
				++mapped;
			}
			else
			{
				grid[ag->gridX * j + i] = 0;
			}
		}
	}
	msg_ELog (mapped ? EF_INFO : EF_PROBLEM, 
		  "%d points mapped out of %d in grid, %d in range", mapped,
		  ag->gridX * ag->gridY, inrange);
	return (grid);
}



int
SetAreaLimits (AreaFile *f, AreaGrid *ag)
/*
 * Return 0 on failure.
 */
{
	AreaImage area;
	int *nav_cod;
	int result = 1;

	if (ag->limits && ag->origin_lat != NO_ORIGIN && 
	    (ag->kmres || (ag->gridX && ag->gridY)))
		return (1);

	ReadArea (f, &area);
	if (! (nav_cod = ReadNavCod (f, &area, NULL, NULL)))
	{
		return (0);
	}
	if (nvxini (1, nav_cod) < 0)
	{
		msg_ELog (EF_PROBLEM, 
			  "Bad navigation initialization for file '%s'", 
			  f->name);
		return (0);
	}

	if (! ag->limits && (result = AreaLimits (&area, ag)))
		msg_ELog (EF_DEBUG, "Limit estimates: NW %g,%g SE %g,%g",
			  ag->maxlat, ag->minlon, ag->minlat, ag->maxlon);
	else if (! ag->limits)
		msg_ELog (EF_PROBLEM, "Failed to estimate area limits");

	/*
	 * Use the middle latitude as the origin latitude.
	 */
	if (ag->limits && ag->origin_lat == NO_ORIGIN)
		ag->origin_lat = (ag->minlat + ag->maxlat) / 2.0;

	/*
	 * If necessary fill in the resolution
	 */
	if (ag->kmres == 0.0)
	{
		ag->kmres = (area.xres + area.yres) / 2.0;
		msg_ELog (EF_DEBUG, "setting default resolution: %.2f",
			  ag->kmres);
	}
	return (result);
}



static int
TestLimits (float line, float elem, AreaGrid *t)
{
	int status;
	float lat, lon;

	status = nvxsae (line, elem, 0, &lat, &lon, 0);
	lon *= -1;
	if (status == 0)
	{
		if (! t->limits)
		{
			t->minlat = t->maxlat = lat;
			t->minlon = t->maxlon = lon;
			t->limits = 1;
		}
		else
		{
			t->minlat = lat < t->minlat ? lat : t->minlat;
			t->minlon = lon < t->minlon ? lon : t->minlon;
			t->maxlat = lat > t->maxlat ? lat : t->maxlat;
			t->maxlon = lon > t->maxlon ? lon : t->maxlon;
		}
	}
	return (status);
}
	


int
AreaLimits (const AreaImage *area, AreaGrid *ag)
/*
 * Find the lat/lon limits of the image by traversing each edge at 
 * regular intervals.  The corners alone leave out possible
 * curvature, usually along the north or south edge.
 * Return non-zero when we succeed in filling in the limits.
 */
{
	float	fline, felem;
	AreaGrid t;
	int	i;
	int	status;
	float	de, dl;		/* Line and element deltas */
#	define NSTEPS 30
/*
 * (NOTE: nvxsae returns W longitudes as positive, so we change the sign)
 */
	status = 0;
	t.limits = 0;
	dl = (area->maxline - area->minline) / NSTEPS;
	de = (area->maxelem - area->minelem) / NSTEPS;

	for (i = 0; i < NSTEPS; ++i)
	{
		fline = area->minline + i*dl;
		felem = area->minelem;
		status += TestLimits (fline, felem, &t);

		fline = area->minline;
		felem = area->minelem + i*de;
		status += TestLimits (fline, felem, &t);

		fline = area->maxline - i*dl;
		felem = area->maxelem;
		status += TestLimits (fline, felem, &t);

		fline = area->maxline;
		felem = area->maxelem - i*de;
		status += TestLimits (fline, felem, &t);
	}
/*
 * Look for problems.  If the status is < 0 here, it means one
 * or more of the "corners" of the image is off of the globe and the user
 * needs to choose the limits for the remapping explicitly.
 */
	if (status < 0)
	{
		msg_ELog (EF_PROBLEM, 
			"Explicit bounds must be set for this image");
		return (0);
	}
	ag->minlat = t.minlat;
	ag->minlon = t.minlon;
	ag->maxlat = t.maxlat;
	ag->maxlon = t.maxlon;
	ag->limits = TRUE;
	return (1);
}




void *
DoFile (AreaFile *f, const char *spec, const AreaGrid *ag)
/*
 * Read the area file, remapping into a grid and returning that
 * grid.  The caller is expected to free the grid when finished with it.
 * NULL is returned on failure.  We expect the image type named in 'spec'.
 */
{
	int	nav_cod[128 * MAXNAVCHUNKS];
	unsigned char	*grid;
	unsigned char 	*map;
	char 	imtype[5];
	unsigned char *Image;
	AreaImage Area;
/*
 * Read the 256 byte "area directory" header and the variable length
 * navigation codicil.
 */
	ReadArea (f, &Area);
	if (! ReadNavCod (f, &Area, nav_cod, imtype))
	{
		return (NULL);
	}
/*
 * Verify that this is the expected image spec
 */
	if (spec && strncmp (imtype, spec, 4))
	{
		msg_ELog (EF_PROBLEM, 
			  "'%s' contains a '%s' image, not %s",
			  f->name, imtype, spec);
		return (NULL);
	}
/*
 * If it isn't one byte data, we can't handle it (for now)
 */
	if (Area.nbytes != 1)
	{
		if (ag->truncate)
		{
			msg_ELog (EF_INFO, 
				  "%d byte data will be truncated to one byte",
				  Area.nbytes);
		}
		else
		{
			msg_ELog (EF_EMERGENCY, 
				  "Can't deal with %d byte %s data",
				  Area.nbytes, imtype);
			return (NULL);
		}
	}
/*
 * Read the image data
 */
	if (! (Image = ReadAreaImage (f, &Area)))
		return (NULL);
/*
 * Look for an image value mapping for this field.
 */
	map = GetFieldMap (F_Lookup (f->field));
/*
 * Initialize the navigation stuff
 */
	if (nvxini (1, nav_cod) < 0)
	{
		msg_ELog (EF_PROBLEM, 
			  "Bad navigation initialization for file '%s'", 
			  f->name);
		grid = NULL;
	}
	else
	{
		/*
		 * Fill the grid.
		 */
		msg_ELog (EF_DEBUG, "%s: mapping image to grid...", f->name);
		grid = MapGrid (Image, &Area, ag, map);
		msg_ELog (EF_DEBUG, "%s: done mapping image.", f->name);
	}
	free (Image);
	return (grid);
}



