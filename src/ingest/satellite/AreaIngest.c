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

# include <mcidas_nav.h>
# include "Area.h"

RCSID("$Id: AreaIngest.c,v 1.6 2003-01-29 22:22:21 burghart Exp $")


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
		lat = ag->maxlat - j * ag->latstep;
  		if (! ((j+1) % 20))
 			msg_ELog (EF_DEBUG, " ...line %d of %d, lat %.2f", 
				  j + 1, ag->gridY, lat);
		if (j % 25 == 0 && msg_Connected())
			while (msg_poll(0) != MSG_TIMEOUT);
  

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
	if (nvxini (nav_cod) < 0)
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
DoFile (AreaFile *f, const AreaGrid *ag, unsigned char *map)
/*
 * Read the area file, remapping into a grid and returning that
 * grid.  The caller is expected to free the grid when finished with it.
 * NULL is returned on failure.
 */
{
	int	nav_cod[128 * MAXNAVCHUNKS];
	unsigned char	*grid;
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
 * Initialize the navigation stuff
 */
	if (nvxini (nav_cod) < 0)
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



