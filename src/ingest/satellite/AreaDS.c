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

RCSID("$Id: AreaDS.c,v 1.3 2003-01-29 22:22:21 burghart Exp $")



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
AreaIngest (AreaFile *chain, AreaGrid *ag, const char *platname)
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
	unsigned char 	*map;
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
	 * Check for an image value mapping for this field.
	 */
		map = GetFieldMap (F_Lookup (f->field));
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
			grid = DoFile (f, ag, map);
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
	

