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

RCSID("$Id: AreaDS.c,v 1.4 2004-08-26 19:53:08 burghart Exp $")

/*
 * swap the order of row data in a row-major grid
 */
void swapRowOrder (unsigned char* grid, int nrows, int ncols);



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
SetupDC (AreaFile *chain, const char *platname, AreaGrid *ag)
{
	int nfiles;
	int nfields;
	ScaleInfo *scale;
	FieldId *fid;
	FieldId latid, lonid, dims[2];
	Location loc;
	int nlats, nlons;
	float *lats, *lons;
	AreaFile *f;
	PlatformId plat;
	int i;
	DataOrganization org;
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
	org = ds_PlatformDataOrg (plat);
	switch (org) {
	  /*
	   * Create an image data chunk for image platforms
	   */
	  case OrgImage:
	  case OrgCmpImage:
	    dc = dc_CreateDC (DCC_Image);
	    dc_SetPlatform (dc, plat);
	    dc_ImgSetup (dc, nfields, fid, scale);
	    break;
	  /*
	   * Create an NSpace data chunk for NSpace platforms
	   */
	  case OrgNSpace:
	    dc = dc_CreateDC (DCC_NSpace);
	    dc_SetPlatform (dc, plat);

	    /*
	     * If we don't have limits, get them from the first file that
	     * gives us good results via SetAreaLimits.
	     */
	    if (ag->reset)
	    {
	      msg_ELog (EF_INFO, "Variable image sizing is disabled "
			"since platform has nspace organization");
	      ag->reset = 0;
	    }

	    f = chain;
	    while (! ag->limits)
	    {
	      if (SetAreaLimits (f, ag))
	      {
		msg_ELog (EF_INFO, "Created image limits using file %s",
			  f->name);
		break;
	      }

	      f = f->next;
	      if (! f)
	      {
		msg_ELog (EF_PROBLEM, "Unable to get limits from any file");
		return (NULL);
	      }
	    }

	    /*
	     * Fill in some unset stuff in the AreaGrid, and get a location
	     */
	    SetGrid (ag, NULL, &loc);

	    /*
	     * Set the location in the data chunk
	     */
	    dc_SetStaticLoc (dc, &loc);

	    /*
	     * Create the lat & lon dimensions
	     */
	    nlats = ag->gridY;
	    latid = F_DeclareField ("latitude", "N latitude", "deg");
	    dc_NSDefineDimension (dc, latid, nlats);
	    dc_NSDefineVariable (dc, latid, 1, &latid, 1);
	    dims[0] = latid;

	    nlons = ag->gridX;
	    lonid = F_DeclareField ("longitude", "E longitude", "deg");
	    dc_NSDefineDimension (dc, lonid, nlons);
	    dc_NSDefineVariable (dc, lonid, 1, &lonid, 1);
	    dims[1] = lonid;

	    /*
	     * Create variables for the data fields
	     */
	    for (i = 0; i < nfields; ++i)
	    {
	      unsigned char badval = 0xff;
	      dc_NSDefineVariable (dc, fid[i], 2, dims, 0);
	      dc_SetType (dc, fid[i], DCT_UnsignedChar);
	      dc_SetFieldBadval (dc, fid[i], &badval);
	    }

	    dc_NSDefineComplete(dc);

	    /*
	     * Fill in the latitudes & longitudes
	     */
	    lats = (float *) malloc (nlats * sizeof (float));
	    for (i = 0; i < nlats; i++)
	      lats[i] = ag->minlat + i * ag->latstep;
	    dc_NSAddStatic (dc, latid, lats);
	    free(lats);

	    lons = (float *) malloc (nlons * sizeof (float));
	    for (i = 0; i < nlons; i++)
	      lons[i] = ag->minlon + i * ag->lonstep;
	    dc_NSAddStatic (dc, lonid, lons);
	    free(lons);

	      
	    break;
	  default:
	    msg_ELog (EF_PROBLEM, 
		      "Platform must have 'image' or 'nspace' organization");
	    msg_ELog (EF_PROBLEM,
		      "Platform %s has organization: %s", platname,
		      ds_OrgName (org));
	    return (NULL);
	}
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
	dc = SetupDC (chain, platname, ag);
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
			if (dc_ClassId (dc) == DCID_Image)
			  
			  dc_ImgAddImage (dc, i, F_Lookup(f->field), 
					  &loc, &rg, &f->when, grid, 
					  ag->gridX * ag->gridY);
			else
			{
			  /*
			   * the grid we got runs N->S, but we need it S->N
			   */
			  swapRowOrder (grid, ag->gridY, ag->gridX);
			  dc_NSAddSample (dc, &f->when, i, F_Lookup(f->field),
					  grid);
			}
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
	

void
swapRowOrder (unsigned char *grid, int nrows, int ncols)
/*
 * Swap the row order in the given row-major grid.
 */
{
  int rowsize = ncols;
  void* tmpRowData = (void*) malloc (rowsize);
  int row;

  for (row = 0; row < (nrows / 2); row++)
  {
    int swaprow = nrows - row - 1;
    memcpy (tmpRowData, grid + swaprow * rowsize, rowsize);
    memcpy (grid + swaprow * rowsize, grid + row * rowsize, rowsize);
    memcpy (grid + row * rowsize, tmpRowData, rowsize);
  }
}

