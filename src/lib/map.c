/*
 * Read and write ASCII or binary map files.
 */
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include <config.h>
#include <defs.h>
#include <message.h>
#include "map.h"

RCSID("$Id: map.c,v 2.2 1997-02-14 07:31:59 granger Exp $")

static unsigned char XDR_MAGIC[4] = { 0x88, 0x88, 0x00, 0x00 };

struct MapFile 
{
	char map_file[CFG_FILEPATH_LEN];
	FILE *map_fp;
	MapFormat map_fmt;
	int map_col;	/* column counter for ascii output */
	XDR map_xdr;
	unsigned char map_buf[4];	/* buffer for first 4 bytes */
	int map_read_buf;		/* non-zero when buf should be read */
	int (*map_read_header)(MapFile *mf, int *npoints, 
			       float *lat0, float *lon0,
				float *lat1, float *lon1, int *flags);
	int (*map_write_header)(MapFile *mf, int npoints, 
				float *lat0, float *lon0, 
				float *lat1, float *lon1, int flags);
	int (*map_point)(MapFile *mf, float *lat, float *lon);
};


static int map_Point (MapFile *mf, float *lat, float *lon);
static int map_ReadHeader (MapFile *mf, int *npoints, 
			   float *lat0, float *lon0,
			   float *lat1, float *lon1, int *flags);
static int map_WriteHeader (MapFile *mf, int npoints,
			    float *lat0, float *lon0, float *lat1, float *lon1,
			    int flags);
static int ascii_ReadPoint (MapFile *mf, float *lat, float *lon);
static int ascii_ReadHeader (MapFile *mf, int *npoints, 
			     float *lat0, float *lon0,
			     float *lat1, float *lon1, int *flags);
static int ascii_WritePoint (MapFile *mf, float *lat, float *lon);
static int ascii_WriteHeader (MapFile *mf, int npoints, 
			      float *lat0, float *lon0,
			      float *lat1, float *lon1, int flags);


MapFormat
MapGetFormat (MapFile *mf)
{
	return (mf->map_fmt);
}



MapFile *
MapWrite (const char *filepath, MapFormat fmt)
/*
 * Open a map file for writing with the given format.  Overwrite
 * any existing file.
 */
{
	FILE *fp;
	MapFile *mf;
	int i;
	
	if (fmt != MF_XDR && fmt != MF_ASCII)
	{
		msg_ELog (EF_PROBLEM, "write map file: %s",
			  "unrecognized map format");
		return (NULL);
	}
	if (! strcmp (filepath, "-"))
	{
		fp = stdout;
	}
	else if (! (fp = fopen (filepath, "w")))
	{
		msg_ELog (EF_PROBLEM, "could not open map file: %s: error %d",
			  filepath, errno);
		return (NULL);
	}
	mf = (MapFile *) malloc (sizeof(MapFile));
	mf->map_fp = fp;
	strncpy (mf->map_file, filepath, sizeof(mf->map_file) - 1);
	mf->map_file[sizeof(mf->map_file) - 1] = '\0';
	mf->map_fmt = fmt;

	if (fmt == MF_XDR)
	{
		mf->map_write_header = map_WriteHeader;
		mf->map_point = map_Point;
		for (i = 0; i < 4; ++i)
		{
			fputc ((int)XDR_MAGIC[i], fp);
		}
		xdrstdio_create (&mf->map_xdr, mf->map_fp, XDR_ENCODE);
	}
	else
	{
		mf->map_col = 0;
		mf->map_write_header = ascii_WriteHeader;
		mf->map_point = ascii_WritePoint;
	}
	return (mf);
}



MapFile *
MapRead (const char *filepath)
/*
 * Open the map file with the given path and return a handle.
 * Set a flag in the handle depending upon whether this is an ASCII
 * format map or an XDR-encoded binary map file.
 */
{
	unsigned char buf[4];
	FILE *fp;
	MapFile *mf;
	int i;

	if (! strcmp (filepath, "-"))
	{
		fp = stdin;
	}
	else if (! (fp = fopen (filepath, "r")))
	{
		msg_ELog (EF_PROBLEM, "could not open map file: %s: error %d",
			  filepath, errno);
		return (NULL);
	}
	/*
	 * Read the first four bytes and check for our magic sequence.
	 */
	if (fread (buf, sizeof(unsigned char), 4, fp) < 4)
	{
		msg_ELog (EF_PROBLEM, "map file error: %s: %s: %d",
			  "failed to detect format", filepath, errno);
		return (NULL);
	}
	for (i = 0; i < 4; ++i)
	{
		if (buf[i] != XDR_MAGIC[i])
			break;
	}
	mf = (MapFile *) malloc (sizeof(MapFile));
	mf->map_fp = fp;
	strncpy (mf->map_file, filepath, sizeof(mf->map_file) - 1);
	mf->map_file[sizeof(mf->map_file) - 1] = '\0';
	mf->map_fmt = (i < 4) ? MF_ASCII : MF_XDR;
	mf->map_read_buf = 0;
	if (mf->map_fmt == MF_ASCII)
	{
		memcpy (mf->map_buf, buf, 4);
		mf->map_read_buf = 1;
		mf->map_read_header = ascii_ReadHeader;
		mf->map_point = ascii_ReadPoint;
	}
	else
	{
		xdrstdio_create (&mf->map_xdr, mf->map_fp, XDR_DECODE);
		mf->map_read_header = map_ReadHeader;
		mf->map_point = map_Point;
	}
	msg_ELog (EF_DEBUG, "map file %s: detected format %s",
		  filepath, (mf->map_fmt == MF_ASCII) ? "ASCII" : "XDR");
	return (mf);
}



int
MapReadHeader (MapFile *mf, int *npoints, 
	       float *lat0, float *lon0, 
	       float *lat1, float *lon1, int *flags)
{
	return ((*mf->map_read_header)(mf, npoints, lat0, lon0, lat1, lon1, 
				       flags));
}


int
MapReadPoint (MapFile *mf, float *lat, float *lon)
{
	return ((*mf->map_point)(mf, lat, lon));
}



int
MapWriteHeader (MapFile *mf, int npoints, 
		float *lat0, float *lon0, 
		float *lat1, float *lon1, int flags)
{
	return ((*mf->map_write_header)(mf, npoints, lat0, lon0, lat1, lon1,
					flags));
}


int
MapWritePoint (MapFile *mf, float *lat, float *lon)
{
	return ((*mf->map_point)(mf, lat, lon));
}



void
MapClose (MapFile *mf)
{
	if (mf->map_fp && mf->map_fp != stdout)
		fclose (mf->map_fp);
	if (mf->map_fmt == MF_XDR)
		xdr_destroy (&mf->map_xdr);
	free (mf);
}



static int
ascii_ReadHeader (MapFile *mf, int *npoints, 
		  float *lat0, float *lon0,
		  float *lat1, float *lon1, int *flags)
/*
 * Read a polyline header from the ascii stream.
 */
{
	char line[200], fill[4];
	char *buf = line;
	float t0, n0, t1, n1;
	int npt;

	if (mf->map_read_buf)
	{
		memcpy (buf, mf->map_buf, 4);
		buf += 4;
		mf->map_read_buf = 0;
	}
	if (fgets (buf, 200, mf->map_fp) == NULL)	/* EOF */
	{
		return (0);
	}
	/*
	 * Figure out how many points we have.  Ignore the four floats that
	 * give us the bounding box, but try for the optional "FILL" string.
	 */
	strncpy (fill, "    ", 4);
	t1 = t0 = n1 = n0 = 0.0;
	if (sscanf (line, "%d %f %f %f %f %4s", &npt, &t1, &t0, &n1, &n0,
		    fill) < 1)	/* need at least the number of points */
	{
		msg_ELog (EF_PROBLEM, "map file %s: %s: invalid header line",
			  mf->map_file, line);
		return (0);
	}
	if (flags)
		*flags = (! strncmp (fill, "FILL", 4)) ? PF_FILL : 0;
	if (npoints)
		*npoints = npt;
	if (lat0)
		*lat0 = t0;
	if (lat1)
		*lat1 = t1;
	if (lon0)
		*lon0 = n0;
	if (lon1)
		*lon1 = n1;
	return (1);
}



static int
ascii_ReadPoint (MapFile *mf, float *lat, float *lon)
/*
 * Read a lat/lon pair from the ascii stream.
 */
{
	if (fscanf (mf->map_fp, "%f %f ", lat, lon) < 2)
	{
		msg_ELog (EF_PROBLEM, "map file %s: premature eof",
			  mf->map_file);
		return (0);
	}
	return (1);
}





static int
ascii_WriteHeader (MapFile *mf, int npoints, 
		   float *lat0, float *lon0,
		   float *lat1, float *lon1, int flags)
{
	if (mf->map_col > 0)
		fputc ('\n', mf->map_fp);
	fprintf (mf->map_fp, "%4d %9.3f %9.3f %9.3f %9.3f %s\n", 
		 npoints, *lat1, *lat0, *lon1, *lon0, 
		 (flags & PF_FILL) ? "FILL" : "");
	mf->map_col = 0;
	return (1);
}



static int
ascii_WritePoint (MapFile *mf, float *lat, float *lon)
/*
 * Write a lat/lon pair to the ascii stream.
 */
{
	fprintf (mf->map_fp, " %9.3f %9.3f", *lat, *lon);
	if (++mf->map_col == 4)
	{
		fputc ('\n', mf->map_fp);
		mf->map_col = 0;
	}
	return (1);
}



static int
map_ReadHeader (MapFile *mf, int *npoints, 
		float *lat0, float *lon0,
		float *lat1, float *lon1, int *flags)
{
	Polyline pl;

	if (! xdr_Polyline (&mf->map_xdr, &pl))
		return (0);
	if (npoints)
		*npoints = pl.npoints;
	if (flags)
		*flags = pl.flags;
	if (lat0)
		*lat0 = pl.lat0;
	if (lat1)
		*lat1 = pl.lat1;
	if (lon0)
		*lon0 = pl.lon0;
	if (lon1)
		*lon1 = pl.lon1;
	return (1);
}



static int
map_WriteHeader (MapFile *mf, int npoints,
		 float *lat0, float *lon0, float *lat1, float *lon1,
		 int flags)
{
	Polyline pl;

	pl.npoints = npoints;
	pl.lat0 = *lat0;
	pl.lat1 = *lat1;
	pl.lon0 = *lon0;
	pl.lon1 = *lon1;
	pl.flags = flags;
	if (! xdr_Polyline (&mf->map_xdr, &pl))
		return (0);
	return (1);
}



static int
map_Point (MapFile *mf, float *lat, float *lon)
{
	if (! xdr_float (&mf->map_xdr, lat))
		return (0);
	if (! xdr_float (&mf->map_xdr, lon))
		return (0);
}



