/*
 * Extract information from USGS 1:2,000,000 DLG SDTS map files into a 
 * Zebra-compatible form.
 */
/*		Copyright (C) 1996 by UCAR
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
# include <stdlib.h>
# include <stdarg.h>
# include <string.h>
# include <errno.h>
#if linux
# include <unistd.h>
# include <getopt.h>	/* for getopt external variables */
#endif
# include "fips123/stc123.h"

typedef char	boolean;

/*
 * Machine byte order
 */
boolean LittleEndian;

/*
 * Might as well share these
 */
char	Tag[128], LeadId[4], Data[32*1024];
char	*MapDir, *State;
FILE	*OutFile;

/*
 * The "themes" by which the USGS map files are divided.
 */
# define T_BOUNDARY	0x001
# define T_HYDRO	0x002
# define T_TRANSPORT	0x004
# define T_MANMADE	0x008
# define T_PLSS		0x010

unsigned long	WhichThemes = 0;

/*
 * Our internal choices for types of maps to generate.
 */
# define M_STATE	0x001
# define M_COUNTY	0x002
# define M_FED		0x004
# define M_ROAD		0x008
# define M_WATER	0x010
# define M_FILLEDWATER	0x020

unsigned long	WhichMaps = 0;

/*
 * Line structure
 */
typedef struct _LineInfo
{
    int	maxpts;
    int	npts;
    short	attr_ndx;
    short	start_node;
    short	end_node;
    short	left_polygon;
    short	right_polygon;
    float	*lon;
    float	*lat;
} LineInfo;

/*
 * Line arrays for boundary, hydro, and road info
 */
int	NBLines = 0;
int	MaxBLines = 0;
LineInfo	*BLines;

int	NHLines = 0;
int	MaxHLines = 0;
LineInfo	*HLines;

int	NRLines = 0;
int	MaxRLines = 0;
LineInfo	*RLines;

int	MaxRoadClass;	/* Highest class road to write 
			   (bigger class -> smaller road) */


/*
 * Boundary attributes
 */
typedef struct _BoundAttr
{
    int		entity;
    short	state;	/* state FIPS code */
    short	county;	/* county or county equivalent FIPS code */
    int		township_code;
    int		monument_num;
    boolean	civil_township;
    boolean	city;
    boolean	natl_forest;
    boolean	wilderness;
    boolean	ahupuaa;
    boolean	hawaiian_homestead;
    boolean	fed_admin;
    boolean	in_dispute;
    boolean	photorevised;
    boolean	best_estimate;
} BoundAttr;

int	NBAttrs = 0;
int	MaxBAttrs = 0;
BoundAttr	*BAttrs;


/*
 * Hydrographic attributes.  All of the "char" elements except the 
 */
typedef struct _HydroAttr
{
    int		entity;
    float	river_mile;
    float	elev;
    short	rotation_ang;
    boolean	photorevised;
    char	ground_relation;	/* 'U', underground
    					 * 'E', elevated
					 * 'T', tunnel
					 * ' ', not applied
					 */
    char	vertical_relation;	/* 'O', overpassing
					 * 'U', underpassing
					 * ' ', not applied
					 */
    char	bank;			/* 'L', left
					 * 'R', right
					 * ' ', not applied
					 */
    char	op_status;		/* 'U', under construction
					 * 'A', abandoned
					 * ' ', not applied
					 */
    boolean	salt;
    boolean	unsurveyed;
    boolean	intermittent;
    boolean	submerged;
    boolean	dry;
    boolean	mineral_or_hot;
    boolean	navigable;
    boolean	earthen;
    boolean	interpolated;
    boolean	best_estimate;
} HydroAttr;

int	NHAttrs = 0;
int	MaxHAttrs = 0;
HydroAttr	*HAttrs;


/*
 * Road/trail attributes
 */
typedef struct _RoadAttr
{
    int		entity;
    short	lanes;
    short	road_width;	/* feet */
    boolean	arbitrary_ext;
    char	ground_relation;	/* 'T', tunnel
					 * 'S', submerged
					 * 'D', double-decked
					 * 'E', elevated
					 * 'R', on drawbridge
					 * ' ', not applied
					 */
    char	vertical_relation;	/* 'O', overpassing
					 * 'U', underpassing
					 * ' ', not applied
					 */
    char	op_status;		/* 'U', under construction, 
					 *      classification known
					 * 'X', under construction,
					 *      classification unknown
					 * ' ', not applied
					 */
    char	access_restriction;	/* 'T', toll road
					 * 'P', private
					 * ' ', not applied
					 */
    boolean	old_rail_grade;
    boolean	with_railroad;
    boolean	covered;
    boolean	historical;
    boolean	limited_access;
    boolean	photorevised;
    boolean	best_estimate;
} RoadAttr;

int	NRAttrs = 0;
int	MaxRAttrs = 0;
RoadAttr	*RAttrs;


/*
 * Administering agencies
 */
typedef short	Agency;

int	NBAgencies = 0;
int	MaxBAgencies = 0;
Agency	*BAgencies;


/*
 * Polygon structure
 */
typedef struct _PolygonInfo
{
    short	attr_ndx;
    short	agency_ndx;
} PolygonInfo;

/*
 * Polygons for boundary, hydro, and road info
 */
int	NBPolys = 0;
int	MaxBPolys = 0;
PolygonInfo	*BPolygons;

int	NHPolys = 0;
int	MaxHPolys = 0;
PolygonInfo	*HPolygons;

int	NRPolys = 0;
int	MaxRPolys = 0;
PolygonInfo	*RPolygons;

/*
 * Protos
 */
void	Usage (char *ourname);
void	ReadLines (char *fname, char *type, LineInfo **lines, int *maxlines, 
		   int *nlines);
void	InitLine (LineInfo *line);
void	CheckLine (LineInfo *line);
void	GetBoundaryInfo (void);
void	GetHydroInfo (void);
void	GetRoadInfo (void);
void	ReadBAttrs (char *fname);
void	ReadHAttrs (char *fname);
void	ReadRAttrs (char *fname);
void	ReadBAgencies (char *fname);
void	ReadPolygons (char *fname, char *type, PolygonInfo **polygons, 
		      int *maxpolys, int *npolys);
void	WriteStates (void);
void	WriteCounties (void);
void	WriteFedAreas (void);
void	WriteWaterBodies (void);
void	WriteFilledWaterBodies (void);
void	WriteStreams (void);
void	WriteRoads (void);
void	WritePolyline (LineInfo *line, boolean filled);
boolean	IsStream (LineInfo *line);
boolean IsWaterBody (PolygonInfo *poly);
boolean IsRoadOfClass (LineInfo *line, int class);
boolean IsWantedFederal (PolygonInfo *poly);
void	die (char *format, ...);



main (int argc, char *argv[])
{
    int c, order;

    while ((c = getopt (argc, argv, "scfr:wW")) != EOF)
    {
	switch (c)
	{
	  case 's':
	    WhichMaps |= M_STATE;
	    WhichThemes |= T_BOUNDARY;
	    break;
	  case 'c':
	    WhichMaps |= M_COUNTY;
	    WhichThemes |= T_BOUNDARY;
	    break;
	  case 'f':
	    WhichMaps |= M_FED;
	    WhichThemes |= T_BOUNDARY;
	    break;
	  case 'r':
	    WhichMaps |= M_ROAD;
	    WhichThemes |= T_TRANSPORT;
	    /*
	     * Optional maximum road class to write, in range 1-4.  
	     * (Interstates are class 1, small paved roads class 4)
	     */
	    if (! sscanf (optarg, "%d", &MaxRoadClass))
	    {
		MaxRoadClass = 4;
		optind--;	/* back up an arg since it wasn't our
				   optional int */
	    }
	    
	    if (MaxRoadClass < 1 || MaxRoadClass > 4)
	    {
		fprintf (stderr, "Road class %d out of range 1-4, using 4.\n", 
			 MaxRoadClass);
		MaxRoadClass = 4;
	    }
	    break;
	  case 'w':
	    WhichMaps |= M_WATER;
	    WhichThemes |= T_HYDRO;
	    break;
	  case 'W':
	    WhichMaps |= M_FILLEDWATER;
	    WhichThemes |= T_HYDRO;
	    break;
	  case 'h':
	  case 'H':
	  case '?':
	    Usage (argv[0]);
	}
    }
	  
    /*
     * Usage: %s [options] <top_map_dir> <2_letter_state>
     */
    if ((argc - optind) < 2)
	Usage (argv[0]);

    MapDir = argv[optind];
    State = argv[optind+1];
    
    /*
     * Machine byte order
     */
    g123order (&order);
    LittleEndian = (order == 0);
    fprintf (stderr, "This machine is %sEndian...\n", 
	     LittleEndian ? "Little" : "Big");

    /*
     * Load the necessary information
     */
    if (WhichThemes & T_BOUNDARY)
	GetBoundaryInfo ();

    if (WhichThemes & T_HYDRO)
	GetHydroInfo ();

    if (WhichThemes & T_TRANSPORT)
	GetRoadInfo ();

    /*
     * Write the requested maps
     */
    if (WhichMaps & M_STATE)
    {
	if (! (OutFile = fopen ("state.map", "w")))
	{
	    perror ("Opening state.map");
	    exit(1);
	}
	WriteStates ();
	fclose (OutFile);
    }

    if (WhichMaps & M_COUNTY)
    {
	if (! (OutFile = fopen ("county.map", "w")))
	{
	    perror ("Opening county.map");
	    exit(1);
	}
	WriteCounties ();
	fclose (OutFile);
    }

    if (WhichMaps & M_FED)
    {
	if (! (OutFile = fopen ("fed.map", "w")))
	{
	    perror ("Opening fed.map");
	    exit(1);
	}
	WriteFedAreas ();
	fclose (OutFile);
    }

    if (WhichMaps & M_WATER)
    {
	if (! (OutFile = fopen ("water.map", "w")))
	{
	    perror ("Opening water.map");
	    exit(1);
	}
	WriteStreams ();
	WriteWaterBodies ();
	fclose (OutFile);
    }

    if (WhichMaps & M_FILLEDWATER)
    {
	if (! (OutFile = fopen ("fwater.map", "w")))
	{
	    perror ("Opening fwater.map");
	    exit(1);
	}
	WriteFilledWaterBodies ();
	fclose (OutFile);
    }

    if (WhichMaps & M_ROAD)
    {
	if (! (OutFile = fopen ("road.map", "w")))
	{
	    perror ("Opening road.map");
	    exit(1);
	}
	WriteRoads ();
	fclose (OutFile);
    }
    
    exit (0);
}



void
Usage (char *ourname)
{
    fprintf (stderr, 
	     "Usage: %s [options] <top_map_dir> <2_letter_state>\n", 
	     ourname);
    fprintf (stderr, "Options:\n");
    fprintf (stderr, "	-s  state bounds -> state.map\n");
    fprintf (stderr, "	-c  county bounds -> county.map\n");
    fprintf (stderr, "	-f  federally administered land -> fed.map\n");
    fprintf (stderr, "	-r[<digit>]  roads, class 1 to <digit> -> road.map\n");
    fprintf (stderr, "	             (default 4, i.e., everything)\n");
    fprintf (stderr, "	-w  streams & water body outlines -> water.map\n");
    fprintf (stderr, "	-W  filled polygon bodies of water -> fwater.map\n");
    die ("");
}



void
GetBoundaryInfo (void)
/*
 * get boundary info
 */
{
    char	fname[128];

    sprintf (fname, "%s/%s/%sBDABDF.DDF", MapDir, State, State);
    ReadBAttrs (fname);

    sprintf (fname, "%s/%s/%sBDLE01.DDF", MapDir, State, State);
    ReadLines (fname, "boundary", &BLines, &MaxBLines, &NBLines);

    sprintf (fname, "%s/%s/%sBDABDM.DDF", MapDir, State, State);
    ReadBAgencies (fname);

    sprintf (fname, "%s/%s/%sBDPC01.DDF", MapDir, State, State);
    ReadPolygons (fname, "boundary", &BPolygons, &MaxBPolys, &NBPolys);
}



void
GetHydroInfo (void)
/*
 * get hydrographic info
 */
{
    char	fname[128];

    sprintf (fname, "%s/%s/%sHYAHYF.DDF", MapDir, State, State);
    ReadHAttrs (fname);

    sprintf (fname, "%s/%s/%sHYLE01.DDF", MapDir, State, State);
    ReadLines (fname, "hydrography", &HLines, &MaxHLines, &NHLines);

    sprintf (fname, "%s/%s/%sHYPC01.DDF", MapDir, State, State);
    ReadPolygons (fname, "hydrography", &HPolygons, &MaxHPolys, &NHPolys);
}



void
GetRoadInfo (void)
/*
 * get road info
 */
{
    char	fname[128];

    sprintf (fname, "%s/%s/%sTRARDF.DDF", MapDir, State, State);
    ReadRAttrs (fname);

    sprintf (fname, "%s/%s/%sTRLE01.DDF", MapDir, State, State);
    ReadLines (fname, "road/trail", &RLines, &MaxRLines, &NRLines);

    sprintf (fname, "%s/%s/%sTRPC01.DDF", MapDir, State, State);
    ReadPolygons (fname, "road/trail", &RPolygons, &MaxRPolys, &NRPolys);
}




void
ReadLines (char *fname, char *type, LineInfo **lines, int *maxlines, 
	   int *nlines)
/*
 * Read a line file
 */
{
    char	ice[4], ccs[4];
    long	level;
    FILE	*infile;
    long	datalen, *val;
    int		status, lon_is_next;
    LineInfo	line;

    fprintf (stderr, "Reading %s line file...", type);
    fflush (stderr);

    /*
     * Open the file
     */
    if (! beg123file (fname, 'r', &level, ice, ccs, &infile))
	die ("Error %d opening file '%s'\n", errno, fname);
    
    /*
     * Get past the data descriptive record
     */
    rd123ddrec (infile, Data, &status);

    /*
     * Loop through the line records
     */
    while (1)
    {
	int		line_id;
	LineInfo	*line = 0;
	int		attrs_next = 0;

	lon_is_next = 1;	/* lon precedes lat */

	/*    
	 * Read the record subfield by subfield
	 */
	while (rd123sfld (infile, Tag, LeadId, Data, &datalen, &status))
	{
	    /*
	     * Get the line id from the second LINE subfield
	     */
	    if (! strcmp (Tag, "LINE") && Data[0] != 'L')
	    {
		line_id = atoi (Data);

		/*
		 * Make sure we have enough line space
		 */
		if (line_id >= *maxlines)
		{
		    *maxlines += 1024;
		    *lines = (LineInfo*) 
			realloc (*lines, *maxlines * sizeof (LineInfo));
		}

		InitLine (*lines + line_id);
		if ((line_id + 1) > *nlines)
		    *nlines = line_id + 1;

		line = *lines + line_id;
		continue;
	    }

	    /*
	     * Can't do this other stuff 'til we have a line ID
	     */
	    if (! line)
		continue;

	    /*
	     * Index for our attributes
	     */
	    if (! strcmp (Tag, "ATID"))
	    {
		if (attrs_next)
		    line->attr_ndx = atoi (Data);

		attrs_next = (Data[0] == 'A');
	    }

	    /*
	     * Polygons to the left and right
	     */
	    if (! strcmp (Tag, "PIDL") && Data[0] != 'P')
		line->left_polygon = atoi (Data);
	    else if (! strcmp (Tag, "PIDR") && Data[0] != 'P')
		line->right_polygon = atoi (Data);

	    /*
	     * Start and end nodes
	     */
	    else if (! strcmp (Tag, "SNID") && Data[0] != 'N')
		line->start_node = atoi (Data);
	    else if (! strcmp (Tag, "ENID") && Data[0] != 'N')
		line->end_node = atoi (Data);
	    
	    /*
	     * The lons & lats are in spatial address (SADR) subfields
	     */
	    else if (! strcmp (Tag, "SADR"))
	    {
		if (LittleEndian)
		{
		    char c;
		    c = Data[0]; Data[0] = Data[3]; Data[3] = c;
		    c = Data[1]; Data[1] = Data[2]; Data[2] = c;
		}
		
		val = (long *)Data;
		
		if (lon_is_next)
		{
		    /*
		     * Get more point space if necessary
		     */
		    CheckLine (line);
		    line->lon[line->npts] = 1.0e-6 * (*val);
		    lon_is_next = 0;
		}
		else
		{
		    line->lat[line->npts++] = 1.0e-6 * (*val);
		    lon_is_next = 1;
		}
	    }
	    

	    /*
	     * Quit on end-of-record (3) or end-of-file (4)
	     */
	    if (status == 3 || status == 4)
		break;
	}

	if (! lon_is_next)
	    fprintf (stderr, "Error: Missed a latitude at line %d\n", line_id);
	
	/*
	 * Stop at end-of-file (status == 4)
	 */
	if (status == 4)
	    break;
    }

    end123file (&infile);
    fprintf (stderr, "done.\n");
}



void
ReadBAttrs (char *fname)
/*
 * Extract the boundary primary attributes
 */
{
    char	ice[4], ccs[4];
    long	level;
    FILE	*infile;
    long	datalen, *val;
    int		status, lon_is_next;
    LineInfo	line;

    fprintf (stderr, "Reading boundary attributes file...");
    fflush (stderr);

    /*
     * Open the file
     */
    if (! beg123file (fname, 'r', &level, ice, ccs, &infile))
	die ("Error %d opening file '%s'\n", errno, fname);
    
    /*
     * Get past the data descriptive record
     */
    rd123ddrec (infile, Data, &status);

    /*
     * Loop through the records
     */
    while (1)
    {
	int	attp = 0;	/* which ATTP subfield currently */
	BoundAttr	*attr = 0;

	/*
	 * Read the record subfield by subfield
	 */
	while (rd123sfld (infile, Tag, LeadId, Data, &datalen, &status))
	{
	    /*
	     * Get the attr id num from the second ATPR subfield
	     */
	    if (! strcmp (Tag, "ATPR") && Data[0] != 'A')
	    {
		int	attr_ndx = atoi (Data);

		/*
		 * Make sure we have enough line space
		 */
		if (attr_ndx >= MaxBAttrs)
		{
		    MaxBAttrs += 1024;
		    BAttrs = (BoundAttr*) 
			realloc (BAttrs, MaxBAttrs * sizeof (BoundAttr));
		}

		if ((attr_ndx + 1) > NBAttrs)
		    NBAttrs = attr_ndx + 1;

		attr = BAttrs + attr_ndx;
		continue;
	    }

	    /*
	     * Can't do this other stuff 'til we have an attr ID
	     */
	    if (! attr)
		continue;

	    if (! strcmp (Tag, "ATTP"))
	    {
		switch (attp++)
		{
		  case 0:
		    attr->entity = atoi (Data);
		    break;
		  case 1:
		    attr->civil_township = (Data[0] == 'Y');
		    break;
		  case 2:
		    attr->city = (Data[0] == 'Y');
		    break;
		  case 3:
		    attr->natl_forest = (Data[0] == 'Y');
		    break;
		  case 4:
		    attr->wilderness = (Data[0] == 'Y');
		    break;
		  case 5:
		    attr->ahupuaa = (Data[0] == 'Y');
		    break;
		  case 6:
		    attr->hawaiian_homestead = (Data[0] == 'Y');
		    break;
		  case 7:
		    attr->fed_admin = (Data[0] == 'Y');
		    break;
		  case 8:
		    attr->in_dispute = (Data[0] == 'Y');
		    break;
		  case 9:
		    attr->state = atoi (Data);
		    break;
		  case 10:
		    attr->county = atoi (Data);
		    break;
		  case 11:
		    attr->township_code = atoi (Data);
		    break;
		  case 12:
		    attr->photorevised = (Data[0] == 'Y');
		    break;
		  case 13:
		    attr->monument_num = atoi (Data);
		    break;
		  case 14:
		    attr->best_estimate = (Data[0] == 'Y');
		    break;
		}
	    }

	    /*
	     * Quit on end-of-record (3) or end-of-file (4)
	     */
	    if (status == 3 || status == 4)
		break;
	}

	/*
	 * Stop at end-of-file (status == 4)
	 */
	if (status == 4)
	    break;
    }

    end123file (&infile);

    /*
     * Set BAttrs[0] to be an empty attribute entry.  (Attribute IDs in the
     * files start at 1, so entry 0 should be free for us to use)
     */
    memset ((void *) BAttrs, 0, sizeof (BoundAttr));

    fprintf (stderr, "done.\n");
}



void
ReadHAttrs (char *fname)
/*
 * Extract the hydrography primary attributes
 */
{
    char	ice[4], ccs[4];
    long	level;
    FILE	*infile;
    long	datalen, *val;
    int		status, lon_is_next;
    LineInfo	line;

    fprintf (stderr, "Reading hydrography attributes file...");
    fflush (stderr);

    /*
     * Open the file
     */
    if (! beg123file (fname, 'r', &level, ice, ccs, &infile))
	die ("Error %d opening file '%s'\n", errno, fname);
    
    /*
     * Get past the data descriptive record
     */
    rd123ddrec (infile, Data, &status);

    /*
     * Loop through the records
     */
    while (1)
    {
	int	attp = 0;	/* which ATTP subfield currently */
	HydroAttr	*attr = 0;

	/*
	 * Read the record subfield by subfield
	 */
	while (rd123sfld (infile, Tag, LeadId, Data, &datalen, &status))
	{
	    /*
	     * Get the attr id num from the second ATPR subfield
	     */
	    if (! strcmp (Tag, "ATPR") && Data[0] != 'A')
	    {
		int	attr_ndx = atoi (Data);

		/*
		 * Make sure we have enough line space
		 */
		if (attr_ndx >= MaxHAttrs)
		{
		    MaxHAttrs += 1024;
		    HAttrs = (HydroAttr*) 
			realloc (HAttrs, MaxHAttrs * sizeof (HydroAttr));
		}

		if ((attr_ndx + 1) > NHAttrs)
		    NHAttrs = attr_ndx + 1;

		attr = HAttrs + attr_ndx;
		continue;
	    }
	    /*
	     * Can't do this other stuff 'til we have an attr ID
	     */
	    if (! attr)
		continue;

	    if (! strcmp (Tag, "ATTP"))
	    {
		switch (attp++)
		{
		  case 0:
		    attr->entity = atoi (Data);
		    break;
		  case 1:
		    attr->photorevised = (Data[0] == 'Y');
		    break;
		  case 2:
		    attr->ground_relation = Data[0];
		    break;
		  case 3:
		    attr->vertical_relation = Data[0];
		    break;
		  case 4:
		    attr->bank = Data[0];
		    break;
		  case 5:
		    attr->op_status = Data[0];
		    break;
		  case 6:
		    attr->salt = (Data[0] == 'Y');
		    break;
		  case 7:
		    attr->unsurveyed = (Data[0] == 'Y');
		    break;
		  case 8:
		    attr->intermittent = (Data[0] == 'Y');
		    break;
		  case 9:
		    attr->submerged = (Data[0] == 'Y');
		    break;
		  case 10:
		    attr->dry = (Data[0] == 'Y');
		    break;
		  case 11:
		    attr->mineral_or_hot = (Data[0] == 'Y');
		    break;
		  case 12:
		    attr->navigable = (Data[0] == 'Y');
		    break;
		  case 13:
		    attr->earthen = (Data[0] == 'Y');
		    break;
		  case 14:
		    attr->interpolated = (Data[0] == 'Y');
		    break;
		  case 15:
		    attr->elev = (float) atof (Data);
		    break;
		  case 16:
		    attr->rotation_ang = (short) atoi (Data);
		    break;
		  case 17:
		    attr->river_mile = (float) atof (Data);
		    break;
		  case 18:
		    attr->best_estimate = (Data[0] == 'Y');
		    break;
		}
	    }

	    /*
	     * Quit on end-of-record (3) or end-of-file (4)
	     */
	    if (status == 3 || status == 4)
		break;
	}

	/*
	 * Stop at end-of-file (status == 4)
	 */
	if (status == 4)
	    break;
    }

    end123file (&infile);

    /*
     * Set HAttrs[0] to be an empty attribute entry.  (Attribute IDs in the
     * files start at 1, so entry 0 should be free for us to use)
     */
    memset ((void *) HAttrs, 0, sizeof (HydroAttr));

    fprintf (stderr, "done.\n");
}



void
ReadRAttrs (char *fname)
/*
 * Extract the road/trail primary attributes
 */
{
    char	ice[4], ccs[4];
    long	level;
    FILE	*infile;
    long	datalen, *val;
    int		status, lon_is_next;
    LineInfo	line;

    fprintf (stderr, "Reading road/trail attributes file...");
    fflush (stderr);

    /*
     * Open the file
     */
    if (! beg123file (fname, 'r', &level, ice, ccs, &infile))
	die ("Error %d opening file '%s'\n", errno, fname);
    
    /*
     * Get past the data descriptive record
     */
    rd123ddrec (infile, Data, &status);

    /*
     * Loop through the records
     */
    while (1)
    {
	int	attp = 0;	/* which ATTP subfield currently */
	RoadAttr	*attr = 0;

	/*
	 * Read the record subfield by subfield
	 */
	while (rd123sfld (infile, Tag, LeadId, Data, &datalen, &status))
	{
	    /*
	     * Get the attr id num from the second ATPR subfield
	     */
	    if (! strcmp (Tag, "ATPR") && Data[0] != 'A')
	    {
		int	attr_ndx = atoi (Data);

		/*
		 * Make sure we have enough line space
		 */
		if (attr_ndx >= MaxRAttrs)
		{
		    MaxRAttrs += 1024;
		    RAttrs = (RoadAttr*) 
			realloc (RAttrs, MaxRAttrs * sizeof (RoadAttr));
		    memset ((void *)(RAttrs + MaxRAttrs - 1024), 0, 
			    1024 * sizeof (RoadAttr));
		}

		if ((attr_ndx + 1) > NRAttrs)
		    NRAttrs = attr_ndx + 1;

		attr = RAttrs + attr_ndx;
		continue;
	    }
	    /*
	     * Can't do this other stuff 'til we have an attr ID
	     */
	    if (! attr)
		continue;

	    if (! strcmp (Tag, "ATTP"))
	    {
		switch (attp++)
		{
		  case 0:
		    attr->entity = atoi (Data);
		    break;
		  case 1:
		    attr->arbitrary_ext = (Data[0] == 'Y');
		    break;
		  case 2:
		    attr->ground_relation = Data[0];
		    break;
		  case 3:
		    attr->vertical_relation = Data[0];
		    break;
		  case 4:
		    attr->op_status = Data[0];
		    break;
		  case 5:
		    attr->access_restriction = Data[0];
		    break;
		  case 6:
		    attr->old_rail_grade = (Data[0] == 'Y');
		    break;
		  case 7:
		    attr->with_railroad = (Data[0] == 'Y');
		    break;
		  case 8:
		    attr->covered = (Data[0] == 'Y');
		    break;
		  case 9:
		    attr->historical = (Data[0] == 'Y');
		    break;
		  case 10:
		    attr->limited_access = (Data[0] == 'Y');
		    break;
		  case 11:
		    attr->photorevised = (Data[0] == 'Y');
		    break;
		  case 12:
		    attr->lanes = (short) atoi (Data);
		    break;
		  case 13:
		    attr->road_width = (short) atoi (Data);
		    break;
		  case 14:
		    attr->best_estimate = (Data[0] == 'Y');
		    break;
		}
	    }

	    /*
	     * Quit on end-of-record (3) or end-of-file (4)
	     */
	    if (status == 3 || status == 4)
		break;
	}

	/*
	 * Stop at end-of-file (status == 4)
	 */
	if (status == 4)
	    break;
    }

    end123file (&infile);

    /*
     * Set RAttrs[0] to be an empty attribute entry.  (Attribute IDs in the
     * files start at 1, so entry 0 should be free for us to use)
     */
    memset ((void *) RAttrs, 0, sizeof (RoadAttr));

    fprintf (stderr, "done.\n");
}



void
ReadBAgencies (char *fname)
/*
 * Extract the boundary agency attributes
 */
{
    char	ice[4], ccs[4];
    long	level;
    FILE	*infile;
    long	datalen, *val;
    int		status, lon_is_next;
    LineInfo	line;

    fprintf (stderr, "Reading boundary agencies file...");
    fflush (stderr);

    /*
     * Open the file
     */
    if (! beg123file (fname, 'r', &level, ice, ccs, &infile))
	die ("Error %d opening file '%s'\n", errno, fname);
    
    /*
     * Get past the data descriptive record
     */
    rd123ddrec (infile, Data, &status);

    /*
     * Loop through the line records
     */
    while (1)
    {
	int	ndx = -1;

	/*
	 * Read the record subfield by subfield
	 */
	while (rd123sfld (infile, Tag, LeadId, Data, &datalen, &status))
	{
	    /*
	     * Get the attr id num from the second ATPR subfield
	     */
	    if (! strcmp (Tag, "ATPR") && Data[0] != 'A')
	    {
		ndx = atoi (Data);

		/*
		 * Make sure we have enough line space
		 */
		if (ndx >= MaxBAgencies)
		{
		    MaxBAgencies += 1024;
		    BAgencies = (Agency*) 
			realloc (BAgencies, MaxBAgencies * sizeof (Agency));
		}

		if ((ndx + 1) > NBAgencies)
		    NBAgencies = ndx + 1;

		continue;
	    }

	    /*
	     * Can't do this other stuff 'til we have an index
	     */
	    if (ndx < 0)
		continue;

	    if (! strcmp (Tag, "ATTP"))
		BAgencies[ndx] = atoi (Data);

	    /*
	     * Quit on end-of-record (3) or end-of-file (4)
	     */
	    if (status == 3 || status == 4)
		break;
	}

	/*
	 * Stop at end-of-file (status == 4)
	 */
	if (status == 4)
	    break;
    }

    end123file (&infile);

    fprintf (stderr, "done.\n");
}



void
ReadPolygons (char *fname, char *type, PolygonInfo **polygons, int *maxpolys,
	      int *npolys)
/*
 * Extract the boundary polygons
 */
{
    char	ice[4], ccs[4];
    long	level;
    FILE	*infile;
    long	datalen, *val;
    int		status, lon_is_next;
    LineInfo	line;

    fprintf (stderr, "Reading %s polygons file...", type);
    fflush (stderr);

    /*
     * Open the file
     */
    if (! beg123file (fname, 'r', &level, ice, ccs, &infile))
	die ("Error %d opening file '%s'\n", errno, fname);
    
    /*
     * Get past the data descriptive record
     */
    rd123ddrec (infile, Data, &status);

    /*
     * Loop through the line records
     */
    while (1)
    {
	PolygonInfo	*poly = 0;
	int	attrs_next = 0, agency_next = 0;

	/*
	 * Read the record subfield by subfield
	 */
	while (rd123sfld (infile, Tag, LeadId, Data, &datalen, &status))
	{
	    /*
	     * Get the attr id num from the second POLY subfield
	     */
	    if (! strcmp (Tag, "POLY") && Data[0] != 'P')
	    {
		int	poly_id = atoi (Data);

		/*
		 * Make sure we have enough polygon space
		 */
		if (poly_id >= *maxpolys)
		{
		    int nadd = 256;
		    int firstnew = *maxpolys;
		    
		    *maxpolys += nadd;
		    *polygons = (PolygonInfo*) 
			realloc (*polygons, *maxpolys * sizeof (PolygonInfo));
		    memset ((void*)((*polygons) + firstnew), 0, 
			    nadd * sizeof (PolygonInfo));
		}

		if ((poly_id + 1) > *npolys)
		    *npolys = poly_id + 1;

		poly = *polygons + poly_id;
		poly->attr_ndx = poly->agency_ndx = 0;
		continue;
	    }

	    /*
	     * Can't do this other stuff 'til we have a polygon ID
	     */
	    if (! poly)
		continue;

	    /*
	     * Get this polygon's indices into the attributes and agency 
	     * lists
	     */
	    if (! strcmp (Tag, "ATID"))
	    {
		if (attrs_next)
		    poly->attr_ndx = atoi (Data);
		else if (agency_next)
		    poly->agency_ndx = atoi (Data);
		/*
		 * We get a file identifier just before the index.
		 * The name is AxxF for the primary attribute file and
		 * AxxM for the agency file.
		 */
		attrs_next = (Data[3] == 'F');
		agency_next = (Data[3] == 'M');
	    }

	    /*
	     * Quit on end-of-record (3) or end-of-file (4)
	     */
	    if (status == 3 || status == 4)
		break;
	}

	/*
	 * Stop at end-of-file (status == 4)
	 */
	if (status == 4)
	    break;
    }

    end123file (&infile);

    fprintf (stderr, "done.\n");
}



void
InitLine (LineInfo *line)
{
/*
 * Start this line with space for 8 points
 */
    line->maxpts = 8;
    line->npts = 0;
    line->attr_ndx = 0;
    line->lon = (float *) malloc (line->maxpts * sizeof (float));
    line->lat = (float *) malloc (line->maxpts * sizeof (float));
}


void
CheckLine (LineInfo *line)
{
/*
 * Make sure we can add another point to this line
 */    
    if (line->npts == line->maxpts)
    {
	line->maxpts *= 2;
	line->lon = (float*) 
	    realloc (line->lon, line->maxpts * sizeof (float));
	line->lat = (float*) 
	    realloc (line->lat, line->maxpts * sizeof (float));
    }
}


void
WriteStates (void)
/*
 * Write out every line that has a different state on either side
 */
{
    int	i;
    PolygonInfo	*pleft, *pright;
    int	sleft, sright;

    for (i = 1; i < NBLines; i++)
    {
	pleft = BPolygons + BLines[i].left_polygon;
	pright = BPolygons + BLines[i].right_polygon;

	sleft = BAttrs[pleft->attr_ndx].state;
	sright = BAttrs[pright->attr_ndx].state;

	if (sleft != sright)
	    WritePolyline (BLines + i, 0);
    }
} 



void
WriteCounties (void)
/*
 * Write out every line that has a different county on either side
 */
{
    int	i;
    PolygonInfo	*pleft, *pright;
    int	cleft, cright;

    for (i = 1; i < NBLines; i++)
    {
	pleft = BPolygons + BLines[i].left_polygon;
	pright = BPolygons + BLines[i].right_polygon;

	cleft = BAttrs[pleft->attr_ndx].county;
	cright = BAttrs[pright->attr_ndx].county;

	if (cleft != cright)
	    WritePolyline (BLines + i, 0);
    }
} 



void
WriteFedAreas (void)
/*
 * Write borders for federally administered areas
 */
{
    int	i;
    PolygonInfo	*pleft, *pright;

    for (i = 1; i < NBLines; i++)
    {
	pleft = BPolygons + BLines[i].left_polygon;
	pright = BPolygons + BLines[i].right_polygon;
	/*
	 * Write out every line obeying all of the following:
	 *	o has different areas on either side
	 *	o at least one of the sides meets IsWantedFederal() criteria
	 *	o the sides are administered by different agencies
	 */
	if ((pleft != pright) &&
	    (IsWantedFederal (pleft) || IsWantedFederal (pright)) &&
	    (BAgencies[pleft->agency_ndx] != BAgencies[pright->agency_ndx]))
	    WritePolyline (BLines + i, 0);
    }
} 



void
WriteWaterBodies (void)
/*
 * Write out every line that borders a body of water
 */
{
    int	i;

    PolygonInfo	*pleft, *pright;

    for (i = 1; i < NHLines; i++)
    {
	pleft = HPolygons + HLines[i].left_polygon;
	pright = HPolygons + HLines[i].right_polygon;

	if (IsWaterBody (pleft) || IsWaterBody (pright))
	    WritePolyline (HLines + i, 0);
    }
} 




void
WriteFilledWaterBodies (void)
/*
 * Generate filled polygons for all bodies of water
 */
{
    int	i, poly, nextnode;
# define LINELISTLEN 1024
    LineInfo	*linelist[LINELISTLEN];
    int	nlines;
    int	pathstart, path;
    LineInfo	polyline;
    float	firstlat, firstlon;

    InitLine (&polyline);

    for (poly = 0; poly < NHPolys; poly++)
    {
	if (! IsWaterBody (HPolygons + poly))
	    continue;
	/*
	 * Make a list of lines bordering this water body polygon
	 */
	nlines = 0;

	for (i = 0; i < NHLines; i++)
	{
	    if (HLines[i].left_polygon == poly || 
		HLines[i].right_polygon == poly)
	    {
		if (nlines == LINELISTLEN)
		{
		    fprintf (stderr, 
			     "Too many lines in water body polygon %d\n", 
			     poly);
		    continue;
		}

		linelist[nlines++] = HLines + i;
	    }
	}

	/*
	 * Keep the location of the start of the bounding line.  We want
	 * the final bounding line to travel clockwise, so we keep the
	 * polygon to the right. 
	 */
	if (linelist[0]->right_polygon == poly)
	{
	    firstlon = linelist[0]->lon[0];
	    firstlat = linelist[0]->lat[0];
	}
	else
	{
	    int	nlpts = linelist[0]->npts;

	    firstlon = linelist[0]->lon[nlpts-1];
	    firstlat = linelist[0]->lat[nlpts-1];
	}

	path = -1;
	
	/*
	 * Sort the lines after the first one so that we move contiguously
	 * clockwise around the polygon.  If we close a path before we're
	 * done with our lines, it's because the "polygon" really consists
	 * of multiple closed paths (e.g., a lake and its embedded
	 * islands).  We deal with these multi-path polygons by explicitly
	 * connecting each path back to the first one, at the beginning and
	 * end of each extra path. 
	 */
	polyline.npts = 0;
	
	for (i = 0; i < nlines; i++)
	{
	    LineInfo	*line = linelist[i];
	    LineInfo	*temp;
	    int		firstpt, lastpt, step, pt;

	    /*
	     * If we closed the path (or we're just starting), we'll start a 
	     * new path here.
	     */
	    if (i == 0 || nextnode == pathstart)
	    {
		path++;

		if (line->right_polygon == poly)
		    pathstart = line->start_node;
		else
		    pathstart = line->end_node;

		/*
		 * If it's not the first path, join it to the first path
		 */
		if (path)
		{
		    CheckLine (&polyline);
		    polyline.lon[polyline.npts] = firstlon;
		    polyline.lat[polyline.npts++] = firstlat;
		}
	    }
	    /*
	     * Otherwise, we haven't closed the path yet, so find the next
	     * line in the path and swap it with the current line in the
	     * list.
	     */
	    else
	    {
		int	j;

		for (j = i; j < nlines; j++)
		{
		    if ((linelist[j]->start_node == nextnode) ||
			(linelist[j]->end_node == nextnode))
			break;
		}

		temp = linelist[i];
		linelist[i] = linelist[j];
		linelist[j] = temp;

		line = linelist[i];
	    }
	    /*
	     * Figure out the node we share with the next line and the number
	     * and order of segments of this line to write.  The rules are:
	     * 1) keep the polygon on the right, and 2) don't write the
	     * last point of a line, since it's the first point of the next
	     * line (exception: we write the last point for a line that closes
	     * a path).
	     */
	    if (line->right_polygon == poly)
	    {
		nextnode = line->end_node;
		firstpt = 0;
		lastpt = line->npts - 2;
		step = 1;
	    }
	    else
	    {
		nextnode = line->start_node;
		firstpt = line->npts - 1;
		lastpt = 1;
		step = -1;
	    }

	    if (nextnode == pathstart)
		lastpt += step;	/* close the path */

	    for (pt = firstpt; pt != (lastpt + step); pt += step)
	    {
		CheckLine (&polyline);
		polyline.lon[polyline.npts] = line->lon[pt];
		polyline.lat[polyline.npts++] = line->lat[pt];
	    }
	    /*
	     * Finally, if we just closed a path, and it's not the first
	     * path, add a point to join us to the first path.
	     */
	    if (nextnode == pathstart && path > 0)
	    {
		CheckLine (&polyline);
		polyline.lon[polyline.npts] = firstlon;
		polyline.lat[polyline.npts++] = firstlat;
	    }
	}

	/*
	 * Finally, write out the polygon bounding line
	 */
	WritePolyline (&polyline, 1);
    }
}    
    
	
	


void
WriteStreams (void)
/*
 * Write out every line that is a stream
 */
{
    int	i;

    for (i = 1; i < NHLines; i++)
	if (IsStream (HLines + i))
	    WritePolyline (HLines + i, 0);
} 



void
WriteRoads (void)
/*
 * Write out selected roads
 */
{
    int	i;

    for (i = 1; i < NRLines; i++)
 	if (IsRoadOfClass (RLines + i, MaxRoadClass))
	    WritePolyline (RLines + i, 0);
} 



void
WritePolyline (LineInfo *line, boolean filled)
{
    int	p;
    float	minlat = 90.0, maxlat = -90.0;
    float	minlon = 180.0, maxlon = -180.0;

    for (p = 0; p < line->npts; p++)
    {
	if (line->lon[p] < minlon)
	    minlon = line->lon[p];
	if (line->lon[p] > maxlon)
	    maxlon = line->lon[p];
	if (line->lat[p] < minlat)
	    minlat = line->lat[p];
	if (line->lat[p] > maxlat)
	    maxlat = line->lat[p];
    }

    fprintf (OutFile, " %3d %9.3f %9.3f %9.3f %9.3f %s", 2 * line->npts, 
	     maxlat, minlat, maxlon, minlon, filled ? "FILL" : "");

    for (p = 0; p < line->npts; p++)
    {
	if (! (p % 4))
	    fprintf (OutFile, "\n");

	fprintf (OutFile, " %9.3f %9.3f", line->lat[p], line->lon[p]);
    }
    fprintf (OutFile, "\n");
}    
    
	
	


void
die (char *format, ...)
{
    va_list	args;

    va_start (args, format);
    vfprintf (stderr, format, args);
    va_end (args);

    exit (1);
}


boolean
IsStream (LineInfo *line)
/*
 * Return true for a line identified as a stream and not identified as dry
 * or intermittent.
 */
{
    int	attndx = line->attr_ndx;
    /*
     * Entity label 0500412 == stream
     */
    return (HAttrs[attndx].entity == 500412 && ! HAttrs[attndx].dry &&
	    ! HAttrs[attndx].intermittent);
}


boolean
IsWaterBody (PolygonInfo *poly)
/*
 * Return true for a polygon identified as a lake, pond, reservoir, bay,
 * estuary, gulf, ocean, or sea and not identified as dry or intermittent.
 */
{
    HydroAttr	*hattr = HAttrs + poly->attr_ndx;
    /*
     * Entity label 0500421 == lake or pond, 0500101 == reservoir, 
     * 0500116 == bay, estuary, gulf, ocean, or sea
     */
    return ((hattr->entity == 500421 || hattr->entity == 500101 ||
	     hattr->entity == 500116) && 
	    ! (hattr->dry || hattr->intermittent));
}



boolean
IsRoadOfClass (LineInfo *line, int class)
/*
 * Return true for a line identified as a road of the specified class or
 * larger (in size, not class, since class 1 roads are the biggest).
 */
{
    int entity = RAttrs[line->attr_ndx].entity;
    boolean	ok = 0;

    if (class > 4)
	class = 4;
/*
 * Automatic false if this line has an attribute index of zero
 */
    if (! line->attr_ndx)
	return 0;
/*
 * Automatic true if this line's entity is zero.  This happens because some of 
 * the attributes files are incomplete, i.e., some roads want attribute
 * indices that are never provided.  Since we don't know for sure about these
 * roads, it's better to include them than to leave them out.
 */
    if (! entity)
	return 1;
/*
 * The real check.  Test the road's entity and see if it fits into one of
 * the desired road classes.
 */    
    switch (class)
    {
      case 4:
	ok |= (entity == 1700210 || entity == 1700219);
	/* fall into */
      case 3:
	ok |= (entity == 1700209 || entity == 1700217 || entity == 1700218);
	/* fall into */
      case 2:
	ok |= (entity >= 1700205 && entity <= 1700208);
	/* fall into */
      case 1:
	ok |= (entity >= 1700201 && entity <= 1700204);
	break;
      default:
	die ("Whoa, bad road class %d in IsRoadOfClass()!\n", class);
    }
    return (ok);
}




boolean
IsWantedFederal (PolygonInfo *poly)
/*
 * Return true for a polygon that's federally administered and whose
 * "agency" is one of the desired ones.
 */
{
    int	fed = BAttrs[poly->attr_ndx].fed_admin;
    int	agency_ndx = BAgencies[poly->agency_ndx];
/*
 * The numbers below come from DLG3MDOM.DDF.  A couple of additional ones
 * that may be of interest at some point:  
 *	363 - National Park
 *	121 - National Forest
 */
    return (fed &&
	    (agency_ndx == 200 ||	/* Department of Defense	*/
	     agency_ndx == 210 ||	/* Air Force			*/
	     agency_ndx == 220 ||	/* Army				*/
	     agency_ndx == 225 ||	/* Army / Corps of Engineers	*/
	     agency_ndx == 230 ||	/* Navy				*/
	     agency_ndx == 235 ||	/* Navy / Marine Corps		*/
	     agency_ndx == 250));	/* Department of Energy		*/
}
