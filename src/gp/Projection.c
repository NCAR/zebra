/*
 * Handle things related to map projections.
 */

/*		Copyright (C) 1987-1995 by UCAR
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

/*
 * Get atof and strtod protos from floatingpoint.h first instead of 
 * parameter-less protos in ANSI location stdlib.h
 */
# include <math.h>

# include <config.h>
# include <defs.h>

MAKE_RCSID ("$Id: Projection.c,v 2.6 1996-03-12 17:41:37 granger Exp $")

# ifdef MAP_PROJECTIONS
static char *projopt[2] = { "@(#)$GP: Map projections NOT compiled $",
				   (char *)projopt };
# else
static char *projopt[2] = { "@(#)$GP: Map projections compiled $",
				   (char *)projopt };
#endif

/*
 * This whole mess can be configured out.
 */

# include <message.h>
# include <pd.h>
# include "GraphProc.h"
# include "PixelCoord.h"

# ifdef MAP_PROJECTIONS
/* # define PJ_LIST_H "ProjList.h" doesnt work!! */
/* Have to undef PI since projects.h defines it and linux/math.h may have
 * defined it above */
# ifdef PI
# undef PI
# endif
# include "projects.h"
/*
 * This "PJ" dude is the magic cookie used by the proj library to describe
 * our current projection scheme.
 */
static PJ *OurPJ = 0;
static float VOffset;	/* N-S offset in m */
static int PKey;	/* The "key" value we return */
char Pname[80];		/* Name of the projection	*/

/*
 * This structure is used for mapping between projections as given
 * in the plot description and those known by the "proj" library.
 */
struct ProjMap
{
	char	pm_Name[32];		/* Name used in PD		*/
	char 	pm_LibName[32];		/* Name to pass to proj lib	*/
	char	pm_LongName[80];	/* Longer name for annotation	*/
	void	(*pm_SParam)();		/* Func for additional params	*/
};

/*
 * Tweaker functions.
 */
static void prj_Conic FP ((struct ProjMap *, int *, char **));
static void prj_Polar FP ((struct ProjMap *, int *, char **));
static void prj_Stere FP ((struct ProjMap *, int *, char **));
static void prj_PStere FP ((struct ProjMap *, int *, char **));

/*
 * Here are the projections we know about.  If you make a change here, make
 * sure the PROJ_HEAD stuff in ProjList.h matches!
 */
static struct ProjMap PMap [] =
{
	{ "aea",	"aea",		"Albers equal area", prj_Conic },
	{ "cass",	"cass",		"Cassini", 0 },
	{ "merc",	"merc",		"Mercator", 0 },
	{ "eqc",	"eqc",	        "Equidistant cylindrical", 0 },
	{ "leac",	"leac",		"Lambert equal area", prj_Conic },
	{ "lcc",	"lcc",		"Lambert conformal", prj_Conic },
	{ "lcpolar",	"lcc",		"Lambert conformal polar", prj_Polar },
	{ "moll",	"moll",		"Mollweide", 0 },
	{ "ortho",	"ortho",	"Orthographic", 0 },
	{ "stere",	"stere",	"Stereographic", prj_Stere },
	{ "stpolar",	"stere",	"Polar stereographic", prj_PStere },
};
# define N_PROJECTION (sizeof (PMap)/sizeof (struct ProjMap))

# endif

/*
 * Our notion of the origin.
 */
static float O_Lat, O_Lon;


/*
 * Local stuff.
 */
static int prj_GetParams FP ((char *, float *, float *));
# ifdef MAP_PROJECTIONS
static int prj_InitProj FP ((struct ProjMap *, double, double));
static struct ProjMap *prj_Lookup FP ((char *));
# endif





int
prj_Setup ()
/*
 * Get everything set up.
 */
{
	char proj[CFG_PLATNAME_LEN];
	float lat, lon;
	int key;
# ifdef MAP_PROJECTIONS
	struct ProjMap *pm;
# endif
/*
 * Dig up all of the parameters.
 */
	if (! prj_GetParams (proj, &lat, &lon))
		return (FALSE);
	O_Lat = lat;
	O_Lon = lon;
/*
 * Stash the lat/lon into the old system, we sometimes need it even
 * with a fancy projection.
 */
	cvt_Origin (O_Lat, O_Lon);
# ifdef MAP_PROJECTIONS
/*
 * Lookup the desired projection, and make a key out of it.  This key
 * incorporates both the desired projection and the origin, and should
 * always change when we need it to.
 */
	pm = prj_Lookup (proj);
	key = (int) (O_Lat + O_Lon) + (int) pm;
	strcpy (Pname, pm ? pm->pm_LongName : "Zebra");
/*
 * If we have an old PJ structure, and a new projection is in the works,
 * dump the old one.
 */
	if (OurPJ && key != PKey)
	{
		pj_free (OurPJ);
		OurPJ = 0;
	}
/*
 * If we have a fancy projection and it has changed, try to initialize it.
 * Since we precheck projection names now, the init should never fail.
 */
	if (pm)
	{
		if (key != PKey)
			PKey = prj_InitProj (pm, lat, lon) ? key : 0;
	}
	else
		PKey = 0;
	msg_ELog (EF_DEBUG, "Projection: %s, Key %d, VOffset %.2f", Pname,
			PKey, VOffset);
# endif
	return (TRUE);
}





void
prj_GetOrigin (lat, lon)
float *lat, *lon;
/*
 * Return our origin.
 */
{
	*lat = O_Lat;
	*lon = O_Lon;
}





char *
prj_GetProjName ()
/*
 * Return the name of this projection.
 */
{
# ifdef MAP_PROJECTIONS
	return (Pname);
# else
	return ("Zebra");
# endif
}




static int
prj_GetParams (proj, lat, lon)
char *proj;
float *lat, *lon;
/*
 * Get the projection control parameters.
 */
{
	bool ok;
/*
 * Figure out what projection they want.
 */
	if (! pda_Search (Pd, "global", "projection", NULL, proj, SYMT_STRING))
		strcpy (proj, "zebra");
/*
 * Get the origin and plot limits
 */
	ok = pda_ReqSearch (Pd, "global", "origin-lat", NULL, (char *) lat, 
		SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, "global", "origin-lon", NULL, (char *) lon, 
		SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, "global", "x-min", NULL, (char *) &Xlo,
			SYMT_FLOAT);

	ok &= pda_ReqSearch (Pd, "global", "x-max", NULL, (char *) &Xhi,
			SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, "global", "y-min", NULL, (char *) &Ylo,
			SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, "global", "y-max", NULL, (char *) &Yhi,
			SYMT_FLOAT);
	return (ok);
}
	





# ifdef MAP_PROJECTIONS




static struct ProjMap *
prj_Lookup (proj)
char *proj;
/*
 * Try to find this projection and return a pointer to it.
 */
{
	int pindex;
/*
 * This linear search won't burn us as long as the list is small.
 *
 *
 * right.
 */
	for (pindex = 0; pindex < N_PROJECTION; pindex++)
		if (! strcmp (proj, PMap[pindex].pm_Name))
			break;
/*
 * If we didn't find anything, gripe if it seems appropriate and put
 * us back into the old scheme.
 */
	if (pindex >= N_PROJECTION)
	{
		if (strcmp (proj, "zebra"))
			msg_ELog (EF_PROBLEM, "Unknown projection: %s", proj);
		return (0);
	}
	return (PMap + pindex);
}






static int
prj_InitProj (pm, lat, lon)
struct ProjMap *pm;
double lat, lon;
/*
 * Try to set up a fancy projection.
 */
{
	char *params[10], pproj[32], plat[32], plon[32], lts[32], *prp;
	int nparam = 0;
	UV origin;
/*
 * Set up the parameters for the init call.
 */
	sprintf (pproj, "proj=%s", pm->pm_LibName);
	params[nparam++] = pproj;
# ifdef notdef
	sprintf (plat, "lat_0=%.4f", lat);
	params[nparam++] = plat;
# endif	
	sprintf (plon, "lon_0=%.4f", lon);
	params[nparam++] = plon;
	sprintf (lts, "lat_ts=%.4f", lat);
	params[nparam++] = lts;
/*
 * Add in any projection-specific ones.
 */
# ifdef notdef
	for (arg = 0; arg < pm->pm_Narg; arg++)
		params[nparam++] = pm->pm_Eargs[arg];
# endif
	if (pm->pm_SParam)
		(*pm->pm_SParam) (pm, &nparam, params);
/*
 * We need a way to get user-specified params in here too.  Someday.
 */
	if ((OurPJ = pj_init (nparam, params)) == 0)
	{
		msg_ELog (EF_PROBLEM, "pj_init failed, proj = %s",
				pm->pm_Name);
		return (FALSE);
	}
# ifdef notdef
/*
 * Make up a "key" for this projection.  Kludge.
 */
	PKey = 0;
	for (prp = proj; *prp; prp++)
		PKey += *prp;
# endif
/*
 * Not all projections respect lat_0, so we have to remember an offset anyway.
 */
	origin.u = lon*DEG_TO_RAD;
	origin.v = lat*DEG_TO_RAD;
	origin = pj_fwd (origin, OurPJ);
	VOffset = origin.v;
	return (TRUE);
}

# endif



int
prj_Project (lat, lon, x, y)
double lat, lon;
float *x, *y;
/*
 * Project a lat/lon pair into XY space.
 */
{
# ifdef MAP_PROJECTIONS
	UV loc, result;

	if (OurPJ)
	{
		loc.u = lon*DEG_TO_RAD;
		loc.v = lat*DEG_TO_RAD;
		result = pj_fwd (loc, OurPJ);
		if (result.u == HUGE_VAL)
			return (FALSE);
		*x = result.u/1000.0;
		*y = (result.v - VOffset)/1000.0;
	}
	else
# endif		
		cvt_ToXY (lat, lon, x, y);
	return (TRUE);
}





int
prj_Reverse (x, y, lat, lon)
double x, y;
float *lat, *lon;
/*
 * Turn an XY pair into a lat/lon pair.
 */
{
# ifdef MAP_PROJECTIONS
	UV xy, result;

	if (OurPJ)
	{
		xy.u = x*1000.0;
		xy.v = y*1000.0 + VOffset;
		result = pj_inv (xy, OurPJ);
		if (result.u == HUGE_VAL)
			return (FALSE);
		*lon = result.u*RAD_TO_DEG;
		*lat = result.v*RAD_TO_DEG;
	}
	else
# endif
		cvt_ToLatLon (x, y, lat, lon);
	return (TRUE);
}





int
prj_ProjKey ()
/*
 * Return a "key" to identify the current projection.  Returns 0 for
 * the old-style rectangular projection.
 */
{
# ifdef MAP_PROJECTIONS
	return (OurPJ ? (PKey + VOffset) : VOffset);
# else
	return (0);
# endif
}




int
prj_FancyProjection ()
/*
 * Return TRUE iff a fancy projection is in use.
 */
{
# ifdef MAP_PROJECTIONS
	return (OurPJ != 0);
# else
	return (FALSE);
# endif
}



/*
 * Projection tweaker functions.
 */
# ifdef MAP_PROJECTIONS


static void
prj_Conic (pm, np, params)
struct ProjMap *pm;
int *np;
char **params;
/*
 * Fix up additional parameters needed for conic projections.
 */
{
	static char lat1p[32], lat2p[32];
	float lat, lon;
/*
 * Try to figure lat1 and lat2 as the upper and lower bounds of the
 * window, using the old projection scheme (since the new is not yet
 * initialized).  Suboptimal, but the best that I think we can do.
 *
 */
	cvt_ToLatLon (Xlo, Ylo, &lat, &lon);
	sprintf (lat1p, "+lat_1=%.4f", lat);
	params[(*np)++] = lat1p;
	cvt_ToLatLon (Xhi, Yhi, &lat, &lon);
	sprintf (lat2p, "+lat_2=%.4f", lat);
	params[(*np)++] = lat2p;
	msg_ELog (EF_DEBUG, "Conic: lat1 '%s', lat2 '%s'", lat1p, lat2p);
/*
 * Some projections can use a "+south" to do better in the southern
 * hemisphere.  And it doesn't hurt to have it for the rest.
 */
	if (O_Lat < 0)
		params[(*np)++] = "+south";
}


static void
prj_Polar (pm, np, params)
struct ProjMap *pm;
int *np;
char **params;
/*
 * A limiting form of conic that sets things up for polar projections.
 */
{
	int south = O_Lat < 0;
	params[(*np)++] = south ? "+lat_1=-90" : "+lat_1=90";
	params[(*np)++] = south ? "+lat_2=-90" : "+lat_2=90";
	if (south)
		params[(*np)++] = "+south";
}



static void
prj_Stere (pm, np, params)
struct ProjMap *pm;
int *np;
char **params;
/*
 * Make a stereographic projection centered on our origin
 */
{
	static char slat[32];
/*
 * Stereographic requires that we set lat_0.  Let's hope nobody else does.
 */
	sprintf (slat, "+lat_0=%.4f", O_Lat);
	params[(*np)++] = slat;

	if (O_Lat < 0)
		params[(*np)++] = "+south";
}



static void
prj_PStere (pm, np, params)
struct ProjMap *pm;
int *np;
char **params;
/*
 * Make a polar stereographic projection.
 */
{
	static char slat[32];
	int	south = (O_Lat < 0);
/*
 * Polar means lat_0 = +/-90
 */
	sprintf (slat, "+lat_0=%.4f", south ? -90 : 90);
	params[(*np)++] = slat;

	if (south)
		params[(*np)++] = "+south";
}


# endif
