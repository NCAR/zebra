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
# include <string.h>
# include <ctype.h>

# include <config.h>
# include <defs.h>

MAKE_RCSID ("$Id: Projection.c,v 2.13 2004-11-17 17:37:22 burghart Exp $")

# ifdef MAP_PROJECTIONS
static char *projopt[2] = { "@(#)$GP: Map projections compiled $",
				   (char *)projopt };
# else
static char *projopt[2] = { "@(#)$GP: Map projections NOT compiled $",
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

static struct ProjMap UserProj =
{
	"user", "user", "User", 0
};

# endif

/*
 * Our notion of the origin.
 */
static float O_Lat, O_Lon;


/*
 * Local stuff.
 */
static int prj_GetParams FP ((char *, int *, char **, float *, float *));
static void prj_FreeParams FP ((int *nparam, char **params));
static void prj_SetParam FP ((char *p, char *v, int *nparam, char **params));
static int prj_AddParam FP ((char *p, char *v, int *nparam, char **params));
static int prj_AddFloatParam FP ((char *, double v, int *nparam, char **p));
# ifdef MAP_PROJECTIONS
static int prj_InitProj FP ((struct ProjMap *pm, int *nparam, char **params,
			     double lat, double lon));
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
	char *params[32];
	int nparam = 0;
/*
 * Dig up all of the parameters.
 */
	if (! prj_GetParams (proj, &nparam, params, &lat, &lon))
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
 * If it fails, revert to zebra projection.
 */
	if (pm)
	{
		if (key != PKey)
			PKey = prj_InitProj (pm, &nparam, params, lat, lon)
				? key : 0;
	}
	else
		PKey = 0;
/*
 * Set the name here rather than above in case the initialization failed.
 */
	strcpy (Pname, (pm && PKey) ? pm->pm_LongName : "Zebra");
	msg_ELog (EF_DEBUG, "Projection setup: %s, Key %d, VOffset %.2f",
		  Pname, PKey, VOffset);
# endif
	prj_FreeParams (&nparam, params);
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
prj_GetParams (proj, nparam, params, lat, lon)
char *proj;
int *nparam;
char **params;
float *lat, *lon;
/*
 * Get the projection control parameters.
 */
{
	zbool ok;
	char *space;
	char *c;
	int param;
	Location loc;
/*
 * Figure out what projection they want.
 */
	if (! pda_Search (Pd, "global", "projection", NULL, proj, SYMT_STRING))
		strcpy (proj, "zebra");
/*
 * Get the origin and plot limits.  Allow for the common location format
 * first before reverting to the original unique forms -lat and -lon.
 */
	ok = 1;
	if (GetLocation ("origin", &PlotTime, &loc))
	{
	    *lat = loc.l_lat;
	    *lon = loc.l_lon;
	}
	else
	{
	    ok &= pda_ReqSearch (Pd, "global", "origin-lat", NULL, 
				(char *) lat, SYMT_FLOAT);
	    ok &= pda_ReqSearch (Pd, "global", "origin-lon", NULL,
				 (char *) lon, SYMT_FLOAT);
	}
	ok &= pda_ReqSearch (Pd, "global", "x-min", NULL, (char *) &Xlo,
			SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, "global", "x-max", NULL, (char *) &Xhi,
			SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, "global", "y-min", NULL, (char *) &Ylo,
			SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, "global", "y-max", NULL, (char *) &Yhi,
			SYMT_FLOAT);
/*
 * Parse the projection for space-separated user parameters.
 */
	c = proj;
	param = 0;
	while (ok && *c)
	{
		while (*c && isspace (*c))
			c++;
		if (! *c)
			break;
		space = c;
		while (*space && ! isspace (*space))
			space++;
		if (*space)
			*(space++) = '\0';
		if (param)	/* skip the initial projection name */
			prj_SetParam (c, NULL, nparam, params);
		else
			param = 1;
		c = space;
	}
	return (ok);
}
	




static void
prj_FreeParams (nparam, params)
int *nparam;
char **params;
{
	int i;
	for (i = 0 ; i < *nparam ; ++i)
	{
		if (params[i])
			free (params[i]);
	}
	*nparam = 0;
}




static void
prj_SetParam (p, v, nparam, params)
char *p;
char *v;
int *nparam;
char **params;
/*
 * Just set this parameter at the end of the list.
 */
{
	int len = strlen(p) + (v ? strlen(v) : 0) + 2;
	params[(*nparam)] = (char *) malloc (len);
	sprintf (params[(*nparam)], "%s%s%s", p, v ? "=" : "", v ? v : "");
	(*nparam)++;
}




static int
prj_AddParam (p, v, nparam, params)
char *p;
char *v;
int *nparam;
char **params;
/*
 * The params array of pointers is assumed to have enough room to append
 * the given parameter.  The parameter is only added if it does not
 * already exist in the array.  If v is null, the parameter is added
 * without the '=' and with no value.  Return non-zero if parameter was
 * actually added.  Once used the parameter list needs to be freed with
 * prj_FreeParams().
 */
{
	int i;

	for (i = 0; i < *nparam; ++i)
	{
		if (strstr (params[i], p) == params[i])
			return (0);
	}
/*
 * So add the parameter already...
 */
	prj_SetParam (p, v, nparam, params);
	return (1);
}



static int
prj_AddFloatParam (p, v, nparam, params)
char *p;
double v;
int *nparam;
char **params;
{
	char value[32];
	sprintf (value, "%.4f", v);
	return (prj_AddParam (p, value, nparam, params));
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
		if (! strcmp (proj, "user"))
			return (&UserProj);
		if (strcmp (proj, "zebra"))
			msg_ELog (EF_PROBLEM, "Unknown projection: %s", proj);
		return (0);
	}
	return (PMap + pindex);
}





static int
prj_InitProj (pm, nparam, params, lat, lon)
struct ProjMap *pm;
int *nparam;
char **params;
double lat, lon;
/*
 * Try to set up a fancy projection.
 */
{
	int i;
	projUV origin;
	char log[256];
/*
 * Set up the parameters for the init call.  For special "user" projection,
 * don't set any parameters.
 */
	if (pm != &UserProj)
	{
		prj_AddParam ("proj", pm->pm_LibName, nparam, params);
		prj_AddFloatParam ("lon_0", lon, nparam, params);
		prj_AddFloatParam ("lat_ts", lat, nparam, params);
		prj_AddParam ("ellps", "GRS80", nparam, params);
	}
/*
 * Add in any projection-specific ones.
 */
	if (pm->pm_SParam)
		(*pm->pm_SParam) (pm, nparam, params);

	sprintf (log, "Init proj: ");
	for (i = 0; i < *nparam; ++i)
		sprintf (log+strlen(log), "%s ", params[i]);
	msg_ELog (EF_DEBUG, "%s", log);

	if ((OurPJ = pj_init (*nparam, params)) == 0)
	{
		msg_ELog (EF_PROBLEM, "pj_init failed, proj = %s",
			  pm->pm_Name);
		return (FALSE);
	}
/*
 * Not all projections respect lat_0, so we have to remember an offset anyway.
 */
	origin.u = lon*DEG_TO_RAD;
	origin.v = lat*DEG_TO_RAD;
	origin = pj_fwd (origin, OurPJ);
	VOffset = origin.v;
	if (origin.u == HUGE_VAL)
	{
		msg_ELog (EF_PROBLEM,
			  "projection of origin to get VOffset failed");
		VOffset = 0;
	}
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
	projUV loc, result;

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
	projUV xy, result;

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
	float lat, lon;
/*
 * Try to figure lat1 and lat2 as the upper and lower bounds of the
 * window, using the old projection scheme (since the new is not yet
 * initialized).  Suboptimal, but the best that I think we can do.
 *
 */
	cvt_ToLatLon (Xlo, Ylo, &lat, &lon);
	prj_AddFloatParam ("lat_1", lat, np, params);
	cvt_ToLatLon (Xhi, Yhi, &lat, &lon);
	prj_AddFloatParam ("lat_2", lat, np, params);
/*
 * Some projections can use a "+south" to do better in the southern
 * hemisphere.  And it doesn't hurt to have it for the rest.
 */
	if (O_Lat < 0)
		prj_AddParam ("south", NULL, np, params);
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
	prj_AddFloatParam ("lat_1", south ? -90.0 : 90.0, np, params);
	prj_AddFloatParam ("lat_2", south ? -90.0 : 90.0, np, params);
	if (south)
		prj_AddParam ("south", NULL, np, params);
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
/*
 * Stereographic requires that we set lat_0.  Let's hope nobody else does.
 */
	prj_AddFloatParam ("lat_0", O_Lat, np, params);

	if (O_Lat < 0)
		prj_AddParam ("south", NULL, np, params);
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
	int	south = (O_Lat < 0);
/*
 * Polar means lat_0 = +/-90
 */
	prj_AddFloatParam ("lat_0", south ? -90.0 : 90.0, np, params);
	if (south)
		prj_AddParam ("south", NULL, np, params);
}


# endif
