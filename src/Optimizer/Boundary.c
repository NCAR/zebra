/*
 * Volume boundary handling
 * $Id: Boundary.c,v 1.1 1991-06-16 17:02:25 burghart Exp $
 */
# include <math.h>
# include <defs.h>
# include <DataStore.h>
# include "globals.h"
# include "prototypes.h"
# include "radar.h"

/*
 * Radius of the earth, in km
 */
# define R_EARTH	6372.


/*
 * private prototypes
 */
# ifdef __STDC__
	static void	bnd_SetLimits (float *, float *, int);
	static void	bnd_CheckAzRng (Radar *, double, double, double, 
				double);
	static void	bnd_NewBoundary (PlatformId, int, time);
	static bool	bnd_GetBoundary (PlatformId);
# else
	static void	bnd_SetLimits ();
	static void	bnd_CheckAzRng ();
	static void	bnd_NewBoundary ();
	static bool	bnd_GetBoundary ();
# endif




void
bnd_InitBoundary ()
/*
 * Get the initial volume boundary and request the data store to give us
 * new boundaries as they show up (if possible).
 */
{
	PlatformId	pid;
	float		*lat, *lon;
	bool		have_boundary = FALSE;
/*
 * Get the initial boundary from the data store if possible
 */
	if (Ds)
	{
	/*
	 * Get the platform ID for "boundary"
	 */
		pid = ds_LookupPlatform ("boundary");
	/*
	 * Get the boundary
	 */
		if (pid != BadPlatform)
		{
			have_boundary = bnd_GetBoundary (pid);
		/*
		 * Request the data store to inform us when a new boundary
		 * shows up
		 */
			ds_RequestNotify (pid, 0, bnd_NewBoundary);
		}
		else
			ui_warning ("No 'boundary' platform in data store");
	}
/*
 * If we didn't get a boundary from the data store, build a phony one
 */
	if (! have_boundary)
	{
		float	lat[4], lon[4];

		ui_printf ("\nVolume boundaries must be typed in!\n");

		lat[0] = Rad[0].lat + 0.05;
		lat[1] = Rad[0].lat + 0.10;
		lat[2] = Rad[0].lat + 0.10;
		lat[3] = Rad[0].lat + 0.05;

		lon[0] = Rad[0].lon + 0.05;
		lon[1] = Rad[0].lon + 0.05;
		lon[2] = Rad[0].lon + 0.10;
		lon[3] = Rad[0].lon + 0.10;

		bnd_SetLimits (lat, lon, 4);
	}
}




static bool
bnd_GetBoundary (pid)
PlatformId	pid;
/*
 * Get the current boundary for the given platform from the data store.
 */
{
	DataObject	*dobj;
	time		t;
	float		*lat, *lon;
	int		npts, i;
/*
 * Get the current time and see if a boundary exists
 */
	tl_GetTime (&t);
	if (! ds_DataTimes (pid, &t, 1, DsBefore, &t))
		return (FALSE);
/*
 * Get the boundary
 */
	dobj = ds_GetData (pid, NULL, 0, &t, &t, OrgOutline, 0, -9999.0);

	if (! dobj)
	{
		ui_warning ("BUG:  Data store LIED!  Couldn't get boundary");
		return (FALSE);
	}

	npts = dobj->do_desc.d_length[0];
/*
 * Build the lat and lon arrays
 */
	lat = (float *) malloc (npts * sizeof (float));
	lon = (float *) malloc (npts * sizeof (float));

	for (i = 0; i < npts; i++)
	{
		lat[i] = dobj->do_aloc[i].l_lat;
		lon[i] = dobj->do_aloc[i].l_lon;
	}
/*
 * Set the scan limits each radar
 */
	bnd_SetLimits (lat, lon, npts);
/*
 * Free allocated space
 */
	free (lat);
	free (lon);
	ds_FreeDataObject (dobj);
/*
 * Success!
 */
	return (TRUE);
}




static void
bnd_NewBoundary (pid, global, t)
PlatformId	pid;
int		global;
time		t;
/*
 * The data store has a new boundary for us, so go get it
 */
{
	ui_printf ("New boundary on its way...\n");
	bnd_GetBoundary (pid);
	ScanOptions ();
}




static void
bnd_SetLimits (lat, lon, npts)
float	*lat, *lon;
int	npts;
/*
 * Build the scan limits for each radar, given the (lat,lon) values in
 * degrees for the points defining the volume boundary
 */
{
	int	r, p;
	float	origin_lat, origin_lon, latr, lonr, az, azprev;
	float	x, y, dis, disprev, rotation, delta;
/*
 * Loop through the radars
 */
	for (r = 0; r < Nradars; r++)
	{
	/*
	 * Make this radar the origin for (lat,lon)<->(x,y) conversion
	 * purposes
	 */
		origin_lat = DEG_TO_RAD (Rad[r].lat);
		origin_lon = DEG_TO_RAD (Rad[r].lon);
	/*
	 * Loop through the boundary points to find the azimuth limits 
	 * of the boundary.  Do the first point again at the end to close 
	 * the volume.
	 */
		rotation = 0.0;

		for (p = 0; p <= npts; p++)
		{
		/*
		 * Get lat and lon for this point in radians
		 */
			if (p == npts)
			{
				latr = DEG_TO_RAD (lat[0]);
				lonr = DEG_TO_RAD (lon[0]);
			}
			else
			{
				latr = DEG_TO_RAD (lat[p]);
				lonr = DEG_TO_RAD (lon[p]);
			}
		/*
		 * Find x and y distances from the radar
		 */
			x = R_EARTH * cos (latr) * sin (lonr - origin_lon);
			y = R_EARTH * sin (latr - origin_lat);
		/*
		 * Get the distance and azimuth in degrees
		 */
			dis = hypot (x, y);

			az = 90 - RAD_TO_DEG (atan2 (y, x));
			if (az < 0.0)
				az += 360.0;
		/*
		 * If this is the first point, use it for min range, max range,
		 * left azimuth, and right azimuth.  Otherwise, test the point
		 * and modify the ranges and azimuths if necessary.
		 */
			if (p == 0)
			{
				Rad[r].az_left = Rad[r].az_right = az;
				Rad[r].rng_front = Rad[r].rng_back = dis;
			}
			else
			{
				bnd_CheckAzRng (Rad + r, dis, az, disprev, 
					azprev);
			/*
			 * Keep track of the net rotation in scanning the 
			 * volume
			 */
				delta = az - azprev;
				delta += (delta < -180.0) ? 360.0 : 0.0;
				delta -= (delta > 180.0) ? 360.0 : 0.0;

				rotation += delta;
			}

			azprev = az;
			disprev = dis;
		}
	/*
	 * Is this radar enclosed by the boundary?  (The total rotation 
	 * will be 360.0 or -360.0 if the boundary is enclosed, zero otherwise,
	 * but we test with 350 to allow for floating point errors)
	 */
		Rad[r].inside = (fabs (rotation) > 350.0);

		if (Rad[r].inside)
		{
			ui_printf ("Radar %s is enclosed by the volume\n", 
				Rad[r].name);
			Rad[r].az_left = Rad[r].az_right = 0.0;
			Rad[r].rng_front = 0.0;
		}
	/*
	 * Use the front and back range and the volume height limits
	 * to determine elevation limits
	 */
		Rad[r].el_bottom = RAD_TO_DEG (atan2(Vol_bot,Rad[r].rng_back));
		Rad[r].el_top = RAD_TO_DEG (atan2 (Vol_top, Rad[r].rng_front));
	}
}




static void
bnd_CheckAzRng (rad, d1, az1, d0, az0)
Radar	*rad;
float	d1, az1, d0, az0;
/*
 * Check az1 against az0 and see if this will change the left or right
 * azimuth limits of the given radar.  Also check the line segment between
 * the two points to see if it will change the min or max range from the 
 * radar to the volume.
 */
{
	float	diff, test, theta, d, theta0, theta1, closest, furthest;
	bool	clockwise;
/*
 * See if it's a clockwise or counterclockwise change from azprev to az
 */
	diff = az1 - az0;
	diff += (diff < -180.0) ? 360.0 : 0.0;
	diff -= (diff > 180.0)  ? 360.0 : 0.0;
	clockwise = diff > 0.0;
/*
 * Check against the appropriate azimuth limit and modify if necessary
 */
	if (clockwise)
	{
		test = az1 - rad->az_right;
		test += (test < -180.0) ? 360.0 : 0.0;
		test -= (test > 180.0) ? 360.0 : 0.0;
		if (test > 0.0)
			rad->az_right = az1;
	}
	else
	{
		test = az1 - rad->az_left;
		test += (test < -180.0) ? 360.0 : 0.0;
		test -= (test > 180.0) ? 360.0 : 0.0;
		if (test < 0.0)
			rad->az_left = az1;
	}
/*
 * Range testing:  find the shortest and furthest distances from the radar
 * to the line segment formed by the two points.
 *
 * Start by finding the angle in radians between the two points
 */
	theta = fabs (DEG_TO_RAD (diff));
/*
 * Distance between the two points
 */
	d = sqrt (d0 * d0 + d1 * d1 - 2 * d0 * d1 * cos (theta));
/*
 * Using the triangle formed by the radar and the two points, test the
 * interior angles at the two points.  If one of the angles is obtuse, then
 * the closest point of the line segment is at the point.  Otherwise, the
 * closest point lies somewhere along the line segment.
 */
	if (hypot (d, d1) < d0)
		closest = d1;
	else if (hypot (d, d0) < d1)
		closest = d0;
	else
		closest = d0 * d1 * sin (theta) / d;
/*
 * The furthest point of this line segment will be one of the points
 */
	furthest = (d1 > d0) ? d1 : d0;
/*
 * Now test against the current max and min ranges for the radar and 
 * change them if necessary
 */
	rad->rng_front = (closest < rad->rng_front) ? closest : rad->rng_front;
	rad->rng_back = (furthest > rad->rng_back) ? furthest : rad->rng_back;
}
