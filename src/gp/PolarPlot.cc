//
// Utilities for plotting polar data.
//
# include <config.h>

# if C_CAP_POLAR

# include <X11/Intrinsic.h>
# include <math.h>

# include <zebra.h>
# include <message.h>
# include <DataStore.h>
extern "C"
{
# include <GraphicsW.h>
}

# include "GraphProc.h"
# include "RasterImage.h"
# include "PolarPlot.h"
# include "PixelCoord.h"
# include "FakeFloat.hh"

static inline void
FSwap (float &d1, float &d2)
/*
 * Swap two floats
 */
{
	float tmp;

	tmp = d1;
	d1 = d2;
	d2 = tmp;
}

//
// Abandon all hope ye who enter here...
//



//
// A little class for tracking line intersections.
//
class Intersection
{
public:
	float x, y;		// Where the intersection happens
	float range;		// Distance from originating point
	int faredge;		// Is this from a far angle?
	int vertical;		// Intersect with vertical edge?

	void Set (int fe, float ix, float iy, float ir, int vert)
	{
		faredge = fe;
		x = ix;
		y = iy;
		range = ir;
		vertical = vert;
	}
};


//
// The bulk of this file is dedicated to the definition of the 
// ScanConverter class, below.  At the bottom we have some C wrapper
// functions.
//


class ScanConverter
{
private:
	DestImage	*sc_Dest;	// Where this stuff is going
	float		sc_xlo, sc_xhi, sc_ylo, sc_yhi;  // km coords
//	SCParams	sc_params;	// Params for current beam
//
// Scan conversion parameters for the next bit.
//
// We put the radar location here, since it may differ from one scan to
// the next, when the scan mode changes.
//
	float sp_Xradar;
	float sp_Yradar;
	int sp_NGates;
//
// How much physical distance is covered by a pixel.
//
	float sp_PixScale;
//
// Default width of a beam.
//
	float sp_AzFill;
//
// Other parameters.
//
	float sp_Project;
	float sp_lastra;	// Rotation angle of prev beam
	int sp_Vertical;	// Are we pointing more-or-less up/downward?
	int sp_CFill, sp_FFill;	// Triangular filling needed?  (near/far)
	int sp_DoTFill;
	int sp_UseTransparency;
	unsigned long sp_TransparentValue;
//
// The various increments.
//
	float sp_inc1, sp_inc2, sp_rowinc, sp_colinc;
//
// Stuff from the old "rastinfo" structure -- endpoints.
//
	int sp_major1, sp_major2;
	float sp_minor1, sp_minor2, sp_gate;
//
// Internal routines for rasterization.
//
	int Setup (PolarBeam *pb);	// Get set up.
	int GetIntersections (int, float, float, Intersection &, 
			Intersection &);
	void GetVIncrements (double, double, double, double, double, double);
	void GetVEndpoints (const Intersection &, const Intersection &, int);
	void GetHIncrements (double, double, double, double, double, double);
	void GetHEndpoints (const Intersection &, const Intersection &, int);
//
// Pixel fillers.
//
	void PFill_1_R (void *);
	void PFill_1_C (void *);
	void PFill_2_R (void *);
	void PFill_2_C (void *);
	void PFill_4_R (void *);
	void PFill_4_C (void *);
	void ScanConverter::TFillR (int, int, float, float, float,
			unsigned int *);
	void ScanConverter::TFillC (int, int, float, float, float,
			unsigned int *);
	int SetPixel (int, int, unsigned int);
//
// Some convenience routines.
//
	float DegToRad (double theta)
	{
		return (theta*M_PI/180.0);
	}

	int CheckIntersection (const Intersection &inter)
	//
	// See if this intersection is real.
	//
	{
		return (inter.x >= 0 && inter.x < sc_Dest->di_w &&
			inter.y >= 0 && inter.y < sc_Dest->di_h &&
			inter.range >= 0);
	}

	void SwapFill ()
	{
		int t = sp_CFill;
		sp_CFill = sp_FFill;
		sp_FFill = t;
	}


public:
	ScanConverter (DestImage *dest, double xlo, double ylo, double xhi, 
		       double yhi, int project,	int tfill, 
		       int transparent, unsigned long transparent_pixel);
	void DoBeam (PolarBeam *pb, void *data, float xr, float yr);
	
	DestImage *getDest () { return sc_Dest; };
};





ScanConverter::ScanConverter (DestImage *dest, double xlo, double ylo,
			      double xhi, double yhi, int project, int tfill,
			      int transparent, unsigned long transparent_pixel)
//
// Get one of these things set up.
//
{
//
// Stash away the stuff they gave us.
//
	sc_Dest = dest;
	sc_xlo = xlo;
	sc_xhi = xhi;
	sc_ylo = ylo;
	sc_yhi = yhi;
	sp_Project = project;
	sp_DoTFill = tfill;
	sp_UseTransparency = transparent;
	sp_TransparentValue = transparent_pixel;
//
// Initialize a couple of other things.
//
	sp_PixScale = sc_Dest->di_w/(xhi - xlo);
	sp_lastra = -9999;
	sp_AzFill = 0.5;	// XXX where should this really be?
}




void
ScanConverter::DoBeam (PolarBeam *pb, void *data, float xr, float yr)
//
// Actually convert a beam of data.
//
{
//
// Stash the radar location, then go off and figure out all of the
// obnoxious scan parameters.
//
	sp_Xradar = xr;
	sp_Yradar = yr;
	if (! Setup (pb))
		return;
//
// If we need to swap data order, make a copy and swap there
//
	int swapping = sc_Dest->di_needswap;
	unsigned int* newdata = 0;
	if (swapping)
	{
	//
	// Allocate a temporary data array, copy and swap the data, then
	// point "data" to the new swapped array.
	//
	    int ngates = pb->pb_NGates;
	    newdata = new unsigned int[ngates];
	    memcpy (newdata, data, ngates * sizeof (unsigned int));

	    for (int i = 0; i < ngates; i++)
		swap4 (newdata + i);
	    
	    data = (void*)newdata;
	}
//
// Do we need to triangle fill on the close end?  If so, be done with it.
//
	if (sp_CFill)
	{
		if (sp_Vertical)
			TFillR (-1, sp_major1, sp_minor1, sp_minor2,
					sp_gate, (unsigned int *) data);
		else
			TFillC (-1, sp_major1, sp_minor1, sp_minor2,
					sp_gate, (unsigned int *) data);
	}
//
// Now do some pixel filling.  There has got to be a way to put a function
// pointer into a C++ Class and still be able to call it as a method, 
// using some twisted syntax, but I don't have time for that.  So here's
// an ugly combination switch/if to get the right pixel fill routine.
//
	switch (sc_Dest->di_bdepth)
	{
	    case 1:
		if (sp_Vertical)
			PFill_1_R (data);
		else
			PFill_1_C (data);
		break;
	    case 2:
		if (sp_Vertical)
			PFill_2_R (data);
		else
			PFill_2_C (data);
		break;
	    case 4:
		if (sp_Vertical)
			PFill_4_R (data);
		else
			PFill_4_C (data);
		break;
	    default:
		msg_ELog (EF_PROBLEM, "Funky depth %d in DoBeam", 
				sc_Dest->di_bdepth);
		return;
	}
//
// If we made a swapped copy of the data, release it now
//
	if (newdata)
	  delete[] newdata;
}






int
ScanConverter::Setup (PolarBeam *pb)
//
// Get set up to convert this beam.
//
{
	float rang, rang1, rang2, adiff;	// rotation angles
	float sin1, sin2, cos1, cos2, sinc, cosc; // trig funcs of edges
	int far = 0;
//
// Deal with (hopefully) one-time PRECIP98 problem: drop data with
// weird elevations.
//
	if (pb->pb_ScanMode == SM_RHI && pb->pb_Elevation >= 89.0)
		return (0);
//
// What is the rotational angle of this beam.  Kludge it to look the
// way we want in the RHI case.
//
	rang = pb->pb_RotAngle;
	if (pb->pb_ScanMode == SM_RHI)
		rang = 90.0 - rang;
	if (rang < 0)
		rang += 360.0;
//
// Come up with the angles defining the influence of this beam.
//
	if ((adiff = fabs (rang - sp_lastra)*0.7) > 1.5 || adiff < 0.1)
		adiff = sp_AzFill;
	if ((rang1 = rang - adiff) < 0)
		rang1 += 360.0;
	if ((rang2 = rang + adiff) >= 360.0)
		rang2 -= 360.0;
	sp_lastra = rang;
	sp_NGates = pb->pb_NGates;
//
// Figure out the orientation of the beam.  Then, if need be, swap the two
// angles to correspond to the direction in which we will be rasterizing.
//
	sp_Vertical = (rang <= 45.0 ||
			(rang > 135.0 && rang < 225.0) ||
			rang > 315.0) ? TRUE : FALSE;
	if ((sp_Vertical && rang > 90.0 && rang < 270.0) ||
			(! sp_Vertical && rang > 180.0))
		FSwap (rang1, rang2);
//
// Calculate the sines/cosines of the two beam angles and keep them around,
// we'll be using them a bit.
//
	sin1 = sin (DegToRad (rang1));
	cos1 = cos (DegToRad (rang1));
	sin2 = sin (DegToRad (rang2));
	cos2 = cos (DegToRad (rang2));	
//
// Find the edge which is furthest right or below the other.
//
	if ((sp_Vertical && rang > 180.0) ||
		       ((rang < 90.0 || rang > 270) && !sp_Vertical))
	{
		sinc = sin2;
		cosc = cos2;
		far++;
	}
	else
	{
		sinc = sin1;
		cosc = cos1;
	}
//
// Gate spacing info, in pixel space.
//
	float pr0 = pb->pb_R0*sp_PixScale;
	float pgs = 1.0/(pb->pb_GateSpacing*sp_PixScale);
	if (sp_Project && pb->pb_ScanMode != SM_RHI)
		pgs /= cos (DegToRad (pb->pb_Elevation));
//
// Find where each edge of the beam intersects with the box of the display
// area.  If there is no such intersection, we can't see this beam and
// give up now.
//
	float ng, npix, npixr;
	Intersection closeints[2], farints[2];

	if (! GetIntersections (0, sin1, cos1, closeints[0], farints[0]) ||
		     ! GetIntersections (1, sin2, cos2, closeints[1], 
				     farints[1]))
		return (0);
//
// Decide which close intersection to use.  Normally take the one farthest
// from the radar, to insure that we get all gates completely within the
// display area.  However, if the beam intersects a corner, we must take
// the intersection which corresponds to the rasterization direction; 
// otherwise we'll get fooled and overrun things.
//
	if (closeints[0].vertical == closeints[1].vertical)
	{
		if (closeints[1].range > closeints[0].range)
			closeints[0] = closeints[1];
	}
	else if ((sp_Vertical && ! closeints[0].vertical) ||
			(!sp_Vertical && closeints[0].vertical))
		closeints[0] = closeints[1];
//
// Similar logic applies for the far intersection, except that we take the
// one closest to the radar.
//
	if (farints[0].vertical == farints[1].vertical)
	{
		if (farints[1].range < farints[0].range)
			farints[0] = farints[1];
	}
	else if ((sp_Vertical && ! farints[0].vertical) ||
			(! sp_Vertical && farints[0].vertical))
		farints[0] = farints[1];
//
// Decide whether triangular filling will be needed to clean up the edges.
// This is something we need to do when the intersection edge and the 
// rasterization have the same direction.
//
	sp_CFill = sp_DoTFill && (closeints[0].vertical == sp_Vertical) &&
		closeints[0].range > 0;
	sp_FFill = sp_DoTFill && (farints[0].vertical == sp_Vertical);
//
// Convert the ranges to the intersections into gates.  If the closest 
// intersection is beyond the range of this beam, we can't see it.
//
	closeints[0].range *= pgs;
	farints[0].range *= pgs;
	if (closeints[0].range >= pb->pb_NGates)
		return (0);
//
// It's also possible that the far intersection is beyond range, meaning 
// the beam ends inside the window.  Correct for this case.
//
	if (farints[0].range >= pb->pb_NGates)
	{
		farints[0].range = pb->pb_NGates - 1;
		farints[0].x = sp_Xradar + farints[0].range*sinc/pgs;
		farints[0].y = sp_Yradar - farints[0].range*cosc/pgs;
		farints[0].faredge = far;
		sp_FFill = FALSE;
	}
//
// Time to calculate our increments and get the endpoints.  Then we be done.
//
	if (sp_Vertical)
	{
		GetVIncrements (pgs, rang, sin1, cos1, sin2, cos2);
		GetVEndpoints (closeints[0], farints[0], far);
	}
	else
	{
		GetHIncrements (pgs, rang, sin1, cos1, sin2, cos2);
		GetHEndpoints (closeints[0], farints[0], far);
	}
	return (1);
}





int
ScanConverter::GetIntersections (int second, float sine, float cosine, 
		Intersection &close, Intersection &far)
//
// Find the two intersections of this beam edge with the display box.
//
{
	Intersection hints[2], vints[2];
//
// Figure the vertical intersections.  If the beam is vertical, however,
// there are no such intersections.
//
	if (ABS (sine) < 0.01)
	{
		vints[0].Set (second, 0, 999999, -999999, TRUE);
		vints[1].Set (second, sc_Dest->di_w - 1, 999999, -999999,
				TRUE);
	}
//
// Actually calculate the intersections.
//
	else
	{
		float slope = cosine/sine;
		vints[0].Set (second, 0, sp_Yradar + sp_Xradar*slope,
				-sp_Xradar/sine, TRUE);
		vints[1].Set (second, sc_Dest->di_w - 1, sp_Yradar +
				(sp_Xradar - sc_Dest->di_w)*slope,
				-(sp_Xradar - sc_Dest->di_w)/sine, TRUE);
	}
//
// Horizontal beams have no intersections with the horizontal edges.
//
	if (ABS (cosine) < 0.01)
	{
		hints[0].Set (second, -999999, 0, -9999999, FALSE);
		hints[1].Set (second, 999999, sc_Dest->di_h, -9999999, FALSE);
	}
//
// Otherwise we calculate the intersections.
//
	else
	{
		float slope = sine/cosine;
		hints[0].Set (second, sp_Yradar*slope + sp_Xradar,
			       0, sp_Yradar/cosine, FALSE);
		hints[1].Set (second,(sp_Yradar - sc_Dest->di_h)*slope +
				sp_Xradar, sc_Dest->di_h - 1,
				(sp_Yradar - sc_Dest->di_h)/cosine, FALSE);
	}
//
// Now find the good ones.
//
	Intersection good[4];
	int nint = 0;
	if (CheckIntersection (hints[0]))
		good[nint++] = hints[0];
	if (CheckIntersection (hints[1]))
		good[nint++] = hints[1];
	if (CheckIntersection (vints[0]))
		good[nint++] = vints[0];
	if (CheckIntersection (vints[1]))
		good[nint++] = vints[1];
//
// Time to see how many we had.
//
	switch (nint)
	{
	//
	// No intersections means that the beam doesn't cross our box
	// at all.  Cool, that means we don't have to rasterize it.
	//
	    case 0:
		return (0);
	//
	// One intersection means we're inside the box.  Set our 'close'
	// intersection at the radar in this case.
	//
	    case 1:
		close.Set (second, sp_Xradar, sp_Yradar,
				0, FALSE);
		far = good[0];
		return (1);
	//
	// And two means we cross through the box.  Sort the two.
	//
	    case 2:
		if (good[0].range < good[1].range)
		{
			close = good[0];
			far = good[1];
		}
		else
		{
			close = good[1];
			far = good[0];
		}
		return (1);
	//
	// And anything else will not happen, at least as long as we only
	// deploy the radar in places where euclidean geometry applies.  But
	// you'll notice I dimensioned good to 4 anyway...
	//
	    default:
		msg_ELog (EF_PROBLEM, "%d intersections! Dropping beam", nint);
		return (0);
	}
}







void
ScanConverter::GetHIncrements (double pgs, double az, double sin1, double cos1,
		double sin2, double cos2)
//
// Calculate increments for horizontal beams.
//
{
//
// The width increments are simply the cotangents of the 
// relevant angles.
//
	sp_inc1 = -cos1/sin1;
	sp_inc2 = -cos2/sin2;
//
// The per-row gate increment.
//
	sp_rowinc = -pgs*cos (DegToRad (az));
	sp_colinc = (cos1 > cos2) ? pgs/sin1 : pgs/sin2;
}






void
ScanConverter::GetVIncrements (double pgs, double az, double sin1, double cos1,
		double sin2, double cos2)
//
// Calculate increments for horizontal beams.
//
{
//
// The width increments are simply the tangents of the 
// relevant angles.
//
	sp_inc1 = -sin1/cos1;
	sp_inc2 = -sin2/cos2;
//
// The per-row gate increment.
//
	sp_rowinc = (sin1 < sin2) ? -pgs/cos1 : -pgs/cos2;
	sp_colinc = pgs*sin (DegToRad (az));
}








void
ScanConverter::GetVEndpoints (const Intersection &closei, 
		const Intersection &fari, int far)
//
// Find the end points for each chunk of data (vertical variant).
//
{
//
// For vertical beams, the major coordinates are in the Y direction.
// At this point, we also invert the coords to be right for the
// real array.
//
//
// Calculate the end points, based on the number of gates
// in this ray.
//
	float y0 = closei.y;
	float y1 = fari.y;
//
// Swap them if need be.
//
	if (y0 > y1)
	{
		FSwap (y0, y1);
		sp_gate = rint (fari.range);
		far = fari.faredge;
		SwapFill ();
	}
	else
	{
		far = closei.faredge;
		sp_gate = rint (closei.range);
	}
//
// Store the info.
//
	sp_major1 = (int) rint (y0);
	sp_major2 = (int) rint (y1);
	sp_minor1 = sp_Xradar -	(sp_Yradar - y0)*sp_inc1;
	sp_minor2 = sp_Xradar -	(sp_Yradar - y0)*sp_inc2;
//
// Every now and then our minor coords end up just barely over the edge.
// In this case, we have to nudge them back.  This shifts the whole edge
// of the beam, which is not cool, but the error is on the order of a
// few hundredths of a pixel.  Even the fussiest scientist is probably
// not going to catch on.
//
	if (sp_minor1 < 0)
		sp_minor1 = 0;
	else if (sp_minor1 >= sc_Dest->di_w)
		sp_minor1 = (float) sc_Dest->di_w - 0.1;
	if (sp_minor2 < 0)
		sp_minor2 = 0;
	else if (sp_minor2 >= sc_Dest->di_w)
		sp_minor2 = (float) sc_Dest->di_w - 0.1;
//
// As a final step, if we have determined our limits with the far edge
// of the beam, we'll need to adjust the beginning gate to account for
// the fact that we begin rasterizing at the near edge.
//
	if (far)
		sp_gate -= sp_colinc*(sp_minor2	- sp_minor1);
}





void
ScanConverter::GetHEndpoints (const Intersection &closei,
		const Intersection &fari, int far)
//
// Find the end points for each chunk of data (horizontal variant).
//
{
//
// Calculate the end points.
//
	float x0 = closei.x;
	float x1 = fari.x;
//
// Swap them if necessary.
//
	if (x0 > x1)
	{
		FSwap (x0, x1);
		SwapFill ();
		sp_gate = rint (fari.range);
		far = fari.faredge;
	}
	else
	{
		sp_gate = rint (closei.range);
		far = closei.faredge;
	}
//
// Store everything.
//
	sp_major1 = (int) rint (x0);
	sp_major2 = (int) rint (x1);
	sp_minor1 = sp_Yradar -	(sp_Xradar - x0)*sp_inc1;
	sp_minor2 = sp_Yradar -	(sp_Xradar - x0)*sp_inc2;
//
// Limit tweaks, see comment in GetVEndpoints
//
	if (sp_minor1 < 0)
		sp_minor1 = 0;
	else if (sp_minor1 >= sc_Dest->di_h)
		sp_minor1 = (float) sc_Dest->di_h - 0.1;
	if (sp_minor2 < 0)
		sp_minor2 = 0;
	else if (sp_minor2 >= sc_Dest->di_h)
		sp_minor2 = (float) sc_Dest->di_h - 0.1;
//
// Adjust to the near edge if need be.
//
	if (far)
		sp_gate -= sp_rowinc*(sp_minor2	- sp_minor1);
}









int
ScanConverter::SetPixel (int x, int y, unsigned int v)
//
// Set an explicit pixel, the slow way.  Avoid this if possible.  Returns
// TRUE iff it was actually possible (i.e. the location was in bounds).
//
{
	if (x < 0 || x >= sc_Dest->di_w || y < 0 || y >= sc_Dest->di_h)
		return (FALSE);
	void *vp = ((char *) sc_Dest->di_image) + sc_Dest->di_ioffset +
		sc_Dest->di_bpl*y + sc_Dest->di_bdepth*x;
	switch (sc_Dest->di_bdepth)
	{
	    case 1:
		* (unsigned char *) vp = v;
		break;

	    case 2:
		* (unsigned short *) vp = v;
	        break;
		
	    case 4:
		* (unsigned int *) vp = v;
	        break;
	}
	return (TRUE);
}


	    


void
ScanConverter::TFillR (int dir, int row, float col1, float col2, float gate,
		unsigned int *vdata)
//
// Perform a triangular fill on a vertical beam.
//
{
	int set = TRUE;
	float gc;

	for (row += dir; set && row >= 0 && row < sc_Dest->di_h; row += dir)
	{
		set = FALSE;
		col1 += sp_inc1*dir;
		col2 += sp_inc2*dir;
		gate += sp_rowinc*dir;
		gc = gate;
		for (int col = (int) col1; col <= (int) col2; col++)
		{
			if (gc >= sp_NGates)
				break;
		/*
		 * Set this pixel unless we're using transparency and
		 * the value is the transparent value.
		 */
			if (!sp_UseTransparency || 
			    (vdata[(int)gc] != sp_TransparentValue))
			    set |= SetPixel (col, row, vdata[(int) gc]);
			gc += sp_colinc;
		}
	}
}





void
ScanConverter::TFillC (int dir, int col, float row1, float row2, float gate,
		unsigned int *vdata)
//
// Perform a triangular fill on a horizontal beam.
//
{
	int set = TRUE;
	float gc;

	for (col += dir; set && col >= 0 && col < sc_Dest->di_w; col += dir)
	{
		set = FALSE;
		row1 += sp_inc1*dir;
		row2 += sp_inc2*dir;
		gate += sp_colinc*dir;
		gc = gate;
		for (int row = (int) row1; row <= (int) row2; row++)
		{
			if (gc >= sp_NGates)
				break;
		/*
		 * Set this pixel unless we're using transparency and
		 * the value is the transparent value.
		 */
			if (!sp_UseTransparency || 
			    (vdata[(int)gc] != sp_TransparentValue))
			    set |= SetPixel (col, row, vdata[(int) gc]);
			gc += sp_rowinc;
		}
	}
}









void
ScanConverter::PFill_1_R (void *vdata)
/*
 * Scan convert a single, vertical beam.
 */
{
	register int row, col;
	FakeFloat gate = sp_gate - 1;
	FakeFloat gate_col;	
	FakeFloat col_inc = sp_colinc;
	FakeFloat row_inc = sp_rowinc;
	FakeFloat left = sp_minor1 /* + 1 */;
	FakeFloat right = sp_minor2;
	FakeFloat left_inc = sp_inc1;
	FakeFloat right_inc = sp_inc2;
	const int xr = sc_Dest->di_bpl, width = sc_Dest->di_w;
	unsigned char *dp = (unsigned char *) sc_Dest->di_image + 
		sc_Dest->di_ioffset + ((int) sp_major1)*xr;
	unsigned int *data = (unsigned int *) vdata;
/*
 * Move down the rows.
 */
	for (row = sp_major1; row < sp_major2; row++)
	{
	/*
	 * Set up raster and gate pointers.
	 */
		gate_col = gate;
	/*
	 * Move through the columns, filling in the pixels.
	 */
		for (col = left.ival (); col <= right.ival (); col++)
		{
		/*
		 * Set this pixel unless we're using transparency and
		 * the value is the transparent value.
		 */
			unsigned int v = data[gate_col.ival()];
			if (!sp_UseTransparency || (v != sp_TransparentValue))
			    dp[col] = v;
			gate_col += col_inc;
		}

	/*
	 * Update the gate and beam width info.
	 */
		gate += row_inc;
		left += left_inc;
		right += right_inc;
		if (left.ival () < 0 || right.ival () >= width)
			break;
		dp += xr;
	}
//
// If we have to clean up at the far end, go for it.
//
	if (sp_FFill)
		TFillR (1, row - 1, left.fval (), right.fval (),
				gate.fval (), (unsigned int *) data);
}





void
ScanConverter::PFill_1_C (void *vdata)
//
// Scan convert a horizontal, single field beam.
//
{
	register int row, col;
	FakeFloat gate = sp_gate - 1;
	FakeFloat gate_row;
	FakeFloat row_inc = sp_rowinc;
	FakeFloat col_inc = sp_colinc;
	FakeFloat top = sp_minor1 /* + 1 */;
	FakeFloat bottom = sp_minor2;
	FakeFloat top_inc = sp_inc1;
	FakeFloat bottom_inc = sp_inc2;
	const int xr = sc_Dest->di_bpl, height = sc_Dest->di_h;
	unsigned char *dest = (unsigned char *) sc_Dest->di_image +
		sc_Dest->di_ioffset;
	unsigned int *data = (unsigned int *) vdata;
/*
 * Step through the columns of this beam.
 */
	for (col = sp_major1; col < sp_major2; col++)
	{
	/*
	 * Initialize raster line and gate pointers.
	 */
		unsigned char *dp = dest + top.ival ()*xr + col;
		gate_row = gate;
	/*
	 * Go down the column, filling in the pixel values.
	 */
		for (row = top.ival (); row <= bottom.ival (); row++)
		{
		/*
		 * Set this pixel unless we're using transparency and
		 * the value is the transparent value.
		 */
			unsigned int v = data[gate_row.ival()];
			if (!sp_UseTransparency || (v != sp_TransparentValue))
			    *dp = v;
			dp += xr;	/* Go to next scan line */
			gate_row += row_inc;
		}
	/*
	 * Update the gate values and the width of the beam.
	 */
		gate += col_inc;
		top += top_inc;
		bottom += bottom_inc;
		if (top.ival () < 0 || bottom.ival () >= height)
			break;
	}
//
// If we have to clean up at the far end, go for it.
//
	if (sp_FFill)
		TFillC (1, col - 1, top.fval (), bottom.fval (),
				gate.fval (), (unsigned int *) data);
}





void
ScanConverter::PFill_2_R (void *vdata)
/*
 * Scan convert a single, vertical beam.  Two-byte version.
 */
{
	register int row, col;
	FakeFloat gate = sp_gate - 1;
	FakeFloat gate_col;	
	FakeFloat col_inc = sp_colinc;
	FakeFloat row_inc = sp_rowinc;
	FakeFloat left = sp_minor1 /*+ 1*/ ;
	FakeFloat right = sp_minor2;
	FakeFloat left_inc = sp_inc1;
	FakeFloat right_inc = sp_inc2;
	const int xr = sc_Dest->di_bpl, width = sc_Dest->di_w;
	unsigned char *dp = (unsigned char *) sc_Dest->di_image + 
		sc_Dest->di_ioffset + ((int) sp_major1)*xr;
	unsigned int *data = (unsigned int *) vdata;
	unsigned short *sdp;
/*
 * Move down the rows.
 */
	for (row = sp_major1; row < sp_major2; row++)
	{
	/*
	 * Set up raster and gate pointers.
	 */
		sdp = (unsigned short *) dp;
		gate_col = gate;
	/*
	 * Move through the columns, filling in the pixels.
	 */
		for (col = left.ival (); col <= right.ival (); col++)
		{
		/*
		 * Set this pixel unless we're using transparency and
		 * the value is the transparent value.
		 */
			unsigned int v = data[gate_col.ival()];
			if (!sp_UseTransparency || (v != sp_TransparentValue))
			    sdp[col] = v;
			gate_col += col_inc;
		}
	/*
	 * Update the gate and beam width info.
	 */
		gate += row_inc;
		left += left_inc;
		right += right_inc;
		if (left.ival () < 0 || right.ival () >= width)
			break;
		dp += xr;
	}
//
// If we have to clean up at the far end, go for it.
//
	if (sp_FFill)
		TFillR (1, row - 1, left.fval (), right.fval (),
				gate.fval (), (unsigned int *) data);
}





void
ScanConverter::PFill_2_C (void *vdata)
//
// Scan convert a horizontal, single field beam.  Two-byte version
//
{
	register int row, col;
	FakeFloat gate = sp_gate - 1;
	FakeFloat gate_row;
	FakeFloat row_inc = sp_rowinc;
	FakeFloat col_inc = sp_colinc;
	FakeFloat top = sp_minor1 /* + 1 */;
	FakeFloat bottom = sp_minor2;
	FakeFloat top_inc = sp_inc1;
	FakeFloat bottom_inc = sp_inc2;
	const int xr = sc_Dest->di_bpl, height = sc_Dest->di_h;
	unsigned char *dest = (unsigned char *) sc_Dest->di_image +
		sc_Dest->di_ioffset;
	unsigned int *data = (unsigned int *) vdata;
/*
 * Step through the columns of this beam.
 */
	for (col = sp_major1; col < sp_major2; col++)
	{
	/*
	 * Initialize raster line and gate pointers.
	 */
		unsigned char *dp = dest + top.ival ()*xr + 2*col;
		gate_row = gate;
	/*
	 * Go down the column, filling in the pixel values.
	 */
		for (row = top.ival (); row <= bottom.ival (); row++)
		{
		/*
		 * Set this pixel unless we're using transparency and
		 * the value is the transparent value.
		 */
			unsigned int v = data[gate_row.ival()];
			if (!sp_UseTransparency || (v != sp_TransparentValue))
			    *(unsigned short*)dp = v;
			dp += xr;	/* Go to next scan line */
			gate_row += row_inc;
		}
	/*
	 * Update the gate values and the width of the beam.
	 */
		gate += col_inc;
		top += top_inc;
		bottom += bottom_inc;
		if (top.ival () < 0 || bottom.ival () >= height)
			break;
	}
//
// If we have to clean up at the far end, go for it.
//
	if (sp_FFill)
		TFillC (1, col - 1, top.fval (), bottom.fval (),
				gate.fval (), (unsigned int *) data);
}



void
ScanConverter::PFill_4_R (void *vdata)
/*
 * Scan convert a single, vertical beam.  Four-byte version.
 */
{
	register int row, col;
	FakeFloat gate = sp_gate - 1;
	FakeFloat gate_col;	
	FakeFloat col_inc = sp_colinc;
	FakeFloat row_inc = sp_rowinc;
	FakeFloat left = sp_minor1 /* + 1 */;
	FakeFloat right = sp_minor2;
	FakeFloat left_inc = sp_inc1;
	FakeFloat right_inc = sp_inc2;
	const int xr = sc_Dest->di_bpl, width = sc_Dest->di_w;
	unsigned char *dp = (unsigned char *) sc_Dest->di_image + 
		sc_Dest->di_ioffset + ((int) sp_major1)*xr;
	unsigned int *data = (unsigned int *) vdata;
	unsigned int *ldp;
/*
 * Move down the rows.
 */
	for (row = sp_major1; row < sp_major2; row++)
	{
	/*
	 * Set up raster and gate pointers.
	 */
		ldp = (unsigned int *) dp;
		gate_col = gate;
	/*
	 * Move through the columns, filling in the pixels.
	 */
		for (col = left.ival (); col <= right.ival (); col++)
		{
		/*
		 * Set this pixel unless we're using transparency and
		 * the value is the transparent value.
		 */
			unsigned int v = data[gate_col.ival()];
			if (!sp_UseTransparency || (v != sp_TransparentValue))
			    ldp[col] = v;
			gate_col += col_inc;
		}
	/*
	 * Update the gate and beam width info.
	 */
		gate += row_inc;
		left += left_inc;
		right += right_inc;
		if (left.ival () < 0 || right.ival () >= width)
			break;
		dp += xr;
	}
//
// If we have to clean up at the far end, go for it.
//
	if (sp_FFill)
		TFillR (1, row - 1, left.fval (), right.fval (),
				gate.fval (), (unsigned int *) data);
}





void
ScanConverter::PFill_4_C (void *vdata)
//
// Scan convert a horizontal, single field beam.  Two-byte version
//
{
	register int row, col;
	FakeFloat gate = sp_gate - 1;
	FakeFloat gate_row;
	FakeFloat row_inc = sp_rowinc;
	FakeFloat col_inc = sp_colinc;
	FakeFloat top = sp_minor1 /*+ 1 */;
	FakeFloat bottom = sp_minor2;
	FakeFloat top_inc = sp_inc1;
	FakeFloat bottom_inc = sp_inc2;
	const int xr = sc_Dest->di_bpl, height = sc_Dest->di_h;
	unsigned char *dest = (unsigned char *) sc_Dest->di_image +
		sc_Dest->di_ioffset;
	unsigned int *data = (unsigned int *) vdata;
/*
 * Step through the columns of this beam.
 */
	for (col = sp_major1; col < sp_major2; col++)
	{
	/*
	 * Initialize raster line and gate pointers.
	 */
		unsigned char *dp = dest + top.ival ()*xr + 4*col;
		gate_row = gate;
	/*
	 * Go down the column, filling in the pixel values.
	 */
		for (row = top.ival (); row <= bottom.ival (); row++)
		{
		/*
		 * Set this pixel unless we're using transparency and
		 * the value is the transparent value.
		 */
			unsigned int v = data[gate_row.ival()];
			if (!sp_UseTransparency || (v != sp_TransparentValue))
			    *(unsigned int*)dp = v;
			dp += xr;	/* Go to next scan line */
			gate_row += row_inc;
		}
	/*
	 * Update the gate values and the width of the beam.
	 */
		gate += col_inc;
		top += top_inc;
		bottom += bottom_inc;
		if (top.ival () < 0 || bottom.ival () >= height)
			break;
	}
//
// If we have to clean up at the far end, go for it.
//
	if (sp_FFill)
		TFillC (1, col - 1, top.fval (), bottom.fval (),
				gate.fval (), (unsigned int *) data);
}






//
// C-access functions from here on down.
//
extern "C" PPCookie
pol_DisplaySetup (int project, int tfill, int transparent,
		  unsigned long transparent_pixel)
//
// Set up to plot a sweep into display memory.
//
{
	ScanConverter *sc;
	DestImage *img;
//
// Set up a nice display area, then create a scanconverter to work
// with it.
//
	if (transparent)
	    img = ri_GetTransparentImage (DrawFrame, XPIX (Xlo), YPIX (Yhi),
					  XPIX (Xhi) - XPIX (Xlo), 
					  YPIX (Ylo) - YPIX (Yhi));
	else
	    img = ri_GetDestImage (DrawFrame, XPIX (Xlo), YPIX (Yhi),
				   XPIX (Xhi) - XPIX (Xlo), 
				   YPIX (Ylo) - YPIX (Yhi));

	sc = new ScanConverter (img, Xlo, Ylo, Xhi, Yhi, project, tfill,
				transparent, transparent_pixel);
	return ((PPCookie) sc);
}





extern "C" PPCookie
pol_GridSetup (int project, DestImage *di, double xlo, double ylo, double xhi,
		double yhi)
//
// Set up to rasterize into a memory grid.
//
{
	ScanConverter *sc = new ScanConverter(di, xlo, ylo, xhi, yhi, project,
			FALSE, FALSE, 0);
	return ((PPCookie) sc);
}





extern "C" void
pol_PlotBeam (PPCookie pc, PolarBeam *pb, void *data, float xr, float yr)
//
// Plot this beam.  Data comes in as a separate pointer so that it
// can be optionally munged by the caller before plotting.
//
{
	ScanConverter *sc = (ScanConverter *) pc;
	DestImage *img = sc->getDest ();
	sc->DoBeam (pb, data, xr - img->di_x, yr - img->di_y);
}



extern "C" void
pol_Finished (PPCookie pc)
//
// Clean up.
//
{
	ScanConverter *sc = (ScanConverter *) pc;
	ri_ShipImage (sc->getDest ());
	delete sc;
}



# endif // C_CAP_POLAR
