/*
 * Global stuff for the radar ingest module.
 */
/*		Copyright (C) 1987,88,89,90,91 by UCAR
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

/* $Id: radar_ingest.h,v 2.4 1993-08-18 15:34:49 burghart Exp $ */


/*
 * Beams of data are passed around as this:
 */
typedef struct _GDesc
{
	int	gd_first;		/* First gate		*/
	int	gd_ngate;		/* How many gates	*/
	unsigned char	*gd_data;		/* Where the data is	*/
} GDesc;

# define MAXGD 10
typedef struct _Beam
{
	Housekeeping *b_hk;		/* Housekeeping		*/
	int	b_npart;		/* How many data chunks	*/
	GDesc	b_gdesc[MAXGD];		/* The actual chunks	*/
} Beamst, *Beam;

/*
 * Where the radar is, in pixel and real space.
 */
extern int XRadar, YRadar;
extern float RadarLat, RadarLon;

/*
 * Resolution issues.
 */
extern int XRes, YRes;
extern float AzFill;

/*
 * The sortest sweep that interests us.
 */
extern int MinSweep;

/*
 * The offset in hours to GMT from the time recorded by the radars.
 */
extern int GMTOffset;

extern int NBeam, NMissed;
/*
 * Field and image info.
 */
typedef struct _RDest
{
	int	rd_foffset;		/* Field offset		*/
	unsigned char *rd_image;	/* Image destination	*/
} RDest;

/*
 * Do we trust the scan and volume flags?
 */
extern bool TrustSweep, TrustVol;

/*
 * The scale factor, in pixels per kilometer.
 */
extern float PixScale;

/*
 * The (temporary) color map.
 */
unsigned char CMap[256];

/*
 * Thresholding information.
 */
extern bool DoThresholding;		/* Thresholding is enabled	*/
extern int ThrFldOffset;		/* Offset to threshold field	*/
unsigned char ThrCounts;		/* Threshold count value	*/

/*
 * Mhr mode -- drop .5's.
 */
extern bool MhrMode;
extern float MhrTop;
/*
 * Do we do elevation projection?
 */
extern bool Project;

/*
 * Functions.
 */
# ifdef __STDC__
	void 	SetupInput (void);
	Beam 	GetBeam (void);
	void	Rasterize (Beam, RDest *, int, int);
	void	OutputSweep (UItime *, double, int, int, int, int, int, int);
# else
# 	define const
	void 	SetupInput ();
	Beam 	GetBeam ();
	void	Rasterize ();
	void	OutputSweep ();
# endif

/*
 * Command constants go here.
 */
# define RIC_GO		1
# define RIC_SOURCE	2
# define RIC_FILE	3
# define RIC_INTERFACE	4
# define RIC_FIELD	5
# define RIC_THRESHOLD	6
# define RIC_CONSUMER	7
# define RIC_MHRSTATE	8
