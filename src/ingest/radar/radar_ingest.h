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

/* $Id: radar_ingest.h,v 2.12 1999-03-11 17:39:01 burghart Exp $ */


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
 * The shortest sweep that interests us.  For RHIs, we'll accept anything
 * bigger than *either* MinSweep or MinRHI.
 */
extern int MinSweep;
extern int MinRHI;

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
extern zbool TrustSweep, TrustVol;

/*
 * The scale factor, in pixels per kilometer.
 */
extern float PixScale;

/*
 * The (temporary) color map.
 */
unsigned char CMap[256];

/*
 * The difference we tolerate between fixed angle and actual elevation
 */
extern float ElTolerance;

/*
 * Thresholding information.
 */
extern zbool DoThresholding;		/* Thresholding is enabled	*/
extern int ThrFldOffset;		/* Offset to threshold field	*/
extern unsigned char ThrCounts;		/* Threshold count value	*/
extern int SMinusXThresh;

/*
 * Mhr mode -- drop .5's.
 */
/* extern zbool MhrMode; */
extern float MhrTop;
/*
 * Do we do elevation projection?
 */
extern zbool Project;

/*
 * Doing beam buffering?
 */
extern int Using_BB;

/*
 * Data formats with which we can deal in this driver (see ADRad and UF
 * for others)
 *
 * Numbers on the enums match those in the .state file, of course.
 */
typedef enum
{
	RF_CBAND = 0,	/* Latter day C-Band */
	RF_CP2 = 1,	/* Ancient, musty, disgusting CP2 */
	RF_MHR = 2,	/* Mile High RIP */
	RF_ADRAD = 3,	/* Aggie Doppler	*/
	RF_UF = 4	/* Universal format 	*/
} RadarFormat;
extern RadarFormat RFormat;

/*
 * Field information.
 */
# define MFIELD 8
extern RDest Rd[MFIELD];
extern int NField;
extern char *Fields[MFIELD];
extern ScaleInfo Scale[MFIELD];


/*
 * Functions.
 */
# ifdef __STDC__
	void	InputInitialize (void);
	void 	SetupInput (void);
	Beam 	GetBeam (void);
	void	Rasterize (Beam, RDest *, int, int);
	void	RasterizeDone (void);
	void	OutputSweep (UItime *, double, int, int, int, int, int, int);
# else
# 	define const
	void	InputInitialize ();
	void 	SetupInput ();
	Beam 	GetBeam ();
	void	Rasterize ();
	void	RasterizeDone ();
	void	OutputSweep ();
# endif


void HandleCP2Mess FP ((Beam, Housekeeping *, ScaleInfo *));
void CP2_CheckParams FP ((Beam, Housekeeping *, ScaleInfo *));
void CP2_DoDerivation FP ((Housekeeping *, Beam, int, unsigned char *));
void CP2_LoadCal FP ((int));

void OpenEthernet FP ((char *));
Beam GetEtherBeam FP ((Beam));

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
# define RIC_FORMAT	9
# define RIC_CALIBRATION 10
# define RIC_PAIR	11
# define RIC_ENDCAL	12
# define RIC_BEAMBUF	13
# define RIC_DUMPRHI	14
# define RIC_TAPE	15
