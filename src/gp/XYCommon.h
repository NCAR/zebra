/*
 * Include this in X-Y plot-routines
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
/*
 * General definitions
 *
 * MAX_PLAT sets a limit on the number of platforms we'll try to plot, 
 * NOT on the capacity for reading parameter platform lists (for which
 * PlatformListLen is the limit).
 */
# define MAX_PLAT	10
# define BADVAL		-999.0

/*
 * Data vector structure for use when calling xy_GetDataVectors()
 */
typedef struct _xydatavector
{
	char		*fname;		/* field name */
	DataValPtr	data;		/* data vector */
	short		npts;		/* length of data vector */
	DataValRec	min, max;	/* min and max from vector */
} xyDataVector;

/*
 * Observation info structure (each is associated with a data vector
 * structure)
 */
# define MAX_DV_OBS	50
typedef struct _xyobsinfo
{
	short	nobs;			/* number of observations contained */
	short	*obsndx;		/* array index to each observation */
	/* static space for almost all cases */
        short   obsndx_static[MAX_DV_OBS];
	int     nalloc;
} xyObsInfo;


/*
 * Prototypes
 */
void	xy_GetScaleModes FP ((char*, zbool*, zbool*, zbool*, zbool*));
void	xy_GetZModes FP ((char *, zbool *, zbool *));
void	xy_SetPrivateScale FP ((char*, DataValPtr, DataValPtr, DataValPtr,
				DataValPtr));
void	xy_GetPrivateScale FP ((char*, DataValPtr, DataValPtr, DataValPtr,
				DataValPtr));
int	xy_ManualScale FP ((char*, int, char*, DataValPtr, DataValPtr));
void	xy_SaveDataTimes  FP ((char*, ZebTime*, ZebTime*));
void	xy_GetTimes FP ((char*, ZebTime*, ZebTime*, ZebTime*, ZebTime*, int*));
void	xy_GetPlotColors FP ((char*, int, Pixel*));
void	xy_GetDataMinMax FP ((int, DataValPtr, DataValPtr, DataValPtr, int));
int	xy_AvailableData FP ((PlatformId, ZebTime*, ZebTime*, ZebTime*, 
			      ZebTime*, ZebTime*));
int	xy_DetermineBounds FP ((char *, int, DataValPtr, DataValPtr, int, 
				char *, int));
int	xy_GetDataVectors FP ((PlatformId, ZebTime*, ZebTime*, int, int, 
			       xyDataVector*, int, xyObsInfo*, char *comp));
xyObsInfo *xy_ObsAlloc (int n);
void xy_ObsFree (xyObsInfo *obsinfo, int n);


