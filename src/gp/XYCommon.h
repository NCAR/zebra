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
 */
# define MAX_PLAT	10
# define BADVAL		-999.0

/* Scale control routines */
extern void xy_GetScaleInfo FP((plot_description,char*,int,short*));
extern void xy_SetScaleBounds FP((plot_description,char*,int,int,DataValPtr,
		DataValPtr));
extern void xy_GetCurrentScaleBounds FP((plot_description,char*,int,int,
		DataValPtr,DataValPtr,char*));

/* Axis Control routines */
extern void xy_AdjustAxes FP((plot_description,char*, int,int,int,int));

/* Data Control routines */
extern void xy_SetPrivateDD  FP(( plot_description, char*, ZebTime*,ZebTime*,int*));
extern void xy_GetDataDescriptors FP(( plot_description,char*,int,ZebTime*,ZebTime*,
		ZebTime*,ZebTime*,int*,int*));
extern void xy_GetPlotColors FP((plot_description,char*,int,char*[],char*));
extern void xy_GetDataMinMax FP((int, DataValPtr, DataValPtr, DataValPtr, int));
extern int xy_AvailableData FP((PlatformId, ZebTime*,ZebTime*,ZebTime*, ZebTime*,ZebTime*));
