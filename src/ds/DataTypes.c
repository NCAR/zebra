/*
 * Convenient public access to names of enumerated types
 */
/*		Copyright (C) 1998 by UCAR
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

#include <defs.h>
#include "DataStore.h"

RCSID ("$Id: DataTypes.c,v 3.1 1999-03-01 16:32:07 burghart Exp $")

static const char *_OrgName[] =
{
	"unknown",	/* OrgUnknown	= 0 */
	"2dgrid",	/* Org2dGrid	= 1 */
	"irgrid",	/* OrgIRGrid	= 2 */
	"scalar", 	/* OrgScalar	= 3 */
	"image",	/* OrgImage	= 4 */
	"outline", 	/* OrgOutline	= 5 */
	"3dgrid",	/* Org3dGrid	= 6 */
	"cmpimage",	/* OrgCmpImage	= 7 */
        "1dgrid",	/* Org1dGrid       = 8 */
	"transparent",	/* OrgTransparent  = 9 */
	"fixedscalar",	/* OrgFixedScalar  = 10 */
	"nspace" 	/* OrgNSpace	= 11 */
};
#define OrgName(org) (_OrgName[(org)])
#define NOrg (sizeof(_OrgName)/sizeof(char *))

const char *
ds_OrgName (org)
DataOrganization org;
{
	return ((org >= 0 && org < NOrg) ? OrgName(org) : OrgName(OrgUnknown));
}

static const char *_FTypeName[] =
{
	"unknown",	/* FTUnknown = -1 */
	"netcdf",	/* FTNetCDF = 0 */
	"boundary",	/* FTBoundary = 1 */
	"raster",	/* FTRaster = 2 */
	"compressed_raster", /* FTCmpRaster = 3 */
	"zebra",	/* FTZebra = 4 */
	"grib",		/* FTGRIB = 5 */
	"grib_sfc", 	/* FTGRIBSfc = 6 */
	"grads",	/* FTGrads = 7 */
	"grads_model",  /* FTGradsModel = 8 */
	"hdf"		/* FTHDF = 9 */
};
#define FTypeName(ftype) (_FTypeName[(ftype)+1])
#define NFType ((sizeof(_FTypeName)/sizeof(char *)))

const char *
ds_FTypeName (ft)
FileType ft;
{
	return ((ft >= -1 && ft < NFType) ? FTypeName(ft) : 
		FTypeName(FTUnknown));
}

static const char *_InheritDirFlagName[] =
{
	"none",		/* InheritNone = 0 */
	"append",	/* InheritAppend */
	"copy"		/* InheritCopy */
};
#define InheritDirFlagName(id) (_InheritDirFlagName[(id)])
#define NInheritDirFlags ((sizeof(_InheritDirFlagName)/sizeof(char *)))

const char *
ds_InheritDirFlagName (id)
InheritDirFlag id;
{
	return ((id >= 0 && id < NInheritDirFlags) ? 
		InheritDirFlagName(id) : "unknown");
}


static const char *_InstanceDirFlagName[] =
{
	"root", 	/* InstanceDefault = 0, InstanceRoot = 0 */
	"copyclass",	/* InstanceCopyClass */
	"subdirclass",	/* InstanceSubdirClass */
	"copyparent",	/* InstanceCopyParent */
	"subdirparent"	/* InstanceSubdirParent */
};
#define InstanceDirFlagName(id) (_InstanceDirFlagName[(id)])
#define NInstanceDirFlags ((sizeof(_InstanceDirFlagName)/sizeof(char *)))

const char *
ds_InstanceDirFlagName (id)
InstanceDirFlag id;
{
	return ((id >= 0 && id < NInstanceDirFlags) ? 
		InstanceDirFlagName(id) : "unknown");
}
/* -------------------------------------------------------------------- */


