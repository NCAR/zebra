/*
 * Structures describing the zeb native file format.
 */
/* $Id: znfile.h,v 1.4 1994-10-11 16:25:01 corbet Exp $ */
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
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
 * This is the file header.
 */
typedef struct _zn_Header
{
	int	znh_Magic;		/* Magic ID number		*/
	/* Space allocation info */
	int	znh_Free;		/* First free list entry	*/
	int	znh_NFree;		/* Number of free chunks	*/
	int	znh_NFreeB;		/* Number of internal free bytes */
	int	znh_Len;		/* Length of this file		*/
	/* General contents info */
	int	znh_NSample;		/* Number of data samples 	*/
	int	znh_NSampAlloc;		/* How much sample space allocd */
	int	znh_NField;
	DataOrganization znh_Org;	/* Organization of this file	*/
	Location znh_Loc;		/* Static location		*/
	/* Offsets to header info */
	int	znh_OffTime;		/* Offset to time array		*/
	int	znh_OffSample;		/* Offset to sample info array	*/
	int	znh_OffField;		/* Offset to field array	*/
	int	znh_OffRg;		/* Offset to rgrid array	*/
	int	znh_OffLoc;		/* Offset to location array	*/
	/* Station information for IRGRID files */

	int	znh_NStation;		/* Number of stations		*/
	int	znh_OffStation;		/* Where the array is		*/
	/* Global attributes			*/
	int	znh_GlAttrLen;		/* How big they are		*/
	int	znh_OffGlAttr;		/* Where they are		*/
	int	znh_OffAttr;		/* Per-sample attribute array	*/
/* 
 * Version 2: znh_Version and znh_altUnits added
 */
	int	znh_Version;		/* Zeb Native version number	*/
	int	znh_AltUnits;	/* Altitude units		*/
} zn_Header;

# define ZN_OLDMAGIC	0x920611	/* Version 1 */
# define ZN_MAGIC	0x940218	/* Version 2 and beyond */
# define ZN_GRAIN	16		/* How much to expand per-sample
					   tables at a time */
/*
 * Version stuff
 */
# define ZN_VERSION	2

# define ZN_V1_HDRLEN	(sizeof(zn_Header) - 8) /* Length of V1 header */

/*
 * Free chunks have this sort of appearance at their head.
 */
typedef struct _zn_Free
{
	int	znf_FMagic;		/* Free chunk magic		*/
	int	znf_Size;		/* Size of this chunk		*/
	int	znf_Next;		/* Next free chunk		*/
} zn_Free;

# define ZN_FREE_MAGIC	0x920602

/*
 * The sample offset array
 */
typedef struct _zn_Sample
{
	int	znf_Size;		/* How big this one is		*/
	int	znf_Offset;		/* Where to find it		*/
} zn_Sample;

/*
 * Per-field information.
 */
typedef enum
{
	DF_Float,		/* 4-byte floating point		*/
	DF_Byte			/* 1-byte scaled			*/
} DataFormat;

# define ZN_FLD_LEN 40
/*
 * Version 1 field structure
 */
typedef struct _zn_FieldV1
{
	char	zf_Name[ZN_FLD_LEN];	/* The name of this field	*/
	float	zf_Badval;		/* Bad value flag		*/
	DataFormat zf_Format;		/* Storage format		*/
	ScaleInfo zf_Scale;		/* Scaling (for scaled fmts)	*/
} zn_FieldV1;

/*
 * Version 2 field structure (version 1 structure with appended per-field 
 * attribute info)
 */
typedef struct _zn_Field
{
	char	zf_Name[ZN_FLD_LEN];	/* The name of this field	*/
	float	zf_Badval;		/* Bad value flag		*/
	DataFormat zf_Format;		/* Storage format		*/
	ScaleInfo zf_Scale;		/* Scaling (for scaled fmts)	*/
	int	zf_AttrLen;		/* How big they are		*/
	int	zf_OffAttr;		/* Where they are		*/
} zn_Field;

/*
 * Stations in irregular grid files are stored this way.
 */
# define ZN_STA_LEN 32
typedef struct _zn_Station
{
	char	zns_Name[ZN_STA_LEN];	/* Name of this station		*/
	Location zns_Loc;		/* Where it is			*/
} zn_Station;

/*
 * Altitude units we understand.
 */
# define ZAU_BAD	-1	/* unknown units */
# define ZAU_KMMSL	0	/* km MSL */
# define ZAU_MMSL	1	/* m MSL */
# define ZAU_MB		2	/* mb */



