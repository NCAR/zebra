/* $Id: RasterFile.h,v 2.4 1997-06-19 20:19:31 granger Exp $ */

/*
 * This is the file for FCC native raster files.  Note that these are not
 * (necessarily) *image* files -- they are simply a form of gridded data.
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
 * Fields are described by the following:
 */
typedef struct s_RFField
{
	char 	rff_Name[20];		/* Name of the field		*/
	char	rff_Desc[40];		/* It's description		*/
	ScaleInfo rff_Scale;		/* Scaling info			*/
} RFField;


/*
 * The file header.
 */
# define RF_MAGIC	0x970331
# define RF_OLDMAGIC	0x910714
# define RF_ANCIENTMAGIC	0x910702	/* no longer supported */
# define MaxRFField	32		/* Max number of fields	*/
# define OldMaxRFField	10		/* Max number of fields	*/

typedef struct s_RFHeader
{
	int	rf_Magic;		/* == RF_MAGIC			*/
	char	rf_Platform[40];	/* Name of platform stored here */
	int	rf_MaxSample;		/* Max number of samples	*/
	int	rf_NSample;		/* How many we have		*/
	int	rf_NField;		/* How many fields		*/
	RFField rf_Fields[MaxRFField];	/* Fields			*/
	int	rf_Flags;		/* Flag info			*/
} RFHeader;

# define RFF_COMPRESS	0x0001		/* Images are compressed	*/

/*
 * The previous version of the header, differing only in the number of
 * fields handled.
 */
typedef struct s_OldRFHeader
{
	int	rf_Magic;		/* == RF_MAGIC			*/
	char	rf_Platform[40];	/* Name of platform stored here */
	int	rf_MaxSample;		/* Max number of samples	*/
	int	rf_NSample;		/* How many we have		*/
	int	rf_NField;		/* How many fields		*/
	RFField rf_Fields[OldMaxRFField]; /* Fields			*/
	int	rf_Flags;		/* Flag info			*/
} OldRFHeader;

/*
 * Immediately after the header comes the table of contents, which is
 * dimensioned by the MaxSample header field.
 */
typedef struct s_RFToc
{
	UItime	rft_Time;		/* Time of this image		*/
	long	rft_Offset[MaxRFField];	/* It's place in the file	*/
	long	rft_Size[MaxRFField];	/* Length			*/
	RGrid	rft_Rg;			/* Geometry info		*/
	Location rft_Origin;		/* Image origin			*/
	int	rft_AttrLen;		/* Length of attrs		*/
	long	rft_AttrOffset;		/* Where they are		*/
} RFToc;



typedef struct s_OldRFToc
{
	UItime	rft_Time;		/* Time of this image		*/
	long	rft_Offset[OldMaxRFField];/* It's place in the file	*/
	long	rft_Size[OldMaxRFField];/* Length			*/
	RGrid	rft_Rg;			/* Geometry info		*/
	Location rft_Origin;		/* Image origin			*/
	int	rft_AttrLen;		/* Length of attrs		*/
	long	rft_AttrOffset;		/* Where they are		*/
} OldRFToc;


/*
 * Prototypes for the low-level RasterFile.c interface.
 */
int drf_ReadHeader (int fd, RFHeader *hdr);
RFToc *drf_ReadTOC (RFHeader *, int);
void drf_SwapHeader (RFHeader *hdr);
void drf_SwapTOC (RFToc *toc, int ntoc);


# ifdef notdef /* ancient */
typedef struct s_OldRFToc
{
	UItime	rft_Time;		/* Time of this image		*/
	long	rft_Offset[MaxRFField];	/* It's place in the file	*/
	long	rft_Size[MaxRFField];	/* Length			*/
	RGrid	rft_Rg;			/* Geometry info		*/
	Location rft_Origin;		/* Image origin			*/
} OldRFToc;

# endif
