/*
 * The structures defining a boundary file.
 */
/* $Id: BoundaryFile.h,v 2.1 1991-09-26 22:37:48 gracio Exp $ */

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

# define BH_MAGIC	0x910221
# define MAXBFPLAT	60
/*
 * The boundary file header looks like this:
 */
struct BFHeader
{
	int	bh_Magic;		/* Magic ID == BH_MAGIC		*/
	char	bh_Platform[MAXBFPLAT];	/* The platform stored here	*/
	int	bh_MaxBoundary;		/* Max samples/platform		*/
	time	bh_Begin;		/* Earliest time in file	*/
	time	bh_End;			/* Latest time in file		*/
	int	bh_NBoundary;		/* How many boundaries now?	*/
};

/*
 * Immediately following the header is the boundary table, describing
 * each individual boundary in the file.
 */
struct BFBTable
{
	int	bt_NPoint;		/* How many points 		*/
	time	bt_Time;		/* When is this boundary?	*/
	int	bt_Offset;		/* Where first point is in file */
};

/*
 * Points themselves are just a series of Location structures, located
 * at bt_Offset within the file.
 */
