/*
 * "$Id: dslib.h,v 3.11 1995-02-10 01:22:19 granger Exp $"
 * Internal info for the data store application interface library.
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

#ifndef __zeb_dslib_h_
#define __zeb_dslib_h_

/*
 * The platform lookup table.
 */
extern stbl Pf_Names;


/*
 * This is the format of the data request list, which is generated as
 * part of the process of satisfying each application data grab.
 */
typedef struct _GetList
{
	int	gl_dfindex;		/* Corresponding DF entry	*/
# ifdef DF_USE
	int	gl_dfuse;		/* Use count for this entry	*/
# endif
	ZebTime	gl_begin;		/* Begin time			*/
	ZebTime	gl_end;			/* End time			*/
	int	gl_flags;		/* Flag values			*/
	int	gl_npoint;		/* Number of data points	*/
	int	gl_nsample;		/* Number of samples		*/
	struct _GetList *gl_next;	/* Next in the list		*/
	int	gl_sindex;		/* Sample index for entire rq	*/
} GetList;

/*
 * Flags for the above.
 */
# define GLF_SATISFIED	0x0001		/* This piece is satisfied	*/
# define GLF_REMOTE	0x0002		/* This is a remote data grab	*/
# define GLF_TRIED	0x0004		/* We have tried this one	*/

/*
 * Prototypes required by the application interface but not fit
 * for public visibility.
 */
GetList *dgl_MakeGetList FP ((PlatformId, ZebTime *, ZebTime *));
void	dgl_ReturnList FP ((GetList *));
void 	dgl_ForceClosure FP ((void));

int	ds_GetDetail FP ((char *, dsDetail *, int, SValue *));
void	ds_GetFileStruct FP ((int, DataFile *));
void	ds_GetPlatStruct FP ((PlatformId, ClientPlatform *, int));
int	ds_FindDF FP ((PlatformId, ZebTime *, int));

#endif /* __zeb_dslib_h_ */
