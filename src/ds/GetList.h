/*
 * Declaration of the GetList interface from GetList.c
 */

/* $Id: GetList.h,v 3.2 1999-03-01 02:03:34 burghart Exp $ */

/*		Copyright (C) 1987-1995 by UCAR
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

# ifndef _zebra_getlist_h_
# define _zebra_getlist_h_

# if __cplusplus
extern "C" {
# endif
    

/*
 * This is the format of the data request list, which is generated as
 * part of the process of satisfying each application data grab.
 */
typedef struct _GetList
{
	DataFile gl_df;			/* Corresponding DataFile	*/
	ZebTime	gl_begin;		/* Begin time			*/
	ZebTime	gl_end;			/* End time			*/
	int	gl_flags;		/* Flag values			*/
	struct _GetList *gl_next;	/* Next in the list		*/
} GetList;

/*
 * Flags for the above.
 */
# define GLF_SATISFIED	0x0001		/* This piece is satisfied	*/
# define GLF_REMOTE	0x0002		/* This is a remote data grab	*/
# define GLF_TRIED	0x0004		/* We have tried this one	*/
# define GLF_END_INCL	0x0008		/* End time is inclusive	*/
# define GLF_BEG_INCL	0x0010		/* End time is inclusive	*/

/*
 * The GetList interface prototypes
 */
GetList *dgl_MakeGetList FP ((PlatformId, ZebTime *, ZebTime *));
void	dgl_ReturnList FP ((GetList *));
void 	dgl_ForceClosure FP ((void));

# if __cplusplus
}	// close extern "C"
# endif

# endif	/* _zebra_getlist_h_ */

