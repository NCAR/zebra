/*
 * Header file for private variables shared between the interface modules
 * Appl.c and DFA_Appl.c
 */

/* $Id: Appl.h,v 3.1 1995-07-06 10:21:19 granger Exp $ */

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

# ifndef _zeb_appl_h_
# define _zeb_appl_h_

/*
 * How far in the future we are willing to accept data.
 */
extern int MaxFuture;

/*
 * Shared functions, but not for public consumption.
 */
void	ds_SendToDaemon FP ((void *, int));
int	ds_FindAfter FP ((PlatformId, ZebTime *));
void	ds_ZapCache FP ((DataFile *));
void	ds_WriteLock FP ((PlatformId));
void	ds_FreeWLock FP ((PlatformId));
void	ds_FreeCache FP((void));


# endif /* ! _zeb_appl_h_ */

