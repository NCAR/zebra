/*
 * "$Id: dslib.h,v 3.15 1999-03-01 02:03:45 burghart Exp $"
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

#ifndef __zebra_dslib_h_
#define __zebra_dslib_h_

/* -------------------------------------------------------------------
 * Prototypes and declarations shared by the CLIENT API modules only but
 * not meant for public distribution are all contained in Appl.h 
 * ------------------------------------------------------------------- */

/* ======================================================================
 * This file contains prototypes and declarations shared privately by the
 * client API and the daemon implementations: Appl.c and d_Appl.c,
 * respectively.  Prototypes shared publicly are in a special section
 * of DataStore.h
 */

int	ds_GetFileStruct (int, DataFile *);
const PlatformClass *ds_GetClassStruct (PlatClassId, PlatformClass *);

#endif /* __zebra_dslib_h_ */
