/*
 * Daemon Source-related functions.  These are mostly C bindings to the
 * methods of class Source.
 * $Id: d_Source.h,v 3.3 2001-10-16 22:26:30 granger Exp $
 *
 *
 *		Copyright (C) 1998 by UCAR
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

# ifndef _zebra_d_Source_h_
# define _zebra_d_Source_h_
/* 
 * Leave these out here, since they're already C++-able
 */
# include "DataFiles.h"
# include "DataStore.h"
# include "Platforms.h"

# if __cplusplus

class Source;

extern "C"
{

# else /* !__cplusplus */

typedef void Source;

# endif


/*
 * Handle source/platform -> directory mapping
 */

/*
 * These are basically C bindings to Source class methods
 */
Source *src_Open (const char *srcname, const char *rootdir, const char *fname);
void src_Close (Source *src);

zbool src_First (Source *src, const Platform *p, DataFileCore *dfc);
zbool src_Next (Source *src, const Platform *p, DataFileCore *dfc);
zbool src_Prev (Source *src, const Platform *p, DataFileCore *dfc);

zbool src_LastTime (Source *src, const Platform *p, ZebraTime *last);

zbool src_FindBefore (Source *src, const Platform *p, const ZebraTime *t, 
		     DataFileCore *dfc);
zbool src_FindAfter (Source *src, const Platform *p, const ZebraTime *t, 
		     DataFileCore *dfc);
zbool src_FindExact (Source *src, const Platform *p, const ZebraTime *t,
		    DataFileCore *dfc);

void src_UpdateFile (Source *src, const Platform *p, const DataFileCore *dfc);
void src_RemoveFile (Source *src, const Platform *p, const DataFileCore *dfc);

void src_SetPlatDir (Source *src, const char *platname, const char *dir);

int src_NFiles (Source *src, const Platform *p);

const char *src_Name (const Source *src);
const char *src_RootDir (const Source *src);
const char* src_DataDir (Source *src, const Platform *p);
int src_ConfirmDataDir (Source *src, const Platform *p);

/*
 * Handle some runtime flags that are not kept in the persistent representation
 * of the file list.
 */

void src_SetReadOnly (Source *src, zbool readonly);
zbool src_IsReadOnly (const Source *src);

void src_SetDirConst (Source *src, zbool dconst);
zbool src_IsDirConst (const Source *src);

void src_SetFileConst (Source *src, zbool fconst);
zbool src_IsFileConst (const Source *src);

void src_SetRememberAll (Source *src, zbool rall);
zbool src_RemembersAll (const Source *src);

void src_SetForceDirs (Source *src, zbool force);
zbool src_DirsAreForced (const Source *src);

void src_Dump (Source *src);

# if __cplusplus
} // end of extern "C"
# endif

# endif /* ifndef _zebra_d_Source_h_ */
