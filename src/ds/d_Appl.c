/*
 * Daemon counterparts to client routines Appl.c for returning platform
 * info.  Used by dfa methods when linked with the daemon.
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

# include <unistd.h>
# include <stdio.h>
# include <string.h>

# include <ui.h>
# include "defs.h"
# include "message.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "Platforms.h"
# include "dsDaemon.h"

RCSID("$Id: d_Appl.c,v 3.1 1996-11-19 09:13:50 granger Exp $")




PlatformId
ds_LookupPlatform (name)
const char *name;
{
	PlatformInstance *p;

	p = dt_FindPlatform (name);
	return (p ? (p - PTable) : BadPlatform);
}




PlatClassId
ds_LookupClass (name)
const char *name;
{
	PlatformClass *pc;

	pc = dt_FindClass (name);
	return (pc ? (pc - CTable) : BadClass);
}



PlatClassId
ds_PlatformClass (pid)
PlatformId pid;
/*
 * Return the class id of this platform
 */
{
	if (dt_CheckId (pid))
		return (PTable[pid].dp_class);
	else
		return (BadClass);
}



const char *
ds_ClassName (id)
PlatClassId id;
{
	if (dt_CheckClassId (id))
		return (CTable[id].dpc_name);
	else
		return ("(BadClassId)");
}



char *
ds_PlatformName (pid)
PlatformId pid;
{
	if (dt_CheckId (pid))
		return (pi_Name (PTable+pid));
	else
		return ("(BadPlatformId)");
}



DataOrganization
ds_PlatformDataOrg (id)
PlatformId id;
{
	PlatformInstance *p = NULL;

	if (dt_CheckId (id))
		p = PTable + id;
	return (p ? (pi_DataOrg(p)) : OrgUnknown);
}



int
ds_IsMobile (id)
PlatformId id;
{
	PlatformInstance *p = NULL;

	if (dt_CheckId (id))
		p = PTable + id;
	return (p ? (pi_Mobile(p)) : FALSE);
}



int
ds_IsModelPlatform (id)
PlatformId id;
{
	PlatformInstance *p = NULL;

	if (dt_CheckId (id))
		p = PTable + id;
	return (p ? (pi_Model(p)) : FALSE);
}



int
ds_MaxSamples (id)
PlatformId id;
{
	PlatformInstance *p = NULL;

	if (dt_CheckId (id))
		p = PTable + id;
	return (p ? (pi_MaxSamp(p)) : 0);
}



char *
ds_FilePath (pid, dfindex)
PlatformId pid;
int dfindex;
/*
 * Generate the full name of this data file.  The name is returned in
 * static space and will get zapped with the next call.  
 */
{
	static char fname[1024];
	DataFile *df = DFTable + dfindex;
	PlatformInstance *pi = PTable + pid;

	sprintf (fname, "%s/%s", (df->df_flags & DFF_Remote) ?
		pi_RDir (pi) : pi_Dir (pi), df->df_name);
	return (fname);
}



int
ds_GetFileStruct (index, df)
int index;
DataFile *df;
{
	*df = DFTable[index];
	return (1);	/* always succeeds */
}


const PlatformClass *
ds_GetClassStruct (id, pc)
PlatClassId id;
PlatformClass *pc;
{
	PlatformClass *ret = NULL;

	if (dt_CheckClassId (id))
		ret = CTable + id;
	if (ret && pc)
		*pc = *ret;
	return (ret);
}


