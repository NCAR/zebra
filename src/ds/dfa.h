/*
 * $Id: dfa.h,v 2.9 1999-03-01 02:03:44 burghart Exp $
 * Internal DFA declarations.  Requires DataStore.h and dslib.h.
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

#ifndef __zebra_dfa_h_
#define __zebra_dfa_h_

#include "GetList.h"

# if __cplusplus
extern "C" {
# endif

zbool	dfa_CreateFile (const DataFile *df, DataChunk *dc, ZebraTime *t, 
			dsDetail *details, int ndetail);
void	dfa_ForceClose (const DataFile *df);
int	dfa_GetObsSamples (const DataFile *df, ZebraTime *times, 
			   Location *locs, int max);
int	dfa_GetFields (const DataFile *df, const ZebraTime *t, int *nfld, 
		       FieldId *flist);
char*	dfa_GetAttr (const DataFile *df, const ZebraTime *t, int *len);
int	dfa_QueryDate (int type, const char *name, ZebraTime *begin, 
		       ZebraTime *end, int *nsample);
DataChunk* dfa_Setup (GetList *gl, FieldId *fields, int nfield, 
		      DataClass dclass);
void	dfa_GetData (DataChunk *dc, GetList *gl, dsDetail *details, 
		     int ndetail);
int	dfa_PutBlock (const DataFile *df, DataChunk *dc, int sample, 
		      int nsample, WriteCode wc, dsDetail *details, 
		      int ndetail);
int	dfa_GetAlts (const DataFile *df, FieldId fid, int offset, float *alts, 
		     int *nalts, AltUnitType *altunits);
int	dfa_GetForecastTimes (const DataFile *df, int *times, int *ntimes);
int	dfa_DataTimes (const DataFile *df, const ZebraTime *when, 
		       TimeSpec which, int n, ZebraTime *dest);
char**	dfa_GetAssociatedFiles (const DataFile *df, int *nfiles);
int	dfa_CheckName (int type, const char *name);
void	dfa_MakeFileName (const Platform *plat, const ZebraTime *t, char *dest,
			  dsDetail *details, int ndetail);
void	dfa_NoteRevision (const DataFile *df);
void	dfa_ForceClosure (void);

# if __cplusplus
}	// close extern "C"
# endif

#endif /* __zebra_dfa_h_ */
