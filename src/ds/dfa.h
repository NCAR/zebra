/*
 * $Id: dfa.h,v 2.8 1998-10-28 21:21:09 corbet Exp $
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

void	dfa_ForceClose FP ((int));
int	dfa_CheckName FP ((int, char *));
int	dfa_QueryDate FP ((int, char *, ZebTime *, ZebTime *, int *));
int	dfa_InqNPlat FP ((int));
DataChunk *dfa_Setup FP ((GetList *, FieldId *, int, DataClass));
void	dfa_GetData FP ((DataChunk *, GetList *, dsDetail *, int));
int	dfa_InqRGrid FP ((int, Location *, RGrid *));
int	dfa_DataTimes FP ((int, ZebTime *, TimeSpec, int, ZebTime *));
void	dfa_MakeFileName FP ((ClientPlatform *, ZebTime *, char *,
			      dsDetail *details, int ndetail));
zbool	dfa_CreateFile FP ((int, DataChunk *, ZebTime *, dsDetail *, int));
void	dfa_NoteRevision FP ((int dfindex, long revision));
void	dfa_ForceClosure FP ((void));
char	*dfa_GetAttr FP ((int, ZebTime *, int *));
int	dfa_GetFields FP((int dfile, ZebTime *t, int *nfld, FieldId *flist));
int	dfa_GetAlts FP ((int index, FieldId fid, int offset, float *alts,
			 int *nalts, AltUnitType *altunits));
int	dfa_GetForecastTimes FP ((int index, int *times, int *ntimes));
int	dfa_GetObsSamples FP ((int dfile, ZebTime *times,
			       Location *locs, int max));
int	dfa_PutBlock FP ((int dfile, DataChunk *dc, int sample, int nsample,
			  WriteCode wc, dsDetail *details, int ndetail));
char ** dfa_GetAssociatedFiles FP ((int df, int *nfiles));
#endif /* __zebra_dfa_h_ */
