/*
 * "$Id: dslib.h,v 3.4 1993-05-04 21:42:11 granger Exp $"
 * Internal info for the data store application interface library.
 */

/*
 * Shared memory segment parameters.
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

# ifdef KLUDGE_SHM
int	Semaphore;		/* The semaphores		*/
char	*ShmSegment;		/* The actual segment		*/
struct ds_ShmHeader *SHeader;	/* The memory header		*/
Platform *PTable;
DataFile *DFTable;
# endif

/*
 * The platform lookup table.
 */
stbl Pf_Names;



/*
 * This is the format of the data request list, which is generated as
 * part of the process of satisfying each application data grab.
 */
typedef struct _GetList
{
	int	gl_dfindex;		/* Corresponding DF entry	*/
	int	gl_dfuse;		/* Use count for this entry	*/
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




int 	dsm_Init FP ((void));
void	dsm_ShmLock FP ((void));
void	dsm_ShmUnlock FP ((void));
int	dfa_CheckName FP ((int, char *));
int	dfa_QueryDate FP ((int, char *, ZebTime *, ZebTime *, int *));
int	dfa_InqNPlat FP ((int));
DataChunk *dfa_Setup FP ((GetList *, FieldId *, int, DataClass));
void	dfa_GetData FP ((DataChunk *, GetList *, dsDetail *, int));
int	dfa_InqRGrid FP ((int, Location *, RGrid *));
int	dfa_DataTimes FP ((int, ZebTime *, TimeSpec, int, ZebTime *));
void	dfa_MakeFileName FP ((Platform *, ZebTime *, char *));
GetList *dgl_MakeGetList FP ((PlatformId, ZebTime *, ZebTime *));
void	dgl_ReturnList FP ((GetList *));
bool	dfa_CreateFile FP ((int, DataChunk *, ZebTime *, dsDetail *, int));
void	dfa_NoteRevision FP ((Platform *, int));
char	*dfa_GetAttr FP ((int, ZebTime *, int *));
int	ds_GetDetail FP ((char *, dsDetail *, int, SValue *));
char	*dfa_FilePath FP ((Platform *, DataFile *));
long	dfa_GetRevision FP ((Platform *, DataFile *));
void	ds_GetFileStruct FP ((int, DataFile *));
void	ds_GetPlatStruct FP ((PlatformId, Platform *, int));
int	ds_FindDF FP ((PlatformId, ZebTime *, int));
