/*
 * $ID$
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

int	Semaphore;		/* The semaphores		*/
char	*ShmSegment;		/* The actual segment		*/
struct ds_ShmHeader *SHeader;	/* The memory header		*/
Platform *PTable;
DataFile *DFTable;

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
	time	gl_begin;		/* Begin time			*/
	time	gl_end;			/* End time			*/
	int	gl_flags;		/* Flag values			*/
	int	gl_npoint;		/* Number of data points	*/
	int	gl_nsample;		/* Number of samples		*/
	DataObject *gl_dobj;		/* The DO in progress		*/
	float	*gl_data[MAXFIELD];	/* Where the data goes		*/
	struct _GetList *gl_next;	/* Next in the list		*/
	time	*gl_time;		/* Where to put sample times	*/
	Location *gl_locs;		/* Location array for mobile	*/
	int	gl_sindex;		/* Sample index for entire rq	*/
} GetList;

/*
 * Flags for the above.
 */
# define GLF_SATISFIED	0x0001		/* This piece is satisfied	*/
# define GLF_REMOTE	0x0002		/* This is a remote data grab	*/




# ifdef __STDC__
	int 	dsm_Init (void);
	void	dsm_ShmLock (void);
	void	dsm_ShmUnlock (void);
	int	dfa_CheckName (int, char *);
	int	dfa_QueryDate (int, char *, time *, time *, int *);
	int	dfa_InqNPlat (int);
	void	dfa_Setup (GetList *);
	void	dfa_GetData (GetList *);
	int	dfa_InqRGrid (int, Location *, RGrid *);
	int	dfa_DataTimes (int, time *, TimeSpec, int, time *);
	GetList *dgl_MakeGetList (DataObject *);
	void	dgl_ReturnList (GetList *);
	int	dgl_GetDestFile (DataObject *, int, int, int *);
	bool	dfa_CreateFile (int, DataObject *);
	void	dfa_PutData (int, DataObject *, int, int);
	void	dfa_NoteRevision (int);
	char	*dfa_GetAttr (int, time *, int *);
# else
	int 	dsm_Init ();
	void	dsm_ShmLock ();
	void	dsm_ShmUnlock ();
	int	dfa_CheckName ();
	int	dfa_QueryDate ();
	int	dfa_InqNPlat ();
	void	dfa_Setup ();
	void	dfa_GetData ();
	int	dfa_InqRGrid ();
	int	dfa_DataTimes ();
	GetList *dgl_MakeGetList ();
	void	dgl_ReturnList ();
	int	dgl_GetDestFile ();
	bool	dfa_CreateFile ();
	void	dfa_PutData ();
	void	dfa_NoteRevision ();
	char	*dfa_GetAttr ();
# endif
