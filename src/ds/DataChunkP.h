/* $Id: DataChunkP.h,v 1.1 1991-11-16 01:18:54 corbet Exp $ */
/*
 * Internal data chunk definitions.
 */
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
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

/*
 * Methods for the raw data chunk -- always included first in any other
 * data chunk class.
 */
# define RAW_DC_METHODS \
	char		*dcm_Name;		/* Class name		*/ \
	DataChunkClass	dcm_Parent;		/* Parent class		*/ \
	DataChunk	*(*dcm_Create)();	/* Create one of these	*/ \
	void		(*dcm_Destroy)();	/* Get rid of it again	*/ \
	void		(*dcm_AddData)();	/* Add data		*/ \
	void		(*dcm_Dump)();		/* Dump out structures  */

/*
 * Method inheritance is marked with this guy.
 */
# define InheritMethod	0

typedef struct _RawDCClass
{
	RAW_DC_METHODS
} RawDCClass;



/*
 * Internally useful routines.
 */
# ifdef __STDC__
	bool		dc_ReqSubClassOf (DataChunkClass, DataChunkClass,
				char *);
	DataChunkClass	dc_GetSuperClass (DataChunkClass);
	void		dc_AddADE (DataChunk *, DataPointer, DataChunkClass,
				int, int, int);
	DataPointer	dc_FindADE (DataChunk *, DataChunkClass, int, int *);
	void		dc_ChangeADE (DataChunk *, DataPointer, DataChunkClass,
				int, int);
	void		dc_RemoveADE (DataChunk *, DataChunkClass, int);
# else
# endif
