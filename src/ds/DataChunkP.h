/* $Id: DataChunkP.h,v 1.3 1992-06-02 14:48:51 corbet Exp $ */
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
	DataClass	dcm_Parent;		/* Parent class		*/ \
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
bool		dc_ReqSubClassOf FP ((DataClass, DataClass, char *));
DataClass	dc_GetSuperClass FP ((DataClass));
void		dc_AddADE FP ((DataChunk *, DataPtr, DataClass,
			int, int, int));
DataPtr		dc_FindADE FP ((DataChunk *, DataClass, int, int *));
void		dc_ChangeADE FP ((DataChunk *, DataPtr, DataClass,
			int, int));
void		dc_RemoveADE FP ((DataChunk *, DataClass, int));

void		dca_AddAttr FP ((DataChunk *, DataClass, int, char *, char *));
char *		dca_GetAttr FP ((DataChunk *, DataClass, int, char *));
int		dca_ProcAttrs FP ((DataChunk *, DataClass, int, char *,
			int (*) ()));
