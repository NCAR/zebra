/*
 * Fields module stuff.  Don't #include this file in your code, though.  
 * Include DataStore.h instead, since they are interdependent and DataStore.h
 * includes this file.
 *
 * $Id: ds_fields.h,v 1.9 1998-10-28 21:21:12 corbet Exp $
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
# ifndef __DS_FIELDS_H__
# define __DS_FIELDS_H__

# ifdef __cplusplus
extern "C" 
{
# endif /* __cplusplus */

/*
 * The basic field ID type.
 */
typedef void* FieldId;


# define BadField ((FieldId)0)

void	F_Init (void);
void	F_Closure (void);
void 	F_Reset (void);
FieldId	F_Lookup (const char *name);
FieldId F_DeclareField (const char *name, const char *desc, const char *units);
FieldId F_Field (const char *name, const char* type, const char *desc, 
		 const char *units);
FieldId F_Declared (const char *name);
FieldId F_Alias (const char *name, const char *alias);
char*	F_GetName (FieldId id);
char*	F_GetUnits (FieldId id);
char*	F_GetDesc (FieldId id);
char*	F_GetTypeName (FieldId id);
char*	F_GetFullName (FieldId id);
zbool	F_CanYield (FieldId src, FieldId wanted, double *slope, 
		    double *intercept);


# ifdef __cplusplus

// and one function for those who can deal with class Field

class Field;
FieldId F_FieldId( const Field& f );

} // end of extern "C"

# endif

# endif /* __DS_FIELDS_H__ */
