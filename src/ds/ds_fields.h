/*
 * Fields module stuff.
 *
 * $Id: ds_fields.h,v 1.6 1996-11-21 22:20:25 granger Exp $
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

/*
 * The basic field ID type.
 */
typedef int FieldId;

# define BadField -1

void	F_Init FP((void));
void	F_Closure FP((void));
void 	F_Reset FP((void));
FieldId	F_Lookup FP((const char *));
FieldId F_DeclareField FP((const char *, const char *, const char *));
FieldId F_Declared FP((const char *name));
FieldId F_Alias FP((const char *, const char *));
char *	F_GetName FP((FieldId));
char *	F_GetUnits FP((FieldId));
char *	F_GetDesc FP((FieldId));

# endif /* __DS_FIELDS_H__ */
