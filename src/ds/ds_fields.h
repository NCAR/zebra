/*
 * Fields module stuff.
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

# ifdef __STDC__
	void	F_Init (void);
	FieldId	F_Lookup (char *);
	FieldId F_DeclareField (char *, char *, char *);
	FieldId F_Alias (char *, char *);
	char *	F_GetName (FieldID);
	char *	F_GetUnits (FieldId);
	char *	F_GetDesc (FieldId);
# else
# endif

# endif /* __DS_FIELDS_H__ */
