/*
 * Altitude units convenience routines
 */
/*		Copyright (C) 1994 by UCAR
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

# include "copyright.h"
# include "defs.h"

MAKE_RCSID("$Id: altunits.c,v 2.1 1994-02-16 22:21:51 burghart Exp $")

/*
 * The order of strings in the arrays below *must* match the order in
 * the AltUnitType enum declaration.
 */
char *Units[] =
{
	"km MSL",	/* AU_kmMSL */
	"m MSL",	/* AU_mMSL */
	"mb",		/* AU_mb */
};

char *LongUnits[] =
{
	"km above MSL",	/* Make these udunits compliant at some point... */
	"m above MSL",
	"millibars",
};

char *Format[] =
{
	"%.2f", "%d", "%d"
};

char	*Unknown = "unknown";

/*
 * Return string for labels we build.
 */
char	ReturnLabel[40];	/* *should* be long enough... */



char *
au_UnitsName (atype)
AltUnitType	atype;
{
	if (atype > (sizeof (Units) / sizeof (char *) - 1))
		return (Unknown);
	else
		return (Units[atype]);
}




char *
au_LongUnitsName (atype)
AltUnitType	atype;
{
	if (atype > (sizeof (LongUnits) / sizeof (char *) - 1))
		return (Unknown);
	else
		return (LongUnits[atype]);
}



char *
au_PrintFormat (atype)
AltUnitType	atype;
{
	if (atype > (sizeof (Format) / sizeof (char *) - 1))
		return (Unknown);
	else
		return (Format[atype]);
}




char *
au_AltLabel (alt, atype)
double	alt;
AltUnitType	atype;
/*
 * Return a nicely formatted, null-terminated string containing the given 
 * altitude and its units.  The string returned will be valid until the next
 * call to au_AltLabel() or au_LongAltLabel(), and should *not* be modified
 * by the caller.
 */
{
	sprintf (ReturnLabel, au_PrintFormat (atype), alt);
	sprintf (ReturnLabel + strlen (ReturnLabel), " %s", 
		 au_UnitsName (atype));
}




char *
au_LongAltLabel (alt, atype)
double	alt;
AltUnitType	atype;
/*
 * Return a nicely formatted, null-terminated string containing the given
 * altitude and its long units.  The string returned will be valid until
 * the next call to au_AltLabel() or au_LongAltLabel(), and should *not* be
 * modified by the caller.
 */
{
	sprintf (ReturnLabel, au_PrintFormat (atype), alt);
	sprintf (ReturnLabel + strlen (ReturnLabel), " %s", 
		 au_LongUnitsName (atype));
}

	
	
