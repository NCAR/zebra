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

# include <string.h>

# include "copyright.h"
# include "defs.h"

RCSID("$Id: altunits.c,v 2.13 2002-04-26 19:58:41 burghart Exp $")

/*
 * 
 * The order of strings in the arrays below *must* match the order of
 * the AltUnitType enum declaration in defs.h.
 */
static const struct _unames
{
	char	*shortname;	/* default short name	*/
	char	*longname;	/* default long name	*/
	char	*format;	/* print format		*/
	char	*aliases[10];	/* acceptable aliases	*/
} Unames[] =
{
	/* AU_kmMSL */
	{"km MSL", "km above MSL", "%.2f", 
	 {"km > MSL", "km", "kilometers", ""}},
	/* AU_mMSL */
	{"m MSL", "meters above Mean Sea Level", "%.0f", 
	 {"m > MSL", "m", "meters", "Meters, ASL", ""}},
	/* AU_kmAGL */
	{"km AGL", "km AGL", "%.2f", 
	 {"km > AGL", ""}},
	/* AU_mAGL */
	{"m AGL", "meters AGL", "%.0f", 
	 {"m > AGL", ""}},
	/* AU_mb */
	{"mb", "millibars", "%.0f", 
	 {"mbar", "hPa", "millibar", ""}},
	/* AU_sigma */
	{"sigma", "sigma", "%.4f",
	 {""}},
	/* AU_level */
	{"level", "level", "%.0f",
	 {"plane", "z", "index", ""}},
	/* AU_degrees (kluge for radar elevations as altitude) */
	{"deg", "degrees", "%.1f", 
	 {"deg", "deg.", ""}},
	/* AU_unknown */
	{"unknown", "unknown", "%.0f",
	 {""}}
};


static const int Ntypes = sizeof (Unames) / sizeof (struct _unames);

static const char *Unknown = "unknown";

/*
 * Return string for labels we build.
 */
static char	ReturnLabel[80];	/* *should* be long enough... */



const char *
au_UnitsName (atype)
AltUnitType	atype;
{
	if (atype > Ntypes)
		return (Unknown);
	else
		return ((const char *)Unames[atype].shortname);
}




const char *
au_LongUnitsName (atype)
AltUnitType	atype;
{
	if (atype > Ntypes)
		return (Unknown);
	else
		return ((const char *)Unames[atype].longname);
}



const char *
au_PrintFormat (atype)
AltUnitType	atype;
{
	if (atype > Ntypes)
		return (Unknown);
	else
		return ((const char *)Unames[atype].format);
}




const char *
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

	return ((const char *)ReturnLabel);
}




const char *
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

	return ((const char *)ReturnLabel);
}

	
	

zbool
au_ConvertName (name, atype)
char	*name;
AltUnitType	*atype;
/*
 * Try to convert the given name into an AltUnitType.  If successful, 
 * 'atype' is set to the type we found and we return TRUE.  Otherwise,
 * we return FALSE and 'atype' is untouched.
 */
{
	int	i, a;
/*
 * Start by checking against our "standard" short names and long names
 */
	for (i = 0; i < Ntypes; i++)
	{
		if (! strcasecmp (name, Unames[i].shortname) || 
		    ! strcasecmp (name, Unames[i].longname))
		{
			*atype = (AltUnitType) i;
			return (TRUE);
		}
	}
/*
 * No luck.  Try the aliases.
 */
	for (i = 0; i < Ntypes; i++)
	{
		for (a = 0; *Unames[i].aliases[a]; a++)
		{
			if (! strcasecmp (name, Unames[i].aliases[a]))
			{
				*atype = (AltUnitType) i;
				return (TRUE);
			}
		}
	}
/*
 * No matches at all
 */
	return (FALSE);
}

	
