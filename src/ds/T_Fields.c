/*
 * Some quick checks on the fields interface.
 */
#include <stdio.h>

#include <defs.h>
#include <Test.h>
#include "DataStore.h"

#include "apple.h"

RCSID("$Id: T_Fields.c,v 3.4 1997-12-11 20:49:35 burghart Exp $")

extern void F_Warnings (int);

static int
T_FieldNames ()
{
	static char *name = "thisnameis_too-long_butstilllegalcharacters12345";
	static char *longname =
	"too many characters for a legal description; should get truncated";
	static char *units =
		"this units string needs to be shorter";
	int err = 0;

	/*
	 * Just make sure we catch illegal names, but they will still
	 * be defined.  If the fields have already been defined, then
	 * we won't get any warnings.
	 */
	F_Warnings (1);

	TX_Catch ("declare field 'thisname.*name longer than");
	err += (F_DeclareField (name, longname, units) == BadField);
	err += TX_Caught();

	TX_Catch ("declare field 'xxx.*illegal characters");
	err += (F_DeclareField ("xxx.&.xxx", "X", "X") == BadField);
	err += TX_Caught();

	return (err);
}

		

TestRoutine FieldTests[] = 
{
	{ "fieldnames", FTUnknown, DCC_None, TR_BEGIN, T_FieldNames,
	  "check for detection of illegal field names" },
	END_TESTS
};



