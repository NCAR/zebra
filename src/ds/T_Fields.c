/*
 * Some quick checks on the fields interface.
 */
#include <stdio.h>

#include <defs.h>
#include <Test.h>
#include "DataStore.h"

#include "apple.h"

RCSID("$Id: T_Fields.c,v 3.5 1997-12-12 16:34:08 burghart Exp $")

extern void F_Warnings (int);

static int
T_FieldNames ()
{
	static char *name = "thisnameis_too-long_butstilllegalcharacters12345";
	static char *longname =
	"too many characters for a legal description; should get truncated";
	static char *units =
		"this units string needs to be shorter";
	static int pass = 0;
	int err = 0;

	/*
	 * Just make sure we catch illegal names, but they will still
	 * be defined.  After the first pass for any given name, we 
	 * shouldn't get any warnings.
	 */
	F_Warnings (1);

	if (pass == 0)
	{
	    TX_Catch ("declare field 'thisname.*name longer than");
	    err += (F_DeclareField (name, longname, units) == BadField);
	    err += TX_Caught();

	    TX_Catch ("declare field 'xxx.*illegal characters");
	    err += (F_DeclareField ("xxx.&.xxx", "X", "X") == BadField);
	    err += TX_Caught();
	}
	else
	{
	    err += (F_DeclareField (name, longname, units) == BadField);
	    err += (F_DeclareField ("xxx.&.xxx", "X", "X") == BadField);
	}

	pass++;

	return (err);
}

		

TestRoutine FieldTests[] = 
{
	{ "fieldnames", FTUnknown, DCC_None, TR_BEGIN, T_FieldNames,
	  "check for detection of illegal field names" },
	END_TESTS
};



