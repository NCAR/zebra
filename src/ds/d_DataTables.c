/*
 * Maintenance of the data tables.
 */
static char *rcsid = "$Id: d_DataTables.c,v 1.1 1990-10-24 09:41:07 corbet Exp $";

# include "../include/defs.h"
# include "../include/message.h"
# include "dsPrivate.h"
# include "commands.h"
# include "dsDaemon.h"


/*
 * We use this symbol table to find platforms quickly by name.
 */
static stbl Platforms;



void
dt_InitTables ()
/*
 * Initialize the data tables.  Assumes that the shared memory segment
 * is already in place.
 */
{
/*
 * Table offsets.
 */
	PTable = (Platform *) (ShmSegment + ShmHeader->sm_PTOffset);
/*
 * Create the symbol table to hold the platform names.
 */
	Platforms = usy_c_stbl ("Platforms");
}





static void
dt_SetNames (name, p)
char *name;
Platform *p;
/*
 * Associate this name (and all subnames) with p.
 */
{
	char *cp, *strchr ();
	SValue v;

	v.us_v_ptr = (char *) p;
	cp = name;
	do
	{
		usy_s_symbol (Platforms, cp, SYMT_POINTER, &v);
		if (cp = strchr (cp, '/'))
			cp++;
	}
	while (cp);
}





Platform *
dt_NewPlatform (name)
char *name;
/*
 * Add this platform to the list.
 */
{
	Platform *new;
/*
 * See if this guy already exists.
 */
	if (new = dt_FindPlatform (name, TRUE))
	{
		msg_ELog (EF_INFO, "WARNING: platform '%s' redefined", name);
		new->dp_flags = 0;
		return (new);
	}
/*
 * Nope.  However, if we're done with defining platforms, we must gripe
 * severely.
 */
	if (ShmHeader->sm_nDataTable)
	{
		msg_ELog (EF_EMERGENCY, "PANIC: new platform after closure");
		Shutdown ();
	}
/*
 * Allocate a new platform table entry.
 */
	new = PTable + ShmHeader->sm_nPlatform++;
	dt_SetNames (name, new);
/*
 * Fill it in and return it.
 */
	strcpy (new->dp_name, name);
	strcpy (new->dp_class, "");
	new->dp_flags = 0;
	return (new);
}




Platform *
dt_FindPlatform (name, full)
char *name;
int full;
/*
 * Look up this platform.
 */
{
	SValue v;
	int type;

	if (! usy_g_symbol (Platforms, name, &type, &v))
		return (0);
	return ((Platform *) v.us_v_ptr);
}
