/*
 * The data store application interface.
 */
static char *rcsid = "$Id: Appl.c,v 1.1 1990-10-30 09:51:10 corbet Exp $";

# include "../include/defs.h"
# include "../include/message.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"


/*
 * Local stuff.
 */
# ifdef __STDC__
	static void ds_InitPFTable (void);
# else
	static void ds_InitPFTable ();
# endif


int
ds_Initialize ()
/*
 * Hook into the data store.
 */
{
/*
 * Hook into the shared memory segment.
 */
	if (! dsm_Init ())
		return (FALSE);
/*
 * Set up the platform lookup table.
 */
	ds_InitPFTable ();

	return (TRUE);
}





static void
ds_InitPFTable ()
/*
 * Create the platform lookup table.
 */
{
	int i;
	char *slash, *strchr ();
	SValue v;
/*
 * Create the table itself.
 */
	Pf_Names = usy_c_stbl ("Platform_names");
/*
 * Just go through the platform list and make all the entries.
 */
	dsm_ShmLock ();
	for (i = 0; i < SHeader->sm_nPlatform; i++)
	{
		v.us_v_ptr = (char *) (PTable + i);
		usy_s_symbol (Pf_Names, PTable[i].dp_name, SYMT_POINTER, &v);
		slash = strchr (PTable[i].dp_name, '/');
		while (slash)
		{
			usy_s_symbol (Pf_Names, slash + 1, SYMT_POINTER, &v);
			slash = strchr (slash + 1, '/');
		}
	}
	dsm_ShmUnlock ();
}
