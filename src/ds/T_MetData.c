
#define METDATA_STATS
#include "dc_MetData.c"		/* access to datachunk privates */

#include "apple.h"


/*
 * Test a datachunk for the correct optimizations
 */
int
T_MetUniform (dc)
DataChunk *dc;
/* Verify all fields are uniform */
{
	FldInfo *finfo = FIP(dc);

	return ((! finfo || ! finfo->fi_Uniform) ? 1 : 0);
}



void
T_MetDataStats ()
{
	msg_ELog (EF_STATS, "metdata index hash: %d hits, %d rolls",
		  GetIndexHits, GetIndexRolls);
}


