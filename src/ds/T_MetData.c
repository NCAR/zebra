
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



static int
T_MetSerial (start)
ZebraTime *start;
/*
 * Create a simple datachunk, serialize it, dump it, clear fields,
 * then localize it, dump it again, and verify new fields declared.
 */
{
	DataChunk *dc;

	dc = T_SimpleScalarChunk (start, 10, 10, 5, FALSE, TRUE);
	dc_Serialize (dc);
	msg_ELog (EF_TEST, "serialized datachunk: ");
	T_DumpDC (dc);
	F_Reset ();
	dc_Localize (dc);
	msg_ELog (EF_TEST, "localized datachunk after fields reset: ");
	T_DumpDC (dc);
	dc_Destroy (dc);
	return (0);
}




TestRoutine MetDataTests[] = 
{
	{ "metserial", FTUnknown, DCC_MetData, TR_BEGIN, T_MetSerial,
	  "test metdata serialize and localize of field ids" },
	END_TESTS
};

