/*
 * Override some of the usual datachunk interface routines to perform
 * internal checking.
 */

#define T_DATACHUNKS
#include "DataChunk.c"

#include "apple.h"

static int Checking = 0;


#ifdef notdef
static int
T_InternalDC ()
{
	Checking = 1;	/* enable internal checking routines */
	msg_ELog (EF_DEBUG, "enabling internal datachunk checks");
	return (0);
}
#endif


static int
T_NoFreeADE (dc)
DataChunk *dc;
{
	AuxDataChain ade;
	int i, j;
	int err = 0;

	for (i = 0; i < ADE_DCC_LEVELS; ++i)
		for (j = 0; j < ADE_HASH_SIZE; ++j)
		{
			ade = dc->dc_AuxData[i][j];
			while (ade)
			{
				if (ade->dca_Free)
					++err;
			}
		}
	return (err);
}




DataChunk *
dc_Create (class)
DataClassP class;
{
	DataChunk *dc;
	int err;

	/* create the chunk as usual, then verify ADEs */
	dc = _dc_Create (class);
	if (Checking)
	{
		msg_ELog (EF_DEBUG, "checking ADEs for created dc");
		err = T_NoFreeADE (dc);		/* no attributes added yet */
		if (err)
			msg_ELog (EF_PROBLEM,
				  "dc_Create: found ADE to be auto freed");
		Errors += err;
	}
	return (dc);
}




void
dc_Destroy (dc)
DataChunk *dc;
/*
 * Copy this chunk, diff the copies and report differences, then
 * destroy both of them.
 */
{
	DataChunk *copy = NULL;

	if (Checking)
	{
		msg_ELog (EF_DEBUG, "creating copy before destroying dc");
		copy = dc_Copy (dc);
		T_DumpDC (dc);
	}
	_dc_Destroy (dc);
	if (copy)
		_dc_Destroy (copy);
}




TestRoutine DataChunkTests[] = 
{
#ifdef notdef
	{ "0datachunks", FTUnknown, DCC_None, TR_BEGIN, T_InternalDC,
	  "enable internal datachunk tests for all dc calls" },
#endif
	END_TESTS
};

