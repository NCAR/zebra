/*
 * Frame cache maintenance.
 */
static char *rcsid = "$Id: FrameCache.c,v 1.1 1990-06-27 14:06:04 corbet Exp $";

# include "../include/defs.h"
# include "../include/message.h"
# include "../include/pd.h"
# include "GraphProc.h"



/*
 * An entry in the frame cache.
 */
# define BFLEN 40
# define NCACHE 40
struct FrameCache
{
	int	fc_number;	/* The corresponding frame number	*/
	time	fc_time;	/* The time of this entry		*/
	char	fc_base[BFLEN];	/* Base field				*/
} FCache[NCACHE];

/*
 * Marker for invalid entries.
 */
# define InvalidEntry	-1





void
fc_InvalidateCache ()
/*
 * Clear out the frame cache.
 */
{
	int i;

	for (i = 0; i < NCACHE; i++)
		FCache[i].fc_number = InvalidEntry;
}






void
fc_AddFrame (when, number)
time *when;
int number;
/*
 * Add this frame to the cache.
 */
{
	int i;
	char **complist;
/*
 * Attempt to find an entry already referring to this frame.  Failing that,
 * find an empty one.
 */
	for (i = 0; i < NCACHE; i++)
		if (FCache[i].fc_number == number)
			break;
	if (i >= NCACHE)
		for (i = 0; i < NCACHE; i++)
			if (FCache[i].fc_number == InvalidEntry)
				break;
	if (i >= NCACHE)
	{
		msg_ELog (EF_PROBLEM, "Frame cache full");
		return;
	}
/*
 * Add the info.
 */
	FCache[i].fc_number = number;
	FCache[i].fc_time = *when;
	complist = pd_CompList (Pd);
	pd_Retrieve (Pd, complist[1], "field", FCache[i].fc_base, SYMT_STRING);

	msg_ELog (EF_DEBUG, "Cache %d, fld '%s' at %d %d, frame %d", i,
		FCache[i].fc_base, when->ds_yymmdd, when->ds_hhmmss, number);

}






int
fc_LookupFrame (when)
time *when;
/*
 * Try to find a cache entry that matches PD at this time.
 */
{
	int i;
	char **complist, base[BFLEN];
/*
 * Get the base field from the PD.
 */
	complist = pd_CompList (Pd);
	pd_Retrieve (Pd, complist[1], "field", base, SYMT_STRING);
/*
 * Now go searching.
 */
	for (i = 0; i < NCACHE; i++)
		if (FCache[i].fc_number != InvalidEntry && 
		    FCache[i].fc_time.ds_yymmdd == when->ds_yymmdd &&
		    FCache[i].fc_time.ds_hhmmss == when->ds_hhmmss &&
		    ! strcmp (FCache[i].fc_base, base))
		    	return (FCache[i].fc_number);
	return (-1);
}
