/*
 * Frame cache maintenance.
 */
static char *rcsid = "$Id: FrameCache.c,v 1.3 1990-11-29 16:49:17 corbet Exp $";

# include "../include/defs.h"
# include "../include/message.h"
# include "../include/pd.h"
# include "GraphProc.h"



/*
 * An entry in the frame cache.  Entries are indexed by the frame number.
 */
# define BFLEN 40
# define NCACHE 40
struct FrameCache
{
	time	fc_time;	/* The time of this entry		*/
	char	fc_base[BFLEN];	/* Base field				*/
	float	fc_alt;		/* Altitude (for now) of this frame	*/
	int	fc_lru;		/* LRU counter				*/
	bool	fc_keep;	/* Save this frame if possible.		*/
	bool	fc_valid;	/* Is this frame valid?			*/
} FCache[NCACHE];
static int Lru = 0;

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
		FCache[i].fc_valid = FCache[i].fc_keep = FALSE;
}






void
fc_AddFrame (when, number)
time *when;
int number;
/*
 * Add this frame to the cache.
 */
{
	char **complist;
/*
 * Sanity checking.
 */
	if (number >= NCACHE)
	{
		msg_ELog (EF_PROBLEM, "Frame number %d exceeds cache", number);
		return;
	}
/*
 * Add the info.
 */
	FCache[number].fc_time = *when;
	complist = pd_CompList (Pd);
	(void) (pd_Retrieve (Pd, complist[1], "field", FCache[number].fc_base,
			SYMT_STRING) ||
		pd_Retrieve (Pd, complist[1], "color-code-field",
			FCache[number].fc_base, SYMT_STRING) ||
		pd_Retrieve (Pd, complist[1], "u-field",
			FCache[number].fc_base, SYMT_STRING));
	pd_Retrieve (Pd, "global", "altitude", (char *) &FCache[number].fc_alt,
		SYMT_FLOAT);
	FCache[number].fc_lru = ++Lru;
	FCache[number].fc_valid = TRUE;
	FCache[number].fc_keep = FALSE;

	msg_ELog (EF_DEBUG, "Cache %d, fld '%s' alt %.2f at %d %d, lru %d",
		number, FCache[number].fc_base, FCache[number].fc_alt,
		when->ds_yymmdd, when->ds_hhmmss, FCache[number].fc_lru);

}






int
fc_LookupFrame (when)
time *when;
/*
 * Try to find a cache entry that matches PD at this time.
 */
{
	int i;
	float alt;
	char **complist, base[BFLEN];
/*
 * Get the base field from the PD.
 */
	complist = pd_CompList (Pd);
	(void) (pd_Retrieve (Pd, complist[1], "field", base, SYMT_STRING) ||
		pd_Retrieve (Pd, complist[1], "color-code-field", base,
				SYMT_STRING) ||
		pd_Retrieve (Pd, complist[1], "u-field", base, SYMT_STRING));
	pd_Retrieve (Pd, "global", "altitude", (char *) &alt, SYMT_FLOAT);
/*
 * Now go searching.
 */
	for (i = 0; i < FrameCount; i++)
		if (FCache[i].fc_valid && 
		    FCache[i].fc_time.ds_yymmdd == when->ds_yymmdd &&
		    FCache[i].fc_time.ds_hhmmss == when->ds_hhmmss &&
		    FCache[i].fc_alt == alt &&
		    ! strcmp (FCache[i].fc_base, base))
		{
			FCache[i].fc_lru = ++Lru;
		    	return (i);
		}
	return (-1);
}






int
fc_GetFrame ()
/*
 * Return the number of a frame to draw in.
 */
{
	int minlru = 99999, minframe = -1, kframe = -1, klru = 99999;
	int i;
/*
 * Try to find an invalid frame.  Failing that, get the oldest without the
 * keep flag set; finally, take the minimum LRU period.
 */
	for (i = 0; i < FrameCount; i++)
	{
		if (! FCache[i].fc_valid)
			return (i);
		if (FCache[i].fc_lru < minlru)
		{
			minframe = i;
			minlru = FCache[i].fc_lru;
		}
		if (! FCache[i].fc_keep && FCache[i].fc_lru < klru)
		{
			kframe = i;
			klru = FCache[i].fc_lru;
		}
	}
/*
 * Take the best we can get.
 */
	i = (kframe > 0) ? kframe : minframe;
	FCache[i].fc_keep = FCache[i].fc_valid = FALSE;
	return (i);
}






void
fc_MarkFrames (times, ntime)
time *times;
int ntime;
/*
 * Go through and mark all frames that match one of these times to be kept.
 */
{
	int frame, t;
	float alt;
	char base[BFLEN], **complist = pd_CompList (Pd);
/*
 * Get the current base field, and only mark those which match.
 */
	(void) (pd_Retrieve (Pd, complist[1], "field", base, SYMT_STRING) ||
		pd_Retrieve (Pd, complist[1], "color-code-field", base,
				SYMT_STRING) ||
		pd_Retrieve (Pd, complist[1], "u-field", base, SYMT_STRING));
	pd_Retrieve (Pd, "global", "altitude", (char *) &alt, SYMT_FLOAT);
/*
 * Now go through all frames.
 */
	for (frame = 0; frame < NCACHE; frame++)
	{
		struct FrameCache *fc = FCache + frame;

		fc->fc_keep = FALSE;
		for (t = 0; fc->fc_valid && t < ntime; t++)
			if (fc->fc_time.ds_yymmdd == times[t].ds_yymmdd &&
			    fc->fc_time.ds_hhmmss == times[t].ds_hhmmss &&
			    fc->fc_alt == alt && ! strcmp (fc->fc_base, base))
			{
				fc->fc_keep = TRUE;
				break;
			}
	}
}
