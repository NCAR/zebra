
#include "dc_Transp.c"
#include "apple.h"

/*
 * Return some stats about a transparent datachunk
 */
int
T_TrHints (dc, ns, nsa, hns, hss, no)
DataChunk *dc;
int *ns;	/* NSample */
int *nsa;	/* NSampleAlloc */
int *hns;	/* HintNSample */
int *hss;	/* HintSampSize */
int *no;	/* NextOffset */
/*
 * Any parameter other than dc can be null.
 */
{
	AuxTrans *tp = ATP(dc);

	if (ns) *ns = tp->at_NSample;
	if (nsa) *nsa = tp->at_NSampAlloc;
	if (hns) *hns = tp->at_HintNSample;
	if (hss) *hss = tp->at_HintSampSize;
	if (no) *no = tp->at_NextOffset;
	return (1);
}
		


static int
T_TransparentAdd (dc, start, nsample, is_mobile, addatts)
DataChunk *dc;
ZebTime *start;
int nsample;
zbool is_mobile;	/* Set locations 		*/
zbool addatts;		/* per-sample atts only 	*/
/*
 * Just adds variable length text as opaque samples of varying sizes.
 * Tests the hint functions.
 */
{
	static char *text[] = {
"burghart        - Died on level   8. Started on level   1.  Score:     7532.",
"Died on level  17",
"You are quite disappointing:",
"Started on level   1.  Score:     9542.",
" burghart     - Died on level   9. Started on level   1.  Score:     9542.\n\
 burghart     - Died on level  18. Started on level  12.  Score:     8583.\n\
 burghart     - Died on level   8. Started on level   1.  Score:     8420.\n\
 burghart     - Died on level  17. Started on level  12.  Score:     8153.\n\
 burghart     - Died on level  10. Started on level   1.  Score:     7905.\n\
 burghart     - Died on level   8. Started on level   1.  Score:     7800.",
"You are quite disappointing: *granger" };
	int ntext = sizeof(text)/sizeof(text[0]);
	int i, len;
	char *data, *src;
	ZebTime when;
	static Location loc = { 1.0, 2.0, 4.0 };
	int errors = 0;

	when = *start;
	for (i = 0; i < nsample; ++i)
	{
		src = text[i%ntext];
		dc_AddSample(dc, &when, src, strlen(src)+1);
		if (is_mobile)
			dc_SetLoc (dc, i, &loc);
		if (addatts)
			dc_SetSampleAttr(dc, i, "key", "value");
		++when.zt_Sec;
		data = dc_GetSample (dc, i, &len);
		if (len != strlen(src)+1)
			++errors;
		if (strcmp(data, src))
			++errors;
	}
	return (errors);
}



static int
T_Transparent (now)
ZebTime *now;
{
	DataChunk *dc;
	char *platform = "t_transparent";
	char *data = "Transparent chunk holding text and newline\n";
	char buf[128];
	ZebTime when;
	zbool atts = FALSE;
	static Location loc = { 40.0, -160.0, 5280.0 };
	int errors = 0;

	when = *now;
	sprintf(buf,"Testing transparent datachunks on '%s'",platform);
	Announce (buf);
	dc = dc_CreateDC (DCC_Transparent);
	dc->dc_Platform = NeedPlatform (platform);
	dc_SetStaticLoc (dc, &loc);
	dc_AddSample (dc, &when, data, strlen(data)+1);
	errors += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC(dc);

	/* Now get a much larger one and overwrite the first sample above */
	dc = dc_CreateDC (DCC_Transparent);
	dc->dc_Platform = NeedPlatform (platform);
	dc_SetStaticLoc (dc, &loc);
	errors += T_TransparentAdd (dc, &when, 100, TRUE, atts);
	errors += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	errors += !ds_Store (dc, TRUE, NULL, 0);
	errors += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC(dc);

	/* Now try it with some hints set */
	dc = dc_CreateDC (DCC_Transparent);
	dc->dc_Platform = NeedPlatform (platform);
	dc_SetStaticLoc (dc, &loc);
	dc_HintNSamples (dc, 25, FALSE);
	errors += T_TransparentAdd (dc, &when, 50, TRUE, atts);
	when.zt_Sec += 25;
	dc_HintNSamples (dc, 10, TRUE);
	errors += T_TransparentAdd (dc, &when, 10, TRUE, atts);
	when.zt_Sec += 10;
	dc_HintSampleSize (dc, 50, FALSE);
	dc_HintMoreSamples (dc, 50, FALSE);
	errors += T_TransparentAdd (dc, &when, 50, TRUE, atts);
	when.zt_Sec += 50;
	errors += !ds_StoreBlocks (dc, TRUE, NULL, 0);
	dc_DestroyDC (dc);
	return (errors);
}



TestRoutine TransparentTests[] = 
{
	{ "transparent", FTZebra, DCC_Transparent, TR_BEGIN, T_Transparent,
	  "test storing and fetching of text and transparent data" },
	END_TESTS
};

