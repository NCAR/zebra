

#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include <assert.h>

#include <defs.h>
#include <message.h>
#include "DataStore.h"
#include "apple.h"




struct AttrDesc {
	enum { Global, Field, Sample } which;
	DataChunk *dc;
	int sample;
	FieldId field;
};



static int
T_RemoveAttr (key, value, nval, type, arg)
char *key;
void *value;
int nval;
DC_ElemType type;
void *arg;
{
	struct AttrDesc *ad = (struct AttrDesc *)arg;

	switch (ad->which)
	{
	   case Global:
		dc_RemoveGlobalAttr (ad->dc, key);
		break;
	   case Field:
		dc_RemoveFieldAttr (ad->dc, ad->field, key);
		break;
	   case Sample:
		dc_RemoveSampleAttr (ad->dc, ad->sample, key);
		break;
	}
	return (0);
}



static int
T_Attributes (when)
ZebTime *when;
/*
 * Create a simple DataChunk and add/remove/dump its attributes
 */
{
	DataChunk *dc, *dc2;
	FieldId field;
	char *pname = "t_att_types_cdf";
	PlatformId plat = NeedPlatform (pname);
	static Location loc = { 40.0, -160.0, 5280.0 };
	char key[30];
	char value[50];
	float data = 1.0;
	char **keys;
	void **values;
	int natts, nval, i;
	DC_ElemType type;
#	define NUM(ra) (sizeof(ra)/(sizeof((ra)[0])))
	static double ddata[] = { 0.0, 1.0, 2.0, 4.0, 8.0 };
	double *dget;
	static float fdata[] = { -1.2, -3.4, -5.6, -6.7, -8.9, -10.0 };
	static unsigned char bytes[] = { 1, 3, 7, 15, 31, 63, 127, 255 };
	static short sdata[] = { 512, 1024, 2048, 4096, 8192, 16384, 32767 };
	short *sget;
	char *cdata = "array of characters";
	char *cptr;
	struct AttrDesc ad;
	int nfield = 10;
	FieldId fields[10];
	char *dash = " ";
	char diffs[2048];
	int errors = 0;
	float badval;
	char buf[1024];
	void *ptr;

	Announce ("Testing DataChunk attributes...");
	dc = dc_CreateDC(DCC_Scalar);
	dc->dc_Platform = plat;
	badval = 2001.0;
	dc_SetBadval (dc, badval);	/* usual global float default */
	if (dc_GetBadval (dc) != badval)
	{
		++errors;
		msg_ELog (EF_PROBLEM, "get badval returns %f, not %f",
			  dc_GetBadval(dc), badval);
	}
	dc_SetStaticLoc (dc, &loc);
	dc_SetGlobalAttr (dc, "global_key", "global_value");
	dc_SetGlobalAttrArray (dc, "global_doubles", DCT_Double, 
			       NUM(ddata), (void *)ddata);
	dc_SetGlobalAttrArray (dc, "global_floats", DCT_Float, 
			       NUM(fdata), (void *)fdata);
	dc_SetGlobalAttrArray (dc, "global_bytes", DCT_UnsignedChar, 
			       NUM(bytes), (void *)bytes);
	dc_SetGlobalAttrArray (dc, "global_shorts", DCT_ShortInt, 
			       NUM(sdata), (void *)sdata);
	dc_SetGlobalAttrArray (dc, "global_string", DCT_String,
			       1, "this is a global attribute string");
	dc_SetGlobalAttrArray (dc, "global_char", DCT_Char,
			       strlen(cdata), cdata); /* no null char */
	EXPECT(1);
	dc_SetGlobalAttrArray (dc, "global_string", DCT_String,
			       2, "this is a global attribute string");
	SEEN;
	/*
	 * Get the typed arrays added above
	 */
	sprintf (buf, "%s Double attribute: ", dash);
	dget = (double *)dc_GetGlobalAttrArray (dc, "global_doubles", 
						&type, &nval);
	for (i = 0; i < nval; ++i) sprintf (buf+strlen(buf), " %f ", dget[i]);
	msg_ELog (EF_DEVELOP, "%s", buf);
	sprintf (buf, "%s Short attribute: ", dash);
	sget = (short *)dc_GetGlobalAttrArray (dc, "global_shorts", 
					       &type, &nval);
	for (i = 0; i < nval; ++i) sprintf (buf+strlen(buf), " %hd ", sget[i]);
	msg_ELog (EF_DEVELOP, "%s", buf);
	field = F_Lookup ("temp");
	dc_SetScalarFields (dc, 1, &field);
	/* before adding field-specific badval, check inheritance */
	if (!(ptr = dc_FindFieldBadval (dc, field)) || *(float *)ptr != badval)
	{	
		++errors;
		msg_ELog (EF_PROBLEM, "field badval does not match global");
	}
	badval = 2010.0;
	dc_SetFieldBadval (dc, field, &badval);
	if (!(ptr = dc_FindFieldBadval (dc, field)) || *(float *)ptr != badval)
	{	
		++errors;
		msg_ELog (EF_PROBLEM, "field getbadval is not %f", badval);
	}
	dc_AddScalar (dc, when, 0, field, &data);
	dc_SetFieldAttr (dc, field, "field_key", "field_value");
	dc_SetSampleAttr (dc, 0, "sample_key", "sample_value");
	msg_ELog (EF_DEVELOP, "%s dc should include non-string attributes:", 
		  dash);
	T_DumpDC (dc);
	/*
	 * Now really crank out some attributes
	 */
	for (i = 0; i < 50; ++i)
	{
		sprintf (key, "key_%d", i);
		sprintf (value, "value_%d", i);
		dc_SetGlobalAttr (dc, key, value);
		if (strcmp(dc_GetGlobalAttr(dc, key), value))
		{
			msg_ELog (EF_PROBLEM, 
				  "Global att '%s' should equal '%s'", 
				  key, value);
			++errors;
		}
		dc_SetFieldAttr (dc, field, key, value);
		if (strcmp(dc_GetFieldAttr(dc, field, key), value))
		{
			msg_ELog (EF_PROBLEM, 
				  "Field att '%s' should equal '%s'", 
				  key, value);
			++errors;
		}
		sprintf (key, "sample_key_%d", i);
		dc_SetSampleAttr (dc, 0, key, value);
		if (strcmp(dc_GetSampleAttr(dc, 0, key), value))
		{
			msg_ELog (EF_PROBLEM,
				  "Sample att '%s' should equal '%s'", 
				  key, value);
			++errors;
		}
	}
	/*
	 * Add some more typed attributes
	 */
	dc_SetFieldAttrArray (dc, field, "field_doubles", DCT_Double, 
			       NUM(ddata), (void *)ddata);
	dc_SetFieldAttrArray (dc, field, "field_floats", DCT_Float, 
			       NUM(fdata), (void *)fdata);
	dc_SetFieldAttrArray (dc, field, "field_bytes", DCT_UnsignedChar, 
			       NUM(bytes), (void *)bytes);
	dc_SetFieldAttrArray (dc, field, "field_shorts", DCT_ShortInt, 
			       NUM(sdata), (void *)sdata);
	dc_SetFieldAttrArray (dc, field, "field_string", DCT_String,
			       1, "this is a field attribute string");
	dc_SetFieldAttrArray (dc, field, "field_char", DCT_Char,
			       strlen(cdata), cdata);
	dc_SetSampleAttrArray (dc, 0, "sample_doubles", DCT_Double, 
			       NUM(ddata), (void *)ddata);
	dc_SetSampleAttrArray (dc, 0, "sample_floats", DCT_Float, 
			       NUM(fdata), (void *)fdata);
	dc_SetSampleAttrArray (dc, 0, "sample_bytes", DCT_UnsignedChar, 
			       NUM(bytes), (void *)bytes);
	dc_SetSampleAttrArray (dc, 0, "sample_shorts", DCT_ShortInt, 
			       NUM(sdata), (void *)sdata);
	dc_SetSampleAttrArray (dc, 0, "sample_string", DCT_String,
			       1, "this is a sample attribute string");
	T_DumpDC (dc);
	/*
	 * Store this datachunk and see what we get in the file
	 */
	msg_ELog (EF_DEBUG, "%s Storing datachunk to platform '%s'", 
		  dash, pname);
	errors += !ds_Store (dc, TRUE, 0, 0);
	{ /* DIFF_DATACHUNKS */
	i = dc_CmpGlobalAttr (dc, dc, diffs, sizeof(diffs));
	msg_ELog (EF_DEBUG, "%d differences between same chunk", i);
	errors += (i != 0);
	msg_ELog (EF_DEVELOP, "%s", diffs);
	/*
	 * A quick fetch to check the diffs before hacking away
	 */
	errors += !ds_GetFields (plat, when, &nfield, fields);
	dc2 = ds_FetchObs (plat, DCC_Scalar, when, fields, nfield, NULL, 0);
	if (! dc2)
		++errors;
	else
	{
		i = dc_CmpGlobalAttr (dc, dc2, diffs, sizeof(diffs));
		msg_ELog (EF_DEBUG,
			  "%d differences between stored and fetched", i);
		msg_ELog (EF_DEBUG, "%s", diffs);
		dc_DestroyDC (dc2);
	}
	} /* DIFF_DATACHUNKS */
	/*
	 * Now try doing all of them again, which will cause them all to be
	 * removed and then re-added.
	 */
	for (i = 0; i < 50; ++i)
	{
		sprintf (key, "key_%d", i);
		sprintf (value, "value_%d", i);
		dc_SetGlobalAttr (dc, key, value);
		if (strcmp(dc_GetGlobalAttr(dc, key), value))
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
				  "Global att '%s' should equal '%s'", 
				  key, value);
		}
		dc_SetFieldAttr (dc, field, key, value);
		if (strcmp(dc_GetFieldAttr(dc, field, key), value))
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
				  "Field att error: '%s' should equal '%s'", 
				  key, value);
		}
		sprintf (key, "sample_key_%d", i);
		dc_SetSampleAttr (dc, 0, key, value);
		if (strcmp(dc_GetSampleAttr(dc, 0, key), value))
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
				  "Sample att error: '%s' should equal '%s'", 
				  key, value);
		}
	}
	/*
	 * Now just verify all at once
	 */
	for (i = 0; i < 50; ++i)
	{
		sprintf (key, "key_%d", i);
		sprintf (value, "value_%d", i);
		if (strcmp(dc_GetGlobalAttr(dc, key), value))
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
				  "Global att '%s' should equal '%s'", 
				  key, value);
		}
		if (strcmp(dc_GetFieldAttr(dc, field, key), value))
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
				  "Field att error: '%s' should equal '%s'", 
				  key, value);
		}
		sprintf (key, "sample_key_%d", i);
		if (strcmp(dc_GetSampleAttr(dc, 0, key), value))
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
				  "Sample att error: '%s' should equal '%s'", 
				  key, value);
		}
	}
	msg_ELog (EF_DEBUG, "%s Deleting the even keys...", dash);
	for (i = 0; i < 50; i += 2)
	{
		sprintf (key, "key_%d", i);
		dc_RemoveGlobalAttr (dc, key);
		dc_RemoveFieldAttr (dc, field, key);
		sprintf (key, "sample_key_%d", i);
		dc_RemoveSampleAttr (dc, 0, key);
	}
	msg_ELog (EF_DEBUG, "%s Making sure they're gone...", dash);
	for (i = 0; i < 50; i += 2)
	{
		sprintf (key, "key_%d", i);
		if (dc_GetFieldAttr (dc, field, key) != NULL)
		{
			++errors;
			msg_ELog (EF_PROBLEM,
				  "   field key '%s' still exists", key);
		}
		if (dc_GetGlobalAttr (dc, key) != NULL)
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
				  "   global key '%s' still exists", key);
		}
		sprintf (key, "sample_key_%d", i);
		if (dc_GetSampleAttr (dc, 0, key) != NULL)
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
				  "   sample key '%s' still exists", key);
		}
	}
	if (dc_GetNGlobalAttrs(dc) != 57 - 25)
	{
		++errors;
		msg_ELog (EF_PROBLEM, "%d global attrs should be %d",
			dc_GetNGlobalAttrs(dc), 57 - 25);
	}
	T_DumpDC (dc);
	/*
	 * Test retrieval of an attribute list
	 */
	msg_ELog (EF_DEBUG, "%s retrieving a key/value list for field atts", 
		  dash);
	keys = dc_GetFieldAttrList(dc, field, NULL, &values, &natts);
	if (natts != dc_GetNFieldAttrs (dc, field))
	{
		++errors;
		msg_ELog (EF_PROBLEM, "getlist returns %d natts != getn %d",
			  natts, dc_GetNFieldAttrs (dc, field));
	}
	buf[0] = '\0';
	for (i = 0; i < natts; ++i)
	{
		dc_GetFieldAttrArray (dc, field, keys[i], &type, &nval);
		sprintf (buf+strlen(buf),
			 "   %s=%s:%d%c", keys[i], (type != DCT_String) ?
			 dc_ElemToString(values[i], type) : (char *)values[i],
			 nval, (i+1)%3 ? ' ':'\n');
	}
	msg_ELog (EF_DEVELOP, "%s", buf);
	msg_ELog (EF_DEVELOP, 
		  "%s keys matching the pattern 'key_.[12345]':", dash);
	keys = dc_GetFieldAttrList(dc, field, "key_.[12345]",
				   &values, &natts);
	buf[0] = '\0';
	for (i = 0; i < natts; ++i)
	{
		dc_GetFieldAttrArray (dc, field, keys[i], &type, &nval);
		sprintf (buf+strlen(buf),
			 "   %s=%s:%d%c", keys[i], (type != DCT_String) ?
			 dc_ElemToString(values[i], type) : (char *)values[i],
			 nval, (i+1)%3 ? ' ':'\n');
	}
	msg_ELog (EF_DEVELOP, "%s", buf);
	/*
	 * Get a keys list
	 */
	keys = dc_GetGlobalAttrKeys(dc, &natts);
	msg_ELog (EF_DEVELOP, 
		  "%s using dc_GetGlobalAttrKeys, %d atts, (getn=%d):", 
		  dash, natts, dc_GetNGlobalAttrs (dc));
	buf[0] = '\0';
	for (i = 0; i < natts; ++i)
		sprintf (buf+strlen(buf),
			 "   %s%c", keys[i], (i+1)%5 ? ' ':'\n');
	msg_ELog (EF_DEVELOP, "%s", buf);

	/*
	 * Now for a clincher: process each attribute list to remove each key
	 */
	msg_ELog (EF_DEBUG, "%s trying to remove all attributes...", dash);
	ad.dc = dc;
	ad.sample = 0;
	ad.field = field;
	ad.which = Global;
	dc_ProcessAttrArrays (dc, NULL, T_RemoveAttr, (void *)&ad);
	ad.which = Sample;
	dc_ProcSampleAttrArrays (dc, 0, NULL, T_RemoveAttr, (void *)&ad);
	ad.which = Field;
	dc_ProcFieldAttrArrays (dc, field, NULL, T_RemoveAttr, (void *)&ad);
	T_DumpDC (dc);
	msg_ELog (EF_DEBUG,
		  "%s Attributes with zero values and empty strings:",
		  dash);
	dc_SetGlobalAttrArray (dc, "global_flag", DCT_Unknown, 0, NULL);
	dc_SetGlobalAttr (dc, "global_empty", "");
	T_DumpDC (dc);
	msg_ELog (EF_DEBUG, "%s Deleting empty- and zero-valued global atts",
		  dash);
	ad.which = Global;
	dc_ProcessAttrArrays (dc, NULL, T_RemoveAttr, (void *)&ad);
	T_DumpDC (dc);
	/*
	 * Fetch the previously stored file, hopefully including all of the
	 * typed attributes, and dump it
	 */
	msg_ELog (EF_DEBUG, "%s Fetching the attributes from '%s'", 
		  dash, pname);
	errors += !ds_GetFields (plat, when, &nfield, fields);
	dc2 = ds_FetchObs (plat, DCC_Scalar, when, 
			  fields, nfield, NULL, 0);
	if (!dc2)
		++errors;
	else
	{
		T_DumpDC (dc2);
		{ /* DIFF_DATACHUNKS */
		/*
		 * Diff the global attributes
		 */
		i = dc_CmpGlobalAttr (dc, dc2, diffs, sizeof(diffs));
		msg_ELog (EF_DEBUG, "CmpGlobalAttr reports %d differences", i);
		msg_ELog (EF_DEVELOP, "%s", diffs);
		} /* DIFF_DATACHUNKS */
		dc_DestroyDC (dc);

		dc = dc2;
		/*
		 * Test that the character arrays came back as strings
		 */
		msg_ELog (EF_DEBUG,
			  "%s verifying char arrays converted to strings", 
			  dash);
		cptr = dc_GetGlobalAttr(dc, "global_char");
		if (!cptr || strcmp(cptr, cdata))
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
			  "Global att error: '%s' should be string '%s'", 
			  "global_char", cdata);
		}
		cptr = dc_GetFieldAttr(dc, field, "field_char");
		if (!cptr || strcmp(cptr, cdata))
		{
			++errors;
			msg_ELog (EF_PROBLEM, 
				  "Field att '%s' should be string '%s'", 
				  "field_char", cdata);
		}
		dc_DestroyDC (dc);
	}
	msg_ELog (EF_DEBUG, "T_Attributes done.");
	return (errors);
}




TestRoutine AttributeTests[] = 
{
	{ "attributes", FTNetCDF, DCC_None, TR_BEGIN, T_Attributes,
	  "test field, global, and sample attributes interfaces" },
	END_TESTS
};

