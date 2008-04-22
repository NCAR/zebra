
#include "DFA_Zebra.c"
#include "apple.h"

/*
 * The first function tests the low level free block algorithms.
 * 
 * Situations we need to test:
 * 	freeing and getting 4-byte free blocks
 *	freeing and getting 4-byte blocks at the magic offset
 *	using blocks which occur at the end of the file to partially fill
 *		a request
 *	truncating file size when large free blocks exist at end of file
 *	
 */

static void
zn_DumpFL (tag, hdr)
znTag *tag;
zn_Header *hdr;
/*
 * Dump out the free list.
 */
{
	long block;
	zn_Free fb;
	int i = 0;

	printf ("   ffree %d, nfree %d, nfreeb %d len %d\n", 
	   hdr->znh_Free, hdr->znh_NFree, hdr->znh_NFreeB, hdr->znh_Len);
	for (block = hdr->znh_Free; block > 0; block = fb.znf_Next)
	{
		zn_GetFreeBlock (tag, block, &fb);
		printf ("   %2d at %7ld size %5d next %7d\n", i++,
			block, fb.znf_Size, fb.znf_Next);
	}
	printf ("\n");
}


static int
CheckFree (tag, ffree, nfree, nfb, len)
znTag *tag;
{
	int errors = 0;

	zn_Header *hdr = &tag->zt_Hdr;
	errors += (hdr->znh_Free != ffree) ? 1 : 0;
	errors += (hdr->znh_NFree != nfree) ? 1 : 0;
	errors += (hdr->znh_NFreeB != nfb) ? 1 : 0;
	errors += (hdr->znh_Len != len) ? 1 : 0;
	if (errors)
		msg_ELog (EF_PROBLEM, "znf free space check failed");
	return (errors);
}



static void
zn_TestFreeSpace (tag, offset, len)
znTag *tag;
long offset;
int len;
{
	msg_ELog (EF_DEBUG, "Freeing %6d at %6li ... ", len, offset);
	zn_FreeSpace (tag, offset, len);
	if (DumpDataChunks)
		zn_DumpFL (tag, &tag->zt_Hdr);
}


static long
zn_TestGetSpace (tag, len)
znTag *tag;
int len;
{
	long offset;

	offset = zn_GetSpace (tag, len);
	msg_ELog (EF_DEBUG, "Getting %6d ... offset returned: %6li", 
		  len, offset);
	if (DumpDataChunks)
		zn_DumpFL (tag, &tag->zt_Hdr);
	return (offset);
}



static int
zn_TestFreeBlocks (fname)
char *fname;
{
	znTag _tag;
	znTag *tag = &_tag;
	zn_Header *hdr = &(tag->zt_Hdr);
	long offsets[20];
	int base;
	int errors = 0;
	int i;

	hdr->znh_NFreeB = hdr->znh_NFree = 0;
	hdr->znh_Free = -1;
	hdr->znh_Len = sizeof(zn_Header);

	if ((tag->zt_Fd = open (fname, O_RDWR|O_TRUNC|O_CREAT, 0666)) <0)
	{
		msg_ELog (EF_PROBLEM, "Can't create file %s (%d)",
			fname, errno);
		return (++errors);
	}
	msg_ELog (EF_DEBUG, "ZNF testing: created file '%s'", fname);
/*
 * Initialize the file tag.
 */
	tag->zt_Sync = SF_HEADER;
	tag->zt_Sample = 0; tag->zt_Fids = 0;
	tag->zt_Fields = 0; tag->zt_Ids = 0; tag->zt_Locs = 0;
	tag->zt_Write = TRUE;
	tag->zt_Append = FALSE;
	tag->zt_Attr = 0;
	tag->zt_GlAttr = 0;
	tag->zt_Rg = 0;
/*
 * Header initialization.
 */
	hdr->znh_Magic = ZN_MAGIC;
	hdr->znh_Free = -1;
	hdr->znh_Len = hdr->znh_NFree = hdr->znh_NFreeB = 0;
	hdr->znh_NSample = hdr->znh_NField = 0;
	hdr->znh_Org = OrgScalar;
	hdr->znh_OffLoc = -1;
	hdr->znh_OffGlAttr = -1;
	hdr->znh_OffAttr = -1;
	hdr->znh_GlAttrLen = 0;
	hdr->znh_OffRg = -1;
	hdr->znh_Version = ZN_VERSION;
	hdr->znh_AltUnits = ZAU_KMMSL;
/*
 * Allocate the space for the header itself
 */
	(void) zn_TestGetSpace (tag, sizeof (zn_Header));
	base = sizeof (zn_Header);
/*
 * Get some 4-byte blocks
 */
	for (i = 0; i < 5; ++i)
		offsets[i] = zn_TestGetSpace (tag, 4);
	errors += CheckFree (tag, -1, 0, 0, base+20);
/*
 * Free the blocks, and watch them coalesce into a big block!
 */
	for (i = 0; i < 5; ++i)
		zn_TestFreeSpace (tag, offsets[i], 4);
	errors += CheckFree (tag, base, 1, 20, base+20);
/*
 * Truncate the free space by allocating a big block and then
 * freeing it.  The existing space and the big block should merge
 * and the file should be truncated.
 */
	(void)zn_TestGetSpace (tag, 10000);
	TX_Expect (EF_ALL, FALSE, "truncating");
	zn_TestFreeSpace (tag, offsets[0], 10000);
	TX_Seen();
	errors += CheckFree (tag, -1, 0, 0, base);
/*
 * Get some 4-byte blocks
 */
	for (i = 0; i < 5; ++i)
		offsets[i] = zn_TestGetSpace (tag, 4);
/*
 * Free the blocks backwards this time
 */
	for (i = 4; i >= 0; --i)
		zn_TestFreeSpace (tag, offsets[i], 4);
	(void)zn_TestGetSpace (tag, 10000);
	zn_TestFreeSpace (tag, offsets[0], 10000);
/*
 * Try the more difficult 8-byte blocks
 */
	for (i = 0; i < 5; ++i)
		offsets[i] = zn_TestGetSpace (tag, 8);
/*
 * Free the blocks backwards
 */
	for (i = 4; i >= 0; --i)
		zn_TestFreeSpace (tag, offsets[i], 8);
/*
 * This should produce a warning about freeing past end of file,
 * but things should still work.
 */
	(void)zn_TestGetSpace (tag, 10000);
	TX_Catch ("znf WaRnInG.*free past end of file");
	zn_TestFreeSpace (tag, offsets[0], 10020);
	errors += TX_Caught();
	msg_ELog (EF_DEBUG, "Resetting NFreeB to 0 ...");
	hdr->znh_NFreeB = 0;
/*
 * Get a whole bunch of space, parcel it into 12-byte free blocks, then
 * retrieve 8-byte blocks from each of the free blocks.
 */
	offsets[0] = zn_TestGetSpace (tag, 80);
	for (i = 0; i < 5; ++i)
		zn_TestFreeSpace (tag, offsets[0] + (i * 16), 12);
	errors += CheckFree (tag, base, 5, 60, base+80);
	for (i = 0; i < 5; ++i)
		offsets[i+1] = zn_TestGetSpace (tag, 8);
	errors += CheckFree (tag, base, 5, 20, base+80);
/*
 * Now grab 4-byte blocks
 */
	for (i = 0; i < 5; ++i)
		offsets[6+i] = zn_TestGetSpace (tag, 4);
	errors += CheckFree (tag, -1, 0, 0, base+80);
/*
 * That leaves one 4-byte at the end of the file, which we want to
 * subsume in a block that won't fit anywhere else.
 */
	msg_ELog (EF_DEBUG, "File length should increase by 36 bytes:");
	offsets[11] = zn_TestGetSpace (tag, 36);
	errors += CheckFree (tag, -1, 0, 0, base+116);
/*
 * Now we have 116 bytes following the header.  Get 9884 to bring total
 * to 10000, then free it and watch file truncate itself.
 */
	offsets[12] = zn_TestGetSpace (tag, 9884);
	TX_Expect (EF_ALL, FALSE, "truncating");
	zn_TestFreeSpace (tag, offsets[0], 10000);
	TX_Seen();
	msg_ELog (EF_DEBUG, "Should be zero free blocks after last free.");
	errors += CheckFree (tag, -1, 0, 0, base);
/*
 * Get a 4-byte block, try to free it as two 2-byte blocks.
 */
	offsets[0] = zn_TestGetSpace (tag, 4);
	TX_ExpectMany (EF_ALL, TRUE, 2, "free block size.*too small");
	zn_TestFreeSpace (tag, offsets[0], 2);
	zn_TestFreeSpace (tag, offsets[0]+2, 2);
	errors += TX_Caught();
/*
 * Test blocks at ZN_FREE_MAGIC offset.
 */
	msg_ELog (EF_DEBUG, "Testing 4-byte blocks near magic offset...");
	offsets[1] = zn_TestGetSpace (tag, ZN_FREE_MAGIC + 256);
	for (i = 0; i < 5; ++i)
		zn_TestFreeSpace (tag, ZN_FREE_MAGIC - 24 + (12 * i), 8);
	for (i = 0; i < 5; ++i)
		zn_TestFreeSpace (tag, ZN_FREE_MAGIC - 24 +(12*i)+ 8 , 4);
/*
 * Good sign if we get here without any problems.
 * Free that last block (which should result in truncation), and include
 * the 4-bytes that got left by the 2-byte frees above, then call it quits.
 */
	TX_Expect (EF_ALL, FALSE, "truncating");
	zn_TestFreeSpace (tag, ZN_FREE_MAGIC - 24 + 60,
			  (offsets[1] + ZN_FREE_MAGIC + 256)
			  - (ZN_FREE_MAGIC - 24 + 60));
	zn_TestFreeSpace (tag, offsets[0], 
			  (ZN_FREE_MAGIC - 24) - offsets[1] + 4);
	TX_Seen();
	errors += CheckFree (tag, -1, 0, 0, base);
	msg_ELog (EF_DEBUG, "ZNF Free Blocks test completed.");
	return (errors);
}


/*
 * Begin the testing module routines
 */


static int
T_Dedit (begin, platid)
ZebTime begin;
PlatformId platid;
{
	FieldId fields[30];
	int nfield = 30;
	DataChunk *dc;
	int i;
	char attval[128];
	int err = 0;

	Announce("dedit read and overwrite simulation...");
	err += !ds_GetFields (platid, &begin, &nfield, fields);
	dc = ds_FetchObs (platid, DCC_Scalar, &begin, fields, nfield, NULL, 0);
	if (! dc)
		return (++err);
	/*
	 * Add some sample attributes
	 */
	for (i = 0; i < dc_GetNSample(dc); i += 5)
	{
		sprintf (attval, "sample %d", i);
		dc_SetSampleAttr (dc, i, "key", attval);
	}

	/* 
	 * now store it all back, overwriting what's already there
	 */
	ds_StoreBlocks (dc, FALSE, 0, 0);
	dc_DestroyDC(dc);

	/*
	 * Now do the fetch/store cycle several times to make sure it
	 * doesn't mess up the reserve space
	 */
	msg_ELog (EF_DEBUG, "performing repeated fetch/store cycles");
	for (i = 0; i < 50; ++i)
	{
		dc = ds_FetchObs (platid, DCC_Scalar, &begin, fields, 
				  nfield, NULL, 0);
		if (dc)
		{
			ds_StoreBlocks (dc, FALSE, 0, 0);
			dc_DestroyDC(dc);
		}
		else
			++err;
	}
	return (err);
}



static int
T_Nexus (begin)
ZebTime *begin;
/*
 * The idea is this: create single-sample DataChunks and store them
 * individually to a single ZNF file, then fetch a DataChunk for the entire
 * file, then overwrite the entire file, and see what's so slow about it.
 * Halfway through, rewrite the current contents of the file and include
 * some sample attributes (ala dedit).
 */
{
	DataChunk *dc;
	char *pname = "t_virtual";
	PlatformId plat_id;
	FieldId fields[40];
	int i, n, fld;
	float value;
	Location loc;
	ZebTime when;
	dsDetail details[5];
	int ndetail;
	char buf[128];
	int errors = 0;

	when = *begin;
	plat_id = NeedPlatform(pname);
	n = 0;
	fields[n] = F_Lookup("pres"); ++n;
	fields[n] = F_Lookup("rh"); ++n;
	fields[n] = F_Lookup("uwind"); ++n;
	fields[n] = F_Lookup("vwind"); ++n;
	fields[n] = F_Lookup("wspd"); ++n;
	fields[n] = F_Lookup("wdir"); ++n;
	fields[n] = F_Lookup("temp"); ++n;
	fields[n] = F_Lookup("dpt"); ++n;
	fields[n] = F_Lookup("tdry"); ++n;
	fields[n] = F_Lookup("twet"); ++n;
	fields[n] = F_Lookup("Qpres"); ++n;
	fields[n] = F_Lookup("Qtdry"); ++n;
	fields[n] = F_Lookup("Qu"); ++n;
	fields[n] = F_Lookup("Qv"); ++n;
	fields[n] = F_Lookup("alt"); ++n;
	fields[n] = F_Lookup("theta"); ++n;
	fields[n] = F_Lookup("range"); ++n;
	fields[n] = F_Lookup("azimuth"); ++n;
	fields[n] = F_Lookup("elev"); ++n;
	fields[n] = F_Lookup("latitude"); ++n;
	fields[n] = F_Lookup("longitude"); ++n;
	fields[n] = F_Lookup("ascent"); ++n;
	fields[n] = F_Lookup("mr"); ++n;
	loc.l_lat = 40.0;
	loc.l_lon = -120.0;
	loc.l_alt = 1600.0;
	ndetail = 0;
	ds_SetDetail (DD_ZN_APPEND_SAMPLES, details, ndetail++);

	/*
	 * Set some optimization parameters
	 */
	dc_CheckClass (FALSE);

	/* now begin creating a single sample DataChunk and storing it */
	Announce ("Nexus simulation...");
	sprintf (buf, "creating '%s' observation of 500 samples, 1 at a time",
		 pname);
	msg_ELog (EF_DEBUG, buf);
	for (i = 0; i < 500; ++i)
	{
		dc = dc_CreateDC (DCC_Scalar);
		dc->dc_Platform = plat_id;
		dc_SetScalarFields (dc, n, fields);
		dc_SetBadval (dc, 999.9);
		/* dc_SetGlobalAttr (dc, "global_key", "global_value"); */

		for (fld = 0; fld < n; ++fld)
		{
			value = i*10.0 + fld*0.1;
			dc_AddScalar (dc, &when, 0, fields[fld], &value);
		}
		dc_SetLoc (dc, 0, &loc);

		/*
		 * For the first store, which creates the file, we have a
		 * few extra details to send.
		 */
		if (i == 0)
		{
			ds_SetIntDetail (DD_ZN_HINT_NSAMPLES, 1000, details,
					 ndetail++);
			/* Reserve space for 100 sample atts of 50 bytes */
			ds_SetIntDetail (DD_ZN_RESERVE_BLOCK, 100*50, details,
					 ndetail++);
			errors += !ds_StoreBlocks (dc, TRUE, details, ndetail);
			ndetail -= 2;
		}
		else
			errors += !ds_StoreBlocks(dc, FALSE, details, ndetail);
		dc_DestroyDC(dc);
		loc.l_alt += 5.0;
		when.zt_Sec += 4;

		/*
		 * If we're halfway, simulate interjection by dedit
		 */
		if (i == 250)
			errors += T_Dedit (*begin, plat_id);
	}

	/* now have a file of 500 samples, try to fetch the whole thing */
	msg_ELog (EF_DEBUG, 
		  "fetching the entire observation from '%s'...", pname);
	dc = ds_FetchObs (plat_id, DCC_Scalar, begin, fields, n, NULL, 0);
	if (!dc)
		return (++errors);
	T_DumpDC (dc);
	/* now store it all back, overwriting what's already there; this
	 * is so that we can verify the fetch through what's in the file */
	msg_ELog (EF_DEBUG, 
		  "overwriting observation with fetched datachunk...");
	errors += !ds_StoreBlocks (dc, FALSE, details, ndetail);
	msg_ELog (EF_DEBUG, "Done.");
	dc_DestroyDC(dc);
	return (errors);
}



static int
T_ZnfBlocks ()
{
	int errors = 0;
	/* Call znf free block testing routine */
	Announce ("Testing ZNF low-level free blocks, file 'test.znf'");
	errors += zn_TestFreeBlocks ("test.znf");
	fflush (stdout);
	fflush (stderr);
	return (errors);
}



TestRoutine ZNFTests[] = 
{
	{ "znfblocks", FTZebra, DCC_None, TR_BEGIN, T_ZnfBlocks,
	  "test low-level znf free blocks algorithms" },
	{ "nexus", FTZebra, DCC_Scalar, TR_BEGIN, T_Nexus,
	  "nexus simulation of dedit halfway through 500 samples" },
	END_TESTS
};



