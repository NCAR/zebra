/*
 * Test out data chunk routines.
 */
# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "DataChunk.h"


/* ARGSUSED */
int
msg_handler (msg)
struct message *msg;
{
	msg_ELog (EF_INFO, "Message received");
	return (0);
}



unsigned char buf[1024];
static Location Locs[] = {
	{ 11.11, 22.22, 33.33 },
	{ 44.44, 55.55, 66.66 },
	{ 77.77, 88.88, 99.99 },
	{ 10.10, 20.20, 30.30 },
	{ 40.40, 50.50, 60.60 }
};


main ()
{

	msg_connect (msg_handler, "dc tester");
	BndTest ();
	/* MDTest (); */
	/* SCTest (); */
	/* IRTest (); */
	/* CvtTest (); */
}






BndTest ()
{
	DataChunk *dc;
	int i;
	static time t = {911114, 123456 };

	dc = dc_CreateDC (DCC_Boundary);
	dc->dc_Platform = 20;
	dc_SetGlobalAttr (dc, "Funky", "LikeWow");
	dc_SetGlobalAttr (dc, "cell-number", "14");
	dc_SetGlobalAttr (dc, "Phase of moon", "Just past new");
	dc_BndAdd (dc, &t, 20, Locs, 2);
	dc_BndAdd (dc, &t, 20, Locs, 4);
	dc_BndAdd (dc, &t, 20, Locs, 5);
	dc_BndAdd (dc, &t, 20, Locs + 2, 2);

	printf ("Data chunk has %d samples\n", dc_GetNSample (dc));
	for (i = 0; i < 10; i++)
	{
		int len;
		char *dp = (char *) dc_GetSample (dc, i, &len);
		if (! dp)
			break;
		printf ("\tSample %d, len %d at 0x%x, 0x%x\n", i, len, dp, dp[0]);
	}
	dc_DumpDC (dc);
	printf ("Attribute 'Funky' is '%s'\n", dc_GetGlobalAttr (dc, "Funky"));
	dc_DestroyDC (dc);
}





setbuf (buf, len, val)
char *buf;
int len, val;
{
	for (; len > 0; len--)
		*buf++ = val;
}


MDTest ()
{
	DataChunk *dc;
	const int fsize = 100;
	int f;
	static time t = {911114, 123456 };
	static FieldId flds[5] = { 10, 20, 30, 40, 50 };
	static char dbuf[100];

	dc = dc_CreateDC (DCC_MetData);
	dc_SetupUniformFields (dc, 0, 5, flds, fsize);
	for (f = 0; f < 5; f++)
	{
		setbuf (dbuf, fsize, f+10);
		dc_AddMData (dc, &t, flds[f], fsize, 0, 1, dbuf);
	}
	printf ("Data: ");
	for (f = 0; f < 5; f++)
	{
		char *data = dc_GetMData (dc, 0, flds[f], 0);
		printf ("[%d: %d] ", flds[f], data[5]);
	}
	printf ("\n");
	dc_DumpDC (dc);
	dc_DestroyDC (dc);
}




SCTest ()
{
	static time t = {911114, 120000 };
	static FieldId flds[5] = { 10, 20, 30, 40, 50 };
	int i;
	DataChunk *dc = dc_CreateDC (DCC_Scalar);

	dc_SetScalarFields (dc, 2, flds);
	for (i = 0; i < 10; i++)
	{
		float v = i*100.0;
		dc_AddScalar (dc, &t, i, flds[0], &v);
		v *= 10;
		dc_AddScalar (dc, &t, i, flds[1], &v);
		t.ds_hhmmss += 100;
	}
	for (i = 0; i < 10; i++)
		printf ("\tSamp %d, f1 %.2f, f2 %.2f\n", i,
			dc_GetScalar (dc, i, flds[0]),
			dc_GetScalar (dc, i, flds[1]));
	dc_DumpDC (dc);
	dc_DestroyDC (dc);
}



setfbuf (buf, len, value)
float *buf, value;
int len;
{
	for (; len > 0; len--)
		*buf++ = value;
}


IRTest ()
/*
 * Test out irregular grids.
 */
{
	DataChunk *dc;
	int f;
	static time t = {911114, 123456 };
	static FieldId flds[5] = { 10, 20, 30, 40, 50 };
	static PlatformId plats[4] = { 5, 6, 7, 8 };
	float fbuf[4], *data;

	dc = dc_CreateDC (DCC_IRGrid);
	dc_IRSetup (dc, 4, plats, Locs, 5, flds);
	for (f = 0; f < 5; f++)
	{
		setfbuf (fbuf, 4, (float) f+10);
		dc_IRAddGrid (dc, &t, 0, flds[f], fbuf);
	}
	for (f = 0; f < 5; f++)
	{
		data = dc_IRGetGrid (dc, 0, flds[f]);
		printf   ("\t Fld %d, data %.2f\n", flds[f], data[1]);
	}
	dc_DumpDC (dc);
	dc_DestroyDC (dc);
}





CvtTest ()
/*
 * Test out conversion from old data objects.
 */
{
	DataObject *dobj;
	DataChunk *dc;
	PlatformId pid;
	static char *fields[] = { "tdry" };
	static time begin = { 910802, 183000 };
	static time end = { 910802, 183500 };
	DataChunk *ConvertDObj ();

/*
 * Initialize.
 */
	usy_init ();
	ds_Initialize ();
	F_Init ();
	F_DeclareField ("tdry", "Temperature", "dg c");
/*
 * Grab an old-style data object.
 */
	pid = ds_LookupPlatform ("mesonet");
	if ((dobj = ds_GetData (pid, fields, 1, &begin, &end, OrgIRGrid,
			99.99, 99.99)) == NULL)
	{
		msg_ELog (EF_PROBLEM, "ds_GetData failed");
		return;
	}
/*
 * Now convert it to the new style.
 */
	dc = ConvertDObj (dobj);
	dc_DumpDC (dc);
	dc_DestroyDC (dc);
	ds_FreeDataObject (dobj);
}
