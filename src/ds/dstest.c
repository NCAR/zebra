/*
 * Test various data store routines.
 */
/*		Copyright (C) 1987,88,89,90,91 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

# include "defs.h"
# include "message.h"
# include <copyright.h>
# include "DataStore.h"
MAKE_RCSID ("$Id: dstest.c,v 3.2 1993-07-01 20:12:58 granger Exp $")


msg_handler ()
{ }




main (argc, argv)
int argc;
char **argv;
{
	int i, pid, np, nf;
	DataObject *data;
	DataChunk *dc, *rdc;
	static char *field = "velocity";
	ZebTime begin, end, ts[5], rdt;
	UItime kludge;
	Location locs[5];
	char atime[40], *fields[20];
	FieldId fid, fidlist[30];
	PlatformId bnd, tbnd, trast, mesonet, plane;

	msg_connect (msg_handler, "DSTest");
	usy_init ();
	if (! ds_Initialize ())
		exit (1);
/*
 * Basic platform lookup.
 */
	if ((pid = ds_LookupPlatform ("ddop")) == BadPlatform)
	{
		printf ("Lookup failure on 'ddop");
		exit (1);
	}
	printf ("\nddop", pid);
/*
 * Serious kludge.
 */
	F_DeclareField ("fstrength", "Electric field strength", "v/m");
	F_DeclareField ("tdry", "Temperature", "deg c");
	F_DeclareField ("reflectivity", "Radar reflectivity", "dbz");
/*
 * Field lookup.
 */
	kludge.ds_yymmdd = 911802;
	kludge.ds_hhmmss = 220300;
	TC_UIToZt (&kludge, &begin);
	nf = 10;
 	ds_GetFields (pid, &begin, &nf, fidlist);
	printf ("%d fields:", nf);
	for (i = 0; i < nf; i++)
		printf (" %d %s", fidlist[i], F_GetName (fidlist[i]));
	printf ("\n");
	/* fid = F_Lookup (fields[0]); */
	fid = fidlist[0];
/*
 * Get some sample info.
 */
	np = ds_GetObsSamples (pid, &begin, ts, locs, 5);
	printf ("ds_GetObsSamples: Got %d samples\n", np);
	for (i = 0; i < np; i++)
	{
		TC_EncodeTime (ts + i, TC_Full, atime);
		printf ("\t%d: %s, alt %.2f\n", i, atime, locs[i].l_alt);
	}
/*
 * Obs times.
 */
	np = ds_GetObsTimes (pid, &begin, ts, 5, 0);
	printf ("ds_GetObsTimes (no attr): Got %d times\n", np);
	for (i = 0; i < np; i++)
	{
		TC_EncodeTime (ts + i, TC_Full, atime);
		printf ("\t%d: %s\n", i, atime);
	}
	np = ds_GetObsTimes (pid, &begin, ts, 5, "sur");
	printf ("ds_GetObsTimes (attr sur): Got %d times\n", np);
	for (i = 0; i < np; i++)
	{
		TC_EncodeTime (ts + i, TC_Full, atime);
		printf ("\t%d: %s\n", i, atime);
	}
/*
 * Just plain times.
 */
	printf ("%d DataTimes: ", ds_DataTimes (pid, &begin, 5, DsBefore, ts));
	for (i = 0; i < 5; i++)
	{
		TC_EncodeTime (ts + i, TC_TimeOnly, atime);
		printf ("%s ", atime);
	}
	printf ("\n");
/*
 * Print out some data chunks.
 */
	rdt = ts[0];
	/* rdc = dc = ds_Fetch (pid, DCC_RGrid, ts, ts, &fid, 1, 0, 0); */
	/* dc_DumpDC (dc); */
	printf ("\n");

	mesonet = ds_LookupPlatform ("mesonet/2");
	np = ds_GetObsTimes (mesonet, &begin, ts, 5, 0);
	fid = F_Lookup ("tdry");
	dc_DumpDC (ds_Fetch (mesonet, DCC_Scalar, ts, ts + 1, &fid, 1, 0, 0));

	plane = ds_LookupPlatform ("n312d");
	end = begin = ts[0];
	end.zt_Sec += 500;
	fid = F_Lookup ("trans");
	dc_DumpDC (ds_Fetch (plane, DCC_Location, &begin, &end, 0, 0, 0, 0));
	exit (0);	/*-------------------------------------------------*/
/*
 * Now on to the storage side.  Find our platform and clean out anything
 * that is there now.
 */
	printf ("\n\n");
	tbnd = ds_LookupPlatform ("test");
	printf ("Test platform is %d, old %d\n", tbnd, bnd);
	ds_DeleteData (tbnd, 0);
	sleep (2);
/*
 * Get times on the source boundary platform.
 */
	printf ("%d bnd DataTimes: ",
		ds_DataTimes (bnd, &begin, 5, DsBefore, ts));
	for (i = 0; i < 5; i++)
	{
		TC_EncodeTime (ts + i, TC_TimeOnly, atime);
		printf ("%s ", atime);
	}
	printf ("\n");
/*
 * Pull a boundary out of the existing dataset and store it in the new one.
 */
	dc = ds_Fetch (bnd, DCC_Boundary, ts + 2, ts + 2, 0, 0, 0, 0);
	dc->dc_Platform = tbnd;
	printf ("Store ret %d\n", ds_Store (dc, TRUE, 0, 0));
	dc_DestroyDC (dc);
/*
 * Append case.
 */
	dc = ds_Fetch (bnd, DCC_Boundary, ts, ts, 0, 0, 0, 0);
	dc->dc_Platform = tbnd;
	printf ("Store2 ret %d\n", ds_Store (dc, FALSE, 0, 0));
	dc_DestroyDC (dc);
/*
 * Now the insert case.
 */
	dc = ds_Fetch (bnd, DCC_Boundary, ts + 1, ts + 1, 0, 0, 0, 0);
	dc->dc_Platform = tbnd;
	printf ("Store3 ret %d\n", ds_Store (dc, FALSE, 0, 0));
	dc_DestroyDC (dc);
/*
 * Tests with the raster file format.
 */
	trast = ds_LookupPlatform ("raster");
	ds_DeleteData (trast, 0);
	rdc->dc_Platform = trast;
	dc_SetGlobalAttr (rdc, "testing", "true");
	printf ("Radar store ret %d\n", ds_Store (rdc, FALSE, 0, 0));
/*
 * Try to fetch back the radar chunk.
 */
	rdc = ds_Fetch (trast, DCC_Image, &rdt, &rdt, &fid, 1, 0, 0);
	printf ("TRast dc dump:\n");
	dc_DumpDC (rdc);
	exit (0);
}

