
# include <defs.h>
# include <config.h>
# include <message.h>
# include "DataStore.h"

# define ds_PlatformDataOrg(id) (Org2dGrid)

# include "DFA_HDF.c"


/*
 * Test the HDF interface routines
 */

static void
dh_DumpTag (fname, tag)
char *fname;
Htag *tag;
{
	int i;
# 	define Show(fld,fmt) \
	printf ("%15s: " fmt, #fld, tag->h_##fld )

	printf ("Tag '%s'\n", fname);
	Show (fid,"%d");
	Show (vgid,"%d");
	Show (plat,"%d");
	printf ("\n");
	printf ("%15s: %s\n", "time", TC_AscTime (&tag->h_time, TC_Full));
	Show (center.l_lat,"%6.2f");
	Show (center.l_lon,"%6.2f");
	Show (center.l_alt,"%6.2f");
	printf ("\n");
	Show (nVar,"%d");
	Show (org,"%d");
	Show (pixel,"%f");
	printf ("\n");
	for (i = 0; i < tag->h_nVar; ++i)
	{
		printf ("%15s: '%s' (%s)", F_GetName (tag->h_Fids[i]),
		   F_GetDesc (tag->h_Fids[i]), F_GetUnits (tag->h_Fids[i]));
		if (i % 2) printf ("\n");
	}
	printf ("\n");
#	undef Show
}


int
main (argc, argv)
int argc;
char *argv[];
{
	int i;
	ZebTime begin, end;
	char ctime[64];
	int nsamp;
	int ret = 0;
	DataFile df;
	OpenFile *ofp;
	DataChunk *dc;
	Htag *tag;

	df.df_rev = 0;
	df.df_inode = 0;
	df.df_index = 0;
	df.df_flags = 0;
	df.df_ftype = 42;
	df.df_FLink = 0;
	df.df_BLink = 0;

	msg_connect (NULL, "hdftest");
	F_Init ();
	/*
	 * Expect a list of HDF files to test on the command line.
	 * Query each for a time and print the results, then open
	 * each and dump the tag before closing them.
	 */
	ofp = (OpenFile *) malloc (hdfFormat->f_of_size);
	tag = HTAGP (ofp);
	for (i = 1; i < argc; ++i)
	{
		ret |= dh_QueryTime (argv[i], &begin, &end, &nsamp);
		TC_EncodeTime (&begin, TC_Full, ctime);
		printf ("%s (%d samples): %s to %s\n", argv[i],
			nsamp, ctime, TC_AscTime (&end, TC_Full));
		/*
		 * Create a dummy datafile structure
		 */
		df.df_platform = 1234;
		strcpy (df.df_name, argv[i]);
		df.df_nsample = nsamp;
		df.df_begin = begin;
		df.df_end = end;

		/*
		 * Now try to open the file
		 */
		if (! dh_OpenFile (ofp, df.df_name, &df, /*write*/ 0))
		{
			printf ("%s: open failed.\n", df.df_name);
			continue;
		}
		/*
		 * Dump the tag
		 */
		dh_DumpTag (argv[i], tag);

		/*
		 * Test the setup and getdata methods and dump the datachunk
		 */
		dc = dh_Setup (ofp, tag->h_Fids, tag->h_nVar, DCC_RGrid);
		if (! dc)
		{
			msg_ELog (EF_PROBLEM, "setup failed");
		}
		else
		{
			dh_GetData (ofp, dc, 0, 1, NULL, 0);
			dc_DumpDC (dc);
			dc_DestroyDC (dc);
		}
		dh_CloseFile (ofp);	
	}
	free (ofp);
	return (ret);
}


