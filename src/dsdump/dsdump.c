/*
 * Data Store dumpout.
 */
# include "../include/defs.h"
# include "../include/message.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"


msg_handler ()
{ }




main ()
{
	int i, pid, np;
	DataObject *data;
	static char *field = "velocity";
	time begin, end, ts[5];
	Location locs[5];

	msg_connect (msg_handler, "DSDump");
	usy_init ();
	if (! ds_Initialize ())
		exit (1);
	dsm_ShmLock ();
	dsm_Dump ();
	for (i = 0; i < SHeader->sm_nPlatform; i++)
		dump_platform (PTable + i);
	dsm_ShmUnlock ();
	exit (0);





/*
 * Try a data get.
 */
	if ((pid = ds_LookupPlatform ("cp4")) == BadPlatform)
	{
		ui_printf ("Lookup failure on 'cp4\n");
		exit (1);
	}
	ui_printf ("\ncp4 PID is %d\n", pid);
	begin.ds_yymmdd = end.ds_yymmdd = 910312;
	begin.ds_hhmmss = 73210;
	end.ds_hhmmss = 73210;
	data = ds_GetData (pid, &field, 1, &begin, &end, OrgImage,
		0.0, 99.9);
/*
 * Get some sample info.
 */
	np = ds_GetObsSamples (pid, &begin, ts, locs, 5);
	ui_printf ("Got %d samples\n", np);
	for (i = 0; i < np; i++)
		ui_printf ("\t%d: %d %06d, alt %.2f\n", i, ts[i].ds_yymmdd,
			ts[i].ds_hhmmss, locs[i].l_alt);
	exit (0);
/*
 * Print it out.
 */
	ui_printf ("%d grid points\n", np = data->do_desc.d_irgrid.ir_npoint);
	ui_printf ("%d plain points\n", data->do_npoint);
	for (i = 0; i < 10; i++)
		ui_printf ("pres[%02d] = %7.2f at %d %d\n", i,
			data->do_data[0][i], data->do_times[i].ds_yymmdd,
			data->do_times[i].ds_hhmmss);
/*
 * Try times. 
 */
 	pid = ds_LookupPlatform ("mesonet");
 	begin.ds_hhmmss = 221000;
	printf ("%d times: ", ds_DataTimes (pid, &begin, 5, DsBefore, ts));
	for (i = 0; i < 5; i++)
		printf ("%06d ", ts[i].ds_hhmmss);
	printf ("\n");
	field = "tdry";
	data = ds_GetData (pid, &field, 1, ts, ts, Org2dGrid, 3.0, 99.9);

	if ((pid = ds_LookupPlatform ("kingair")) == BadPlatform)
		printf ("Kingair platform unknown\n");
	else
	{
		field = "temperature";
		begin.ds_hhmmss = 215000;
		end.ds_hhmmss = 220000;
		data = ds_GetData (pid, &field, 1, &begin, &end, OrgScalar,
			0.0, 9999.9);
	}
}



static struct fname
{
	int	flag;
	char	*name;
} Flags[] =
{
	{	DPF_MOBILE,	"mobile"	},
	{	DPF_COMPOSITE,	"composite"	},
	{	DPF_DISCRETE,	"discrete"	},
	{	DPF_REGULAR,	"regular"	},
	{	DPF_SUBPLATFORM, "subplatform"	},
	{	DPF_REMOTE,	"remote-dir"	},
};

# define NFLAG (sizeof (Flags)/sizeof (struct fname))



dump_platform (p)
Platform *p;
{
	int i;

	if (p->dp_flags & DPF_SUBPLATFORM)
		return;

	ui_printf ("Platform '%s', dir '%s'\n\tFlags 0x%x ( ", p->dp_name,
		p->dp_dir, p->dp_flags);
	for (i = 0; i < NFLAG; i++)
		if (p->dp_flags & Flags[i].flag)
			ui_printf ("%s ", Flags[i].name);
	ui_printf (")\n");
	if (! (p->dp_flags & DPF_SUBPLATFORM))
	{
		dumpchain ("L", p->dp_LocalData);
		if (p->dp_flags & DPF_REMOTE)
			dumpchain ("R", p->dp_RemoteData);
	}
}




dumpchain (which, start)
char *which;
int start;
/*
 * Dump out a datafile chain.
 */
{
	DataFile *dp;


	while (start)
	{
		dp = DFTable + start;
		ui_printf ("  %s %2d/%d '%s' %d %d -> %d [%d]\n", which,
			start, dp->df_use, dp->df_name,
			dp->df_begin.ds_yymmdd, dp->df_begin.ds_hhmmss,
			dp->df_end.ds_hhmmss, dp->df_nsample);
		start = dp->df_FLink;
	}
}
