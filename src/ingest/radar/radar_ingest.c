/*
 * Ingest radar data and rasterize it.
 */
static char *rcsid = "$Id: radar_ingest.c,v 1.2 1991-04-28 17:38:15 corbet Exp $";

# include <errno.h>
# include <sys/time.h>
# include <sys/resource.h>

# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <ImageXfr.h>
# include <signal.h>
# include "HouseKeeping.h"
# include "radar_ingest.h"
# include "display.h"


/*
 * Define globals here.
 */
int XRes = 800, YRes = 800;
int XRadar = 250, YRadar = 400;
float AzFill = 0.6;
float PixScale = 5.0;		/* Pixels per kilometer		*/
float RadarLat = 0, RadarLon = 0;
int MinSweep = 25;
int GMTOffset = 0;
int NFrames = 2;		/* How many frames		*/
int Niceness = 0;
int WidgetUpdate = 20;
int NBeam = 0, NMissed = 0;


struct _ix_desc *ShmDesc = 0;
int	ImageSet = -1;

# define PF_LEN 80
char PlatformName[PF_LEN];

/*
 * We use this data object to write out finished products.
 */
static DataObject OutData;
ScaleInfo Scale[10];

/*
 * Field info.
 */
# define MFIELD 2
RDest Rd[MFIELD];
int NField = 0;
char *Fields[MFIELD];



static int Argc;
static char **Argv;

# ifdef __STDC__
	static int Dispatcher (int, struct ui_command *);
	static void Go (void);
	static void SetupIndirect (void);
	static void ClearFrames (void);
	static void Source (struct ui_command *);
	static void NewField (struct ui_command *);
# else
	static int Dispatcher ();
	static void Go ();
	static void SetupIndirect ();
	static void ClearFrames ();
	static void Source ();
	static void NewField ();
# endif



static unsigned char *Image[4];

die ()
/*
 * Finish gracefully.
 */
{
	ui_finish ();
	if (ShmDesc)
		IX_Detach (ShmDesc);
	exit (0);
}



main (argc, argv)
int argc;
char **argv;
{
/*
 * Initialize.
 */
	msg_connect (die, "Radar Ingest");
	ui_init ("/fcc/lib/radar_ingest.lf", TRUE, TRUE);
	ui_setup ("radar_ingest", &argc, argv, 0);
	DefineWidgets ();
	SetupIndirect ();
	ds_Initialize ();
	Argc = argc;
	Argv = argv;
/*
 * Time to go in to UI mode.
 */
	ui_get_command ("initial", "Radar>", Dispatcher, 0);
	die ();
}





static void
SetupIndirect ()
/*
 * Create all of the indirect variables which are used to control things.
 */
{
	stbl vtable = usy_g_stbl ("ui$variable_table");

	usy_c_indirect (vtable, "x_resolution", &XRes, SYMT_INT, 0);
	usy_c_indirect (vtable, "y_resolution", &YRes, SYMT_INT, 0);
	usy_c_indirect (vtable, "x_radar", &XRadar, SYMT_INT, 0);
	usy_c_indirect (vtable, "y_radar", &YRadar, SYMT_INT, 0);
	usy_c_indirect (vtable, "radar_lat", &RadarLat, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "radar_lon", &RadarLon, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "azimuth_fill", &AzFill, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "pixels_per_km", &PixScale, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "minimum_sweep", &MinSweep, SYMT_INT, 0);
	usy_c_indirect (vtable, "gmt_offset", &GMTOffset, SYMT_INT, 0);
	usy_c_indirect (vtable, "platform", PlatformName, SYMT_STRING, PF_LEN);
	usy_c_indirect (vtable, "nframes", &NFrames, SYMT_INT, 0);
	usy_c_indirect (vtable, "niceness", &Niceness, SYMT_INT, 0);
	usy_c_indirect (vtable, "update", &WidgetUpdate, SYMT_INT, 0);
}




static int
Dispatcher (junk, cmds)
int junk;
struct ui_command *cmds;
/*
 * The command dispatcher.
 */
{
/*
 * See what they wanted.
 */
	switch (UKEY (*cmds))
	{
	/*
	 * Time to actually do things.
	 */
	   case RIC_GO:
	   	Go ();
		break;
	/*
	 * Input definition.
	 */
	   case RIC_SOURCE:
	   	Source (cmds + 1);
		break;
	/*
	 * Fields.
	 */
	   case RIC_FIELD:
	   	NewField (cmds + 1);
		break;
	/*
	 * Time to complain.
	 */
	   default:
	   	msg_ELog (EF_PROBLEM, "Unknown kw %d", UKEY (*cmds));
		break;
	}
	return (TRUE);
}





static void
Source (cmds)
struct ui_command *cmds;
/*
 * Here is a definition of our input source.
 */
{
	if (UKEY (*cmds) == RIC_FILE)
		FileInput (UPTR (cmds[1]));
	else
		NetInput (UPTR (cmds[1]));
}




static void
NewField (cmds)
struct ui_command *cmds;
/*
 * Add another field to the list.
 */
{
/*
 * Make sure we don't have too many.
 */
	if (NField >= MFIELD)
	{
		msg_ELog (EF_PROBLEM, "Too many radar fields");
		return;
	}
/*
 * Now remember the stuff.
 */
	Fields[NField] = usy_string (UPTR (*cmds));
	/* Rd[NField].rd_foffset = UINT (cmds[1]); */
	Rd[NField].rd_foffset = cmds[1].uc_v.us_v_int;
	NField++;
}





static void
Go ()
/*
 * Start really rasterizing.
 */
{
	Beam beam;
	Housekeeping *hk;
	int i, nbeam = 0;
/*
 * Set up our beam input source.
 */
	signal (SIGINT, die);
	SetupInput ();
/*
 * Definition checking.
 */
	if (NField <= 0)
	{
		msg_ELog (EF_EMERGENCY, "No fields given");
		die ();
	}
/*
 * Set up our shared memory segment.
 */
	if (! (ShmDesc = IX_Create (0x910425, XRes, YRes, NField, NFrames,
				Fields)))
	{
		msg_ELog (EF_EMERGENCY, "No shm segment");
		die ();
	}
	IX_LockMemory (ShmDesc);
/*
 * If they have asked for a priority change, try to do it.
 */
	if (Niceness)
		setpriority (PRIO_PROCESS, 0, Niceness);
/*
 * Set up fields until there is a command-based way to do it.
 */
# ifdef notdef
	/* MHR z = 0, v = 1 */
	Rd[0].rd_foffset = 0;	/* Z = 4, v = 2 */
	Rd[1].rd_foffset = 1;
# endif
	beam = GetBeam ();
	hk = beam->b_hk;
	for (i = 0; i < NField; i++)
	{
		Scale[i].s_Scale =
			hk->parm_info[Rd[i].rd_foffset].pi_scale/100.0;
		Scale[i].s_Offset =
			hk->parm_info[Rd[i].rd_foffset].pi_bias/100.0;
	ui_printf ("%s scale %.2f bias %.2f\n", Fields[i], Scale[i].s_Scale,
			Scale[i].s_Offset);
	}
/*
 * Origin setting.
 */
	cvt_Origin (RadarLat, RadarLon);
/*
 * Now plow through the beams.
 */
/*	InitFake (); */
	while (1)
	{
	/*
	 * Get another beam.
	 */
		if (! (beam = GetBeam ()))
		{
			ui_printf ("Get Beam failure!\n");
			exit (1);
		}
	/*
	 * Rasterize it.
	 */
		Rasterize (beam, Rd, 2);
		if ((++nbeam % WidgetUpdate) == 0)
			SetStatus (beam->b_hk);
	}
}





void
OutputSweep (bt, alt, newvol)
time *bt;
float alt;
int newvol;
/*
 * Put this sweep out.
 */
{
	long systime;
	int i;
	RGrid rg;
	Location loc;
/*
 * Radars tend to record in local time; make the move over to GMT now.
 */
	systime = TC_FccToSys (bt) + GMTOffset*3600;
	TC_SysToFcc (systime, bt);
/*
 * Assemble our data object, so that we can send this stuff out to the
 * data store.
 */
	rg.rg_Xspacing = rg.rg_Yspacing = 1.0/PixScale;
	rg.rg_nX = XRes;
	rg.rg_nY = YRes;
	rg.rg_nZ = rg.rg_Zspacing = 0;
	/* Scale info is random! */
	cvt_ToLatLon (-XRadar/PixScale, -YRadar/PixScale, &loc.l_lat,
			&loc.l_lon);
	loc.l_alt = alt;
	IX_SendFrame (ShmDesc, ImageSet, bt, &rg, &loc, Scale, 0, 0, XRes,
			YRes);
	ImageSet = -1;
/*
 * Say something, and make the display show what we've done.
 */
	ui_printf (" Output %s at %d %06d alt %.2f new %c\n", PlatformName,
		bt->ds_yymmdd, bt->ds_hhmmss, alt, newvol ? 't' : 'f');
}





int
BeginSweep ()
/*
 * Get set to do a new sweep.
 */
{
/*
 * If we already have a sweep going, we just clear the frames and start over.
 */
	if (ImageSet >= 0)
	{
		ClearImages ();
		return (TRUE);
	}
/*
 * Otherwise we get a new set.
 */
	if ((ImageSet = IX_GetWriteFrame (ShmDesc, (char **) Image)) < 0)
		return (FALSE);
	Rd[0].rd_image = Image[0];
	Rd[1].rd_image = Image[1];
	/* ClearImages (); */
	return (TRUE);
}






ClearImages ()
{
	int i;

	for (i = 0; i < NField; i++)
		memset (Image[i], 0xff, XRes*YRes); 
}
