/*
 * Ingest radar data and rasterize it.
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

# include <errno.h>
# include <sys/time.h>
# include <sys/resource.h>

/*
 * Solaris priority weirdness.
 */
# if defined(sun) && defined(SVR4)
#  include <sys/types.h>
#  include <sys/procset.h>
#  include <sys/priocntl.h>
#  include <sys/rtpriocntl.h>
#  include <sys/tspriocntl.h> /* maybe we don't need this one? */
static void SetRealTime ();
# endif

# include <ui.h>
# include <config.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <ImageXfr.h>
# include <signal.h>
# include "HouseKeeping.h"
# include "radar_ingest.h"
# include "display.h"
# include "BeamBuffer.h"

RCSID("$Id: radar_ingest.c,v 2.15 1996-12-10 20:50:53 granger Exp $")

/*
 * Define globals here.
 */
int XRes = 800, YRes = 800;
int XRadar = 250, YRadar = 400;
float AzFill = 0.6;
float PixScale = 5.0;		/* Pixels per kilometer		*/
float RadarLat = 0, RadarLon = 0;
float ElTolerance = 1.0;	/* Elevation difference tolerance, deg. */
int MinSweep = 25;
int MinRHI = 20;
int GMTOffset = 0;
int NFrames = 2;		/* How many frames		*/
int Niceness = 0;
int WidgetUpdate = 20;
int NBeam = 0, NMissed = 0;
bool Project = TRUE;
/* bool MhrMode = FALSE; */
bool ForceRealTime = TRUE;	/* Force data times to real time?	*/
float MhrTop = 21.0;
RadarFormat RFormat = RF_CBAND;		/* The format of our data */
int Using_BB = FALSE;		/* Doing beam buffering? 	*/
bool Reinitialize = TRUE;

/*
 * Stuff for forcing a minimum time interval between beams.  Useful when
 * testing off of a tape.
 */
float BeamDelay = 0.0;	/* minimum interval in seconds */
bool OKToGetBeam;

/*
 * Are we hacking out RHI limits?
 */
static bool DumpLimits = FALSE;
static PlatformId LimitPID;

/*
 * Thresholding.
 */
bool DoThresholding = FALSE;
int ThrFldOffset;
unsigned char ThrCounts = 0;
int SMinusXThresh = 12;	/* Just for CP2 in SCMS */

/*
 * Do we trust internal flags?
 */
bool TrustSweep = FALSE;
bool TrustVol = FALSE;

struct _ix_desc *ShmDesc = 0;
int	ImageSet = -1;

# define PF_LEN 80
char PlatformName[PF_LEN];

/*
 * We use this data object to write out finished products.
 */
ScaleInfo Scale[MFIELD];

/*
 * Who consumes our data.
 */
static char Consumer[200];
static char *CArgs[20];
static int NCArg = 0;
static bool CSet = FALSE;
static int CPid;		/* It's process ID	*/

/*
 * Field info.
 */
RDest Rd[MFIELD];
int NField = 0;
char *Fields[MFIELD];

/*
 * States of the MHR radar.
 */
typedef struct _MHRState
{
	float	ms_Elev;	/* Radar elevation angle */
	bool	ms_Keep;	/* Is it a keeper?	*/
} MHRState;

# define MaxMHRState 100
MHRState MHStates[MaxMHRState];
int NStates = 0;
int CurState = -1;


static int Argc;
static char **Argv;

static int Dispatcher FP ((int, struct ui_command *));
static void Go FP ((void));
static void SetupIndirect FP ((void));
static void ClearFrames FP ((void));
static void Source FP ((struct ui_command *));
static void NewField FP ((struct ui_command *));
static void ThreshParams FP ((struct ui_command *));
static void SetConsumer FP ((struct ui_command *));
static void InvokeConsumer FP ((void));
static int MHandler FP ((Message *));
static void CheckMessages FP ((void));
static void DumpRHILimits FP ((float, float));
static void BeamWait FP ((void));
static void SetupBeamDelay FP ((void));
static void WakeUp FP ((int signal));
static void reset FP ((void));
void die FP ((void));



static unsigned char *Image[MFIELD];





main (argc, argv)
int argc;
char **argv;
{
	SValue v;
	char loadfile[200];
	char ourname[32];
/*
 * Initialize.
 */
	sprintf (ourname, "RadarIngest_%x", getpid ());
	msg_connect (MHandler, ourname);
	msg_DeathHandler (die);
	fixdir ("RI_LOAD_FILE", GetLibDir(), "radar_ingest.lf", loadfile);
	if (argc > 1)
	{
		ui_init (loadfile, FALSE, TRUE);
		v.us_v_ptr = argv[1];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
				SYMT_STRING, &v);
	}
	else
		ui_init (loadfile, TRUE, FALSE);

	ui_setup ("radar_ingest", &argc, argv, 0);
	DefineWidgets ();
	SetupIndirect ();
	ds_Initialize ();
	InputInitialize ();
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
	usy_c_indirect (vtable, "el_tolerance", &ElTolerance, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "pixels_per_km", &PixScale, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "minimum_sweep", &MinSweep, SYMT_INT, 0);
	usy_c_indirect (vtable, "minimum_rhi", &MinRHI, SYMT_INT, 0);
	usy_c_indirect (vtable, "gmt_offset", &GMTOffset, SYMT_INT, 0);
	usy_c_indirect (vtable, "platform", PlatformName, SYMT_STRING, PF_LEN);
	usy_c_indirect (vtable, "nframes", &NFrames, SYMT_INT, 0);
	usy_c_indirect (vtable, "niceness", &Niceness, SYMT_INT, 0);
	usy_c_indirect (vtable, "update", &WidgetUpdate, SYMT_INT, 0);
	usy_c_indirect (vtable, "project", &Project, SYMT_BOOL, 0);
/* usy_c_indirect (vtable, "mhrmode", &MhrMode, SYMT_BOOL, 0); */
	usy_c_indirect (vtable, "mhrtop", &MhrTop, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "forcerealtime", &ForceRealTime, SYMT_BOOL, 0);
/* one just for CP2 in SCMS */
	usy_c_indirect (vtable, "sminusxthresh", &SMinusXThresh, SYMT_INT, 0);
/*
 * Thresholding parameters.
 */
	usy_c_indirect (vtable, "threshold", &DoThresholding, SYMT_BOOL, 0);
/*
 * Scan delineation.
 */
	usy_c_indirect (vtable, "trustsweep", &TrustSweep, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "trustvol", &TrustVol, SYMT_BOOL, 0);
/*
 * Beam delay in seconds (for reading from tape)
 */
	usy_c_indirect (vtable, "beamdelay", &BeamDelay, SYMT_FLOAT, 0);
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
	 * Thresholding.
	 */
	   case RIC_THRESHOLD:
	   	ThreshParams (cmds + 1);
		break;
	/*
	 * Consumption, conspicuous or otherwise.
	 */
	   case RIC_CONSUMER:
	   	SetConsumer (cmds + 1);
		break;
	/*
	 * Mhr states.
	 */
	   case RIC_MHRSTATE:
	   	MHStates[NStates].ms_Elev = UFLOAT (cmds[1]);
		MHStates[NStates].ms_Keep = UKEY (cmds[2]);
		NStates++;
		break;
	/*
	 * Formats.
	 */
	    case RIC_FORMAT:
		RFormat = (RadarFormat) cmds[1].uc_v.us_v_int;
		break;
	/*
	 * Radar calibrations.  Ugh.
	 */
	    case RIC_CALIBRATION:
		CP2_LoadCal (cmds[1].uc_v.us_v_int);
		break;
	/*
	 * Beam buffering so that somebody else can snarf the data as well.
	 */
	    case RIC_BEAMBUF:
		if (BB_Setup (UINT (cmds[1]), UINT (cmds[2]), UINT (cmds[3])))
			Using_BB = TRUE;
		break;
	/*
	 * Dump out RHI limits?
	 */
	    case RIC_DUMPRHI:
		if ((LimitPID = ds_LookupPlatform (UPTR (cmds[1]))) ==
				BadPlatform)
			msg_ELog ("Bad RHI limit platform %s", UPTR (cmds[1]));
		else
			DumpLimits = TRUE;
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
	int i, nbeam = 0, pol_mode;
	Widget top;
/*
 * This should end up inside the following loop at some point, but Input.c
 * provides no means for closing an open input source at this point.
 */
	SetupInput ();
/*
 * The big loop
 */
	while (1)
	{
		msg_ELog (EF_INFO, "Initializing...");
		Reinitialize = FALSE;
	/*
	 * Set up our beam input source.
	 */
		signal (SIGINT, die);
	/*
	 * Definition checking.
	 */
		if (NField <= 0)
		{
			msg_ELog (EF_EMERGENCY, "No fields given");
			die ();
		}
	/*
	 * Go into window mode, with our popup.
	 */	
		uw_ForceWindowMode ("status", &top, 0);
	/*
	 * Set up our shared memory segment.
	 */
		if (! (ShmDesc = IX_Create (0x910425, XRes, YRes, NField, 
					    NFrames, Fields)))
		{
			msg_ELog (EF_EMERGENCY, "No shm segment");
			die ();
		}
		IX_LockMemory (ShmDesc);
		IX_Initialize (ShmDesc, 0xff);
	/*
	 * If they have asked for a priority change, try to do it.  Note that 
	 * this happens *after* the consumer is fired off, so that said 
	 * consumer has to fend for itself in the priority arena.
	 */
# ifdef BSD
		if (Niceness)
			setpriority (PRIO_PROCESS, 0, Niceness);
# endif
# if defined(sun) && defined(SVR4)
	/*
	 * You would not believe what's required to make this work under 
	 * Solaris.  The good side is that we get a true real time scheduler...
	 */
		if (Niceness)
			SetRealTime (Niceness);
# endif
	/*
	 * Invoke the consumer to pull stuff out of that segment.
	 */
		InvokeConsumer ();
	/*
	 * Set up fields until there is a command-based way to do it.
	 */
# ifdef notdef
		/* MHR z = 0, v = 1 */
		Rd[0].rd_foffset = 0; /* Z = 4, v = 2 */
		Rd[1].rd_foffset = 1;
# endif
		beam = GetBeam ();
		hk = beam->b_hk;
		if (RFormat == RF_CP2)
			HandleCP2Mess (beam, hk, Scale);
		else
		{
			for (i = 0; i < NField; i++)
			{
				Scale[i].s_Scale =
					hk->parm_info[Rd[i].rd_foffset].pi_scale/100.0;
				Scale[i].s_Offset =
					hk->parm_info[Rd[i].rd_foffset].pi_bias/100.0;
				ui_printf ("%s scale %.2f bias %.2f\n", 
					   Fields[i], Scale[i].s_Scale, 
					   Scale[i].s_Offset);
			}
		}
	/*
	 * Origin setting.
	 */
		cvt_Origin (RadarLat, RadarLon);
	/*
	 * Now plow through the beams.
	 */
	/*	InitFake (); */
		SetupBeamDelay ();
		if (RFormat == RF_CP2)
			pol_mode = hk->dual_pol_mode;
	
		while (! Reinitialize)
		{
			BeamWait ();
		/*
		 * Get another beam.
		 */
			if (! (beam = GetBeam ()))
			{
				ui_printf ("Get Beam failure!\n");
				exit (1);
			}
		/*
		 * Check for mode change
		 */
			if (RFormat == RF_CP2 && hk->dual_pol_mode != pol_mode)
			{
				pol_mode = hk->dual_pol_mode;
				HandleCP2Mess (beam, hk, Scale);
			}
		/*
		 * Rasterize it.
		 */
			Rasterize (beam, Rd, NField, TRUE);
			if ((++nbeam % WidgetUpdate) == 0)
			{
				SetStatus (beam->b_hk);
				CheckMessages ();
			}
		}
	}
}





void
OutputSweep (bt, alt, newvol, left, right, up, down, mode)
UItime *bt;
float alt;
int newvol, left, right, up, down, mode;
/*
 * Put this sweep out.
 */
{
	long systime;
	int i;
	RGrid rg;
	Location loc;
	UItime t;
	char attr[100];
	static float rhileft, rhiright;
	static int rhivol = FALSE;
/*
 * MHR filtering.
 */
	if (RFormat == RF_MHR && ! MHR_Filter (alt))
		return;
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
/*
 * Figure out attributes.
 */
	strcpy (attr, newvol ? "newfile," : "");
	switch (mode)
	{
	    case SM_PPI:
		strcat (attr, "radar,ppi");
		break;
	    case SM_RHI:
		strcat (attr, "radar,rhi");
		break;
	    case SM_SUR:
		strcat (attr, "radar,sur");
		break;
	    default:
		msg_ELog (EF_INFO, "Dropping mode %d volume", mode);
		return;
	}
/*	strcat (attr, (mode == SM_PPI) ? "radar,ppi" : "radar,sur");*/
/*
 * Force time unless told not to -- we usually know better than they do.
 */
	if (ForceRealTime)
		tl_GetTime (&t);
	else
		t = *bt;
/*
 * Ship it out!
 */
	IX_SendFrame (ShmDesc, ImageSet, &t, &rg, &loc, Scale, left, up,
			right, down, attr);
	ImageSet = -1;
/*
 * Say something, and make the display show what we've done.
 */
	ui_printf (" Output %s at %d %06d ang %.2f new %c scale %.2f %.2f\n",
			PlatformName, t.ds_yymmdd, t.ds_hhmmss, alt,
			newvol ? 't' : 'f', PixScale, rg.rg_Xspacing);
/*
 * Look at maybe dumping out RHI limits.
 */
	if (DumpLimits)
	{
		if (newvol && rhivol)
		{
			DumpRHILimits (rhileft, rhiright);
			rhivol = FALSE;
		}
		if (mode == SM_RHI)
		{
			if (newvol)
				rhileft = alt;
			rhiright = alt;
			rhivol = TRUE;
		}
	}
}




static int
AzEq (a1, a2, tol)
float a1, a2, tol;
{
	float diff = a1 - a2;
	if (diff < 0)
		diff = -diff;
	return (diff < tol);
}




int
MHR_Filter (alt)
float alt;
/*
 * See if we should filter out this stuff.
 */
{
	int next = CurState + 1;
	if (next >= NStates)
		next = 0;
/*
 * See if this altitude matches what we are expecting.  If not, try to
 * resynchronize.
 */
	if (! AzEq (alt, MHStates[next].ms_Elev, 0.2))
	{
		msg_ELog (EF_PROBLEM, "MHR Sync problem, alt %.2f, exp %.2f",
			alt, MHStates[next].ms_Elev);
		for (next = 0; next < NStates; next++)
			if (AzEq (alt, MHStates[next].ms_Elev, 0.2))
				break;
		if (next >= NStates)
		{
			msg_ELog (EF_PROBLEM, "Can't find right state!");
			CurState = -1;
		}
		else
			CurState = next;
		return (TRUE);	/* Always keep when resync */
	}
/*
 * We got it.  Move to the new state and return the proper keep value.
 */
	msg_ELog (EF_DEBUG, "State %d, %s", next,
		MHStates[next].ms_Keep ? "KEEP" : "TOSS");
	CurState = next;
	return (MHStates[next].ms_Keep);
}







int
BeginSweep ()
/*
 * Get set to do a new sweep.
 */
{
	int i;
	char s[50];

	UpdateThreshold ();
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
	while ((ImageSet = 
		IX_GetWriteFrame (ShmDesc, (char **) Image, FALSE)) < 0)
	{
		msg_ELog (EF_DEBUG, "Waiting for image frame...");
		sleep (1);
	}

	for (i = 0; i < NField; i++)
		Rd[i].rd_image = Image[i];

	/* ClearImages (); */
	return (TRUE);
}






ClearImages ()
{
	int i;

	for (i = 0; i < NField; i++)
		memset (Image[i], 0xff, XRes*YRes); 
}





static void
ThreshParams (cmds)
struct ui_command *cmds;
/*
 * Turn on thresholding.
 */
{
	ThrFldOffset = cmds[0].uc_v.us_v_int;
	ThrCounts = (unsigned char) cmds[1].uc_v.us_v_int;
	DoThresholding = TRUE;
}





static void
SetConsumer (cmds)
struct ui_command *cmds;
/*
 * Set our consumer.
 */
{
	int arg;

	CSet = TRUE;
/*
 * The first arg is always the program itself.
 */
	strcpy (Consumer, UPTR (*cmds));
	CArgs[0] = Consumer;
/*
 * Go through and do the rest of the args.
 */
	for (NCArg = 1; cmds[NCArg].uc_ctype != UTT_END; NCArg++)
		CArgs[NCArg] = usy_string (UPTR (cmds[NCArg]));
	CArgs[NCArg] = 0;
}




static void
InvokeConsumer ()
/*
 * Execute the consumer process.
 */
{
/*
 * If there is no consumer, let's hope they plan to fire one off themselves.
 */
	if (! CSet)
	{
		ui_warning ("No consumer process given");
		return;
	}
/*
 * Create the new process, then try to exec the consumer program in the
 * child.
 */
	if ((CPid = vfork ()) == 0)
	{
		execvp (Consumer, CArgs);
		/* What??? We're still here? */
		msg_ELog (EF_EMERGENCY, "Exec of consumer %s failed err %d",
				Consumer, errno);
		_exit (1);
	}
}






static void
CheckMessages ()
/*
 * Drain our incoming message queue.
 */
{
	int fd = msg_get_fd ();
	fd_set fds;
	static struct timeval tv = { 0, 0 };
/*
 * While something is pending on the message socket, dispatch it.
 */
	for (;;)
	{
		FD_ZERO (&fds);
		FD_SET (fd, &fds);
		if (select (fd + 1, &fds, 0, 0, &tv) <= 0)
			return;
		msg_incoming (fd);
	}
}


static int
MHandler (msg)
Message *msg;
/*
 * Deal with messages.
 */
{
	struct mh_template *tmpl = (struct mh_template *) msg->m_data;

	switch (msg->m_proto)
	{
	    case MT_MESSAGE:
		if (tmpl->mh_type == MH_DIE)
			die ();
		break;
	    case MT_COMMAND:
		reset ();
		Reinitialize = TRUE;
		ui_perform (msg->m_data);
		break;
	}
		
	msg_ELog (EF_PROBLEM, "Unknown msg proto %d", msg->m_proto);
}



# if defined(sun) && defined(SVR4)
/*
 * Herein lies the code to tweak ourselves into a realtime class under
 * solaris.
 *
 * SGI has a similar but totally incompatible set of incantations to
 * follow for this.  So much for standards.  I can't bring myself to even
 * look at the HP.
 */

static void
SetRealTime (priority)
int priority;
{
	pcinfo_t pcinfo;
	pcparms_t parms;
	rtparms_t *rtparms;
/*
 * Figure out what the class ID is for the real time class.  This is Sun's way
 * of turning a normal integer constant into a character constant instead...
 *
 * Note also that without the (caddr_t) below, the compiler gripes about the
 * type of "argument 5".  Yes, there's only four of them.  Look at
 * <sys/priocntl.h> if you really want to know...
 */
	strcpy (pcinfo.pc_clname, "RT");
	if (priocntl (0, 0, PC_GETCID, (caddr_t) &pcinfo) == -1L)
	{
		msg_ELog (EF_PROBLEM, "GETCID failed on RT class, en %d",
				errno);
		return;
	}
/*
 * Now that we have the magic cookie, make ourselves a real time mondo
 * process.
 */
	parms.pc_cid = pcinfo.pc_cid;
	rtparms = (rtparms_t *) &parms.pc_clparms;
	rtparms->rt_pri = priority;
	rtparms->rt_tqnsecs = RT_TQDEF;
	if (priocntl (P_PID, getpid (), PC_SETPARMS, (caddr_t) &parms) == -1)
		msg_ELog (EF_PROBLEM, "SETPARMS failed, errno %d", errno);
}

# endif /* solaris */




static void
DumpRHILimits (left, right)
float left, right;
/*
 * Dump out our RHI limits.
 */
{
	DataChunk *dc;
	ZebTime when;
	FieldId fids[2];
/*
 * Get our time.  Zap out microsecs because they have no useful
 * meaning here and seem to cause confusion.
 */
	tl_Time (&when);
	when.zt_MicroSec = 0;
/*
 * Fix up a data chunk.
 */
	dc = dc_CreateDC (DCC_Scalar);
	dc->dc_Platform = LimitPID;
	fids[0] = F_Lookup ("left");
	fids[1] = F_Lookup ("right");
	dc_SetScalarFields (dc, 2, fids);
	dc_AddScalar (dc, &when, 0, fids[0], &left);
	dc_AddScalar (dc, &when, 0, fids[1], &right);
/*
 * Store it, trash it, and we're done.
 */
	ds_Store (dc, FALSE, 0, 0);
	dc_DestroyDC (dc);
}



static void
BeamWait ()
/*
 * Wait for our beam interval timer (if any) to expire.
 */
{
	while (! OKToGetBeam)
		pause ();

	OKToGetBeam = (BeamDelay == 0.0);
}


static void
SetupBeamDelay ()
/*
 * Set up the beam interval timer, if requested.
 */
{
	struct sigaction	sa;
	struct itimerval	it;

	OKToGetBeam = TRUE;
	
	if (BeamDelay == 0.0)
		return;
/*
 * We have a delay time, so start an alarm to go off at the given interval.
 */
	signal (SIGALRM, WakeUp);
	it.it_interval.tv_sec = it.it_value.tv_sec = (int) BeamDelay;
	it.it_interval.tv_usec = it.it_value.tv_usec = 
		(int)(BeamDelay * 1000000) % 1000000;
	
	setitimer (ITIMER_REAL, &it, NULL);
}



static void
WakeUp (sig)
int sig;
/*
 * Beam interval timer signal handler
 */
{
# ifdef SVR4
/*
 * Reestablish signal handler under SYSV
 */
	signal (SIGALRM, WakeUp);
# endif
	OKToGetBeam = TRUE;
}


void
die ()
/*
 * Finish gracefully.
 */
{
        reset ();
        ui_finish ();
        exit (0);
}



static void
reset ()
/*
 * Detach and kill stuff in preparation for reinitializing or dying
 */
{
        if (ShmDesc)
        {
        /*
         * Shut down our consumer (if any)
         */
                if (CPid)
                {
                        int     status = 0;

                        msg_send (IX_GetConsumer (ShmDesc), MT_FINISH,
                                  FALSE, &status, sizeof (status));
                }
        /*
         * Then detach from the image transfer shared memory
         */
                IX_Detach (ShmDesc);
        }

	if (Using_BB)
		BB_Done ();
}
