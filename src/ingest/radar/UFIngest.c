/*
 * Rasterize UF radar data and ship it to the data store.
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


# include <copyright.h>
# include <errno.h>
# include <signal.h>
# include <sys/types.h>
# include <sys/time.h>
# include <sys/resource.h>

# include <ui.h>
# include <config.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <ImageXfr.h>
# include "HouseKeeping.h"
# include "radar_ingest.h"
# include "display.h"
 

/*
 * Define globals here. !!! check for compatibilty	
 */
int	XRes = 800, YRes = 800;
int	XRadar = 400, YRadar = 400;
float	AzFill = 0.6;
float	PixScale = 5.0;		/* Pixels per kilometer		*/
float  	CurLat = 0, CurLon = 0;
float	ElTolerance = 1.0;	/* Elevation difference tolerance, deg. */
int	MinSweep = 25;
int	GMTOffset = 0;
int	MinRHI = 0;		/* not used in UFIngest (yet) */
int	NFrames = 2;		/* How many frames		*/
int	Niceness = 0;
int	NBeam = 0;
bool	Project = TRUE;
bool	CheckTrailLen = TRUE;	/* verify trailing reclen in file sources? */

/*
 * We should be able to trust sweep and volume flags in UF data
 */
bool	TrustSweep = TRUE;
bool	TrustVol = TRUE;

/*
 * No thresholding by default
 */
bool	DoThresholding = FALSE;
int	ThrFldOffset;
unsigned char	ThrCounts = 0;

/*
 * Source info
 */
char	SrcName[128];
int	SrcType;

/*
 * Image transfer stuff
 */
struct _ix_desc *ShmDesc = 0;
int	ImageSet = -1;

/*
 * Name of the platform we're ingesting
 */
# define PF_LEN 80
char	PlatformName[PF_LEN];

/*
 * Who consumes our data.
 */
char	Consumer[200];
char	*CArgs[20];
int	NCArg = 0;
bool	CSet = FALSE;
int	CPid = 0;		/* Its process ID	*/

/*
 * Field info.
 */
RDest		Rd[MFIELD];
ScaleInfo	Scale[MFIELD];
int		NField = 0, FIndex[MFIELD];
char		*Fields[MFIELD];
static unsigned char	*Image[MFIELD];
RadarFormat RFormat = RF_UF;		/* The format of our data */
int Using_BB = FALSE;		/* Doing beam buffering? 	*/

/*
 * Prototypes
 */
static int 	Dispatcher FP ((int, struct ui_command *));
static void	Go FP ((void));
static void	SetupIndirect FP ((void));
static void	ClearFrames FP ((void));
static void	Source FP ((struct ui_command *));
static void	NewField FP ((struct ui_command *));
static void	ThreshParams FP ((struct ui_command *));
static void	SetConsumer FP ((struct ui_command *));
static void	InvokeConsumer FP ((void));
static int	MHandler FP ((Message *));
static void	CheckMessages FP ((void));

void		die FP ((int));




main (argc, argv)
int argc;
char **argv;
{
	SValue v;
	char loadfile[200];
	char	ourname[20];
/*
 * Initialize.
 */
	sprintf (ourname, "UFIngest_%x", getpid ());
	msg_connect (MHandler, ourname);
	msg_DeathHandler (die);
	fixdir ("RI_LOAD_FILE", GetLibDir(), "UFIngest.lf", loadfile);
	if (argc > 1)
	{
		ui_init (loadfile, FALSE, TRUE);
		v.us_v_ptr = argv[1];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
				SYMT_STRING, &v);
	}
	else
		ui_init (loadfile, TRUE, FALSE);

	ui_setup ("UFIngest", &argc, argv, 0);
	SetupIndirect ();
	ds_Initialize ();
/*
 * Time to go in to UI mode.
 */
	ui_get_command ("initial", "UFIngest>", Dispatcher, 0);
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
	usy_c_indirect (vtable, "azimuth_fill", &AzFill, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "el_tolerance", &ElTolerance, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "pixels_per_km", &PixScale, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "minimum_sweep", &MinSweep, SYMT_INT, 0);
	usy_c_indirect (vtable, "gmt_offset", &GMTOffset, SYMT_INT, 0);
	usy_c_indirect (vtable, "platform", PlatformName, SYMT_STRING, PF_LEN);
	usy_c_indirect (vtable, "nframes", &NFrames, SYMT_INT, 0);
	usy_c_indirect (vtable, "niceness", &Niceness, SYMT_INT, 0);
	usy_c_indirect (vtable, "project", &Project, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "threshold", &DoThresholding, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "trustvol", &TrustVol, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "check_trailer", &CheckTrailLen, SYMT_BOOL, 0);
}



static int
Dispatcher (junk, cmds)
int junk;
struct ui_command *cmds;
/*
 * The command dispatcher.
 */
{
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
	SrcType = UKEY (cmds[0]);
	strcpy (SrcName, UPTR (cmds[1]));
}
	




static void
NewField (cmds)
struct ui_command *cmds;
/*
 * Add another field to the list.
 */
{
	int	index = cmds[1].uc_v.us_v_int;
	float	min = UFLOAT (cmds[2]), max = UFLOAT (cmds[3]);
/*
 * Make sure we don't have too many.
 */
	if (NField >= MFIELD)
	{
		msg_ELog (EF_PROBLEM, "Too many fields; dropping %s",
			  UPTR (cmds[0]));
		return;
	}
/*
 * Now remember the stuff.
 */
	Fields[NField] = usy_string (UPTR (cmds[0]));
	FIndex[NField] = index;
	Rd[NField].rd_foffset = NField;
/*
 * Take the min and max limits for this field and turn them into 
 * scaling factors such that val = (count * scale) + offset
 */
	Scale[NField].s_Scale = (max - min) / 255.0;
	Scale[NField].s_Offset = min;
	NField++;
}



void
die (sig)
int sig;
/*
 * Finish gracefully.
 */
{
	ui_finish ();
	if (ShmDesc)
	{
	/*
	 * Shut down our consumer (if any)
	 */
		if (CPid)
		{
			int	status = 0;

			msg_send (IX_GetConsumer (ShmDesc), MT_FINISH,
				  FALSE, &status, sizeof (status));
		}
	/*
	 * Then detach from the image transfer shared memory
	 */
		IX_Detach (ShmDesc);
	}

	msg_ELog (EF_INFO, "Exiting.");
	
	exit (0);
}


static void
Go ()
/*
 * Start really rasterizing.
 */
{
	Beam	beam, UFGetBeam ();
	int	i, nbeam = 0;
	Housekeeping	*hk;
/*
 * Set up our beam input source.
 */
	signal (SIGINT, die);

	if (SrcType == RIC_FILE)
	{
		FileInput (SrcName, CheckTrailLen);
		msg_ELog (EF_INFO, "Ingesting file '%s'", SrcName);
	}
	else if (SrcType == RIC_TAPE)
	{
		TapeInput (SrcName);
		msg_ELog (EF_INFO, "Ingesting from tape %s", SrcName);
	}
	else
		perror("FileInput");

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
	IX_Initialize (ShmDesc, 0xff);
/*
 * Invoke the consumer to pull stuff out of that segment.
 * Pause for a bit until the consumer attaches to our image segment.
 */
	InvokeConsumer ();
	if (CPid)
	{
		char	*consumer = IX_GetConsumer (ShmDesc);
		
		while (consumer[0] == '\0')
		{
			msg_ELog (EF_DEBUG, "Waiting for consumer attach...");
			sleep (1);
		}
	}
	
# ifdef BSD
/*
 * If they have asked for a priority change, try to do it.
 */
	if (Niceness)
		setpriority (PRIO_PROCESS, 0, Niceness);
# endif
/*
 * Now plow through the beams.
 */
	while ((beam = UFGetBeam (FIndex, Scale, NField)) != NULL)
	{
		CurLat = beam->b_hk->latitude * BIN_TO_DEG;
		CurLon = beam->b_hk->longitude * BIN_TO_DEG;
	/*
	 * Rasterize it.
	 */
		Rasterize (beam, Rd, NField, FALSE);
		if ((++nbeam % 100) == 0)
			CheckMessages ();
	}
/*
 * Put out the last image if necessary.
 */
	RasterizeDone ();
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
	char attr[100];
	static bool	firstsweep = TRUE;
/*
 * Set newvol to true if this is the first sweep, regardless of what
 * our caller says.
 */
	if (firstsweep)
	{
		newvol = TRUE;
		firstsweep = FALSE;
	}
/*
 * Radars tend to record in local time; make the move over to GMT now.
 */
	systime = TC_FccToSys (bt) + GMTOffset*3600;
	TC_SysToFcc (systime, bt);
/*
 * Assemble our data object, so that we can send this stuff out to the
 * data store.
 */
	rg.rg_Xspacing = rg.rg_Yspacing = 1.0 / PixScale;
	rg.rg_nX = XRes;
	rg.rg_nY = YRes;
	rg.rg_nZ = rg.rg_Zspacing = 0;

	cvt_Origin (CurLat, CurLon);
	cvt_ToLatLon (-XRadar / PixScale, -YRadar / PixScale, &loc.l_lat,
			&loc.l_lon);
	loc.l_alt = alt;
/*
 * Figure out attributes.
 */
	strcpy (attr, newvol ? "newfile," : "");
	strcat (attr, (mode == SM_PPI) ? "radar,ppi" : "radar,sur");
/*
 * Ship it out.
 */
	msg_ELog (EF_DEBUG, "Sending image...");
	IX_SendFrame (ShmDesc, ImageSet, bt, &rg, &loc, Scale, left, up,
			right, down, attr);
	ImageSet = -1;
}



int
BeginSweep ()
/*
 * Get set to do a new sweep.
 */
{
	int i;
	char s[50];
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
	while ((ImageSet = IX_GetWriteFrame (ShmDesc, (char **) Image, 
					     FALSE)) < 0)
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
# ifdef sgi
# 	define vfork fork
# endif

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

	if (msg->m_proto == MT_MESSAGE && tmpl->mh_type == MH_DIE)
		die ();
	msg_ELog (EF_PROBLEM, "Unknown msg proto %d", msg->m_proto);
}


/*
 * Ugliness.  A couple of stubs which should never get called, put here
 * to keep the linker happy (preferable to linking in the whole CP2 mess
 * for the same purpose).
 */

void CP2_CheckParams (beam, hk, scale)
Beam beam;
Housekeeping *hk;
ScaleInfo *scale;
{
/* yawn */
}


void CP2_DoDerivation (hk, beam, n, data)
Housekeeping *hk;
Beam beam;
int n;
unsigned char *data;
{
/* snore */
}
