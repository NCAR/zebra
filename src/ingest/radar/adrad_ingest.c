/*
 * Ingest adrad radar data and rasterize it.
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

/* rewrite for adrad data input by Dan Austin 8/93	*/

# include <math.h>
# include <errno.h>
# include <sys/time.h>
# include <sys/resource.h>
# include <rpc/rpc.h>

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
 
RCSID("$Id: adrad_ingest.c,v 2.13 1999-03-11 17:39:00 burghart Exp $")

/* 
 * Adrad includes for xdr, etc.
 */
#include "raw.h"
#include "portable.h"
#include "sunrise_head.h"
#include "cvrt.h"
/* #include "xdr.h" */


/*
 * Define globals here. !!! check for compatibilty	
 */
int XRes = 800, YRes = 800;
int XRadar = 250, YRadar = 400;
float AzFill = 0.6;
float PixScale = 5.0;		/* Pixels per kilometer		*/
float RadarLat = 0, RadarLon = 0;
float ElTolerance = 1.0;	/* Elevation difference tolerance, deg. */
int MinSweep = 25;
int GMTOffset = 0;
int MinRHI = 0;			/* not used in adrad_ingest yet */
int NFrames = 2;		/* How many frames		*/
int Niceness = 0;
int WidgetUpdate = 20;
int NBeam = 0, NMissed = 0;
zbool Project = TRUE;

/* trust internal flags for adrad data	*/
zbool TrustSweep = TRUE;
zbool TrustVol = TRUE;

/*
 * No Thresholding.
 */
zbool DoThresholding = FALSE;
int ThrFldOffset;
unsigned char ThrCounts = 0;

/* ???	*/
struct _ix_desc *ShmDesc = 0;
int	ImageSet = -1;

/* platform name is adrad	*/
# define PF_LEN 80
char PlatformName[PF_LEN];

/*
 * We use this data object to write out finished products.
 */
ScaleInfo Scale[MFIELD];

/*
 * Who consumes our data.
 */
/* consumer is ds_consumer for the data store	*/
static char Consumer[200];
static char *CArgs[20];
static int NCArg = 0;
static zbool CSet = FALSE;
static int CPid = 0;		/* It's process ID	*/

/*
 * Field info.
 */
/* field defines will nedd changing from raw.h	*/
/* # define MFIELD 5 in radar_ingest.h now */
RDest Rd[MFIELD];
int NField = 0;
char *Fields[MFIELD];
static unsigned char *Image[4];

static int Argc;
static char **Argv;
/*
 * file input stuff
 */ 
#define OK 0
XDR xdrstrm;
FILE *f;
struct volume_summary vol;
/* 1/4/94 (D.A.) added global for ambiguous velocity	*/
float Vu;
/* 1/10/94 (D.A.) added global for fields 	*/
char adfields[5]; 
RadarFormat RFormat = RF_ADRAD;		/* The format of our data */
int Using_BB = FALSE;		/* Doing beam buffering? 	*/

/* function declarations 	*/
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

void die FP ((int sig));

/* main loop 	*/

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
/* set things up to parse args & init the ui file.	*/
	sprintf (ourname, "Adrad_%x", getpid ());
	msg_connect (MHandler, ourname);
	msg_DeathHandler ((int (*)())die);
	fixdir ("RI_LOAD_FILE", GetLibDir(), "adrad_ingest.lf", loadfile);
	if (argc > 1)
	{
		ui_init (loadfile, FALSE, TRUE);
		v.us_v_ptr = argv[1];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
				SYMT_STRING, &v);
	}
	else
		ui_init (loadfile, TRUE, FALSE);

	ui_setup ("adrad_ingest", &argc, argv, 0);
	SetupIndirect ();
	ds_Initialize ();
	Argc = argc;
	Argv = argv;
/*
 * Time to go in to UI mode.
 */
	ui_get_command ("initial", "Adrad>", Dispatcher, 0);
	die (0);
}





static void
SetupIndirect ()
/*
 * Create all of the indirect variables which are used to control things.
 */
/* this all comes in from the file adrad.params	*/

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
	usy_c_indirect (vtable, "gmt_offset", &GMTOffset, SYMT_INT, 0);
	usy_c_indirect (vtable, "platform", PlatformName, SYMT_STRING, PF_LEN);
	usy_c_indirect (vtable, "nframes", &NFrames, SYMT_INT, 0);
	usy_c_indirect (vtable, "niceness", &Niceness, SYMT_INT, 0);
	usy_c_indirect (vtable, "update", &WidgetUpdate, SYMT_INT, 0);
	usy_c_indirect (vtable, "project", &Project, SYMT_BOOL, 0);
/*
 * Thresholding parameters. No thresholding.
 */
	usy_c_indirect (vtable, "threshold", &DoThresholding, SYMT_BOOL, 0);
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
	{
		FileInput (UPTR (cmds[1]));
		msg_ELog (EF_INFO, "Ingesting file '%s'", UPTR (cmds[1]));
	}
	else
		perror("FileInput");
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


/* ARGSUSED */
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

/* this is where most of the changes need to be made	*/
{
	Beam beam;
	Housekeeping *hk;
	int i, nbeam = 0;
	Widget top;
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
		die (0);
	}
/*
 * Set up our shared memory segment.
 */
	if (! (ShmDesc = IX_Create (0x910425, XRes, YRes, NField, NFrames,
				    Fields)))
	{
		msg_ELog (EF_EMERGENCY, "No shm segment");
		die (0);
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
 * Origin setting.
 */
	cvt_Origin (RadarLat, RadarLon);

/*!!! merge in the beam reading parts of volread for use here	*/
	/* set the scaling & etc.	*/
	beam = GetBeam ();
	hk = beam->b_hk;

/*!!!*/
    /*for(i=0;i<5;i++)
        printf("adfields value is: %d %c \n",i,adfields[i]);
        printf("unfolded velocity value is: %f.4  \n",Vu);*/

	for (i = 0; i < NField; i++)
	{
		/* 1/4/94 (D.A.) added correct scaling tests here for fields	*/


		if(adfields[i]=='c' || adfields[i]=='u')
		{
			Scale[i].s_Scale = 0.5;
			Scale[i].s_Offset = -32.0;
			/*printf("field mnemonic is: %c \n",adfields[i]);*/
			msg_ELog (EF_INFO, "%s scale %.2f bias %.2f", Fields[i], 
				   Scale[i].s_Scale, Scale[i].s_Offset);
		}

		if(adfields[i]=='v')
		{
			Scale[i].s_Scale = Vu/128.0;
			Scale[i].s_Offset = -Vu;
			/*printf("field mnemonic is: %c \n",adfields[i]);*/
			msg_ELog (EF_INFO, "%s scale %.2f bias %.2f", Fields[i], 
				   Scale[i].s_Scale, Scale[i].s_Offset);
		}

		if(adfields[i]=='w')
		{
			Scale[i].s_Scale = Vu/256.0;
			Scale[i].s_Offset = 0.0;
			/*printf("field mnemonic is: %c \n",adfields[i]);*/
			msg_ELog (EF_INFO, "%s scale %.2f bias %.2f", Fields[i], 
				   Scale[i].s_Scale, Scale[i].s_Offset);
		}
	}
/*
 * Now plow through the beams.
 */
	while (1)
	{
	/*
	 * Get another beam.
	 */
		if (! (beam = GetBeam ()))
			die (0);
	/*
	 * Rasterize it.
	 */
		Rasterize (beam, Rd, NField, FALSE);
		if ((++nbeam % 100) == 0)
			CheckMessages ();
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
	char attr[100];
	static zbool	firstsweep = TRUE;
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
	strcat (attr, (mode == SM_PPI) ? "radar,ppi" : "radar,sur");
/*
 * Ship it out.
 */
	msg_ELog (EF_DEBUG, "Sending image...");
	IX_SendFrame (ShmDesc, ImageSet, bt, &rg, &loc, Scale, left, up,
			right, down, attr);
	ImageSet = -1;
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
		die (0);
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

