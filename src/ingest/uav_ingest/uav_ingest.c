/*
 * $Id: uav_ingest.c,v 1.1 1994-04-15 20:14:08 granger Exp $
 *
 * Ingest serial UAV position data, from GCS interface to ARM ground equipment

TODO:

How to test for rollover in GPS reference time, or is there one?  Should we
call GPSReferenceTime in every call to StorePacker(), or better yet
establish a timer event on every Sunday morning which changes the global
reference time (assuming the reference may rollover while the ingestor
is running.

Store UAV position data in znf files, ala NEXUS?  Would that be faster?
We're not archiving so we don't really need to keep the data in netCDF,
and we can always convert it later.

Allow reference time to be specified explicitly (or at least the time
from which the reference time should be calculated).

Allow data records to be read directly from a file.
 -- Done.

Range limit and differential checking of fields: set those not to spec
to a bad value flag.
 -- Range checking done.

Print full time with microsecs, and make sure microsecs being extracted
from floating point gps time.
 -- Done.

Try ignoring samples with bad locations, to make track plots more robust.

*/
/*		Copyright (C) 1987-92 by UCAR
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

#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>

#include <defs.h>
#include <ingest.h>
#include <copyright.h>
#include <ui_expr.h>

#include "uav.h"
#include "serial_svr.h"

extern char *sys_errlist[];
extern int sys_nerr;
extern int errno;

#define PI	3.141592654
#define RADS_2_DEGREES(x) ((x)*(180.0 / PI))
#define DEGREES_2_RADS(x) ((x)*(PI / 180.0))
#define MatchAbbrev(arg,opt) \
        ((strlen(arg) >= 2) && (!strncmp((arg),(opt),strlen(arg))))

#define BADVALUE	-99999.0

MAKE_RCSID(
   "$Id: uav_ingest.c,v 1.1 1994-04-15 20:14:08 granger Exp $")

struct field {
	char *name;
	char *units;
	char *longname;
	FieldId id;
};

struct Source {
	char *s_name;
	enum SourceType { File, Socket } s_type;
	ZebTime s_when;	/* Initial time from which ref time calculated 	*/
	ZebTime s_ref;	/* The GPS reference time for packet offsets	*/
	ZebTime s_last;	/* The time of the most recent valid packet	*/
	int s_fd;
	PlatformId s_pid;
	DataChunk *s_dc;
};

struct Source *GlobalSource;	/* until message handlers pass arguments! */

static void	Usage FP((char *prog_name));
static void	ParseCommandLineOptions FP((int *argc, char *argv[],
					    struct Source *source));
static void	SetupInput FP((struct Source *));
static void	StorePacket FP((struct Source *, GCSpacket21 *pk, 
				ZebTime *ref, PlatformId));
static int	AddPacket FP((DataChunk *dc, GCSpacket21 *pk, ZebTime *ref,
			      ZebTime *latest));
static int	QCPacket FP((GCSpacket21 *pk, float badval));
static int	PacketHandler FP((int fd));
static void	GPSReferenceTime FP((const ZebTime *now, ZebTime *ref));
static struct field *InitFields FP((int *nfield));
static void	OpenSocket FP((struct Source *source));
static void	OpenFile FP((struct Source *source));
static int	BlockRead FP((int fd, char *buf, int len));
static void	Close FP((struct Source *source));
static void	Finish FP((struct Source *source));

#define INGEST_NAME 		"uav_ingest"
#define DEFAULT_PLATFORM 	"sgpuavtrack"
#define DEFAULT_SOCKET 		FIFO_PATH_2
#define FIFO_PERMS		PERMS		/* from serial_svr.h */
#ifndef ERROR_STRING
#define ERROR_STRING	((errno < sys_nerr) ? (sys_errlist[errno]) : \
			 "error number unknown")
#endif                        


static void
Die (msg, code)
char *msg;
int code;
{
	if (msg)
	{
		printf ("%s\n", msg);
	}
	exit (code);
}



static void
InitSource (source)
struct Source *source;
{
	source->s_type = Socket;
	source->s_name = DEFAULT_SOCKET;
	source->s_when.zt_Sec = 0;
	source->s_when.zt_MicroSec = 0;
	source->s_ref.zt_Sec = 0;
	source->s_ref.zt_MicroSec = 0;
	source->s_last.zt_Sec = 0;
	source->s_last.zt_MicroSec = 0;
	source->s_fd = -1;
	source->s_pid = BadPlatform;
	source->s_dc = NULL;
}



static int
message (msg)
struct message *msg;
/*
 * More than likely we're being told to kill ourselves
 */
{
	struct mh_template *mh = (struct mh_template *)msg->m_data;

	switch (msg->m_proto)
	{
	   case MT_MESSAGE:
		if (mh->mh_type == MH_DIE)
		{
			Finish (GlobalSource);
			exit (0);
		}
	}
	IngestLog (EF_PROBLEM, "errant message ignored");
	return (0);
}



int
main (argc, argv)
int argc;
char **argv;
{
	struct Source source;
	ZebTime ref;
	char *platform = DEFAULT_PLATFORM;
	PlatformId pid;
/*
 * Get our command-line options, setting appropriate global variables
 * Only the file name and the names of the fields should remain
 */
	InitSource (&source);
	GlobalSource = &source;
	ParseCommandLineOptions(&argc, argv, &source);
	if (argc > 2)
	{
		printf("%s: too many arguments\n",argv[0]);
		Usage(argv[0]);
		exit(1);
	}
	else if (argc == 2)
		source.s_name = argv[1];
	else if (source.s_type == File)	/* Need a file name arg */
	{
		printf("%s: file input needs name argument\n",argv[0]);
		Usage(argv[0]);
		exit(1);
	}
/*
 * Initialize usy, message, DataStore, and fields all at once, but don't
 * use the Ingest module's default handler, since we have some cleanup to do
 */
	IngestInitialize (INGEST_NAME);
	msg_AddProtoHandler (MT_MESSAGE, message);

	pid = ds_LookupPlatform (platform);
	if (pid == BadPlatform)
	{
		printf ("%s: bad platform '%s'\n", argv[0], platform);
		exit (1);
	}

	if (! source.s_when.zt_Sec)
		tl_Time (&source.s_when);
	GPSReferenceTime (&source.s_when, &source.s_ref);
	source.s_pid = pid;
	SetupInput (&source);

	if (NoMessageHandler && (source.s_type == File))
	{
		while (PacketHandler (source.s_fd) == 0)
			/* keep reading until eof or error */;
	}
	else if (source.s_type == File)
	{
		msg_await(); /* the packet handler returns non-zero of EOF */
	}
	else	/* for pipes, we don't quit for nothin' */
	{
		while (1)
			msg_await();
	}
	IngestLog (EF_INFO, "Done.");
	Close (&source);
	return (0);
}




/* ParseCommandLineOptions --------------------------------------------
 *    Set global variables from command-line options, leaving only
 *    the expected file and field names in the arg list
 */
static void
ParseCommandLineOptions(argc, argv, source)
	int *argc;
	char *argv[];
	struct Source *source;
{
	int i, j;
/*
 * First parse any of the general ingest options
 */
	IngestParseOptions(argc, argv, Usage);
/*
 * Now check for any of our own flags on the command line
 */
	i = 1;
	while (i < *argc)
	{
		if (MatchAbbrev(argv[i],"-file"))
		{
			source->s_type = File;
			IngestRemoveOptions(argc, argv, i, 1);
		}
		else if (MatchAbbrev(argv[i],"-socket"))
		{
			source->s_type = Socket;
			IngestRemoveOptions (argc, argv, i, 1);
		}
		else if (MatchAbbrev(argv[i],"-time"))
		{
			if (*argc <= i + 1)
				Die ("-time: needs argument", 3);
			if (! TC_DecodeTime (argv[i+1], &source->s_when))
				Die ("-time: bad time string", 4);
			IngestRemoveOptions (argc, argv, i, 2);
		}
		else
		   ++i;
	}
}



static void
Usage(prog)
	char *prog;
{
	printf ("Usage: %s [options] [<name>]\n",prog);
	printf ("       %s -help\n",prog);
	printf ("\nOptions:\n");
	printf ("   -file, -f		<name> is a file to read from\n");
	printf ("   -socket, -s		<name> is a socket to read from\n");
	printf ("   -time, -t		Approximate time of source\n");
	printf ("By default, input is read from a socket.  The time of\n");	       printf ("the source is used to calculate the GPS reference time,\n");
	printf ("which is the first Sunday morning, 0:00, prior to the\n");
	printf ("source time.  By default, the current time is used.\n\n");
	IngestUsage();
	printf ("\nExamples:\n");
	printf ("   %s -log pd -sock UAV\n", prog);
	printf ("   %s -f ./rawdata/uavtrack.dat\n\n", prog);
}



static struct field *
InitFields(nfield)
int *nfield;
{
	static bool inited = FALSE;
	static struct field Fields[] = 
	{
		{ "pitch",   "radians", "Pitch, positive is upwards" },
		{ "roll",    "radians", "Roll, clockwise looking from tail" },
		{ "heading", "radians", "Heading" },
		{ "airspeed","knots",	"Airspeed" }
	};
	static const int NFields = ((sizeof(Fields))/sizeof(Fields[0]));
	struct field *f;

	if (inited)
		return;

	for (f = Fields; f - Fields < NFields; ++f)
		f->id = F_DeclareField (f->name, f->longname, f->units);
	*nfield = NFields;
	return ((struct field *)Fields);
}



static void
GPSReferenceTime (now, ref)
const ZebTime *now;
ZebTime *ref;
/*
 * Return the time of the most recent Sunday morning, 12 am, since
 * the time in 'now'
 */
{
	time_t clock;
	struct tm *tm;
	long seconds;
/*
 * Convert now to system long, then split it out into days of week.
 */
	clock = TC_ZtToSys (now);
	tm = gmtime(&clock);
/*
 * Calculate how many seconds between the time in tm and the most recent
 * Sunday morning.
 */
	seconds = tm->tm_sec + ((tm->tm_min + (tm->tm_hour*60))*60);
	seconds += tm->tm_wday * (24*3600);
	
	ref->zt_MicroSec = 0;
	ref->zt_Sec = now->zt_Sec - seconds;
}



static void
StorePacket (source, pk, reftime, pid)
struct Source *source;
GCSpacket21 *pk;
ZebTime *reftime;	/* time which the GPS time in the packet offsets */
PlatformId pid;
/*
 * Create a scalar datachunk for this packet, load the fields from
 * the packet, and store the datachunk.
 */
{
	struct field *fields;
	FieldId fids[ DC_MaxField ];
	int i, nfield;
	DataChunk *dc;

	if (! source->s_dc)
	{
		dc = dc_CreateDC (DCC_Scalar);
		dc->dc_Platform = pid;
		fields = InitFields(&nfield);
		for (i = 0; i < nfield; ++i)
			fids[i] = fields[i].id;
		dc_SetScalarFields (dc, nfield, fids);
		dc_SetBadval (dc, BADVALUE);
		source->s_dc = dc;
	}

	if (AddPacket (source->s_dc, pk, reftime, &source->s_last))
	{
	/*
	 * Store and destroy the datachunk.
	 */
		if (dc_GetNSample(source->s_dc) >= 5)
		{
			ds_StoreBlocks (source->s_dc, FALSE, NULL, 0);
			dc_DestroyDC (source->s_dc);
			source->s_dc = NULL;
		}
	}
}



static int
AddPacket (dc, pk, reftime, latest)
DataChunk *dc;
GCSpacket21 *pk;
ZebTime *reftime;	/* time which the GPS time in the packet offsets */
ZebTime *latest;	/* time of the most recent valid packet received */
/*
 * Add the packet as a sample to this datachunk.  Return non-zero if 
 * we actually add data, zero otherwise.  If we add this packet, the time
 * of the packet is copied into *latest, otherwise *latest is unchanged.
 */
{
	static int out_of_range = 0;
	ZebTime when;
	Location locn;
	char buf[256];
	int sample;
	float badval;
/*
 * If we don't have the time, we can't make the rhyme
 */
	if (pk->gcs_time < 0)
	{
		IngestLog (EF_PROBLEM, "packet 0x21 received with no time");
		return (0);
	}
/*
 * If our time is out of range by more than a couple weeks, skip it
 */
	if ((long)pk->gcs_time > (2*7*24*3600))
	{
		IngestLog ((++out_of_range % 25 == 0) ? EF_PROBLEM:EF_DEVELOP,
			   "GPS offset out of range: %lu (%d)", 
			   (long)pk->gcs_time, out_of_range);
		return (0);
	}
/*
 * Determine the time of our sample
 */
	when = *reftime;
	when.zt_Sec += (long)pk->gcs_time;
	when.zt_MicroSec = (pk->gcs_time - (long)pk->gcs_time) * 1e+6;
	TC_EncodeTime (&when, TC_FullUSec, buf);
/*
 * If this time is significantly earlier or later (an hour) than the most
 * recent, assume its from a glitch rather than from a UAV which does time 
 * travel
 */
	if (latest->zt_Sec && (when.zt_Sec + 3600 < latest->zt_Sec))
	{
		IngestLog (EF_PROBLEM, 
			   "%s: rejecting time from the past", buf);
		return (0);
	}
	else if (latest->zt_Sec && (when.zt_Sec > latest->zt_Sec + 3600))
	{
		IngestLog (EF_PROBLEM, 
			   "%s: rejecting time from the future", buf);
		return (0);
	}
/*
 * Report on our data
 */
	IngestLog (EF_DEVELOP, "%s", buf);
	sprintf (buf, "   lat:%5.2f lon:%6.2f alt:%6.3f",
		 pk->gcs_lat, pk->gcs_lon, pk->gcs_alt);
	sprintf (buf+strlen(buf), 
	   " pitch:%4.2f roll:%4.2f head:%4.2f spd:%4.2f",
	   pk->gcs_pitch, pk->gcs_roll, pk->gcs_heading, pk->gcs_airspeed);
	IngestLog (EF_DEVELOP, "%s", buf);
	badval = dc_GetBadval(dc);
	(void)QCPacket (pk, badval);
/*
 * If any of the location info is bad, don't bother storing this packet
 */
	if ((pk->gcs_lat == badval) || (pk->gcs_lon == badval) ||
	    (pk->gcs_alt == badval))
	{
		return (0);
	}
#ifdef notdef
/*
 * Set our location.  The lat and lon of the packet must be changed to
 * degrees, and convert the altitude units to the default of kilometers.
 */
	locn.l_lat = locn.l_lon = locn.l_alt = badval;
	if (pk->gcs_lat != badval)
		locn.l_lat = RADS_2_DEGREES (pk->gcs_lat);
	if (pk->gcs_lon != badval)
		locn.l_lon = RADS_2_DEGREES (pk->gcs_lon);
	if (pk->gcs_alt != badval)
		locn.l_alt = pk->gcs_alt / 1000.0;
#endif
/*
 * Set our location.  The lat and lon of the packet must be changed to
 * degrees, and convert the altitude units to the default of kilometers.
 */
	locn.l_lat = RADS_2_DEGREES (pk->gcs_lat);
	locn.l_lon = RADS_2_DEGREES (pk->gcs_lon);
	locn.l_alt = pk->gcs_alt / 1000.0;
/*
 * Extract each member of our packet and add it
 */
	sample = dc_GetNSample (dc);
	dc_AddScalar (dc, &when, sample, F_Lookup("pitch"), &pk->gcs_pitch);
	dc_AddScalar (dc, &when, sample, F_Lookup("roll"), &pk->gcs_roll);
	dc_AddScalar (dc, &when, sample, F_Lookup("heading"), 
		      &pk->gcs_heading);
	dc_AddScalar (dc, &when, sample, F_Lookup("airspeed"), 
		      &pk->gcs_airspeed);
	dc_SetLoc (dc, sample, &locn);
/*
 * Record the most recent valid time
 */
	*latest = when;

	return (1);
}



static int
QCPacket (pk, badval)
GCSpacket21 *pk;
float badval;
/*
 * Do some range-checking on the fields of the packet.  If any are out of
 * range, set them to badval instead.  Return non-zero if any were found
 * out of spec, zero otherwise.
 */
{
	struct range {
		bool test;
		float min;
		float max;
	} ranges[] = 
	{
		{ TRUE, DEGREES_2_RADS(34.0), 
			  DEGREES_2_RADS(39.0)		}, /* lat */
		{ TRUE, DEGREES_2_RADS(-100.5), 
			  DEGREES_2_RADS(-92.5)		}, /* lon */
		{ TRUE,		0.0,		20000.0 }, /* alt */
		{ FALSE,	0,		0	}, /* time */
		{ TRUE,		-1*PI/2,	PI/2	}, /* pitch */
		{ TRUE,		-1*PI/2,	PI/2	}, /* roll */
		{ TRUE,		0,		2*PI	}, /* heading */
		{ TRUE,		0,		200	}, /* airspeed */
	};
	static int nranges = sizeof(ranges)/sizeof(ranges[0]);
	float *values;
	int i, glitch;

	values = (float *)pk;
	glitch = 0;
	for (i = 0; i < nranges; ++i)
	{
		if (ranges[i].test && 
		    (values[i] < ranges[i].min || values[i] > ranges[i].max))
		{
			glitch = TRUE;
			values[i] = badval;
		}
	}
	if (glitch)
		IngestLog (EF_DEVELOP, "   QCPacket: field out of range");
	return (glitch);
}




static int
PacketHandler (fd)
int fd;
/*
 * Returns non-zero on end of file or read errors
 */
{
	GCSpacket21 gcs;
	int n;
	struct Source *source = GlobalSource;

	if ((n = BlockRead(source->s_fd, (char *)&gcs, sizeof(gcs))) == 
	       sizeof(gcs))
	{
		StorePacket (source, &gcs, &source->s_ref, source->s_pid);
		return (0);
	}
	else if (n == 0)	/* end of file */
		return (1);
	else
	{
		IngestLog (EF_PROBLEM, "read error on fifo: %s", ERROR_STRING);
		return (n);
	}
}



static void
SetupInput (source)
struct Source *source;
/*
 * Read data from the specified source using the given GPS reference
 * time, and store it to the given platform id.
 */
{
	if (source->s_type == Socket)
		OpenSocket (source);
	else
		OpenFile (source);
	IngestLog (EF_INFO, "Reading %s '%s'", (source->s_type == Socket) ?
		   "named pipe" : "file", source->s_name);

	msg_add_fd (source->s_fd, PacketHandler);
}



static int
BlockRead (fd, buf, len)
int fd;
char *buf;
int len;
/*
 * Fill buf with exactly len bytes, else return the error code from read()
 */
{
	int nread = 0;
	int result;

	while (nread < len)
	{
		result = read (fd, buf + nread, len - nread);
		if (result <= 0)
		{
			if (errno == EWOULDBLOCK)
				sleep (1);
			else if (errno != EINTR)
				return (result);
		}
		else
			nread += result;
	}
	return (nread);		/* which should be len */
}



static void
OpenFile (source)
struct Source *source;
{
	source->s_fd = open (source->s_name, O_RDONLY);
	if (source->s_fd < 0)
	{
		perror (source->s_name);	
		Die ("aborting", 3);
	}
}



static void
OpenSocket (source)
struct Source *source;
/*
 * This function does not return unless the socket was sucessfully
 * created and a file descriptor opened to it.
 */
{
	if (mkfifo (source->s_name, FIFO_PERMS) != 0)
	{
		if (errno != EEXIST)
		{
			IngestLog (EF_EMERGENCY, "pipe %s cannot be made, %s",
				   source->s_name, "aborting");
			Die ("aborting", 4);
		}
		else
		{
			IngestLog (EF_INFO, "using existing named pipe %s",
				   source->s_name);
		}
	}

	source->s_fd = open(source->s_name, O_RDONLY | O_NDELAY);
	if (source->s_fd < 0)
	{
		perror (source->s_name);
		/*
		 * If we couldn't open the fifo, remove the node as well
		 */
		Die (unlink(source->s_name) == 0 ? "aborting"
		     : "aborting; could not remove fifo node", 5);
	}
}



static void
Finish (source)
struct Source *source;
{
	if (source->s_dc && dc_GetNSample(source->s_dc) > 0)
	{
		ds_StoreBlocks (source->s_dc, FALSE, NULL, 0);
		dc_DestroyDC (source->s_dc);
		source->s_dc = NULL;
	}
	Close (source);
}



static void
Close (source)
struct Source *source;
{
	if (source->s_fd >= 0)
		close (source->s_fd);
	if (source->s_type == Socket)
		unlink (source->s_name);
}
