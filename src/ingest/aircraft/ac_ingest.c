/*
 *  Ingest aircraft data from the FAA black box.
 */
/*		Copyright (C) 1991-95 by UCAR
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

static char *rcsid = "$Id: ac_ingest.c,v 1.10 1998-09-12 17:38:18 burghart Exp $";

# include <copyright.h>
# include <errno.h>
# include <math.h>
# include <fcntl.h>
# include <sys/time.h>
# include <sys/types.h>
# include <sys/resource.h>
# include <signal.h>
# include <string.h>

# ifdef linux
# include <termios.h>
# endif

# ifdef SUNOS5
# include <termio.h>
# endif

# ifndef SUNOS5
# include <unistd.h>
# endif

# include <config.h>
# include "ac_ingest.h"
# include "ratshex.h"

/*# include <sgtty.h>*/

# define DIAL

OurAc	AircraftList[MAXOURS];
int	NumAc = 0;

static int	Dispatcher (), ProcessData (), CheckValues ();
static int	SetBaudRate (), IsOurs ();
static int	GetAcPacket (); 
int 		DoData ();
static Packet   RATS2packet ();
static void	Go (), SetupIndirect (), Dial (), StoreData(); 
static void	CvtData (), CvtToLatLon ();
static int	AddTrans (), ChangeTrans (), DelTrans ();
static void	GetOurAircraft (), StartPhonyData (), MakePhonyData ();
static void  	doOsLoad();
static void  	writeRats();

MsgDispatcher (msg)
struct message	*msg;
/*
 * Deal with a message.
 */
{
	struct mh_template *tmpl = (struct mh_template *) msg->m_data;
	char sendstr[3];

	switch (msg->m_proto)
	{
		case MT_MESSAGE:
			if (tmpl->mh_type == MH_DIE)
				Die ();
			break;
		case MT_ACINGEST:
			switch (msg->m_data[0])
			{
			  case 'a':
		    	    if (AddTrans (msg->m_data + 2))
				sendstr[0] = 'a';
			    else sendstr[0] = 'n';
			    sendstr[1] = '\0';
			    msg_send (msg->m_from, MT_ACINGEST, FALSE,
				sendstr, sizeof (sendstr));
			    break;
			  case 'c':
			    if (ChangeTrans (msg->m_data + 2))
				sendstr[0] = 'c';
			    else sendstr[0] = 'm';
			    sendstr[1] = '\0';
			    msg_send (msg->m_from, MT_ACINGEST, FALSE,
				sendstr, sizeof (sendstr));
		  	    break;
			  case 'd':
			    if (DelTrans (msg->m_data + 2))
				sendstr[0] = 'd';
			    else sendstr[0] = 'o';
			    sendstr[1] = '\0';
			    msg_send (msg->m_from, MT_ACINGEST, FALSE,
				sendstr, sizeof (sendstr));
			    break;
			  default:
			    msg_ELog (EF_PROBLEM, 
				"Unknown transponder command.");
			    break;
			}
			break;
		default:
			msg_ELog (EF_DEBUG, "What's this %d?", msg->m_proto);
			break;
	}
	return (0);
}


Die ()
/*
 * Finish gracefully.
 */
{
	FILE	*fptr;
	int	i, n_written;
	char	endcmd[2], buf[500];

	msg_ELog (EF_INFO, "Dying...");
	endcmd[0] = 0x0d;
/*
 * Quick death for phony data
 */
	if (PhonyData)
	{
		ui_finish ();
		exit (0);
	}
/*
 * Send the kill command to the black box.
 */
	writeRats (Shut_Down, 1);
	sleep (2);
/*
 * Write out the transponder codes we're using.
 */
	fptr = fopen ("transfile", "w");
	for (i = 0; i < NumAc; i++)
		fprintf (fptr, "%s %s\n", AircraftList[i].platform,
			AircraftList[i].transponder);
	fclose (fptr);
/*
 * Clean out junk.
 */
	/* read (Fd, buf, 500);*/
/*
 * Hang up the modem.
 */
	sleep (2);
	n_written = write (Fd, "+++", 3);
	sleep (2);
	writeRats ("at h0", 5);

	close (Fd);
	ui_finish ();
	exit (0);
}


main (argc, argv)
int	argc;
char	**argv;
{
	SValue	v;
	char	loadfile[100];
	FieldId	fid;
	DC_ElemType	type_int = DCT_Integer;
/*
 * Initialize.
 */
	msg_connect (MsgDispatcher, "Ac_Ingest");
	fixdir ("ACILOADFILE", GetLibDir(), "ac_ingest.lf", loadfile);
	if (argc > 1)
	{
		ui_init (loadfile, FALSE, TRUE);
		v.us_v_ptr = argv[1];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
			SYMT_STRING, &v);
	}
	else
		ui_init (loadfile, TRUE, FALSE);

	ui_setup ("ac_ingest", &argc, argv, 0);
	SetupIndirect ();
	ds_Initialize ();
/*
 * Create and initialize a data chunk with one integer field (transponder code)
 */
	Dc = NULL;
	Dc = dc_CreateDC (DCC_MetData);

	Fid = F_Lookup ("transponder");
	dc_SetupFields (Dc, 1, &Fid);
	dc_SetBadval (Dc, BADVAL);
	dc_SetFieldTypes (Dc, 1, &Fid, &type_int);
/*
 * Deal with signals
 */	
	signal (SIGINT, Die);
	signal (SIGTERM, Die);
/*
 * Go into UI mode.
 */
	ui_get_command ("initial", "ac_ingest>", Dispatcher, 0);
	Die ();
}


static void
GetOurAircraft ()
/*
 * Initialize the AircraftList.
 */
{
	char	temp_ours[BUFLEN], *aircraft[STRLEN];
	int	num, i;

	strcpy (temp_ours, OurAircraft);
	num = CommaParse (temp_ours, aircraft);
	NumAc = (int) num / 2;
	for (i = 0; i < NumAc; i++)
	{
		strcpy (AircraftList[i].platform, aircraft[2*i]);
		strcpy (AircraftList[i].transponder,aircraft[2*i+1]);
	}
}


static int
AddTrans (data)
char	*data;
/*
 * Add a new aircraft and its transponder code to the list of our
 * aircrafts.
 */
{
	char	*temp[STRLEN];
	int	i, num;

	msg_ELog (EF_DEBUG, "Add %s", data);
	num = CommaParse (data, temp);
	if (num != 2)
	{
		msg_ELog (EF_PROBLEM, "Bad data to add %s.", data);
		return (FALSE);
	}
	for (i = 0; i < NumAc; i++)
		if (strcmp (AircraftList[i].platform, temp[0]) == 0)
		{
			msg_ELog (EF_DEBUG, "Platform already added.");
			return (FALSE);
		}
	strcpy (AircraftList[NumAc].platform, temp[0]);
	strcpy (AircraftList[NumAc].transponder, temp[1]);
	NumAc++;
	return (TRUE);
}


static int
ChangeTrans (data)
char	*data;
/*
 * Change the transponder code of one of our aircrafts.
 */
{
	char	*temp[STRLEN];
	int	num, i;

	msg_ELog (EF_DEBUG, "Change %s", data);
	num = CommaParse (data, temp);
	if (num != 2)
	{
		msg_ELog (EF_PROBLEM, "Bad data to change %s.", data);
		return (FALSE);
	}
	for (i = 0; i < NumAc; i++)
	{
		if (strcmp (AircraftList[i].platform, temp[0]) == 0)
		{
			strcpy (AircraftList[i].transponder, temp[1]);
			break;
		}
	}
	if (i >= NumAc)
	{
		msg_ELog (EF_PROBLEM, "Can't find platform %s to change.", temp[0]);
		return (FALSE);
	}
	return (TRUE);
}


static int
DelTrans (data)
char	*data;
/*
 * Delete an aircraft from the list of our aircrafts.
 */
{
	char	*temp[STRLEN];
	int	num, i, itsat = -1;

	msg_ELog (EF_DEBUG, "Delete %s", data);
	num = CommaParse (data, temp);
	if (num != 2)
	{
		msg_ELog (EF_PROBLEM, "Bad data to delete %s.", data);
		return (FALSE);
	}
	for (i = 0; i < NumAc; i++)
	{
		if ((strcmp (AircraftList[i].platform, temp[0]) == 0) &&
		    (strcmp (AircraftList[i].transponder, temp[1]) == 0))
		{
			itsat = i;
			break;
		}
	}
	if (itsat < 0)
	{
		msg_ELog (EF_PROBLEM, "Can't find platform %s to delete.",
			temp[0]);
		return (FALSE);
	}
	else
	{
		for (i = itsat; i < NumAc - 1; i++)
		{
			strcpy (AircraftList[i].platform, 
				AircraftList[i+1].platform);
			strcpy (AircraftList[i].transponder, 
				AircraftList[i+1].transponder);
		}
		NumAc--;
	}
	return (TRUE);
}


static void
SetupIndirect ()
/*
 * Create all the indirect variables which are used to control things.
 */
{
	stbl vtable = usy_g_stbl ("ui$variable_table");

	usy_c_indirect (vtable, "black_box", BlackBox, SYMT_STRING, STRLEN);
	usy_c_indirect (vtable, "dial_out", DialOut, SYMT_STRING, STRLEN);
	usy_c_indirect (vtable, "baud_rate", &BaudRate, SYMT_INT, 0);
	usy_c_indirect (vtable, "our_aircraft", OurAircraft, SYMT_STRING, 
			BUFLEN);
	usy_c_indirect (vtable, "range_res", &RangeRes, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "azimuth_res", &AzimuthRes, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "radar_lat", &RadarLat, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "radar_lon", &RadarLon, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "alt_min", &AltMin, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "alt_max", &AltMax, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "lat_min", &LatMin, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "lat_max", &LatMax, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "lon_min", &LonMin, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "lon_max", &LonMax, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "phony_data", &PhonyData, SYMT_BOOL, 0);
	usy_c_indirect (vtable, "phony_interval", &PhonyTime, SYMT_FLOAT, 0);
}


static int
Dispatcher (junk, cmds)
int	junk;
struct ui_command	*cmds;
/*
 * The command dispatcher.
 */
{
	switch (UKEY (*cmds))
	{
		case AIC_DIAL:
			Dial();
			break;
		case AIC_GO:
			Go ();
			break;
		default:
			msg_ELog (EF_PROBLEM, "Unknown kw %d", UKEY (*cmds));
			break;
	}
	return (TRUE);
}


static void
Dial ()
/*
 * Dial the black box and send the startup commands to it.
 */
{
	int	n_written, n_read;
	Packet bogus;
	int	junk;
	char	buf[BUFLEN];
/*
 * Open the black box file.
 */
	msg_ELog (EF_INFO, "Dialing...");
	if (PhonyData)
		return;
	if ((Fd = open (BlackBox, O_RDWR)) <= 0)
	{
		msg_ELog (EF_EMERGENCY, "Error opening %s (%d).", BlackBox,
			errno);
		Die ();
	}
	if (! SetBaudRate (Fd, BaudRate))
    	{
		msg_ELog (EF_EMERGENCY, "Error setting baud to %d.", BaudRate);
		Die ();
	}

#ifdef DIAL
/*
 * Write ate0 
 */
	writeRats ("at e", 4);
	sleep (2);
	n_read = read (Fd, buf, BUFLEN);
/*
 * Write atv0 
 */
	writeRats ("at v", 4);
	sleep (2);
	n_read = read (Fd, buf, BUFLEN);
/*
 * Dial the black box.
 */
	writeRats (DialOut, strlen (DialOut));
	sleep (2);
/*
 * Read back the ok.
 */
	n_written = read (Fd, buf, BUFLEN);
	msg_ELog (EF_DEBUG, "Dialed (%c).", buf[0]);
	switch (buf[0])
	{
		case '0':
		case '1':
		case '5':
			break;
		case '3':
			msg_ELog (EF_PROBLEM, "No carrier detected.");
			Die ();
			break;
		case '4':
			msg_ELog (EF_PROBLEM, "Command error.");
			Die ();
			break;
		case '6':
			msg_ELog (EF_PROBLEM, "No dialtone.");
			Die ();
			break;
		case '7':
			msg_ELog (EF_PROBLEM, "Black Box is busy.");
			Die ();
			break;
		case '8':
			msg_ELog (EF_PROBLEM, "No answer.");
			Die ();
			break;
		default:
			msg_ELog (EF_PROBLEM, "Didn't expect to get (%c).",
				buf[0]);
			Die ();
			break;
	}
	sleep(2);
#endif
	/* sometimes helps to shut down the system first */
  	/* writeRats (Shut_Down, 1);*/

	/* tcflush (Fd, TCIOFLUSH);
	sleep (2);*/

  	writeRats (Start_Up, 8);
	msg_ELog (EF_DEBUG, "wrote startup %s", Start_Up );

	read (Fd, buf, BUFLEN);

	sleep (2);

#ifndef TEST
 	writeRats (Send_All, 1);
    msg_ELog (EF_INFO, "wrote send all tracks %s", Send_All);

  	sleep(2);
#else
    /* for debugging purposes */
	writeRats ("1", 1);
	writeRats ("3", 1);
	sleep (1);
#endif


	msg_ELog (EF_INFO, "Connected.");
}


static void
Go ()
/*
 * Start reading data from the black box and/or messages.
 */
{
	GetOurAircraft ();
	print_list ();
	if (! PhonyData)
		msg_add_fd (Fd, DoData);
	else
		StartPhonyData ();
	msg_await ();
}


int
DoData ()
/*
 * Handler which deals with data on the black box.
 */
{
	Packet	packet;
	Ac_Data	aircraft;
	int	size;

	if ((size = GetAcPacket (&packet)) > 0)
	{
	/*
	 * Test for our transponder codes and store data.
	 */
	    if (ProcessData (&packet))
	    {
		CvtData (&packet, &aircraft);
		msg_ELog (EF_DEBUG, "transponder %o", 
			  aircraft.transponder);
		msg_ELog (EF_DEBUG, "altitude %f", aircraft.altitude);
		msg_ELog (EF_DEBUG, "latitude %f", aircraft.latitude);
		msg_ELog (EF_DEBUG, "longitude %f", aircraft.longitude);
		StoreData (aircraft);
	    }
	}
		
	return (0);
}


static void
StoreData (aircraft)
Ac_Data	aircraft;
/*
 * Put the aircraft data in the data store.
 */
{
	ZebTime		zt, samp_time;
	Location	loc;
	
	tl_Time (&zt);
	msg_ELog (EF_INFO, "Got %d @ %s", aircraft.transponder, 
		  TC_AscTime (&zt, TC_TimeOnly));

	if ((Dc->dc_Platform = ds_LookupPlatform (Ac_Platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Unknown platform '%s'", Ac_Platform);
		return;
	}

	loc.l_lat = aircraft.latitude;
	loc.l_lon = aircraft.longitude;
	loc.l_alt = aircraft.altitude;

	dc_SetLoc (Dc, 0, &loc);
	dc_AddMData (Dc, &zt, Fid, sizeof (int), 0, 1, &aircraft.transponder);
	dc_GetTime (Dc, 0, &samp_time);
	msg_ELog (EF_INFO, "and stored it with time %s", 
		  TC_AscTime (&samp_time, TC_TimeOnly));
	
	ds_Store (Dc, 0, NULL, 0);
# ifdef notdef
	if (CheckValues (aircraft))
		ds_PutData (&Dobj, FALSE);
	else
	{
		msg_ELog (EF_INFO, "alt %f %f lat %f %f lon %f %f", 
			AltMin, AltMax, LatMin, LatMax, LonMin, LonMax);
		msg_ELog (EF_INFO, "Discarding a track point (%f %f %f).",
		aircraft.altitude, aircraft.latitude, aircraft.longitude);
	}
# endif
}


static int
CheckValues (a)
Ac_Data	a;
{
	if ((a.altitude < AltMin) || (a.altitude > AltMax))
		return (FALSE);
	if ((a.latitude < LatMin) || (a.latitude > LatMax))
		return (FALSE);
	if ((a.longitude < LonMin) || (a.longitude > LonMax))
		return (FALSE);
	return (TRUE);
}


static void
CvtData (packet, aircraft)
Packet	*packet;
Ac_Data	*aircraft;
/*
 * Convert data in the Packet format to the Ac_Data format.
 */
{
	float	temp_az, temp_range, x, y, lat, lon;
/*
 * Convert the transponder code.
 */
	aircraft->transponder = (int) packet->transponder;
/*
 * Convert altitude in hundreds of feet to m.
 */
	aircraft->altitude = (float) packet->altitude * 100.0 * M_PER_FT;
/*
 * Convert azimuth (lsb = AzimuthRes (0.0875 deg)) to radians.
 */
	temp_az = (float) packet->azimuth;
	temp_az = 90.0 - temp_az * AzimuthRes;
	if (temp_az < 0.0) temp_az += 360.0;
	temp_az = temp_az * PI / 180.0;
/*
 * Convert range (lsb = RangeRes (1/8 nm)) to km.
 */
	temp_range = (float) packet->range;
	temp_range = temp_range * RangeRes * KM_PER_NM;
/*
 * Calculate x and y offsets and then convert to latitude and longitude (deg).
 */
	x = temp_range * (float) cos ((double) temp_az);
	y = temp_range * (float) sin ((double) temp_az);
	CvtToLatLon (x, y, &lat, &lon);
	aircraft->latitude = lat;
	aircraft->longitude = lon;
}


static void
CvtToLatLon (x, y, lat, lon)
float	x, y, *lat, *lon;
/*
 * Convert x and y (km) to lat and lon (deg).
 */
{
	float	del_lat, del_lon;
	float	r_lat, r_lon;
/*
 * Test the origin.
 */
	r_lat = RadarLat * PI / 180.0;
	r_lon = RadarLon * PI / 180.0;
	if (r_lat < -2 * PI || r_lon < -2 * PI)
	{
		msg_ELog (EF_EMERGENCY, "Bad radar latitude and longitude.");
		Die ();
	}
/*
 * Do the conversion.
 */
	del_lat = asin (y / R_EARTH);
	*lat = r_lat + del_lat;

	del_lon = asin (x / (R_EARTH * cos (*lat)));
	*lon = r_lon + del_lon;
/*
 * Convert to degrees.
 */
	*lat *= 180.0 / PI;
	*lon *= 180.0 / PI;
}


static int 
ProcessData (data)
Packet	*data;
/*
 * The input ptr is a valid aircraft packet.  If it is one
 * of our aircraft then process it and return TRUE otherwise return FALSE.
 */
{
	msg_ELog (EF_DEBUG, "raw_trans = %o", data->transponder);
/*
 * If this is one of our aircraft then process the rest of the data.
 */
	if (IsOurs (data->transponder))
	{
		msg_ELog (EF_DEBUG, "raw_range = %d", data->range);
		msg_ELog (EF_DEBUG, "raw_az = %d", data->azimuth);
		msg_ELog (EF_DEBUG, "raw_alt = %d", data->altitude);

		return (TRUE);
	}
	else return (FALSE);
}


static int
IsOurs (trans)
short	trans;
/*
 * If the aircraft with this trasnponder code is one of ours, return TRUE,
 * else return FALSE.
 */
{
	char	newtrans[STRLEN];
	int	i;
/*
 * Search through the transponder codes comparing them with the current
 * transponder code.
 */
	if (NumAc <= 0) return (FALSE);
	for (i = 0; i < NumAc; i++)
	{
		sprintf (newtrans, "%04o", trans);
		if (strcmp (newtrans, AircraftList[i].transponder) == 0)
		{
			strcpy (Ac_Platform, AircraftList[i].platform);
			return (TRUE);
		}
	}
	return (FALSE);
}

static int 
GetAcPacket (acPacket)
Packet *acPacket;

/*
 * Read the next raw ac packet and return it.
 * Keep reading chars till n of them get into buf, return # read in.
 */
{
	int	i, j, k;
	char *ptr;
	char buffer[500];
	char c;

	ptr = buffer;
	for (;;)
 	{
		i = read (Fd, &c, 1);
/*
 *      Eat "?" prompt, as we process everything ourselves.  We
 *      guarantee that good info is passed.
 */
		if (c == '?')
			continue;

/*
 *      Eat noise.
 */
		if (c == 0177)
            continue;

/*
 *      Eat "{", which is a noise signal.
 */
		if (c == '{')
			continue;
/*
 *      Eat '\n' and key on '\r'
 */
		if (c == '\n')
			continue;

		if (c != '\r')
			*ptr++ = c;
		else
		{
			*ptr++ = (char) NULL;
			msg_ELog (EF_DEBUG, "RATS output %s", buffer);

#ifdef TEST
			writeRats ("3", 1);
			sleep (1);
#endif
			if (strncmp(buffer,"Invalid command",15) == 0)
			{
				msg_ELog (EF_PROBLEM, "RATS program not running, shutting down software");
				doOsLoad();
				return(-1);
			}
			if (strlen(buffer) == 12)
			{
				*acPacket = RATS2packet (buffer);
				return (12);
			}
			else if (strncmp (buffer,"exception:",10) == 0 ||
					strncmp( buffer, "Illegal Opcode", 9 ) == 0 )
			{
				msg_ELog (EF_PROBLEM, ("RATS fault detected, reloading the os "));
				doOsLoad();
				return (-1);             
			}
			else
			{
				/* get rid of the first 12 characters */
				j = strlen(buffer);

				if (j >= 24)
				{
					j -= 12;
					*acPacket = RATS2packet(&buffer[j]);
					return (12);
				}

			}
			return (-1);
		}
	}
}

static int 
SetBaudRate (fd, baud)
int	fd, baud;
/*
 * Try to put the tty line in raw mode at the baud rate, 
 * return TRUE or FALSE.
 */
{
#ifdef SUNOS4
    struct  sgttyb tbuf;

	if (ioctl(Fd, TIOCGETP, (char *) &tbuf) < 0)
	{
		msg_ELog (EF_DEBUG, "ioctl error (%d)", errno);
		return (FALSE);
	}

	tbuf.sg_flags |= RAW;
	tbuf.sg_flags &= ~ECHO;
#endif

#ifdef linux
	struct  termios temp_mode;

	/*ioctl (Fd, TCGETS, &temp_mode);*/
	tcgetattr (Fd, &temp_mode);
	/* doesn't work :(
    	cfmakeraw (&temp_mode); */

    	temp_mode.c_iflag &= ~(IGNBRK | BRKINT | IGNPAR | PARMRK |
                    INPCK | ISTRIP | INLCR | IGNCR | ICRNL |
                    IXON | IXOFF | IUCLC | IXANY | IMAXBEL);

    	temp_mode.c_oflag &= ~(OPOST  | ONLCR);
    	temp_mode.c_lflag &= ~(ISIG | ICANON | XCASE | ECHO | ECHONL | ECHOPRT);
    	temp_mode.c_lflag |= (IEXTEN | ECHOE | ECHOK | ECHOCTL | ECHOKE);
    
		temp_mode.c_cc[VMIN] = 1;
    	temp_mode.c_cc[VTIME] = 0;
#endif

#ifdef SUNOS5
	struct  termio temp_mode;

	tcgetattr (Fd, &temp_mode);

    	temp_mode.c_iflag = 0;
    	temp_mode.c_oflag &= ~OPOST;
    
    	temp_mode.c_iflag &= ~(ISIG | ICANON | ECHO | XCASE);

    	temp_mode.c_cflag &= ~(CSIZE | PARENB);
    	temp_mode.c_cflag |= CS8; 
	temp_mode.c_cc[VMIN] = 1;
    	temp_mode.c_cc[VTIME] = 1; 
#endif

	switch (baud)
	{
	   case 300:  
#ifdef SUNOS4
          	tbuf.sg_ispeed = B300;
	  	tbuf.sg_ospeed = B300;
#else
	  	cfsetospeed (&temp_mode, B300);
	  	cfsetispeed (&temp_mode, B300);
#endif
	   	break;
	   case 1200:

#ifdef SUNOS4
		tbuf.sg_ispeed = B1200;
		tbuf.sg_ospeed = B1200;
#else
		cfsetospeed (&temp_mode, B1200);
		cfsetispeed (&temp_mode, B1200);
#endif
		break;

	   case 2400:

#ifdef SUNOS4
		tbuf.sg_ispeed = B2400;
		tbuf.sg_ospeed = B2400;
#else
		cfsetospeed (&temp_mode, B2400);
		cfsetispeed (&temp_mode, B2400);
#endif
		break;

	   case 4800:

#ifdef SUNOS4
		tbuf.sg_ispeed = B4800;
		tbuf.sg_ospeed = B4800;
#else
		cfsetospeed (&temp_mode, B4800);
		cfsetispeed (&temp_mode, B4800);
#endif

		break;
	   case 9600:

#ifdef SUNOS4
		tbuf.sg_ispeed = B9600;
		tbuf.sg_ospeed = B9600;
#else
		cfsetospeed (&temp_mode, B9600);
		cfsetispeed (&temp_mode, B9600);       
#endif
            	break;
	   default:
		return (FALSE);
	}
#ifdef SUNOS4
	if (ioctl (Fd, TIOCSETP, &tbuf) < 0)
	{
		msg_ELog (EF_DEBUG, "ioctl error (%d)", errno);
		return (FALSE);
	}
#else
	if (tcsetattr (Fd, TCSANOW, &temp_mode) != 0)
	{
		msg_ELog (EF_DEBUG, "tcsetattr error (%d)", errno);
		return (FALSE);
	}
#endif
	else return (TRUE);
}

print_list ()
{
	int	i;

	for (i = 0; i < NumAc; i++)
		msg_ELog (EF_INFO, "[%d] %s %s", i, AircraftList[i].platform,
			AircraftList[i].transponder);
	msg_ELog (EF_INFO, "End of AircraftList (%d)", NumAc);
}




static void
StartPhonyData ()
/*
 * Start generation of a phony data stream
 */
{
	tl_AddRelativeEvent (MakePhonyData, 0, (int)(PhonyTime * INCFRAC),
		(int)(PhonyTime * INCFRAC));
}



static void
MakePhonyData ()
/*
 * Make a phony data point, and associate it with the first aircraft on
 * the list
 */
{
	static int	count = 0;
	float	ang, x, y, lat, lon;
	Ac_Data	newpoint;
/*
 * "Fly" in a circle 20km from the radar
 */
	count++;
	count %= 360;

	ang = count * 0.017453293;	/* convert to radians */
	x = 20 * cos (ang);
	y = 20 * sin (ang);
	CvtToLatLon (x, y, &lat, &lon);

	strcpy (Ac_Platform, AircraftList[0].platform);
	newpoint.transponder = 0;
	newpoint.altitude = 0.0;
	newpoint.latitude = lat;
	newpoint.longitude = lon;

	msg_ELog (EF_DEBUG, "Adding a phony a/c point for %s", 
		AircraftList[0].platform);
	StoreData (newpoint);
}

static Packet
RATS2packet (buf)
char *buf;
{
    Packet  acPacket;
	int range, azimuth, transponder, altitude;

	range = azimuth = transponder = altitude = 0;

	sscanf (buf, "%3x%3x%3x%3x",&range,&azimuth,&transponder,&altitude);
	acPacket.range = (short) range;
	acPacket.azimuth = (short) azimuth;
	acPacket.transponder = (short) transponder;
	acPacket.altitude = (short) altitude;

	return (acPacket);
}    

static void
doOsLoad()
{
    	char **os_ptr;
	static int max_fault = 0;
	char    endcmd[2];
	int     n_written;
	char    buf[BUFLEN];

	msg_ELog (EF_DEBUG, "calling OS_LOAD...");

	max_fault++;

	if ( max_fault >= MAX_FAULT )
	{
		msg_ELog (EF_EMERGENCY, "Error, Too many faults detected. Shutting down software.");
		Die ();
	}

	os_ptr = rats_os;

	msg_ELog (EF_INFO, "Rats software is being reloaded.");

	writeRats ("LO", 2);

	while (os_ptr[0] != (char) NULL )
	{
		n_written = write (Fd, *os_ptr, strlen(os_ptr[0]));
		if (n_written != strlen(os_ptr[0]))
		{

			Die();
		}

		usleep (50000);
		*os_ptr++;
	}
	/*writeRats ("", 1);*/  /* required! */

	tcflush (Fd, TCIOFLUSH);

	/* sometimes the "box is hung up, a break sometimes fixes it */
	tcsendbreak (Fd, 0);

	sleep (2);

	msg_ELog (EF_DEBUG, "OS_LOAD complete");
	msg_ELog (EF_INFO, "Rats software is being restarted.");

	writeRats (Start_Up, 8);
	msg_ELog (EF_INFO, "wrote startup %s\n", Start_Up);

	read (Fd, buf, BUFLEN);	
	sleep (2);

#ifndef TEST
	writeRats (Send_All, 1);
	msg_ELog (EF_INFO, "wrote send all tracks %s\n", Send_All);
#else
	writeRats ("3",1);
	sleep (1);
#endif
}   

static void
writeRats (char *instruction, int size)
{
    char    endcmd[2];

	endcmd[0] = 0x0d;

	if (write (Fd, instruction, size) != size)
	{
		msg_ELog (EF_EMERGENCY, "Error writing %s to tty port, shutting down", instruction);
		Die();
	}
	if (write (Fd, endcmd, 1) != 1)
	{
		msg_ELog (EF_EMERGENCY, "Error writing endcmd  to tty port, shutting down"); 
		Die();
	}
}  
