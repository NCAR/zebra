/*
 *  Ingest aircraft data from the FAA black box.
 */

# include <errno.h>
# include <math.h>
# include <fcntl.h>
# include <sgtty.h>
# include <sys/time.h>
# include <sys/types.h>
# include <sys/resource.h>
# include <signal.h>
# include <string.h>

# include "ac_ingest.h"


OurAc	AircraftList[MAXOURS];
int	NumAc = 0;

static int	Dispatcher (), ResetFd (), ProcessData (), CheckValues ();
static int	SeemsBad (), RawAlign (), AlignPacket ();
static int	SetBaudRate (), ReadChars (), IsOurs ();
static int	GetRawPacket (); 
int		DoData ();
static void	Go (), SetupIndirect (), Dial (), StoreData(); 
static void	CvtData (), CvtToLatLon ();
static int	AddTrans (), ChangeTrans (), DelTrans ();
static void	GetOurAircraft ();

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
	char	endcmd[2];

	msg_ELog (EF_DEBUG, "Dying...");
/*
 * Write out the transponder codes we're using.
 */
	fptr = fopen ("transfile", "w");
	for (i = 0; i < NumAc; i++)
		fprintf (fptr, "%s %s\n", AircraftList[i].platform,
			AircraftList[i].transponder);
	fclose (fptr);
/*
 * Send the kill command to the black box.
 */
	write (Fd, Kill, 1);
/*
 * Hang up the modem.
 */
	endcmd[0] = 0x0d;
	n_written = write (Fd, "at h0", 5);
  	n_written = write (Fd, endcmd, 1);
	sleep (10);
	close (Fd);
	ui_finish ();
	exit (0);
}


main (argc, argv)
int	argc;
char	**argv;
{
	SValue	v;
/*
 * Initialize.
 */
	msg_connect (MsgDispatcher, "Ac_Ingest");
	if (argc > 1)
	{
		ui_init ("/fcc/lib/ac_ingest.lf", FALSE, TRUE);
		v.us_v_ptr = argv[1];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
			SYMT_STRING, &v);
	}
	else ui_init ("/fcc/lib/ac_ingest.lf", TRUE, FALSE);
	ui_setup ("ac_ingest", &argc, argv, 0);
	SetupIndirect ();
	ds_Initialize ();
	Dobj.do_org = OrgScalar;
	Dobj.do_badval = BADVAL;
	Dobj.do_flags = 0;
	Dobj.do_nfield = 1;
	Dobj.do_fields[0] = "trans";
	Dobj.do_data[0] = ALLOC(float);
	Dobj.do_aloc = ALLOC(Location);
	
	signal (SIGINT, Die);
	signal (SIGTERM, Die);
/*
 * Go into UI mode.
 */
	ui_get_command ("initial", "ac_ingest>", Dispatcher, 0);
	free (Dobj.do_data);
	free (Dobj.do_aloc);
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
		msg_ELog (EF_PROBLEM, "Can't find platform %s to change.",
			temp[0]);
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

	usy_c_indirect (vtable, "black_box", &BlackBox, SYMT_STRING, STRLEN);
	usy_c_indirect (vtable, "dial_out", &DialOut, SYMT_STRING, STRLEN);
	usy_c_indirect (vtable, "baud_rate", &BaudRate, SYMT_INT, 0);
	usy_c_indirect (vtable, "our_aircraft", &OurAircraft, SYMT_STRING, 
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
	char	endcmd[2];
	char	buf[BUFLEN];
/*
 * Open the black box file.
 */
	msg_ELog (EF_INFO, "Dialing...");
	if (strcmp (BlackBox, "fake") == 0)
	{
		Fd = open (BlackBox, O_RDWR);
		return;
	}
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
/*
 * Write ate0 
 */
	endcmd[0] = 0x0d;
	n_written = write (Fd, "at e", 4);
  	n_written = write (Fd, endcmd, 1);
	sleep (10);
	n_read = read (Fd, &buf, BUFLEN);
/*
 * Write atv0 
 */
	n_written = write (Fd, "at v", 4);
  	n_written = write (Fd, endcmd, 1);
	sleep (10);
	n_read = read (Fd, &buf, BUFLEN);
/*
 * Dial the black box.
 */
	n_written = write (Fd, DialOut, strlen (DialOut));
  	n_written = write (Fd, endcmd, 1);
	sleep (10);
/*
 * Read back the ok.
 */
	n_written = read (Fd, &buf, BUFLEN);
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
	sleep(10);
  	n_written = write (Fd, Command, 1);
	msg_ELog (EF_DEBUG, "wrote command %d", n_written);
  	sleep(10);
	
  	n_written = write (Fd, Kill, 1);
	msg_ELog (EF_DEBUG, "wrote kill %d", n_written);
  	n_written = write (Fd, endcmd, 1);
	msg_ELog (EF_DEBUG, "wrote endcmd %d", n_written);
  	sleep(10);

  	n_written = write (Fd, Startup, strlen (Startup));
	msg_ELog (EF_DEBUG, "wrote startup %d", n_written);
  	n_written = write (Fd, endcmd, 1);
	msg_ELog (EF_DEBUG, "wrote endcmd %d", n_written);
  	sleep(10);

  	n_written = write (Fd, SendAll, 1);
	msg_ELog (EF_DEBUG, "wrote sendall %d", n_written);

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
	msg_add_fd (Fd, DoData);
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

	if ((size = GetRawPacket (&packet)) > 0)
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
/*
 * If the data is bad, align it.
 */
	if (size == -1) 
	{
		if (! RawAlign (&packet))
		{
			if (! (Fd = ResetFd (Fd, BlackBox, BaudRate)))
			{
				msg_ELog (EF_EMERGENCY, 
					"Can't reset black box.");
			}
		}
		else if (ProcessData (&packet))
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
/*
 * If there was an error reading, try to reset the black box.
 */
	if (size == -2)
	{
		if (! (Fd = ResetFd (Fd, BlackBox, BaudRate)))
		{
			msg_ELog (EF_EMERGENCY, "Can't reset black box.");
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
	time	t;
	
	tl_GetTime (&t);
	if ((Dobj.do_id = ds_LookupPlatform (Ac_Platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Unknown platform '%s'", Ac_Platform);
		return;
	}
	Dobj.do_begin = t;
	Dobj.do_end = t;
	Dobj.do_times = &t;
	Dobj.do_npoint = 1;
	Dobj.do_data[0][0] = aircraft.transponder;
	Dobj.do_aloc->l_alt = aircraft.altitude; 
	Dobj.do_aloc->l_lat = aircraft.latitude; 
	Dobj.do_aloc->l_lon = aircraft.longitude; 
	if (CheckValues (aircraft))
		ds_PutData (&Dobj, FALSE);
	else
	{
		msg_ELog (EF_INFO, "alt %f %f lat %f %f lon %f %f", 
			AltMin, AltMax, LatMin, LatMax, LonMin, LonMax);
		msg_ELog (EF_INFO, "Discarding a track point (%f %f %f).",
		aircraft.altitude, aircraft.latitude, aircraft.longitude);
	}
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
 * Convert altitude in hundreds of feet to km.
 */
	aircraft->altitude = (float) packet->altitude * 100.0 * M_PER_FT / 
		1000.0;
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
ProcessData (ptr)
char	*ptr;
/*
 * The input ptr is a valid aircraft packet.  If it is one
 * of our aircraft then process it and return TRUE otherwise return FALSE.
 */
{
	Packet		*p;
	unsigned short	raw_range, raw_az, raw_trans;
	short		raw_alt;
  
	p = (Packet *) ptr;
/*
 * Process the transponder code.
 */
	raw_trans = 
	  (p->transponder & 0x1000) >> 12 | (p->transponder & 0x0800) >> 10 |
	  (p->transponder & 0x0400) >>  8 | (p->transponder & 0x0200) >>  6 |
	  (p->transponder & 0x0100) >>  4 | (p->transponder & 0x0040) >>  1 |
	  (p->transponder & 0x0020) <<  1 | (p->transponder & 0x0010) <<  3 |
	  (p->transponder & 0x0008) <<  5 | (p->transponder & 0x0004) <<  7 |
	  (p->transponder & 0x0002) <<  9 | (p->transponder & 0x0001) << 11;

	msg_ELog (EF_DEBUG, "raw_trans = %o", raw_trans);
/*
 * If this is one of our aircraft then process the rest of the data.
 */
	if (IsOurs (raw_trans))
	{
		raw_range = 
		  (p->range & 0x0800) >> 11 |
		  (p->range & 0x0400) >>  9 | (p->range & 0x0200) >>  7 |
		  (p->range & 0x0100) >>  5 | (p->range & 0x0040) >>  2 |
		  (p->range & 0x0020)       | (p->range & 0x0010) <<  2 |
		  (p->range & 0x0008) <<  4 | (p->range & 0x0004) <<  6 |
		  (p->range & 0x0002) <<  8 | (p->range & 0x0001) << 10;

		raw_az = 
		  (p->azimuth & 0x1000) >> 12 | (p->azimuth & 0x0800) >> 10 |
		  (p->azimuth & 0x0400) >>  8 | (p->azimuth & 0x0200) >>  6 |
		  (p->azimuth & 0x0100) >>  4 | (p->azimuth & 0x0040) >>  1 |
		  (p->azimuth & 0x0020) <<  1 | (p->azimuth & 0x0010) <<  3 |
		  (p->azimuth & 0x0008) <<  5 | (p->azimuth & 0x0004) <<  7 |
		  (p->azimuth & 0x0002) <<  9 | (p->azimuth & 0x0001) << 11;

		raw_alt = 
		  (p->altitude & 0x1000) >> 12 | (p->altitude & 0x0800) >> 10 |
		  (p->altitude & 0x0400) >>  8 | (p->altitude & 0x0200) >>  6 |
		  (p->altitude & 0x0100) >>  4 | (p->altitude & 0x0040) >>  1 |
		  (p->altitude & 0x0020) <<  1 | (p->altitude & 0x0010) <<  3 |
		  (p->altitude & 0x0008) <<  5 | (p->altitude & 0x0004) <<  7 |
		  (p->altitude & 0x0002) <<  9;

		if (p->altitude & 0x0001)	/* sign bit is on */
			raw_alt = -raw_alt;

		msg_ELog (EF_DEBUG, "raw_range = %d", raw_range);
		msg_ELog (EF_DEBUG, "raw_az = %d", raw_az);
		msg_ELog (EF_DEBUG, "raw_alt = %d", raw_alt);

		p->transponder = raw_trans;
		p->range = raw_range;
		p->azimuth = raw_az;
		p->altitude = raw_alt;
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
RawAlign (buf)
char	*buf;
/*
 * Align the incoming data from Fd into a packet.  Return FALSE for error 
 * else return TRUE.
 */
{
  int i;
  
	for (i = 0; i < A_FEW; i++)
		if (AlignPacket (buf)) return (TRUE);
	msg_ELog (EF_PROBLEM, "Can't align data.");
	return (FALSE);
}


static int 
GetRawPacket (buf)
char	*buf;
/*
 * Read the next raw ac packet and return it.
 * Return # of chars read in else -1 for bad data or -2 for error reading.
 */
{
	int	i;
  
	i = ReadChars (Fd, buf, sizeof (Packet));
	if (i <= 0)
	{
		msg_ELog (EF_DEBUG, "Error reading data.");
		return (-2);	
	}
	if (SeemsBad ((Packet *) buf))
	{
		msg_ELog (EF_DEBUG, "Data seems bad, need to re-align.");
		return (-1);
	}
	return (i);
}


static int 
AlignPacket (buf)
char 	*buf;
/*
 * Align the packet. Return FALSE for error, TRUE for a good packet.
 */
{
	char	buffer[BUFLEN], buffer2[BUFLEN];
	int	at = -1;
	int	i, j, p_len = sizeof (Packet);
 
/*
 * Read in the packet and find the start of good data.
 */ 
	do 
	{
		for (i = 0; i < p_len; i++)
			buffer[i] = 0;
		i = ReadChars (Fd, &buffer, p_len);
		if (i <= 0) return (FALSE);
  
		for (i = 0; i < p_len; i++)
		/* 
		 * If first three bits are 100, found packet start.
		 */
			if ((buffer[i] & 0xE0) == 0x80)  
			{
				at = i;
				break;
			}
	} 
	while (at == -1);
/* 
 * Copy the "good" part of the buffer to the output packet.
 */
	for (i = at, j = 0; i < p_len; i++, j++)
		buffer2[j] = buffer[i];
  
/*
 * Get the last part of the packet. 
 */
	if (at != 0)
	{
		if ((i = ReadChars(Fd, &buffer2[p_len - at], at)) < 0)
		{
			msg_ELog (EF_DEBUG, "Error reading data (%d).",
				errno);
			return (FALSE);
		}
	}
	strcpy (buf, buffer2);
	if (SeemsBad ((Packet *) buf)) return (FALSE);
	else return (TRUE);
}


static int 
ReadChars (fd, buf, n)
int	fd;
int	n;
char	buf[];
/*
 * Keep reading chars till n of them get into buf, return # read in.
 */
{
	int	i, j, k;
  
	i = n;
	for (j = 0; j < n; )
	{
		if ((k = read (fd, &buf[j], i)) < 0)
		{
			msg_ELog (EF_DEBUG, "Error reading (%d).", errno);
			return (0);
		}
		if (k <= 0) break;
		j += k;
		i -= k;
	}
	return (j);
}


static int 
SeemsBad (p)
Packet	*p;
/*
 * Return TRUE if this seems like it aint no packet.
 */
{
	char	*c = (char *) p;
/*
 * Verify that all bytes have correct id bits set. 
 */
	if (((c[0] & 0xE0) != 0x80) || /* first three bits should be 100 */
    	    ((c[1] & 0x80) != 0x00) || /* first bit should be 0 */
     	    ((c[2] & 0xE0) != 0xC0) || /* first three bits should be 110 */
     	    ((c[3] & 0x80) != 0x00) || /* first bit should be 0 */
     	    ((c[4] & 0xE0) != 0xA0) || /* first three bits should be 101 */
     	    ((c[5] & 0x80) != 0x00) || /* first bit should be 0 */
     	    ((c[6] & 0xE0) != 0xE0) || /* first three bits should be 111 */
     	    ((c[7] & 0x80) != 0x00))   /* first bit should be 0 */
	{
		return (TRUE);
	}
	return (FALSE);
}


static int 
ResetFd (fd, name, baud)
int	fd;
char	*name;
int	baud;
/*
 * Open the given file at the given baud rate.  Return the new fd
 * if successful, else return FALSE.
 */
{
	int	i, j, newfd;
/*  
  
	msg_ELog (EF_DEBUG, "Resetting %s fd %d baud %d.", name, fd, baud);
	close (fd);
	if (*name == 0) newfd = -1;
	else if (strcmp(name, "fake") == 0) 
	{
		newfd = open (name, O_RDWR);
		return (newfd);
	}
	newfd = open (name, O_RDWR );
	if (newfd <= 0 )
	{
		msg_ELog (EF_DEBUG, "Error opening port %s (%d).",
			 name, errno);
		newfd = -1;
	}
	else if (! SetBaudRate (newfd, baud)) 
	{
		close (newfd);
		newfd = -1;
	}
	if (newfd >= 0) 
	{
		Dial ();
		msg_add_fd (newfd, DoData);
		return (newfd);
	}
	else return (FALSE);
*/
}



static int 
SetBaudRate (fd, baud)
int	fd, baud;
/*
 * Try to put the tty line in raw mode at the baud rate, 
 * return TRUE or FALSE.
 */
{
	struct  sgttyb tbuf;
  
	tbuf.sg_flags = RAW;
	switch (baud)
	{
		case 300:
			tbuf.sg_ispeed = B300;
			tbuf.sg_ospeed = B300;
			break;
		case 1200:
			tbuf.sg_ispeed = B1200;
			tbuf.sg_ospeed = B1200;
			break;
		case 2400:
			tbuf.sg_ispeed = B2400;
			tbuf.sg_ospeed = B2400;
			break;
		case 4800:
			tbuf.sg_ispeed = B4800;
			tbuf.sg_ospeed = B4800;
			break;
		case 9600:
			tbuf.sg_ispeed = B9600;
			tbuf.sg_ospeed = B9600;
			break;
		default:
			return (FALSE);
	}
	if (ioctl (fd, TIOCSETP, &tbuf) != 0)
	{
		msg_ELog (EF_DEBUG, "ioctl error (%d)", errno);
		return (FALSE);
	}
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
