/*
 * This program ingests images from a NOAA ISCCP format tape into the 
 * zeb data store
 */
# include <errno.h>
# include <stdio.h>
# include <string.h>
# include <math.h>
# include <fcntl.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include "keywords.h"
# include "tapestruct.h"

/*
 * Source device name and associated file descriptor.
 */
char	Source[80] = "";
int	Fd = -1;

/*
 * Our platform
 */
char	Platname[80] = "";

/*
 * Raw images, their header, time, and IR calibration array
 */
# define NROWS	1100
# define NCOLS	1100
unsigned char	RawImageIR[NROWS * NCOLS];
unsigned char	RawImageVis[NROWS * NCOLS];
float		IRCal[256];
bool		HaveVis;
int		Year, Month, Day, Hour, Minute;
fileheader	Fheader;



/*
 * The grids into which we remap the image
 */
unsigned char	*GridIR, *GridVis;
float  	IRScale, IROffset;
int	GridX = 0, GridY = 0;
float	KmResolution = 0.0;
float	Lon_min, Lon_max, Lat_min, Lat_max;
float	Lat_step, Lon_step;
int 	HaveLimits = FALSE;

/*
 * Reference latitude for x,y <-> lat,lon conversions
 */
float	OriginLat = -999.0;

/*
 * Useful stuff
 */
# define DEG_TO_RAD(x)	((x)*0.017453292)
# define DEG_TO_KM(x)	((x)*111.3238367) /* on a great circle */
# define KM_TO_DEG(x)	((x)*0.008982802) /* on a great circle */

/*
 * Prototypes
 */
static int	Dispatcher FP ((int, struct ui_command *));
static void	UserLimits FP ((struct ui_command *));
static void	Ingest FP ((void));
static int	GetRawImages FP ((void));
static void	MapToGrids FP ((void));
static void	WriteGrids FP ((void));
static int	MDispatcher FP ((struct message *));
static int	ExtractInt FP ((char *, int));
static float	ExtractFloat FP ((char *, int));
static void	EbcdicToAscii FP ((char *, int));
static void	AsciiToEbcdic FP ((char *, int));
static int	my_read ();





main (argc, argv)
int	argc;
char	**argv;
{
	SValue	v;
	stbl	vtable;
	char	loadfile[128];
/*
 * Connect to the message system
 */
	msg_connect (MDispatcher, "gmsISCCP");
/*
 * UI stuff
 */
	fixdir ("GMSISCCP_LOAD_FILE", GetLibDir (), "gmsISCCP.lf", loadfile);

	if (argc > 1)
	{
		ui_init (loadfile, FALSE, TRUE);
		v.us_v_ptr = argv[1];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
			SYMT_STRING, &v);
	}
	else
		ui_init (loadfile, TRUE, FALSE);

	ui_setup ("gmsISCCP", &argc, argv, 0);
/*
 * Initialization
 */
	ds_Initialize ();

	vtable = usy_g_stbl ("ui$variable_table");
	usy_c_indirect (vtable, "originLat", &OriginLat, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "kmResolution", &KmResolution, SYMT_FLOAT, 0);
	usy_c_indirect (vtable, "platform", Platname, SYMT_STRING, 
			sizeof (Platname));
	usy_c_indirect (vtable, "gridX", &GridX, SYMT_INT, 0);
	usy_c_indirect (vtable, "gridY", &GridY, SYMT_INT, 0);
	usy_c_indirect (vtable, "source", Source, SYMT_STRING, 
			sizeof (*Source));
	
/*
 * Get on with it
 */
	ui_get_command ("initial", "gmsISCCP>", Dispatcher, 0);
	ui_finish ();

	msg_ELog (EF_INFO, "Finished.");

	exit (0);
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
	/*
	 * Time to actually do things.
	 */
	    case KW_GO:
		Ingest ();
		break;
	/*
	 * lat/lon limits
	 */
	    case KW_LIMITS:
		UserLimits (cmds + 1);
		break;
	/*
	 * Unknown command
	 */
	    default:
		msg_ELog (EF_PROBLEM, "Unknown kw %d", UKEY (*cmds));
		break;
	}
	return (TRUE);
}




static void
UserLimits (cmds)
struct ui_command	*cmds;
/*
 * Get user specified lat/lon limits for the grid
 * ...and do some sanity checking...
 */
{
	Lat_min = UFLOAT (cmds[0]);
	Lon_min = UFLOAT (cmds[1]);
	Lat_max = UFLOAT (cmds[2]);
	Lon_max = UFLOAT (cmds[3]);

	if ((Lat_min < Lat_max) && (Lon_min < Lon_max))
	{
		/* values are valid, note as much */
		HaveLimits = TRUE;
	}
	else
	{
		/* illegal values: tell user */
		msg_ELog (EF_PROBLEM, "bad lat/lon limits: %f %f %f %f",
			  Lat_min, Lon_min, Lat_max, Lon_max);
		HaveLimits = FALSE;
	}
}





static void
Ingest ()
{
	int	imagecount = 0;
/*
 * Make sure we have lat/lon limits, a platform name, and a source
 */
	if (! HaveLimits)
	{
		msg_ELog (EF_PROBLEM, 
			  "Lat/lon limits not specified.  Exiting.");
		exit (1);
	}

	if (! Platname[0])
	{
		msg_ELog (EF_PROBLEM, "No platform specified.  Exiting.");
		exit (1);
	}

	if (! Source[0])
	{
		msg_ELog (EF_PROBLEM, "No source name specified.  Exiting.");
		exit (1);
	}

	if (OriginLat < -90.0)
	{
		msg_ELog (EF_PROBLEM, "No origin latitude given.  Exiting.");
		exit (1);
	}
	
/*
 * Figure out grid spacing
 */
	if (GridX && GridY)
	{
		if (KmResolution != 0.0)
			msg_ELog (EF_INFO, 
				"Gridsize overrides kmResolution setting");

		Lat_step = (Lat_max - Lat_min) / (GridY - 1);
		Lon_step = (Lon_max - Lon_min) / (GridX - 1);
	}
	else if (KmResolution != 0.0)
	{
		Lat_step = Lon_step = KM_TO_DEG (KmResolution);

		GridX = (int)((Lon_max - Lon_min) / Lon_step) + 1;
		GridY = (int)((Lat_max - Lat_min) / Lat_step) + 1;

		Lon_max = Lon_min + Lon_step * (GridX - 1);
		Lat_max = Lat_min + Lat_step * (GridY - 1);
	}
	else
	{
		msg_ELog (EF_PROBLEM, 
			"gridX and gridY or kmResolution must be given");
		exit (1);
	}

	msg_ELog (EF_INFO, "Lat. limits: %.2f to %.2f every %.2f",
		Lat_min, Lat_max, Lat_step);
	msg_ELog (EF_INFO, "Lon. limits: %.2f to %.2f every %.2f",
		Lon_min, Lon_max, Lon_step);
/*
 * Open the input source (tape device)
 */
	Fd = open (Source, O_RDONLY);
	if (Fd < 1)
	{
		printf ("Error %d opening '%s'\n", errno, Source);
		exit(1);
	}
/*
 * Allocate the grid for the remapped image
 */
	GridIR = (unsigned char *) malloc (GridX * GridY * sizeof (char));
	GridVis = (unsigned char *) malloc (GridX * GridY * sizeof (char));
/*
 * Loop through the images
 */
	while (GetRawImages ())
	{
	/*
	 * Remap the image to our grid and write it out
	 */
		MapToGrids ();
		WriteGrids ();
		msg_ELog (EF_INFO, "Did image %d", ++imagecount);
	}

	msg_ELog (EF_INFO, "Finished after %d images", imagecount);
}




static int
GetRawImages ()
/*
 * Read the next raw image from the given file.  Return FALSE when we
 * hit the EOT.
 */
{
	int	nbytes, i, irline, visline, nblocks, hhmm;
	lcword	lcw;
	taperec	record;
	static int	eofflag = FALSE;
/*
 * Zero out the raw images
 */
	memset (RawImageIR, '\0', sizeof (RawImageIR));
	memset (RawImageVis, '\0', sizeof (RawImageVis));
/*
 * Read until we hit a header record.  Quit if we hit a double EOF.
 */
	nbytes = 0;
	
	while (nbytes != sizeof (fileheader))
	{
		nbytes = my_read (Fd, &Fheader, sizeof (fileheader));
		
		if (nbytes == 0)
		{
			msg_ELog (EF_DEBUG, "Read EOF");
			if (eofflag)
			{
				msg_ELog (EF_INFO, "End of tape");
				return (FALSE);
			}
			eofflag = TRUE;
		}
		else
		{
			msg_ELog (EF_DEBUG, "%d byte rec.", nbytes);
			eofflag = FALSE;
		}
	}
	
	EbcdicToAscii ((char *)&Fheader, sizeof (fileheader));
/*
 * Grab the IR calibration table
 */
	for (i = 0; i < 256; i++)
		IRCal[i] = ExtractFloat (Fheader.irtable + 7*i, 7);

	IROffset = IRCal[0];
	IRScale = 255.0 / (IRCal[255] - IRCal[0]);
/*
 * Read data lines up to the next EOF
 */
	irline = 0;
	visline = 0;
	
	while (TRUE)
	{
	/*
	 * Read the next record
	 */
		nbytes = my_read (Fd, &record, sizeof (taperec));
		
		if (nbytes == 0)
		{
			msg_ELog (EF_DEBUG, "EOF. Raw image complete.");
			eofflag = TRUE;
			break;
		}
		
		msg_ELog (EF_DEBUG, "%d byte data record", nbytes);
	/*
	 * Convert the EBCDIC stuff to ASCII
	 */
		EbcdicToAscii ((char *)&record.rheader, 
			       sizeof (rechdr));
		
		nblocks = ExtractInt (record.rheader.blockcount, 
				      sizeof (record.rheader.blockcount));
		
		for(i = 0; i < nblocks; i++)
		{
			
			EbcdicToAscii ((char *)&record.block[i].lcw, 
				       sizeof (lcword));
			EbcdicToAscii ((char *)record.block[i].tail, 
				       8);
		}
	/*
	 * Copy the pixel data to the image array
	 */
		for (i = 0; i < nblocks; i++)
		{
			char	type = record.block[i].lcw.type;
 		/*
		 * Move the data into the image and increment the
		 * line count.  IR and vis are handled separately.
		 */
			if (type == 'I')
			{
				memcpy (RawImageIR + (irline * NCOLS), 
					record.block[i].pixels, 1100);
				irline++;
			}
			else if (type == 'V')
			{
				memcpy (RawImageVis + (visline * NCOLS), 
					record.block[i].pixels, 1100);
				visline++;
			}
			else
				msg_ELog (EF_DEBUG,
					  "Unknown data type '%c' ignored");
		}		
	}
/*
 * Did we get a vis image?
 */
	HaveVis = visline > 0;
/*
 * Extract a time for the image
 */
	lcw = record.block[0].lcw;

	Year = ExtractInt (lcw.year, sizeof (lcw.year));
	Month = ExtractInt (lcw.month, sizeof (lcw.month));
	Day = ExtractInt (lcw.day, sizeof (lcw.day));

	hhmm = ExtractInt (lcw.gmtime, sizeof (lcw.gmtime));
	Hour = hhmm / 100;
	Minute = hhmm % 100;

	return (TRUE);
}




static void
MapToGrids ()
/*
 * Map the raw satellite image into our final image grid
 */
{
	int	i, j, line, elem, gridpos, rawpos;
	float	lat, lon, val;

	NavInit (&Fheader);

	for (j = 0; j < GridY; j++)
	{
		lat = Lat_max - Lat_step * j;

		for (i = 0; i < GridX; i++)
		{
			lon = Lon_min + Lon_step * i;

			gridpos = j * GridX + i;

			Navigate (lat, lon, &line, &elem);
			rawpos = line * NCOLS + elem;

			val = IRCal[RawImageIR[rawpos]];
			GridIR[gridpos] = (unsigned char)
				(IRScale * (val - IROffset) + 0.5);
		/*
		 * We leave visible unscaled
		 */
			if (HaveVis)
				GridVis[gridpos] = RawImageVis[rawpos];
		}
	}
}
			



static void
WriteGrids ()
/*
 * Write the image in Grid to the data store
 */
{
	ZebTime		t;
	DataChunk	*dc;
	Location	loc;
	RGrid		rg;
	FieldId		fid[2];
	ScaleInfo	scale[2];
	PlatformId	pid;
/*
 * Build the zeb time
 */
	TC_ZtAssemble (&t, Year - 1900, Month, Day, Hour, Minute, 0, 0);
/*
 * Build the location and rgrid information
 */
	loc.l_lat = Lat_min;
	loc.l_lon = Lon_min;
	loc.l_alt = 0.000;

	rg.rg_Xspacing = DEG_TO_KM (Lon_step) * cos (DEG_TO_RAD (OriginLat));
	rg.rg_Yspacing = DEG_TO_KM (Lat_step);
	rg.rg_Zspacing = 0.0;

	rg.rg_nX = GridX;
	rg.rg_nY = GridY;
	rg.rg_nZ = 1;
/*
 * Build a field list
 */
	fid[0] = F_DeclareField ("ir", "infrared", "K");
	scale[0].s_Scale = IRScale;
	scale[0].s_Offset = IROffset;

	fid[1] = F_DeclareField ("vis", "visible", "");
	scale[1].s_Scale = 1.0;
	scale[1].s_Offset = 0.0;
/*
 * Get our platform
 */
	if ((pid = ds_LookupPlatform (Platname)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform '%s'", Platname);
		exit (1);
	}
/*
 * Create and initialize a data chunk
 */
	dc = dc_CreateDC (DCC_Image);
	dc->dc_Platform = pid;
	dc_ImgSetup (dc, HaveVis ? 2 : 1, fid, scale);
/*
 * Insert the images
 */
	dc_ImgAddImage (dc, 0, fid[0], &loc, &rg, &t, GridIR, GridX * GridY);
	if (HaveVis)
		dc_ImgAddImage (dc, 0, fid[1], &loc, &rg, &t, GridVis, 
				GridX * GridY);
/*
 * Write out the data chunk
 */
	ds_Store (dc, TRUE, NULL, 0);
/*
 * Free the data chunk
 */
	dc_DestroyDC (dc);
}




static int
ExtractInt (string, len)
char	*string;
int	len;
/*
 * Read and return an integer from the first len characters of string
 */
{
	char	*copy = malloc ((len + 1) * sizeof (char));
	int	val;

	strncpy (copy, string, len);
	copy[len] = '\0';
	val = atoi (copy);
	free (copy);
	return (val);
}




static float
ExtractFloat (string, len)
char	*string;
int	len;
/*
 * Read and return a float from the first len characters of string
 */
{
	char	*copy = malloc ((len + 1) * sizeof (char));
	float	val;

	strncpy (copy, string, len);
	copy[len] = '\0';
	val = (float) atof (copy);
	free (copy);
	return (val);
}




static int
MDispatcher (msg)
struct message *msg;
/*
 * Deal with a message.
 */
{
	struct mh_template *tmpl = (struct mh_template *) msg->m_data;

	switch (msg->m_proto)
	{
	   case MT_MESSAGE:
		if (tmpl->mh_type == MH_DIE)
		{
			msg_ELog (EF_INFO, "Received DIE message.  Bye.");
			exit (1);
		}
		break;
	}
	return (0);
}   	



static int
my_read (fd, dest, len)
int	fd;
void	*dest;
int	len;
/*
 * Sort of like read(), but we need to buffer the I/O since the tape
 * blocking is incorrect.
 */
{
# define BUFSIZE 65536
	static char	buffer[BUFSIZE];
	static int	buflen = 0;
	static bool	save_eof = FALSE;
	int	nbytes, returnlen;
/*
 * If we have an EOF "saved" from previously, and no data in the
 * buffer, return the EOF now.
 */
	if (save_eof && buflen == 0)
	{
		save_eof = FALSE;
		return (0);
	}
/*
 * Buffer up data until we have the amount requested or until we hit an
 * EOF
 */
	while (buflen < len)
	{
		nbytes = read (fd, buffer + buflen, BUFSIZE - buflen);
		if (nbytes < 0)
			return (nbytes);
		else if (nbytes == 0)
		{
			if (buflen == 0)
				return (0);
			else
			/*
			 * "save" this EOF if we have some data to return
			 */
			{
				save_eof = TRUE;
				break;
			}
		}
		else
			buflen += nbytes;
	}
/*
 * Return what was requested or at least as much as possible
 */
	returnlen = (buflen > len) ? len : buflen;
	memcpy (dest, buffer, returnlen);
/*
 * Move the remaining data to the beginning of the buffer
 */
	buflen -= returnlen;
	memcpy (buffer, buffer + returnlen, buflen);
	return (returnlen);
}

	

/*
 * Below is the stuff for ASCII <-> EBCDIC conversions
 */
static char EbToAs[256] = 
{
	'\000', '\001', '\002', '\003', '\234', '\011', '\206', '\177',
	'\227', '\215', '\216', '\013', '\014', '\015', '\016', '\017',
	'\020', '\021', '\022', '\023', '\235', '\205', '\010', '\207',
	'\030', '\031', '\222', '\217', '\034', '\035', '\036', '\037',
	'\200', '\201', '\202', '\203', '\204', '\012', '\027', '\033',
	'\210', '\211', '\212', '\213', '\214', '\005', '\006', '\007',
	'\220', '\221', '\026', '\223', '\224', '\225', '\226', '\004',
	'\230', '\231', '\232', '\233', '\024', '\025', '\236', '\032',
	'\040', '\240', '\241', '\242', '\243', '\244', '\245', '\246',
	'\247', '\250', '\133', '\056', '\074', '\050', '\053', '\041',
	'\046', '\251', '\252', '\253', '\254', '\255', '\256', '\257',
	'\260', '\261', '\135', '\044', '\052', '\051', '\073', '\136',
	'\055', '\057', '\262', '\263', '\264', '\265', '\266', '\267',
	'\270', '\271', '\174', '\054', '\045', '\137', '\076', '\077',
	'\272', '\273', '\274', '\275', '\276', '\277', '\300', '\301',
	'\302', '\140', '\072', '\043', '\100', '\047', '\075', '\042',
	'\303', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
	'\150', '\151', '\304', '\305', '\306', '\307', '\310', '\311',
	'\312', '\152', '\153', '\154', '\155', '\156', '\157', '\160',
	'\161', '\162', '\313', '\314', '\315', '\316', '\317', '\320',
	'\321', '\176', '\163', '\164', '\165', '\166', '\167', '\170',
	'\171', '\172', '\322', '\323', '\324', '\325', '\326', '\327',
	'\330', '\331', '\332', '\333', '\334', '\335', '\336', '\337',
	'\340', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
	'\173', '\101', '\102', '\103', '\104', '\105', '\106', '\107',
	'\110', '\111', '\350', '\351', '\352', '\353', '\354', '\355',
	'\175', '\112', '\113', '\114', '\115', '\116', '\117', '\120',
	'\121', '\122', '\356', '\357', '\360', '\361', '\362', '\363',
	'\134', '\237', '\123', '\124', '\125', '\126', '\127', '\130',
	'\131', '\132', '\364', '\365', '\366', '\367', '\370', '\371',
	'\060', '\061', '\062', '\063', '\064', '\065', '\066', '\067',
	'\070', '\071', '\372', '\373', '\374', '\375', '\376', '\377'
};


static char AsToEb[256] = 
{
	'\000', '\001', '\002', '\003', '\067', '\055', '\056', '\057', 
	'\026', '\005', '\045', '\013', '\014', '\015', '\016', '\017', 
	'\020', '\021', '\022', '\023', '\074', '\075', '\062', '\046', 
	'\030', '\031', '\077', '\047', '\034', '\035', '\036', '\037', 
	'\100', '\117', '\177', '\173', '\133', '\154', '\120', '\175', 
	'\115', '\135', '\134', '\116', '\153', '\140', '\113', '\141', 
	'\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367', 
	'\370', '\371', '\172', '\136', '\114', '\176', '\156', '\157', 
	'\174', '\301', '\302', '\303', '\304', '\305', '\306', '\307', 
	'\310', '\311', '\321', '\322', '\323', '\324', '\325', '\326', 
	'\327', '\330', '\331', '\342', '\343', '\344', '\345', '\346', 
	'\347', '\350', '\351', '\112', '\340', '\132', '\137', '\155', 
	'\171', '\201', '\202', '\203', '\204', '\205', '\206', '\207', 
	'\210', '\211', '\221', '\222', '\223', '\224', '\225', '\226', 
	'\227', '\230', '\231', '\242', '\243', '\244', '\245', '\246', 
	'\247', '\250', '\251', '\300', '\152', '\320', '\241', '\007', 
	'\040', '\041', '\042', '\043', '\044', '\025', '\006', '\027', 
	'\050', '\051', '\052', '\053', '\054', '\011', '\012', '\033', 
	'\060', '\061', '\032', '\063', '\064', '\065', '\066', '\010', 
	'\070', '\071', '\072', '\073', '\004', '\024', '\076', '\341', 
	'\101', '\102', '\103', '\104', '\105', '\106', '\107', '\110', 
	'\111', '\121', '\122', '\123', '\124', '\125', '\126', '\127', 
	'\130', '\131', '\142', '\143', '\144', '\145', '\146', '\147', 
	'\150', '\151', '\160', '\161', '\162', '\163', '\164', '\165', 
	'\166', '\167', '\170', '\200', '\212', '\213', '\214', '\215', 
	'\216', '\217', '\220', '\232', '\233', '\234', '\235', '\236', 
	'\237', '\240', '\252', '\253', '\254', '\255', '\256', '\257', 
	'\260', '\261', '\262', '\263', '\264', '\265', '\266', '\267', 
	'\270', '\271', '\272', '\273', '\274', '\275', '\276', '\277', 
	'\312', '\313', '\314', '\315', '\316', '\317', '\332', '\333', 
	'\334', '\335', '\336', '\337', '\352', '\353', '\354', '\355', 
	'\356', '\357', '\372', '\373', '\374', '\375', '\376', '\377'
};





static void
EbcdicToAscii (array, len)
char	*array;
int	len;
{
	int	i;

	for (i = 0; i < len; i++)
		array[i] = EbToAs[(unsigned char) array[i]];
}



static void
AsciiToEbcdic (array, len)
char	*array;
int	len;
{
	int	i;

	for (i = 0; i < len; i++)
		array[i] = AsToEb[(unsigned char) array[i]];
}
