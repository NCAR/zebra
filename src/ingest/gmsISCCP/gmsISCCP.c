/*
 * This program reads in a tape of  1100 by 1100 pixel images from a NOAA 
 * ISCCP format tape and puts them into the zeb data store
 */
# include <errno.h>
# include <stdio.h>
# include <fcntl.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include "tapestruct.h"

/*
 * Image size
 */
# define NROWS	1100
# define NCOLS	1100

/*
 * Globals
 */
static unsigned char	Image[NROWS * NCOLS];
static fileheader	Fheader;
static taperec		Record;

/*
 * Prototypes
 */
static int	ExtractInt FP ((char *, int));
static void	WriteImage FP ((int, int, int, int, int));
static int	MDispatcher FP ((struct message *));
static void	EbcdicToAscii FP ((char *, int));
static void	AsciiToEbcdic FP ((char *, int));
static int	my_read ();





main (argc, argv)
int	argc;
char	**argv;
{
	int	fd, nbytes, eofflag, i, line, nblocks;
	int	iyear, imonth, iday, ihhmm, ihour, iminute;
	int	reccount = 0, filecount = 0, imagecount = 0;
	lcword	lcw;
/*
 * Connect to the message system
 */
	msg_connect (MDispatcher, "SatIngest");
/*
 * Open the input file
 */
	fd = open (argv[1], O_RDONLY);
	if (fd < 1)
	{
		printf ("Error %d opening '%s'\n", errno, argv[1]);
		exit(1);
	}
/*
 * Some initializations
 */
	usy_init ();
	F_Init ();
	ds_Initialize ();
/*
 * Loop to read images until we hit the end of the tape
 */
	while (TRUE)
	{
	/*
	 * Read until we hit a header record (i.e., something that's not
	 * 80 bytes and not an EOF).  Quit if we hit a double EOF.
	 */
		nbytes = 0;

		while (nbytes != sizeof (fileheader))
		{
			nbytes = my_read (fd, &Fheader, sizeof (fileheader));
			
			if (nbytes == 0)
			{
				msg_ELog (EF_DEBUG, "Read EOF");
				filecount++;
				if (eofflag)
				{
					msg_ELog (EF_INFO, 
					  "No hdr (%d records, %d files).\n", 
					  reccount, filecount);
					exit (1);
				}
				eofflag = TRUE;
			}
			else
			{
				msg_ELog (EF_DEBUG, "%d byte rec.", nbytes);
				reccount++;
				eofflag = FALSE;
			}
		}
	/*
	 * Make sure it's an image header record
	 */
# ifdef notdef
		if (nbytes != sizeof (fileheader))
		{
			printf ("Short header (%d bytes) on image %d.\n",
				nbytes, ++imagecount);
			exit(1);
		}
# endif

		EbcdicToAscii ((char *)&Fheader, sizeof (fileheader));
	/*
	 * Read data lines up to the next EOF
	 */
		line = 0;

		while (TRUE)
		{
		/*
		 * Read the next record
		 */
			nbytes = my_read (fd, &Record, sizeof (taperec));

			if (nbytes == 0)
			{
				msg_ELog (EF_DEBUG, "Read EOF");
				filecount++;
				break;
			}
			
			msg_ELog (EF_DEBUG, "%d byte data rec.", nbytes);
			reccount++;
		/*
		 * Convert the EBCDIC stuff to ASCII
		 */
			EbcdicToAscii ((char *)&Record.rheader, 
				       sizeof (rechdr));

			nblocks = ExtractInt (Record.rheader.blockcount, 
					  sizeof (Record.rheader.blockcount));

			for(i = 0; i < nblocks; i++)
			{
				
				EbcdicToAscii ((char *)&Record.block[i].lcw, 
					   sizeof (lcword));
				EbcdicToAscii ((char *)&Record.block[i].tail, 
					       8);
			}
		/*
		 * Copy the pixel data to the image array
		 */
			for (i = 0; i < nblocks; i++)
			{
			/*
			 * Only IR for now
			 */
				if (Record.block[i].lcw.type != 'I')
					continue;
			/*
			 * Make sure we don't get too much
			 */
				if (line >= NROWS)
				{
					printf ("Rows > %d lost in img %d\n", 
						NROWS, imagecount + 1);
					break;
				}
			/*
			 * Move the data into the image and increment the
			 * line count
			 */
				memcpy (Image + (line * NCOLS),
					Record.block[i].pixels, 1100);

				line++;
			}		
		}
		msg_ELog (EF_INFO, "Did image %d", ++imagecount);
	/*
	 * Dump the image to the data store
	 */
		lcw = Record.block[0].lcw;

		iyear = ExtractInt (lcw.year, sizeof (lcw.year));
		imonth = ExtractInt (lcw.month, sizeof (lcw.month));
		iday = ExtractInt (lcw.day, sizeof (lcw.day));
		ihhmm = ExtractInt (lcw.gmtime, sizeof (lcw.gmtime));

		ihour = ihhmm / 100;
		iminute = ihhmm % 100;

		WriteImage (iyear, imonth, iday, ihour, iminute);
	}

	msg_ELog (EF_INFO, "Finished after %d images", imagecount);
}




static void
WriteImage (year, month, day, hour, minute)
int	year, month, day, hour, minute;
/*
 * Write the image in array Image to the data store
 */
{
	ZebTime		t;
	DataChunk	*dc;
	Location	loc;
	RGrid		rg;
	FieldId		fid;
	PlatformId	pid;
	ScaleInfo	scale;
/*
 * Build the zeb time
 */
	TC_ZtAssemble (&t, year - 1900, month, day, hour, minute, 0, 0);
/*
 * Build the location and rgrid information
 */
	loc.l_lat = -12.4572;
	loc.l_lon = 130.9253;
	loc.l_alt = 0.000;

	rg.rg_Xspacing = 1.0;
	rg.rg_Yspacing = 1.0;
	rg.rg_Zspacing = 0.0;

	rg.rg_nX = NCOLS;
	rg.rg_nY = NROWS;
	rg.rg_nZ = 1;
/*
 * Build a field list
 */
	fid = F_DeclareField ("ir", "", "");
	scale.s_Scale = 1.0;
	scale.s_Offset = 0.0;
/*
 * Get our platform
 */
	if ((pid = ds_LookupPlatform ("gms")) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform 'gms'");
		exit (1);
	}
/*
 * Create and initialize a data chunk
 */
	dc = dc_CreateDC (DCC_Image);
	dc->dc_Platform = pid;
	dc_ImgSetup (dc, 1, &fid, &scale);
/*
 * Insert the image
 */
	dc_ImgAddImage (dc, 0, fid, &loc, &rg, &t, Image, NROWS * NCOLS);
/*
 * Write out the data chunk
 */
	ds_Store (dc, 1, NULL, 0);
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





void
EbcdicToAscii (array, len)
char	*array;
int	len;
{
	int	i;

	for (i = 0; i < len; i++)
		array[i] = EbToAs[(unsigned char) array[i]];
}



void
AsciiToEbcdic (array, len)
char	*array;
int	len;
{
	int	i;

	for (i = 0; i < len; i++)
		array[i] = AsToEb[(unsigned char) array[i]];
}
