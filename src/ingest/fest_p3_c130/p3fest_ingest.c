/*
 * $Id: p3fest_ingest.c,v 1.4 1992-11-19 02:15:20 granger Exp $
 *
 * Ingest P3 format data files into Zeb using DCC_Scalar class DataChunks.
 * The general program flow is as follows:
 *
 * (1) Open the file given on the command line.
 * (2) Get the header record (type 1) to verify that the given file
 *     has the expected format
 * (3) Create and initialize the Scalar data chunk with the header info.
 *     Estimate the number of samples needed as the number of seconds in the
 *     the duration of the flight (between begin and end) found in the header.
 * (4) Find the first (and hopefully only) type 3 record and load the divisors into
 *     the P3_Fields array, which contains names, units, and locations in
 *     the type 5 record for all the fields we'll be ingesting.
 * (5) Search the rest of the file until EOF or record type 6 is found:
 *     for every type 5 record,
 *         for each field in P3_Fields, pull the short word from the record,
 *		convert to float using the field's divisor,
 *         	and insert the data into the DataChunk
 * (6) If no data found, error, else send the DataChunk to the DataStore.
 * (7) Done.
 *
 * Basically, this program only concerns itself with the processed data
 * rather than the raw data.  Hence records of type 2 and 4 are ignored.
 *----------------------------------------------------------------------*/

#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <errno.h>
#include "copyright.h"
#include "ingest.h"

#define MAX_WORDS_RECORD5	106
#define MAX_RECORD_SIZE		420	/* max size of any record in words */
#define PLATFORM_NAME		"p3"
#define INGEST_NAME		"P-3 Ingest"

/*
 * Some important field indices in a type 5 record
 */
#define LAT_DEG			12	/* location fields */
#define LAT_MIN			13
#define LON_DEG			14
#define LON_MIN			15
#define ALT			16	/* use radar altitude (ra) for locn */
#define HOUR			3	/* time fields */
#define MINUTE			4
#define SECOND			5
#define SW1			8	/* who knows what */
#define SW2			9
#define SW3			35
#define FL1			10	/* error flags */
#define FL2			11

/* ERROR FLAG BIT MASKS */
#define BIT(n)			(1ul<<(32-n))
#define ERR_TI			BIT(1)
#define ERR_LA			BIT(2)
#define ERR_LO			BIT(3)
#define ERR_RA			(1ul<<28)
#define ERR_ALT			ERR_RA
#define ERR_PS			(1ul<<27)
#define ERR_TA			(1ul<<26)
#define ERR_TD			(1ul<<25)
#define ERR_RD			(1ul<<24)
#define ERR_RS			(1ul<<23)
#define ERR_GS			(1ul<<22)
#define ERR_TS			(1ul<<21)
#define ERR_WGS			(1ul<<20)
#define ERR_TK			(1ul<<19)
#define ERR_HD			(1ul<<18)
#define ERR_PC			(1ul<<17)

#define BADVAL_DEFAULT		((float)-9999.0)

#define AttAircraftNumber	"aircraft_number"
#define AttFlightBeginTime	"flight_begin_time"
#define AttFlightEndTime	"flight_end_time"
#define AttIngestVersion	"ingest_version"

#define NUMBER(arr)	((unsigned long)(sizeof(arr)/sizeof(arr[0])))
#define Abs(x)		(((x)>0)?(x):(-(x)))



/*----------------------------------------
 * The first 17 words correspond to the stored format of the header.  I/O
 * should always be done through the 'record' field of the union, since the
 * ZebTime's are strictly ancillary info gathered from the actual header
 * record.
 */
typedef union {
   short record[17];
   struct p3header_s {
	short type;		/* type of record (should be 1) */
	short size;		/* number of words in record */
	short aircraft;		/* aircraft number */
	short year;		/* year of flight */
	short month;
	short day;
	short hour_begin;	/* time flight began */
	short minute_begin;
	short second_begin;
	short hour_end;		/* time flight ended */
	short minute_end;
	short second_end;
	short date_processed;
	short date_program;
	short unused1;
	short unused2;
	short tape;		/* number of tape */
   } p3header;
   struct times_s {
	struct p3header_s dummy;
	ZebTime flight_begin;	/* time flight began */
	ZebTime flight_end;	/* time flight ended */
   } times;
} P3_header_t;

/* 
 * Type 5 data fields: This array lists all of the fields which will be
 * stored into the datachunk.  This array holds info to use in translating
 * the data in a type 5 record into Zeb fields.  Each structure in the
 * array contains the long name of the field, and a corresponding field
 * name to be used in the actual field definition.  The index field of the
 * structure corresponds to the word address of the field in a type 5
 * record.  The field ID's are stored in a separate array since it must be
 * initialized at run-time and passed as a whole to set the scalar fields
 * in the data chunk.  The divisor for each field taken from type 3 records
 * is also kept separately since divisors are needed for some fields,
 * specifically lat/lon, which will not be stored in the datachunk.  
 */

typedef struct p3_field_s {
	short	index;		/* locn (# of word) of this field in type 5 record */	
	char 	*long_name;	/* descriptive name of field */
	char 	*field_name;	/* short field name */
	char	*units;		/* some string describing the units in this field */
} p3_field_t;



/* 
 * Most of the info about type 5 fields will be automatically initialized
 * here.  The field id must wait until the field is declared.  Required
 * fields like lat/lon and sample time are not included here unless they
 * are to be specifically kept as field data in addition to being stored
 * with the DataChunk.  
 */

#define UNITS_UNKNOWN 	"unknown"
#define UNITS_NONE	""

p3_field_t P3_Fields[] =
{
/* RA will be stored with the samples location				*/
/*	{ 16, 	"Radar altitiude", 		"ra",		"m" },  */

	{ 17, 	"Pressure", 			"ps",		"mb" },
	{ 18, 	"Ambient temperature", 		"ta",		"C" },
	{ 19, 	"Dewpoint sensor", 		"tw1",		"C" },
	{ 20, 	"Radiometer (Down)", 		"rd",		"C" },
	{ 21, 	"Radiometer (Side)", 		"rs",		"C" },
	{ 22, 	"Ground Speed", 		"gs",		"m/s" },
	{ 23, 	"True Airspeed", 		"ts",		"m/s" },
	{ 24, 	"A/C Vertical velocity",	"wgs",		"m/s" },
	{ 25,	"Track",			"tk",		"deg" },
	{ 26, 	"Heading (true)", 		"hd",		"deg" },
	{ 27,	"Pitch",			"pc",		"deg" },
	{ 28,	"Roll",				"rl",		"deg" },
	{ 29,	"Attack Angle",			"aa",		"deg" },
	{ 30,	"Slip Angle",			"sa",		"deg" },
	{ 31,	"Liquid Water Content",		"lw",		"g/m3" },
	{ 32,	"Dynamic Pressure",		"pq",		"mb" },
	{ 33,	"Dewpoint Temperature",		"td",		"C" },
	{ 34,	"Radiometer Up",		"ru",		"C" },
	/* { 35,	"Switches",		"sw3",		UNITS_NONE }, */
	{ 36,	"E/W Velocity of Tail",		"utail",	"m/s" },
	{ 37,	"N/S Velocity of Tail",		"vtail",	"m/s" },
	{ 38,	"Vertical Velocity of Tail",	"wtail",	"m/s" },
	/* 39,  ---blank--- */
	{ 40,	"Geopotential Altitude",	"ga",		"m" },
	{ 41,	"Pressure Altitude",		"pa",		"m" },
	{ 42,	"D Value",			"dv",		"m" },
	{ 43,	"Height Standard Pres Surface",	"ht",		"m" },
	{ 44,	"Surface Pressure",		"sp",		"mb" },
	{ 45,	"Relative Humidity",		"rh",		"%" },
	{ 46,	"Virtual Temperature",		"tv",		"K" },
	{ 47,	"Vertical Airspeed",		"was",		"m/s" },
	{ 48,	"Ratio Specific Heats",		"gm",		UNITS_NONE },
	{ 49,	"Mach Number",			"ama",		"dim" },
	{ 50,	"Drift Angle",			"da",		"deg" },
	{ 51,	"E/W Ground Speed",		"gsx",		"m/s" },
	{ 52,	"N/S Ground Speed",		"gsy",		"m/s" },
	{ 53,	"E/W True Airspeed",		"tx",		"m/s" },
	{ 54,	"N/S True Airspeed",		"ty",		"m/s" },
	{ 55,	"E/W Wind Speed",		"wx",		"m/s" },
	{ 56,	"N/S Wind Speed",		"wy",		"m/s" },
	{ 57,	"Vertical Wind Speed",		"wz",		"m/s" },
	{ 58,	"Wind Speed",			"ws",		"m/s" },
	{ 59,	"Wind Direction",		"wd",		"deg" },
	/* 60,  ---blank--- */
	{ 61,	"Vapor Pressure",		"ee",		"mb" },
	{ 62,	"Mixing Ratio",			"mr",		"g/kg" },
	{ 63,	"Potential Temperature",	"pt",		"K" },
	{ 64, 	"Equivalent Potential Temperature", "et",	"K" },
	{ 65,	"E/W Average Wind",		"wxb",		"m/s" },
	{ 66,	"N/S Average Wind",		"wyb",		"m/s" },
	{ 67,	"Average Wind Speed",		"wsb",		"m/s" },
	{ 68,	"Average Wind Direction",	"wdb",		"deg" },
	{ 69,	"Vertical Accelerometer #1",	"av1",		UNITS_UNKNOWN },
	{ 70,	"Vertical Accelerometer #2",	"av2",		UNITS_UNKNOWN },
	{ 71,	"Seconds Wind Averaged Over",	"wac",		"secs" },
	{ 72,	"AXBT1",			"bt1",		UNITS_UNKNOWN },
	{ 73, 	"AXBT2",			"bt2",		UNITS_UNKNOWN },
	{ 74,	"AXBT3",			"bt3",		UNITS_UNKNOWN }
	/* 75-106 ---ignored--- */
};



/*
 * The global field id's array, initialized by InitializeFieldIds(),
 * + 1 for the FlagsFid
 */
FieldId P3_FieldIds[NUMBER(P3_Fields)+1];

FieldId FlagsFid;

/*
 * The global storage for the type 3 divisors
 */
short P3_FieldDivisors[MAX_WORDS_RECORD5 + 1];

/*
 * Error bit fields:
 * Array of field addresses in a type 5 record.
 * The index into the array is the bit number (MSB to LSB, starting at 1)
 * signaling a potential error in the field
 * Use the P3_WordToField map to get the field info corresponding to
 * an error flag.  Error flags for the compound values of lat, lon, and
 * time are handled specially.
 */
struct ErrorField_s {
	char	bit;	/* The number of the flag bit */
	short	word;	/* The word address of the field into record */
}
P3_ErrorFields[] =
{
/*	{ 4, 16 },  */	/* Radar alt, handled specially  */
	{ 5, 17 },	/* Ambient pressure */
	{ 6, 18 },	/* Ambient temperature */
	{ 7, 19 },	/* Dewpoint sensor */
	{ 8, 20 },	/* Radiometer down */
	{ 9, 21 },	/* Radiometer side */
	{ 10, 22 },	/* Ground speed */
	{ 11, 23 },	/* True speed */
	{ 12, 24 },	/* Vertical speed */
	{ 13, 25 },	/* Track */
	{ 14, 26 },	/* Heading */
	{ 15, 27 },	/* Pitch */
	{ 16, 28 },	/* Roll */
	{ 17, 29 },	/* Attack angle */
	{ 18, 30 },	/* Slip angle */
	{ 19, 31 },	/* Liquid water */
	{ 20, 32 }	/* Dynamic pressure */
};


/*
 * To speed processing of a type 3 record (storing divisors into the
 * P3_Fields array), a mapping is created from the type 5 word index to
 * a the corresponding p3_field index in P3_Fields[] using an array of short.
 */
short P3_WordToField[MAX_WORDS_RECORD5 + 1];


/*-----------------------------------------
 * Prototypes
 */
static void ReadHeader FP((FILE *p3file, P3_header_t *hdr));
static void DumpHeader FP((P3_header_t *hdr));
static void PrintMenus FP((FILE *out));
static DataChunk *CreateDataChunk FP((P3_header_t *hdr));
static void StoreDataChunk FP((DataChunk *dc));
static int InitializeFieldIds ();
static void InitWordToFieldMapping ();
static short *ReadNextRecord FP((FILE *p3file, short *rec_type, short *nwords));
static void SearchForRecords FP((FILE *p3file));
static void LoadFieldDivisors FP((short buf[], short rtype, short rsize));
static void IngestDataRecord FP((DataChunk **dc, short buf[], 
				 short rtype, short rsize, P3_header_t *hdr));
static void IngestRecords FP((FILE *p3file, P3_header_t *hdr));
static long SeekFileSize FP((FILE *file));
extern void InterpolateGap
	FP((DataChunk *dc, ZebTime *btime, Location *blocn,
	    ZebTime *etime, Location *elocn, int *sample));

/*-----------------------------------------*/

void
Usage(prog)
	char *prog;
{
	fprintf(stderr,
		"Usage: %s [ingest options] { -m[enus] | <datafile> }\n",prog);
	fprintf(stderr,
		"   If <datafile> is '-', the data will be read from stdin\n");
	fprintf(stderr,
		"   -menus		Write out GP menus for P-3 fields\n");
	IngestUsage();
}



/*----------------------------------------------------------------
 * MAIN
 *
 * Parse options, initialize the ingest package, call initializers for
 * the global arrays, get the header of the file, create the data chunk,
 * fill in the data chunk from the file, and store the data chunk.
 */
int 
main(argc, argv)
	int argc;
	char *argv[];
{
	P3_header_t hdr;
 	FILE *p3file;

	/* Parse any ingest options. */
	IngestParseOptions(&argc, argv, Usage);
	
	if (argc != 2)
	{
		Usage(argv[0]);
		exit(1);
	}

	/*
	 * For the -menu option, nothing is required other than dumping our
	 * field and menu info to stdout.  Do this and get out.
	 */
	if ((strlen(argv[1]) > 1) && !strncmp(argv[1],"-menu",strlen(argv[1])))
	{
		PrintMenus(stdout);
		exit(0);
	}

	/*
	 * Initialize usy, message, DataStore, and Fields simultaneously
	 */
	IngestInitialize(INGEST_NAME);

	/* 
	 * The file to read will be argv[1].  A "-" will indicate
	 * stdin.
	 */
	if (!strcmp(argv[1],"-"))
	{
		p3file = stdin;
	}
	else if ((p3file = fopen(argv[1],"r")) == NULL)
	{
		perror(argv[1]);
		exit(1);
	}

	/*
	 * We have our file open and ready, so go read the header.
	 */
	ReadHeader(p3file,&hdr);

	/*
	 * Define our fields to the DataStore and store the FieldIds in
	 * the P3_FieldIds array
	 */
	if (!InitializeFieldIds())
	{
		IngestLog(EF_EMERGENCY,
			  "Could not define necessary field ids");
		exit(1);
	}

	/*
	 * Initialize our word to field mapping to speed processing of type
	 * 3 records
	 */
	InitWordToFieldMapping();

	/*
	 * Now pull out the type 3 and 5 records.  The hdr is required to
	 * provide the base date and create the data chunk.
	 */
	IngestRecords(p3file, &hdr);

	/*
	 * And we're done...
	 */
	IngestLog(EF_INFO,"Finished.");
	exit(0);
}



void
StoreDataChunk(dc)
	DataChunk *dc;
{
	if (dc_GetNSample(dc) == 0)
	{
		/* Our DC is still empty for some reason */
		IngestLog(EF_PROBLEM,
			  "possible problem, trying to store empty datachunk");
		exit(1);
	}

	IngestLog(EF_INFO,"Storing %i samples...", dc_GetNSample(dc));
	/*
	 * Each raw p3 file will correspond to a single DataStore file
	 */
	ds_Store(dc, /*newfile*/ TRUE, /*details*/ NULL, /*ndetail*/ 0);
	IngestLog(EF_INFO,"Done storing samples");
}



/*----------------------------------------------------------------
 * IngestRecords(DataChunk *dc, FILE *p3file, P3_header_t *hdr)
 *
 * Loop through p3file, reading each recognized buffer into buf[]
 * with ReadNextRecord(), then handling each record according to its
 * type.  All but 3 and 5 are effectively ignored.  InitFieldDivisors()
 * stores the divisors of a type 3 into the P3_FieldDivisors array.
 * Type 5 data is stored into the data chunk with IngestDataRecord().
 */
void
IngestRecords(p3file, hdr)
	FILE *p3file;
	P3_header_t * hdr;
{
	DataChunk *dc;
	short rtype, rsize;
	short type3found;	/* Keep track of multiple type 3 records */
	short type6found;	/* Help detect data following the trailer */
	short *buf;
	int i;

	dc = CreateDataChunk(hdr);
	if (!dc)
	{
		IngestLog(EF_PROBLEM,"Could not create data chunk, aborting...");
		exit(1);
	}

	/* 
	 * Before we start, make sure all divisors in P3_FieldDivisors init'ed to 0
	 */
	for (i = 0; i < NUMBER(P3_Fields); ++i)
		P3_FieldDivisors[i] = 0;

	type6found = 0;
	type3found = 0;
	while (buf = (short *)ReadNextRecord(p3file, &rtype, &rsize))
	{
		/*
		 * We shouldn't have found any more data if type6found is true
		 */
		if (type6found)
		{
			IngestLog(EF_PROBLEM,"More records following the trailer!!");
			IngestLog(EF_PROBLEM,
				  "This probably indicates a serious mixup!!");
		}
		switch (rtype)
		{
		  case 1:
			/* header record not expected */
			IngestLog(EF_INFO, 
				  "header record (type 1) unexpected");
			break;
		  case 2:
			/* ignore polynomial conversion factors */
			break;
		  case 3:
			/* set the divisor for each field */
			if (type3found)
			{
				IngestLog(EF_PROBLEM,
					  "%i type 3 records already found",
					  type3found);
			}
			LoadFieldDivisors(buf, rtype, rsize);
			++type3found;
			break;
		  case 4:
			/* ignore raw data fields */
			break;
		  case 5:
			/* this is the record we really want */
			if (!type3found)
			{
				IngestLog(EF_PROBLEM,
					  "Type 5 record found before type 3, %s",
					  "can't ingest this data!");
			}
			else
				IngestDataRecord(&dc, buf, rtype, rsize, hdr);
			break;
		  case 6:
			/* signifies last record of file */
			IngestLog(EF_DEBUG,"Trailer record found");
			type6found++;
			/* 
			   We won't do much with this for now.  If this is the end,
			   then we'll quit when EOF is reached in next
			   ReadNextRecord.  Otherwise this may be a mistake and
			   more data may follow.  That's why EF_INFO is used, to
			   clue the user that something is amiss if data follows
			   the detection of this record.
			 */
			break;
		  default:
			/* this is a problem, where did it come from? */
			IngestLog(EF_PROBLEM,"Unkown record type %hi, size %hi",
				  rtype, rsize);
			break;
		}
	}
	IngestLog(EF_INFO,"File reading completed");

	StoreDataChunk(dc);

}



/*----------------------------------------------------------------
 * IngestDataRecord(DataChunk *dc, short buf[], short rtype,
 *		    short rsize, P3_header_t *hdr)
 *
 * Stores a type 5 record in the data chunk as a single sample.  The
 * time and location of the sample are retrieved from the buf[], and
 * then every field in P3_Fields[] is retrieved from the record,
 * scaled according to P3_FieldDivisors[], and stored into the DC.
 */
void
IngestDataRecord(dc, buf, rtype, rsize, hdr)
	DataChunk **dc;
	short buf[];
	short rtype;
	short rsize;
	P3_header_t *hdr;
{
	static unsigned long sample = 0; /* The number of samples written so far */
	static Location lprev = { 0.0, 0.0, 0.0 };
	static ZebTime tprev = { 0, 0 };		/* Time of previous sample */
	char c_time[30];
	int i;
	short int fld;
	ZebTime	when;				/* The time of this sample */
	Location locn;				/* The location of this sample */
	float value;
	int eflag;		/* For a time,lat,or lon error, promotes log
				   messages to a higher priority.  eflag
				   stands for Emergency flag */
	unsigned long flags;	/* 32-bit juxtaposition of FL1 and FL2 */

	/*
	 * Get any incoming messages and dispatch with Ingest's default
	 * message handler.
	 */
	if (!(sample % 100))
		while (msg_poll(0) != -1);

	/*
	 * Calculate the time and locn for the entire sample
	 */
	locn.l_lat = (float)buf[LAT_DEG]/(float)P3_FieldDivisors[LAT_DEG]
		+ ((float)buf[LAT_MIN]/(float)P3_FieldDivisors[LAT_MIN])/60.0;
	locn.l_lon = (float)buf[LON_DEG]/(float)P3_FieldDivisors[LON_DEG]
		+ ((float)buf[LON_MIN]/(float)P3_FieldDivisors[LON_MIN])/60.0;
	locn.l_alt = ((float)buf[ALT]/(float)P3_FieldDivisors[ALT])/1000.0; /*km*/

	/*
	 * We'll have to extract the time and add it to the
	 * date in the header record to get the ZebTime of the
	 * sample
	 */
	TC_ZtAssemble(&when,
		      hdr->p3header.year,
		      hdr->p3header.month,
		      hdr->p3header.day,
		      (int)buf[HOUR],
		      (int)buf[MINUTE],
		      (int)buf[SECOND],
		      0 /*microsec*/);
	if (TC_Less(when,hdr->times.flight_begin))
		when.zt_Sec += 24*3600;
	TC_EncodeTime(&when, TC_Full, c_time);

	flags = (unsigned long)buf[FL2] | (((unsigned long)buf[FL1]) << 16);
	eflag = ((flags & (ERR_TI | ERR_LA | ERR_LO | ERR_ALT))?EF_PROBLEM:0);

	/*
	 * Do some checks for errors in the data using the error flags
	 */
	if (flags)
	{
		IngestLog(EF_DEVELOP | eflag,
			  "Error flags: %0#8lx, FL1:%0#4hx FL2:%0#4hx", 
			  flags, buf[FL1], buf[FL2]);

		for (i = 0; i < NUMBER(P3_ErrorFields); ++i)
		{
			if (flags & BIT(P3_ErrorFields[i].bit))
			{
				fld = P3_WordToField[P3_ErrorFields[i].word];
				IngestLog(EF_DEVELOP,
					  "sample %i, possible error in %s, word #%hu",
					  sample,
					  P3_Fields[fld].long_name,
					  P3_Fields[fld].index);
			}
		}

		if (flags & ERR_ALT)
			IngestLog(EF_PROBLEM,
				  "Sample %i, altitude %5.3f (km) may be in error.",
				  sample, locn.l_alt);
		if (flags & ERR_LA)
			IngestLog(EF_PROBLEM,
				  "Sample %i, latitude %8.4f (deg) may be in error.",
				  sample, locn.l_lat);
		if (flags & ERR_LO)
			IngestLog(EF_PROBLEM,
				  "Sample %i, longitude %8.4f (deg) may be in error",
				  sample, locn.l_lon);
	}

	/*
	 * Errors in time will be checked separately, since experience has shown
	 * that not all time gaps are flagged in the data files (naturally...) and
	 * so we must do the checking ourselves...
	 */
	if ((flags & ERR_TI) || (tprev.zt_Sec && (when.zt_Sec - tprev.zt_Sec > 1)))
	{
		IngestLog(EF_PROBLEM,
			  "Sample %i, time error, checking for a gap...",
			  sample);
		/*
		 * According to the error flag, there has probably
		 * been a gap in time between the last two samples.
		 * Handle this condition by storing our current
		 * DataChunk to a file, and starting a new one.
		 * The current sample will be the first sample of
		 * the new data chunk.  If the gap is 5 or less seconds,
		 * we'll just interpolate between the previous sample
		 * and this one.
		 */
		if (!tprev.zt_Sec)
			IngestLog(EF_PROBLEM,"First sample, ignoring flag");
		else if (when.zt_Sec - tprev.zt_Sec <= 5)
		{
			InterpolateGap(*dc, &tprev, &lprev,
				       &when, &locn, &sample);
			/* And continue with the current sample ... */
		}
		else	/* Bigger gaps require a new file */
		{
			IngestLog(EF_PROBLEM,
				  "Gap of %i seconds.  Starting a new file...",
				  (when.zt_Sec - tprev.zt_Sec));
			StoreDataChunk(*dc);
			dc_DestroyDC(*dc);
			*dc = CreateDataChunk(hdr);
			if (!(*dc))
			{
				IngestLog(EF_PROBLEM,
					  "Could not create new datachunk, aborting");
				exit(1);
			}
			sample = 0;
		}
	}

	IngestLog( EF_DEVELOP | eflag | ((sample % 1000)?0:EF_INFO),
		  "Sample #%i, %9.5f lat, %9.5f lon, alt %5.3f km, at %s",
		  sample, locn.l_lat, locn.l_lon, locn.l_alt, c_time);

	/*
	 * For every field in P3_Fields[], pull out the word value from
	 * the buf[] array, divide by the divisor if nonzero, and
	 * store this value in the datachunk
	 */
#	define IDX P3_Fields[i].index
	for (i = 0; i< NUMBER(P3_Fields); ++i)
	{
		if (P3_FieldDivisors[IDX])
			value = ((float)buf[IDX])/((float)P3_FieldDivisors[IDX]);
		else
			value = (float)buf[IDX];
		IngestLog(EF_DEVELOP,
			  "%3hu %-35s: %8.3f (%s)",
			  P3_Fields[i].index,
			  P3_Fields[i].long_name, value, P3_Fields[i].units);
		dc_AddScalar(*dc, &when, sample, P3_FieldIds[i], &value);
	}
	/* Add our error flags for this sample as well */
	dc_AddScalar(*dc, &when, sample, FlagsFid, (float *)&flags);
#	undef IDX
	
	/*
	 * Last but not least, store the dynamic location
	 */
	dc_SetLoc(*dc, sample, &locn);

	tprev.zt_Sec = when.zt_Sec;
	lprev.l_alt = locn.l_alt;
	lprev.l_lat = locn.l_lat;
	lprev.l_lon = locn.l_lon;
	++sample;
}



/*----------------------------------------------------------------
 * LoadFieldDivisors(buf, rtype, rsize)
 *
 * Load the divisors from a type 3 record into the P3_Fields array
 */
void
LoadFieldDivisors(buf, rtype, rsize)
	short buf[];
	short rtype;
	short rsize;
{
	int i;
	unsigned long nread;
	short *s;

	for (i = 0; i < NUMBER(P3_FieldDivisors); ++i)
		P3_FieldDivisors[i] = 0;

	s = &buf[3];		/* Start after the type and size fields */
	while (s < buf+rsize+1) /* +1 since buf indexing starts at 1 */
	{
		/*
		 * All of the divisors will be stored into the
		 * P3_FieldDivisors array, even if the field is
		 * not in P3_Fields[]
		 */
		P3_FieldDivisors[*s] = *(s+1);

		/*
		 * Use the WordToField mapping to find what field, if any,
		 * this divisor is for in P3_Fields[].
		 * If the mapping is -1, that field is not in P3_Fields[].
		 */
		if (P3_WordToField[*s] > -1)
		{
			IngestLog(EF_DEVELOP,
				  "Field %hi, %s, divisor set to %hi",
				  *s,
				  P3_Fields[P3_WordToField[*s]].long_name,
				  *(s+1));
		}
		else
		{
			IngestLog(EF_DEVELOP,
				  "Field %hi, divisor set to %hi",
				  *s, *(s+1));
		}
		s += 2;
	}
}



/*----------------------------------------------------------------
 * Read the header record from the file and fill in the fields
 * of the P3_header_t structure
 */
void
ReadHeader(p3file, hdr)
	FILE *p3file;
	P3_header_t *hdr;
{
	short *buf;
	short rtype, rsize;

	buf = ReadNextRecord(p3file, &rtype, &rsize);
	if (!buf)
	{
		perror("reading header");
		IngestLog(EF_PROBLEM,
			  "couldn't read header record");
		exit(1);
	}

	/*
	 * A simple verification that this is the correct record type
	 */
	if ((rtype != 1) || (rsize != 17))
	{
		IngestLog(EF_PROBLEM,
			  "Header record not found!");
		exit(1);
	}

	/*
	 * Copy the buffer into a header structure
	 */
	memcpy((char *)hdr, (char *)(buf + 1), rsize*2);

	/*
	 * The header record is now stored in the hdr structure.
	 * Convert some of the ancillary info, such as the times.
	 */
	TC_ZtAssemble(&hdr->times.flight_begin,
		      hdr->p3header.year,
		      hdr->p3header.month,
		      hdr->p3header.day,
		      hdr->p3header.hour_begin,
		      hdr->p3header.minute_begin,
		      hdr->p3header.second_begin,
		      0 /* microsec */);

	/*
	 * Convert the flight end time using flight date but end time
	 */
	TC_ZtAssemble(&hdr->times.flight_end,
		      hdr->p3header.year,
		      hdr->p3header.month,
		      hdr->p3header.day,
		      hdr->p3header.hour_end,
		      hdr->p3header.minute_end,
		      hdr->p3header.second_end,
		      0 /* microsec */);
	/*
	 * Now if the end time is less than the begin, then the end time
	 * needed to carry into another day
	 */
	if (TC_Less(hdr->times.flight_end,hdr->times.flight_begin))
		hdr->times.flight_end.zt_Sec += 24*3600;
}



/*------------------------------
 * Dump the info stored in the first record (type 1) of a
 * P3 data file.
 */
void
DumpHeader(hdr)
	P3_header_t *hdr;
{
	char c_time[30];

	fprintf(stderr,"Header info\n");
#define P(name,val) fprintf(stderr,"%20s: %hu\n",name,hdr->p3header.val)
	P("Record Type",type);
	P("Record Size",size);
	P("Aircraft Number",aircraft);
	fprintf(stderr,"%20s: %02hu/%02hu/%02hu\n",
		"Date of flight",
		hdr->p3header.month,
		hdr->p3header.day,
		hdr->p3header.year);
	fprintf(stderr,"%20s: %02hu:%02hu:%02hu\n",
		"Flight began",
		hdr->p3header.hour_begin,
		hdr->p3header.minute_begin,
		hdr->p3header.second_begin);
	fprintf(stderr,"%20s: %02hu:%02hu:%02hu\n",
		"Flight ended",
		hdr->p3header.hour_end,
		hdr->p3header.minute_end,
		hdr->p3header.second_end);
	TC_EncodeTime(&hdr->times.flight_begin, TC_Full, c_time);
	fprintf(stderr,"%20s: %s\n","Zeb time, flight begin", c_time);
	TC_EncodeTime(&hdr->times.flight_end, TC_Full, c_time);
	fprintf(stderr,"%20s: %s\n","Zeb time, flight end", c_time);
#undef 	P
}


/*----------------------------------------------------------------
 * ReadNextRecord(FILE *p3file, short *type, short *nwords)
 *
 * Searches p3file for the next occurrence of a recognized (record type,
 * record size) pair, indicating a probable record.  This approach is
 * necessary because of the uncertainty introduced by the unlabeled type 2
 * record after the header (808 bytes, 404 words (size of type 2) of
 * unlabeled data between header record and the first type 3 record.
 * Hopefully, this will be the only point where this function will actually
 * skip space.  Perhaps later we'll be more sure the 808 bytes will be the
 * only skipped data, and those 808 can be explicitly skipped and the
 * kludge of looking for meaningful type/size pairs won't be necessary.
 * Note that on the return, p3file pointer is set to offset of next
 * expected record.  Buf contains the entire record, including the type and
 * size fields.  Returns a pointer to the record data, or NULL on an
 * error.  
 *
 * Two methods are used for reading:
 * If possible, the file is mmap()'ed, and requests for the next record
 * simply advance a pointer through mapped memory.  Otherwise, the device is
 * sequentially read with fread() and data stored in the static buffer buf[].
 * The latter method is used so that pipes can be used on stdin; however
 * every record type must be read into buf whether it will be used or not.
 * IMPORTANT: buf[] is based at 1 rather than 0 to correspond to the index
 * values used in P3_Fields and the type 3 records.
 */
short *
ReadNextRecord(p3file, rec_type, nwords)
	FILE *p3file;
	short *rec_type;
	short *nwords;
{
	static int inited = 0;	/* Perform tests on first call */
	static short buf[MAX_RECORD_SIZE];
	short rtype;		/* The potential record type */
	short rsize;		/* The word following 'rtype' */
	static long fsize;		/* Size of the file were reading */
	long nread;
	static long cur_offset = 0;	/* Our current offset into the data */
	static long last_rec = 0;	/* offset to next expected record */
	static short mmapped;		/* Boolean, true if using mmap()'ed memory */
	static short *mbase;		/* Base of mapping, if using mmap() */
	static short *mapp;		/* The pointer into mapped memory */
	
	static short record_sizes[7] = {
	   0, 17, 404, 192, 222, 106, 15 };

	if (!inited)
	{
		inited = 1;
		mmapped = 0;
		cur_offset = 0;
		last_rec = 0;			/* Used in both methods */
		fsize = SeekFileSize(p3file);
		IngestLog(EF_DEBUG,"SeekFileSize() returned %li",fsize);

		if (fsize > -1)		/* first try to map it */
		{
			/*
			 * Try to establish the map
			 */
			IngestLog(EF_DEBUG,"Trying to mmap() file device");
			mbase = (short *)mmap((caddr_t)0, (size_t)fsize,
				     PROT_READ, MAP_PRIVATE,
				     fileno(p3file)/* FD */,
				     (off_t)0);
			mmapped = ((long)mbase == -1) ? 0 : 1;
			mapp = mbase;
			IngestLog(EF_DEBUG,
				  "mmap() returned (long)%li, errno %i",
				  (long)mbase, errno);
		}

		if ((fsize < 0) || (!mmapped))		/* else use non-mmap method */
		{
			IngestLog(EF_INFO,"Reading device with fread()");
			mmapped = 0;		/* wont be using mmap() */
		}
		else
		{
			IngestLog(EF_INFO,"Memory mapping file, size %li",
				  fsize);
		}
	}

	/*
	 * Read the next two words of the file into rtype and rsize
	 */
	if (!mmapped)
	{
		fread(&rtype,sizeof(short),1,p3file);
		fread(&rsize,sizeof(short),1,p3file);
	}
	else
	{
		rtype = *mapp++;
		rsize = *mapp++;
	}
	cur_offset += 2*sizeof(short);

	/* Now start looping, checking for a correct
	 * type-size pair.  On every loop, move the rsize word
	 * to rtype, read a new rsize, and try again,
	 * until fread returns an error
	 */
	do {
		if ((rtype >= 1) && (rtype <= 6) &&
		    (record_sizes[rtype] == rsize))
		{
			/* We found a probable record */

			/*
			 * See if there was any dead space between the
			 * end of last record and beginning of this one.
			 * The -4 accounts for the 2 words already read in
			 * as part of the current block.
			 */
			if (cur_offset - 4 - last_rec > 0)
			{
				IngestLog(EF_PROBLEM,
					  "%li bytes of unknown data",
					  cur_offset - 4 - last_rec);
			}
			IngestLog(EF_DEVELOP,
				  "Record type %hu, size %hu, at %li",
				  rtype, rsize, cur_offset - 4);

			if (!mmapped)
			{
				buf[1] = rtype;
				buf[2] = rsize;
				nread = fread(buf+3, sizeof(short), rsize-2, p3file);
				if (nread < rsize-2)
				{
					IngestLog(EF_PROBLEM,
			  "fread() failed for type 3 record, only read %i bytes",
						  nread);
					return(0);
				}
			}
			else
			{


			}
			cur_offset += (rsize - 2) * sizeof(short);

			/*
			 * Set last_rec to offset of next expected record
			 */
			last_rec = cur_offset;
			*rec_type = rtype;
			*nwords = rsize;
			if (!mmapped)
			{
				return(buf);
			}
			else
			{
				mapp += (rsize - 2);
				/* The -1 is so that the record will actually
				   start at index 1 of the returned array, as
				   expected by the caller */
				return(mapp - rsize - 1);
			}
		}
		else
		{
			/*
			 * Move rsize value to rtype and read a new rsize
			 */
			rtype = rsize;
			if (!mmapped)
			{
				if (fread(&rsize,sizeof(short),1,p3file) < 1)
				{
					IngestLog(EF_PROBLEM,"error reading new rsize");
					break;
				}
			}
			else
			{
				if (cur_offset >= fsize)
					break;		/* End of file */
				rsize = *mapp++;
			}
			cur_offset += sizeof(short);
		}
	} while ((mmapped)?(cur_offset < fsize):(!feof(p3file)));

	return((short *)NULL);
}



/*----------------------------------------------------------------
 * InitializeFieldIds()
 *
 * Tries to declare all of the fields in P3_Fields and store their
 * FieldId's in the corresponding element of the P3_FieldIds array.
 * Returns non-zero if successful, 0 otherwise.
 */
int
InitializeFieldIds()
{
	register int i;
	register FieldId fid;

	/*
	 * At the moment, F_DeclareField will return a valid
	 * field id, either previously existing or newly declared.
	 * This may change when units are actually handled rather
	 * than ignored. 
	 */
	IngestLog(EF_INFO,"Declaring %i fields",NUMBER(P3_FieldIds));
	for (i = 0; i < NUMBER(P3_Fields); ++i)
	{
		fid = F_DeclareField(P3_Fields[i].field_name,
				     P3_Fields[i].long_name,
				     P3_Fields[i].units);
		IngestLog(EF_DEBUG, "Declaring %s, %s, (%s), id = %i",
			  P3_Fields[i].field_name,
			  P3_Fields[i].long_name,
			  P3_Fields[i].units,
			  fid);
		P3_FieldIds[i] = fid;
	}

	/* Add an id for the error flags field */
	FlagsFid = F_DeclareField("err_flags",
				  "Error flags (words 10 and 11) of orig data records",
				  "ulong bit field");
	P3_FieldIds[i] = FlagsFid;

	return (1);
}



/*----------------------------------------------------------------
 * CreateDataChunk(P3_header_t *hdr)
 * 
 * Tries to create and initialize a Scalar class DataChunk using
 * info from the header.  Returns either a pointer to the new
 * and empty data chunk, or NULL if an error occurs.
 */
DataChunk *
CreateDataChunk(hdr)
	P3_header_t *hdr;
{
	static char version_info[] = "$RCSfile: p3fest_ingest.c,v $ $Revision: 1.4 $";
	DataChunk *dc;
	unsigned int nsamples;
	int hours, minutes;
	PlatformId plat_id;
	char attr[30];

	/*
	 * Estimate number of samples from # of seconds in flight.
	 */	
	nsamples = (hdr->times.flight_end.zt_Sec - 
		    hdr->times.flight_begin.zt_Sec + 60);
	IngestLog(EF_INFO, "Estimating %u samples in file", nsamples);

	if ((plat_id = ds_LookupPlatform(PLATFORM_NAME)) == BadPlatform)
	{
		IngestLog(EF_PROBLEM,"Platform %s is a bad platform name",
			  PLATFORM_NAME);
		return(NULL);
	}
	dc = dc_CreateDC(DCC_Scalar);
	dc->dc_Platform = plat_id;

	/*
	 * Set some global attributes from the header data
	 */
	TC_EncodeTime(&(hdr->times.flight_begin), TC_Full, attr);
	dc_SetGlobalAttr(dc, AttFlightBeginTime, attr);
	IngestLog(EF_INFO,"%30s %s","Flight begin time:",attr);
	TC_EncodeTime(&(hdr->times.flight_end), TC_Full, attr);
	dc_SetGlobalAttr(dc, AttFlightEndTime, attr);
	IngestLog(EF_INFO,"%30s %s","Flight end time:",attr);
	sprintf(attr,"%hu", hdr->p3header.aircraft);
	dc_SetGlobalAttr(dc, AttAircraftNumber, attr);
	IngestLog(EF_INFO,"%30s %s","Aircraft number:",attr);
	dc_SetGlobalAttr(dc, AttIngestVersion, version_info);

	/*
	 * Initialize the scalar parts of the datachunk.
	 * NOTE: We use the superclass' SetupUniformFields() method
	 * because we'd like to give some guidance on how many samples
	 * we'll be storing to avoid thrashing memory.  However, as of
	 * this writing (10/26/92), dc_SetupUniformFields ignores
	 * the nsamples value! :(  
	 * We still use it here just in case someday it does...
	 */
	dc_SetupUniformFields(dc, nsamples, 
			      (int)NUMBER(P3_FieldIds), P3_FieldIds,
			      sizeof(float));

#ifdef notdef /*---- The Scalar method call -----*/
	dc_SetScalarFields(dc, (int)NUMBER(P3_FieldIds), P3_FieldIds);
#endif

	/*
	 * We'll set a bad value flag here to a default, even though it probably
	 * won't be used 
	 */
	dc_SetBadval(dc, BADVAL_DEFAULT);

#ifdef DEBUG
	dc_DumpDC(dc);
#endif

	return(dc);
}



void
InitWordToFieldMapping()
{
	int i;

	/*
	 * Initialize the P3_WordToField mapping array by stepping through
	 * the P3_Fields array
	 */
	(void)memset((char *)P3_WordToField,(int)-1,sizeof(P3_WordToField));
	for (i = 0; i < NUMBER(P3_Fields); ++i)
	{
		P3_WordToField[P3_Fields[i].index] = i;
	}
}


long
SeekFileSize(file)
	FILE *file;
{
	long off;
	long cur;

	/*
	 * Seek the end of the file, get the offset.
	 * The offset will be the size of the file in bytes.
	 * If any errors occur, return immediately with -1
	 * An error probably indicates that the device is
	 * not seekable, such as a tty or pipe
	 */

	if ((cur = ftell(file)) < 0)
		return(-1);
	if (fseek(file, 0, SEEK_END) != 0)
		return(-1);
	off = ftell(file);
	if (off < 0)
		return(-1);
	fseek(file, cur, SEEK_SET);		/* Return to original offset */

	return(off);
}



void
PrintEntry(out, field, desc, units, parm, comp)
	FILE *out;
	char *field, *desc, *units, *parm, *comp;
{
	char udesc[128];
	char *c;

	/*
	 * Replace whitespace with underscores in the long name (desc)
	 */
	strcpy(udesc,desc);
	for (c = udesc; c < udesc+strlen(udesc); ++c)
	{
		if ((*c == 32) || (*c == 11))  /* space or tab */
			*c = '_';
	}

	fprintf(out,"	entry '%s %s' 'add_xy_component %s ",
		desc, units, PLATFORM_NAME);
	fprintf(out,	"\"%s %s\" \"%s\" %s'\n",
		parm, field, udesc, comp);
}




void
PrintOneMenu(out,menu,parm,comp)
	FILE *out;
	char *menu;
	char *parm;
	char *comp;
{
	int i;

	/*
	 * First do the menu definition stuff
	 */
	fprintf(out,"\ndefine widget %s intmenu 'P3 Air Fields'\n",menu);
	fprintf(out,"	title 'Select Field'\n");
	fprintf(out,"	line\n");

	/*
	 * Our first entry will be the standard 'alt' field 
	 */
	PrintEntry(out,"alt","Radar Altitude","km", parm, comp);

	/*
	 * Now process all of the fields in P3_Fields[]
	 */
	for (i = 0; i < NUMBER(P3_Fields); ++i)
	{
		PrintEntry(out, 
			   P3_Fields[i].field_name,
			   P3_Fields[i].long_name,
			   P3_Fields[i].units,
			   parm,
			   comp);
	}

	fprintf(out,"endmenu\n\n");
}



void
PrintMenus(out)
	FILE *out;
{
	PrintOneMenu(out, "time-p3-air-parts", "y-field", "c_xytime_air");
	PrintOneMenu(out, "p3-air-parts", "x-field", "c_xy_air");
}
