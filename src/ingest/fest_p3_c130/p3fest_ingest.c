/*
 * $Id: p3fest_ingest.c,v 1.1 1992-10-29 19:21:05 granger Exp $
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
#include "copyright.h"
#include "ingest.h"

#define MAX_WORDS_RECORD5	106
#define MAX_RECORD_SIZE		420	/* max size of any record in words */
#define PLATFORM_NAME		"p3"

/*
 * Some important field indices in a type 5 record
 */
#define LAT_DEG			12	/* location fields */
#define LAT_MIN			13
#define LON_DEG			14
#define LON_MIN			15
#define HOUR			3	/* time fields */
#define MINUTE			4
#define SECOND			5
#define SW1			8	/* who knows what */
#define SW2			9
#define SW3			35
#define FL1			10	/* error flags */
#define FL2			11

#define AttAircraftNumber	"aircraft_number"
#define AttFlightBeginTime	"flight_begin_time"
#define AttFlightEndTime	"flight_end_time"

#define NUMBER(arr)	((unsigned long)(sizeof(arr)/sizeof(arr[0])))



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
	{ 16, 	"Radar altitiude", 		"ra",		"m" },
	{ 17, 	"Pressure", 			"ps",		"mb" },
	{ 18, 	"Ambient temperature", 		"ta",		"degC" },
	{ 19, 	"Dewpoint sensor", 		"tw1",		"degC" },
	{ 20, 	"Radiometer down", 		"rd",		"W/m^2" },
	{ 21, 	"Radiometer side", 		"rs",		"W/m^2" },
	{ 22, 	"Ground speed", 		"gs",		"m/s" },
	{ 23, 	"True airspeed", 		"ts",		"m/s" },
	{ 24, 	"A/C Vertical velocity",	"wgs",		"m/s" },
	{ 25,	"Track",			"tk",		"deg" },
	{ 26, 	"Heading (true)", 		"hd",		"deg" },
	{ 27,	"Pitch",			"pc",		"deg" },
	{ 28,	"Roll",				"rl",		"deg" },
	{ 29,	"Attack angle",			"aa",		"deg" },
	{ 30,	"Slip angle",			"sa",		"deg" },
	{ 31,	"Liquid water content",		"lw",		"g/m3" },
	{ 32,	"Dynamic pressure",		"pq",		"mb" },
	{ 33,	"Dewpoint temperature",		"td",		"degC" },
	{ 34,	"Radiometer up",		"ru",		"W/m^2" },
	/* { 35,	"Switches",		"sw3",		UNITS_NONE }, */
	{ 36,	"E/W Velocity of tail",		"utail",	"m/s" },
	{ 37,	"N/S Velocity of tail",		"vtail",	"m/s" },
	{ 38,	"Vertical velocity of tail",	"wtail",	"m/s" },
	/* 39,  ---blank--- */
	{ 40,	"Geopotential altitude",	"ga",		"m" },
	{ 41,	"Pressure altitude",		"pa",		"m" },
	{ 42,	"D Value",			"dv",		UNITS_UNKNOWN },
	{ 43,	"Height standard",		"ht",		UNITS_UNKNOWN },
	{ 44,	"Surface pressure",		"sp",		"mb" },
	{ 45,	"Relative humidity",		"rh",		"%" },
	{ 46,	"Virtual temperature",		"tv",		"K" },
	{ 47,	"Vertical airspeed",		"was",		"m/s" },
	{ 48,	"Ratio specific heats",		"gm",		UNITS_NONE },
	{ 49,	"Mach number",			"ama",		UNITS_UNKNOWN },
	{ 50,	"Drift",			"da",		UNITS_UNKNOWN },
	{ 51,	"E/W Ground speed",		"gsx",		"m/s" },
	{ 52,	"N/S Ground speed",		"gsy",		"m/s" },
	{ 53,	"E/W True airspeed",		"tx",		"m/s" },
	{ 54,	"N/S True airspeed",		"ty",		"m/s" },
	{ 55,	"E/W Wind speed",		"u_wind",	"m/s" },
	{ 56,	"N/S Wind speed",		"v_wind",	"m/s" },
	{ 57,	"Vertical wind speed",		"w_wind",	"m/s" },
	{ 58,	"Wind speed",			"wspd",		"m/s" },
	{ 59,	"Wind direction",		"wdir",		"deg" },
	/* 60,  ---blank--- */
	{ 61,	"Vapor pressure",		"ee",		"mb" },
	{ 62,	"Mixing ratio",			"mr",		"g/kg" },
	{ 63,	"Potential temperature",	"pt",		"K" },
	{ 64, 	"Equivalent potential temperature", "et",	"K" },
	{ 65,	"E/W average wind",		"wxb",		"m/s" },
	{ 66,	"N/S average wind",		"wyb",		"m/s" },
	{ 67,	"Average wind speed",		"wsb",		"m/s" },
	{ 68,	"Average wind direction",	"wdb",		"deg" },
	{ 69,	"Vertical accelerometer #1",	"av1",		UNITS_UNKNOWN },
	{ 70,	"Vertical accelerometer #2",	"av2",		UNITS_UNKNOWN },
	{ 71,	"Seconds wind averaged over",	"wac",		"secs" },
	{ 72,	"AXBT1",			"bt1",		UNITS_UNKNOWN },
	{ 73, 	"AXBT2",			"bt2",		UNITS_UNKNOWN },
	{ 74,	"AXBT3",			"bt3",		UNITS_UNKNOWN }
	/* 75-106 ---ignored--- */
};



/*
 * The global field id's array, initialized by InitializeFieldIds()
 */
FieldId P3_FieldIds[NUMBER(P3_Fields)];

/*
 * The global storage for the type 3 divisors
 */
short P3_FieldDivisors[MAX_WORDS_RECORD5 + 1];


/*
 * To speed processing of a type 3 record (storing divisors into the
 * P3_Fields array), a mapping is created from the type 5 word index to
 * a the corresponding p3_field index in P3_Fields[] using an array of short.
 */
short P3_WordToField[MAX_WORDS_RECORD5 + 1];


/*-----------------------------------------
 * Prototypes
 */
void ReadHeader FP((FILE *p3file, P3_header_t *hdr));
void DumpHeader FP((P3_header_t *hdr));
DataChunk *CreateDataChunk FP((P3_header_t *hdr));
int InitializeFieldIds ();
void InitWordToFieldMapping ();
int ReadNextRecord FP((FILE *p3file, short *rec_type, short *nwords, short buf[]));
void SearchForRecords FP((FILE *p3file));
void LoadFieldDivisors FP((short buf[], short rtype, short rsize));
void IngestDataRecord FP((DataChunk *dc, short buf[], 
			  short rtype, short rsize, P3_header_t *hdr));
void IngestRecords FP((DataChunk *dc, FILE *p3file, P3_header_t *hdr));

/*-----------------------------------------*/



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
	DataChunk *dc;
	P3_header_t hdr;
 	FILE *p3file;

#ifdef	DEBUG
	SetDryRun();	/* The default will be no outside connections */
#endif

	/* Parse any ingest options. */
	IngestParseOptions(&argc, argv, IngestUsage);
	
	if (argc != 2)
	{
		fprintf(stderr,"Usage: %s [ingest options] <datafile>\n",argv[0]);
		fprintf(stderr,"   If <datafile> is '-', stdin is used\n");
		IngestUsage();
		exit(1);
	}

	/*
	 * Initialize usy, message, DataStore, and fields simultaneously
	 */
	IngestInitialize(argv[0]);

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
#ifdef DEBUG
	DumpHeader(&hdr);
#endif

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
	 * Create an empty data chunk with our header info
	 */
	dc = CreateDataChunk(&hdr);
	if (!dc)
	{
		IngestLog(EF_EMERGENCY,"Could not create data chunk. Exiting");
		exit(1);
	}

#ifdef DEBUG
	dc_DumpDC(dc);
#endif

	/*
	 * Initialize our word to field mapping to speed processing of type
	 * 3 records
	 */
	InitWordToFieldMapping();

	/*
	 * Now pull out the type 3 and 5 records.  The hdr is required to
	 * provide the base date.
	 */
	IngestRecords(dc, p3file, &hdr);

	/*
	 * Now the DataChunk should be full of data from the file,
	 * so store the chunk
	 */
	if (dc_GetNSample(dc) == 0)
	{
		/* Our DC is still empty for some reason */
		IngestLog(EF_PROBLEM,
			  "after file ingested, datachunk still empty?");
		exit(1);
	}

	/*
	 * Each raw p3 file will correspond to a single DataStore file
	 */
	ds_Store(dc, /*newfile*/ TRUE, /*details*/ NULL, /*ndetail*/ 0);

	/*
	 * And we're done...
	 */
	exit(0);
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
IngestRecords(dc, p3file, hdr)
	DataChunk *dc;
	FILE *p3file;
	P3_header_t * hdr;
{
	short rtype, rsize;
	short type3found;	/* Keep track of multiple type 3 records */
	short type6found;	/* Help detect data following the trailer */
	short buf[MAX_RECORD_SIZE];
	int i;

	/* 
	 * Before we start, make sure all divisors in P3_FieldDivisors init'ed to 0
	 */
	for (i = 0; i < NUMBER(P3_Fields); ++i)
		P3_FieldDivisors[i] = 0;

	type6found = 0;
	type3found = 0;
	while (ReadNextRecord(p3file, &rtype, &rsize, buf))
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
				IngestDataRecord(dc, buf, rtype, rsize, hdr);
			break;
		  case 6:
			/* signifies last record of file */
			IngestLog(EF_INFO,"Trailer record found");
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
	DataChunk *dc;
	short buf[];
	short rtype;
	short rsize;
	P3_header_t *hdr;
{
	static sample = 0;	/* The number of samples written so far */
	char c_time[30];
	int i;
	ZebTime	when;		/* The time of this sample */
	Location locn;		/* The location of this sample */
	float value;
	unsigned long flags;	/* 32-bit juxtaposition of FL1 and FL2 */

	/*
	 * Calculate the time and locn for the entire sample
	 */
	locn.l_alt = 0;
	locn.l_lat = (float)buf[LAT_DEG]/(float)P3_FieldDivisors[LAT_DEG]
		+ ((float)buf[LAT_MIN]/(float)P3_FieldDivisors[LAT_MIN])/60.0;
	locn.l_lon = (float)buf[LON_DEG]/(float)P3_FieldDivisors[LON_DEG]
		+ ((float)buf[LON_MIN]/(float)P3_FieldDivisors[LON_MIN])/60.0;

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

	/*
	 * Do some checks for errors in the data using the error flags
	 */
	flags = (unsigned long)buf[FL2] | (((unsigned long)buf[FL1]) << 16);
	if (flags)
	{
		IngestLog(EF_PROBLEM,"Error flags present: %0#lx", flags);
	}


	/*
	 * For every field in P3_Fields[], pull out the word value from
	 * the buf[] array, divide by the divisor if nonzero, and
	 * store this value in the datachunk
	 */
#	define IDX P3_Fields[i].index
	TC_EncodeTime(&when, TC_Full, c_time);
	IngestLog(EF_DEBUG,"Sample #%i, %9.5f lat, %9.5f lon, at %s",
		  sample, locn.l_lat, locn.l_lon, c_time);
	for (i = 0; i< NUMBER(P3_Fields); ++i)
	{
		if (P3_FieldDivisors[IDX])
			value = (float)buf[IDX]/(float)P3_FieldDivisors[IDX];
		else
			value = (float)buf[IDX];
		IngestLog(EF_DEVELOP,"   %-35s: %8.3f (%s)",
			  P3_Fields[i].long_name, value, P3_Fields[i].units);
		dc_AddScalar(dc, &when, sample, P3_FieldIds[i], &value);
	}
#	undef IDX
	
	/*
	 * Last but not least, store the dynamic location
	 */
	dc_SetLoc(dc, sample, &locn);

#ifdef notdef
	dc_DumpDC(dc);
#endif

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
	unsigned long nread;

	nread = fread(hdr->record,sizeof(hdr->record),1,p3file);
	if (nread < 1)
	{
		perror("reading header");
		IngestLog(EF_PROBLEM,
			  "reading header: only read %d bytes\n",nread);
		exit(1);
	}

	/*
	 * A simple verification that this is the correct record type
	 */
	if ((hdr->record[0] != 1) || (hdr->record[1] != 17))
	{
		IngestLog(EF_PROBLEM,
			  "Header record not recognized");
		exit(1);
	}

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
 * ReadNextRecord(FILE *p3file, short *type, short *nwords, short buf[])
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
 * size fields.  Returns non-zero if another record found, zero if EOF or
 * error.  fseek() is not used so that pipes can be used on stdin, hence
 * every record type is read into buf whether it will be used or not.
 * IMPORTANT: buf[] is based at 1 rather than 0 to correspond to the index
 * values used in P3_Fields and the type 3 records.
 */
int
ReadNextRecord(p3file, rec_type, nwords, buf)
	FILE *p3file;
	short *rec_type;
	short *nwords;
	short buf[];
{
	short rtype;		/* The potential record type */
	short rsize;		/* The word following 'rtype' */
	long nread;
	static long cur_offset = 0;	/* Our current offset into the data */
	static long last_rec = 0;
	                        /* offset of expected beginning of next record */

	static short record_sizes[7] = {
	   0, 17, 404, 192, 222, 106, 15 };

	/* Since its possible we're not called at the beginning, adjust last_rec */
	if (!last_rec)
	{
		last_rec = ftell(p3file);
		if (last_rec < 0)		/* must be illegal fd for seeks */
			last_rec = 0;
	}
	/*
	 * If this is a seek'able device, then offset and last_rec pointers
	 * will correspond with the file offsets.  Otherwise they will just
	 * be offsets from the point in the input where we started reading
	 * with ReadNextRecord().  The only place ftell() is used is above
	 * trying to reconcile cur_offset with the offset of a seekable device.
	 */
	cur_offset = last_rec;

	/*
	 * Read the next two words of the file into rtype and rsize
	 */
	fread(&rtype,sizeof(short),1,p3file);
	fread(&rsize,sizeof(short),1,p3file);
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
			IngestLog(EF_DEBUG,
				  "Record type %hu, size %hu, at %li",
				  rtype, rsize, cur_offset - 4);

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
			cur_offset += (rsize - 2) * sizeof(short);

			/*
			 * Set last_rec to offset of next expected record
			 */
			last_rec = cur_offset;
			*rec_type = rtype;
			*nwords = rsize;
			return(1);
		}
		else
		{
			/*
			 * Move rsize value to rtype and read a new rsize
			 */
			rtype = rsize;
			if (fread(&rsize,sizeof(short),1,p3file) < 1)
			{
				IngestLog(EF_PROBLEM,"reading new rsize: %s",
					  "system error");
				break;
			}
			cur_offset += sizeof(short);
		}
	} while (!feof(p3file));

	return(0);
}




/* ----------------------------------------
 * Search for recognizable lead sequences for different P3 
 * record types.  Basically this means finding a word value
 * from 1 to 6 followed by a word containing the correct
 * number of words in the corresponding record type.
 * Any matches are shown on stdout.  
 */
void
SearchForRecords(p3file)
	FILE *p3file;
{
	short rtype;		/* The potential record type */
	short rsize;		/* The word following 'rtype' */
	unsigned long last_rec;	/* offset of expected beginning of next record */

	static short record_sizes[7] = {
	   0, 17, 404, 192, 222, 106, 15 };

	/*
	 * Read the first two words of the file to
	 * initialize rtype and rsize.
	 */
	fread(&rtype,sizeof(short),1,p3file);
	fread(&rsize,sizeof(short),1,p3file);
	last_rec = 0;

	/* Now start looping, checking for a correct
	 * type-size pair.  On every loop, move the rsize word
	 * to rtype, read a new rsize, and try again,
	 * until fread returns an error
	 */
	printf("Beginning record search...\n");
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
			if (ftell(p3file)-4 - last_rec > 0)
			{
				printf("%lu bytes of dead space\n",
					ftell(p3file)-4 - last_rec);
			}
			printf("Record type %hu, size %hu, at %lu\n",
				rtype, rsize, ftell(p3file)-2*sizeof(short));
			/*
			 * Now skip what we suppose is the contents
			 * of the record: move the file pointer rsize
			 * words forward, minus the two words of the
			 * block we read into rtype and rsize.
			 */
			if (fseek(p3file,(rsize-2)*sizeof(short),SEEK_CUR))
			{
				perror("seeking next record");
				break;
			}
			/*
			 * Set last_rec to last byte of this record
			 */
			last_rec = ftell(p3file);
			/*
			 * Get new type and size values
			 */
			if ((fread(&rtype,sizeof(short),1,p3file) < 1)
			    || (fread(&rsize,sizeof(short),1,p3file) < 1))
			{
				perror("reading rtype and rsize");
				break;
			}
		}
		else
		{
			/*
			 * Move rsize value to rtype and read a new
			 * rsize
			 */
			rtype = rsize;
			if (fread(&rsize,sizeof(short),1,p3file) < 1)
			{
				perror("reading new rsize");
				break;
			}
		}
	} while (!feof(p3file));

	/*
	 * Finished reading file...
	 */
	printf("Record search completed.\n");
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
	IngestLog(EF_INFO,"Defining %i fields",NUMBER(P3_FieldIds));
	for (i = 0; i < NUMBER(P3_FieldIds); ++i)
	{
		fid = F_DeclareField(P3_Fields[i].field_name,
				     P3_Fields[i].long_name,
				     P3_Fields[i].units);
		IngestLog(EF_DEVELOP,"Declaring %s, %s, (%s), id = %i",
			  P3_Fields[i].field_name,
			  P3_Fields[i].long_name,
			  P3_Fields[i].units,
			  fid);
		P3_FieldIds[i] = fid;
	}

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
	TC_EncodeTime(&(hdr->times.flight_end), TC_Full, attr);
	dc_SetGlobalAttr(dc, AttFlightEndTime, attr);
	sprintf(attr,"%hu", hdr->p3header.aircraft);
	dc_SetGlobalAttr(dc, AttAircraftNumber, attr);

	/*
	 * Initialize the scalar parts of the datachunk.
	 * NOTE: We use the superclass' SetupUniformFields() method
	 * because we'd like to give some guidance on how many samples
	 * we'll be storing to avoid thrashing memory.  However, as of
	 * this writing (10/26/92), dc_SetupUniformFields ignores
	 * the nsamples value! :(  We still use it here just in case
	 * someday it does...
	 */
	dc_SetupUniformFields(dc, nsamples, 
			      (int)NUMBER(P3_FieldIds), P3_FieldIds,
			      sizeof(float));

#ifdef notdef /*---- The Scalar method call -----*/
	dc_SetScalarFields(dc, (int)NUMBER(P3_FieldIds), P3_FieldIds);
#endif

	/*
	 * We'd set the bad value flag here, if we knew of an appropriate value
	 */

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

