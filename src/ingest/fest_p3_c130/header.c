/* $Id: header.c,v 1.1 1993-06-25 23:09:44 granger Exp $
 *
 * A first attempt to look at the data stored in a p3 data tape file
 * Reads the first record, type 1, from the file and displays the
 * info stored there.
 *
 * It is possible that future implementations will use mmap()
 */

#include <unistd.h>	/* To get SEEK_CUR definition */
#include <stdio.h>
#include <sys/types.h>
#include <sys/mman.h>
#include "copyright.h"
#include "ingest.h"

/*----------------------------------------
 * 

/*
 * The first 17 words correspond to the stored format of
 * the header.  I/O should always be done through the
 * 'record' field of the union, since the ZebTime's are
 * strictly ancillary info gathered from the actual header 
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
	ZebTime flight_date;	/* date flight took place */
	ZebTime flight_begin;	/* time flight began */
	ZebTime flight_end;	/* time flight ended */
   } times;
} P3_header_t;

/*
 * Type 5 data fields:
 * This array holds info to use in translating the data in a type 5
 * record into Zeb fields.  Each structure in the array contains the
 * long name of the field, a corresponding field name to be used in the
 * actual field definition, the field id received from the definition,
 * and the divisor, taken from type 3, which should be applied to
 * the word value in a type 5.  The index of the structure corresponds to  
 * the word address of the field in a type 5 record.
 * Thus to store a type 5 data field #n into the DataChunk, divide the
 * the value by t5fields[n].divisor to get floating point value, and store the
 * value into the field id t5fields[n].field_id.
 */

typedef struct p3_field_s {
	short	index5;		/* location of this field in type 5 record */	
	char 	*long_name;	/* descriptive name of field */
	char 	*field_name;	/* short field name */
	FieldID fid;
	short 	divisor;	/* divisor obtained from type 3 record */
} p3_field_t;

/*
 * Most of the info about type 5 fields will be automatically initialized here.
 * The field id must wait until the field is defined and the divisor must
 * be filled in when a type 3 record is read.  Required fields like lat/lon
 * and sample time are not included here unless they are to be specifically
 * kept as field data in addition to being stored with the DataChunk.
 */

p3_field_t p3_fields[NUM_P3_FIELDS] =
{
	{ 16, 	"Radar altitiude (m)", 		"ralt" },
	{ 17, 	"Pressure", 			"pres" },
	{ 18, 	"Ambient temperature", 		"tdry" },
	{ 19, 	"Dewpoint sensor", 		"dpt_sensor" },
	{ 20, 	"Radiometer down", 		0 },
	{ 21, 	"Radiometer side", 		0 },
	{ 22, 	"Ground speed", 		"gspd" },
	{ 23, 	"True airspeed", 		"aspd" },
	{ 24, 	"A/C Vertical velocity",	"vel_v" },
	{ 26, 	"Heading (true)", 		"head" },
	{ 31,	"Liquid water content",		"lwc" },
	{ 32,	"Dynamic pressure",		"dpres" },
	{ 33,	"Dewpoint temperature",		"dpt" },
	{ 34,	"Radiometer up",		0 },
	{ 36,	"E/W Velocity of tail",		"utail" },
	{ 37,	"N/S Velocity of tail",		"vtail" },
	{ 38,	"Vertical velocity of tail",	"wtail" },
	{ 40,	"Geopotential altitude",	"galt" },
	{ 41,	"Pressure altitude",		"palt" },
	{ 44,	"Surface prressure",		"spres" },
	{ 45,	"Relative humidity",		"rh" },
	{ 46,	"Virtual temperature",		"tv" },
	{ 55,	"E/W Wind speed",		"u_wind" },
	{ 56,	"N/S Wind speed",		"v_wind" },
	{ 57,	"Vertical wind speed",		"w_wind" },
	{ 58,	"Wind speed",			"wspd" },
	{ 59,	"Wind direction",		"wdir" },
	{ 61,	"Vapor pressure",		"vpres" },
	{ 62,	"Mixing ratio",			"mr" },
	{ 63,	"Potential temperature",	"theta" },
	{ 64, 	"Equivalent potential temperature", "thetae" },
	{ 65,	"E/W average wind",		"u_wind_avg" },
	{ 66,	"N/S average wind",		"v_wind_avg" },
	{ 67,	"Average wind speed",		"wspd_avg" },
	{ 68,	"Average wind direction",	"wdir_avg" }
}



/*
 * A description of a type 5 record: The first two words, the type (5)
 * and the size (106 words) are expected to be fixed.  The rest of the
 * record is 104 word values for data fields and flags.  The word values
 * must be scaled according to a type 3 record to get the actual data
 * values.  
 */



/*-----------------------------------------
 * Prototypes
 */

void ShowP3Header FP((FILE *p3file));

void SearchForRecords FP((FILE *p3file));

/*-----------------------------------------*/

int 
main(argc, argv)
	int argc;
	char *argv[];
{
	FILE *p3file;

	if (argc != 2)
	{
		fprintf(stderr,"Usage: %s <datafile>",argv[0]);
		exit(1);
	}

	/* The file to read will be argv[1] */
	if ((p3file = fopen(argv[1],"r")) == NULL)
	{
		perror(argv[1]);
		exit(1);
	}

	/*
	 * We have our file open and ready, so go read the header.
	 */

	ShowP3Header(p3file);
	rewind(p3file);
	SearchForRecords(p3file);
}


/*------------------------------
 * Dump the info stored in the first record (type 1) of a
 * P3 data file.
 */
void
ShowP3Header(p3file)
	FILE *p3file;
{
	P3_header_t hdr;
	unsigned long nread;

	nread = fread(hdr.record,sizeof(hdr.record),1,p3file);
	if (nread < 1)
	{
		perror("reading header");
		fprintf(stderr,"Only read %d bytes\n",nread);
		return;
	}

	/*
	 * The header record is now stored in our hdr[] buffer.
	 */

	/*
	 * Now dump the info
	 */
	printf("Header info\n");
#define P(name,val) printf("%20s: %hu\n",name,hdr.p3header.val)
	P("Record Type",type);
	P("Record Size",size);
	P("Aircraft Number",aircraft);
	printf("%20s: %02hu/%02hu/%02hu\n",
		"Date of flight",
		hdr.p3header.month,
		hdr.p3header.day,
		hdr.p3header.year);
	printf("%20s: %02hu:%02hu:%02hu\n",
		"Flight began",
		hdr.p3header.hour_begin,
		hdr.p3header.minute_begin,
		hdr.p3header.second_begin);
	printf("%20s: %02hu:%02hu:%02hu\n",
		"Flight ended",
		hdr.p3header.hour_end,
		hdr.p3header.minute_end,
		hdr.p3header.second_end);
#undef 	P
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
	unsigned long last_rec;	/* index of end of last record found */

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
			 * as part of the current block
			 */
			if (ftell(p3file)-4 - last_rec > 1)
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
			 * Set last_rec to end of this record
			 */
			last_rec = ftell(p3file) - 1;
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

