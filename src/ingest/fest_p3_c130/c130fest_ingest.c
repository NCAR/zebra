/*
 * $Id: c130fest_ingest.c,v 1.1 1992-11-18 00:33:51 granger Exp $
 *
 * Ingest ASCII data files for the U of Washington's C-130 Convair.
 * As far as I can determine from the files, they follow this format:
 *
 * Line 1:	'flight' <flgith #> <mo/dy/yr> <# fields per line>
 * Line 2:	<name of field 1> <name of field 2> ...
 * Line 3:	<units of field 1> <units of field 2> ...
 * Lines 4-?	<value for field 1 at value for timeh field> ...
 * Last two lines: Sometimes seem to be invalid...
 *
 * Other points to note:
 * -99.0 seems to be the "bad value indicator" for ALL fields, including lat/lon.
 * This value will be set to BadValueFlag in the DataChunk.
 * 
 * Strategy:
 * We require that three specific fields be in the file: timeh, latgps,
 * longps, and palt.
 * Each line of the file corresponds to a sample in a DataChunk.
 * The timeh value is converted to ZebTime and used as the time of the
 * sample.  The latgps and longps fields are used to assign a location to
 * the sample.  All of the other fields are stored into the DataChunk.
 * The entire file will be ingested as one DataChunk.
 * The timeh, latgps, longps, and palt fields will not stored as individual
 * fields in the DataChunk, as these will be stored as the location of each
 * sample.  The flight number and date from the first line
 * of the file will be stored as global attributes in the DataChunk.
 *
 * The platform name will be the C-130's STORMFEST id; I think this id
 * was "storm4".  
 * 
 * The DataChunk will be of class DCC_Scalar.
 *
 * Late-breaking development:  The 'parameters' file supplied with the
 * data files seems to describe each of the fields in the data file.  Each
 * line correpsonds to each field.  This file will be read to supply the
 * 'description' part of each field declaration.
 */

#include <stdio.h>
#include "ingest.h"

#define PLATFORM_NAME		"storm4"
#define INGEST_NAME		"C-130 Ingest"
#define LINE_BUFFER_SIZE	256
#define BAD_VALUE_FLAG		(-99.0)
#define TIME_FIELD		"timeh"
#define LAT_FIELD		"latgps"
#define LON_FIELD		"longps"
#define ALT_FIELD		"palt"
#define NREQ_FIELDS		4

#define AttFlightDate		"flight_date"
#define AttFlightNumber		"flight_number"

#define Abs(x)			((x>0)?(x):(-(x)))
#define Min(x,y)		(((x)<(y))?(x):(y))
#define Max(x,y)		(((x)<(y))?(y):(x))

/*----------------------------------------------------------------
 * Prototypes 
 */
static void Usage FP((char *prog));
static DataChunk *CreateDataChunk FP((FILE *in, int *nfields, ZebTime *flight_date));
static char *GetLine FP((FILE *in, char *msg));
static FieldId *GetFields FP((FILE *in, FILE*parms, int nfields));
static void IngestSamples FP((DataChunk *dc, FILE *datafile, int nfields, 
			    FieldId *fields, ZebTime *flight_date));
static int ReadSampleLine
	FP((FILE *in, ZebTime *sample_time, Location *sample_locn,
	    float *values, int nfields, ZebTime *flight_date));
static void DateFromPackedTime FP((ZebTime *sample_time, ZebTime *flight_date,
				   unsigned long packed_time));
extern void InterpolateGap
	FP((DataChunk *dc, ZebTime *btime, Location *blocn,
	    ZebTime *etime, Location *elocn, int *sample));
static DataChunk *ReCreateDataChunk FP((DataChunk *dc));

/*----------------------------------------------------------------
 * Global variables
 * -- to store the positions of the time, lat, and lon fields in the data file
 */

int TimeField = -1;
int LatField = -1;
int LonField = -1;
int AltField = -1;

DataChunk *DestDC;	/* Used to pass dc to CopyAttribute() */

/*----------------------------------------------------------------
 * MAIN
 *
 * (1) Initialize the Ingest package and parse any ingest options.
 * (2) Determine our file stream, either from a file name left on
 *     on the command line, or stdin.
 * (3) CreateDataChunk() with the first line of the file.
 * (4) GetFields() from the next two lines.
 * (5) Setup the DataChunk fields.
 * (6) Read the the rest of lines in the file and ingest each
 *     field value for each sample time and location.
 * (7) Store the data chunk.
 * (8) Done.
 */
int
main(argc, argv)
	int argc;
	char *argv[];
{
	DataChunk *dc;			/* The DataChunk we'll use for the file */
	ZebTime flight_date;		/* Date of the flight data being ingested */
	FILE *datafile;			/* Our datafile stream */
	FILE *parmfile;			/* The file holding field descriptions */
	int nfields;			/* The number of fields we'll ingest.
					 * This should be 3 less than the number
					 * of fields in the file, since the
					 * time, lat, lon fields will not be
					 * explicitly stored as fields */
	FieldId *fields;		/* The field ids we'll be ingesting */

	IngestParseOptions(&argc, argv, Usage);

	if (argc > 3)
	{
		Usage(argv[0]);
		exit(1);
	}

	if (argc == 3)			/* file name was specified */
	{
		if (!(datafile = fopen(argv[1],"r")))
		{
			perror(argv[1]);
			exit(1);
		}
	}
	else
		datafile = stdin;

	/*
	 * No matter what, the parameters file should be the last argument
	 * on the command line
	 */
	if (!(parmfile = fopen(argv[argc-1],"r")))
	{
		perror(argv[argc-1]);
		exit(1);
	}

	IngestInitialize(INGEST_NAME);

	/*
	 * All set to start processing the file.
	 * Get a DataChunk, and the number of fields we'll ingest.
	 */
	dc = CreateDataChunk(datafile, &nfields, &flight_date);

	fields = GetFields(datafile, parmfile, nfields);
	fclose(parmfile);

	/*
	 * Setup the fields in the DataChunk
	 */
	dc_SetScalarFields(dc, nfields, fields);
	dc_SetBadval(dc, BAD_VALUE_FLAG);

	/*
	 * We now have all of our preliminary info.
	 * Start reading all successive lines in datafile and adding each field
	 * from each line to the data chunk
	 */
	IngestSamples(dc, datafile, nfields, fields, &flight_date);

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
	/*
	 * Store the data chunk, as long as we got some samples
	 */
	if (dc_GetNSample(dc) == 0)
	{
		/* Our DC is still empty for some reason */
		IngestLog(EF_PROBLEM,
			  "trying to store an empty data chunk ?");
		return;
	}

	/*
	 * Each raw C-130 file will correspond to a single DataStore file
	 */
	IngestLog(EF_INFO,"Storing to new data file...");
	ds_Store(dc, /*newfile*/ TRUE, /*details*/ NULL, /*ndetail*/ 0);
	IngestLog(EF_INFO,"Done storing to file.");
}



/*
 * Tell the user how to use the program
 */
void
Usage(prog)
	char *prog;
{
	fprintf(stderr,
		"Usage: %s [ingest options] [file name] <parameter file>\n",prog);
	fprintf(stderr,
		"   If no file name specified, stdin will be used for input\n");
	IngestUsage();
}


/*
 * Return a pointer to a buffer containing the next line of 'file'.
 * If an error occurs, then the 'msg' is logged and NULL is returned.
 * Up to two lines can be held and accessed simultaneously since the
 * function alternates between the buffer it uses.  A new buffer is not
 * selected until the current one is successfully filled without error.
 */
char *
GetLine(file, msg)
	FILE *file;
	char *msg;
{
	static unsigned long line_no = 1;
	static char buf1[LINE_BUFFER_SIZE];
	static char buf2[LINE_BUFFER_SIZE];
	static char *buf = buf1;

	/*
	 * First test for end of file
	 */
	if (feof(file) && msg)
	{
		IngestLog(EF_PROBLEM,"%s: end of file unexpected", msg);
		return(NULL);
	}

	/*
	 * Try to read the next line.
	 */
	if (!fgets(buf, LINE_BUFFER_SIZE, file) && !feof(file))
	{
		IngestLog(EF_PROBLEM,"%s: could not read line %lu", 
			  msg, line_no);
		return(NULL);
	}

	/*
	 * Make sure the whole line fit into our buffer.
	 */
	if ((strlen(buf) > 0) && (buf[strlen(buf)-1] != '\n'))
	{
		if (strlen(buf) == LINE_BUFFER_SIZE - 1)
		{
			IngestLog(EF_PROBLEM,"%s: line %lu too long", msg, line_no);
			return(NULL);
		}
		/* otherwise we just got the last line of the file before EOF */
	}

	/*
	 * If we made it this far, everything must be o.k.
	 */
	buf = (buf == buf1) ? buf2 : buf1;
	++line_no;
	return((buf == buf1) ? buf2 : buf1);
}



/*----------------------------------------------------------------
 * DataChunk *CreateDataChunk(FILE *in, int *nfields)
 *
 * Create and initialize an empty DataChunk with the C-130 data file
 * on 'in'.  'in' is expected to be at the beginning of the file.
 * On return, *nfields holds the number of fields to ingest, 3 less than
 * the number read from the first line of the file, and the function returns
 * a pointer to the new data chunk.  If any errors occur, a message is
 * logged and the program exits.
 */
DataChunk *
CreateDataChunk(in, nfields, flight_date)
	FILE *in;
	int *nfields;
	ZebTime *flight_date;
{
	char *buf;
	int num_fields;
	int flight_no;
	int month,day,year;
	DataChunk *dc;
	char att_val[30];
	PlatformId plat_id;

	while (1)
	{
		if (!(buf = GetLine(in,"searching for header line")))
		{
			exit(1);
		}

		if (sscanf(buf, " flight %d %d/%d/%d %d ",
			   &flight_no, &month, &day, &year, &num_fields) != 5)
		{
			IngestLog(EF_PROBLEM,
				  "Line: '%s': not in expected format, %s",
				  buf, "searching for correct line");
		}
		else
			break;
	}

	/*-------------- Perform lots of error and sanity checking ------------*/

	if (num_fields <= NREQ_FIELDS)
	{
		IngestLog(EF_PROBLEM,
			  "the number of fields, %i, needs to be more than %i",
			  num_fields, NREQ_FIELDS);
		exit(0);
	}

	if ((plat_id = ds_LookupPlatform(PLATFORM_NAME)) == BadPlatform)
	{
		IngestLog(EF_PROBLEM,"%s bad platform name, aborting.",
			  PLATFORM_NAME);
		exit(1);
	}
	
	dc = dc_CreateDC(DCC_Scalar);
	dc->dc_Platform = plat_id;

	TC_ZtAssemble(flight_date,
		      year, month, day, 0, 0, 0, 0);
	TC_EncodeTime(flight_date, TC_DateOnly, att_val);
	dc_SetGlobalAttr(dc, AttFlightDate, att_val);

	IngestLog(EF_INFO,"ingesting flight no. %i on %s",
		  flight_no, att_val);
	sprintf(att_val, "%i", flight_no);
	dc_SetGlobalAttr(dc, AttFlightNumber, att_val);

	*nfields = num_fields - NREQ_FIELDS;	/* Don't include time or locn */
	return(dc);
}



/*
 * Parse the second and third lines of a file from a list of field names and a
 * list of units into a list
 * of field id's.  The TIME_FIELD, LAT_FIELD, and LON_FIELD are not
 * included in the list.  The list is then used to set-up the DataChunk fields.
 * 'nfields' holds the number of field ids to create; it does not include
 * time, lat, and lon.
 * Any errors result in a message and exit().  Returns the array of field id's.
 *
 * We're gonna have to kludge.  Units on the third line include spaces, so there's
 * no easy way to pull the units from there.  Instead the units are statically
 * stored here, but the units read from the line are verified with what we've
 * stored internally.
 */
FieldId *
GetFields(in, parms, nfields)
	FILE *in;
	FILE *parms;
	int nfields;
{
	int i;
	int n;
	char *fields_buf;
	char *units_buf;
	char desc[LINE_BUFFER_SIZE];
	char *units;
	char field_name[LINE_BUFFER_SIZE];
	int num_ids;
	FieldId *ids;
	static char units_list[][12] =
	{
		"hhmmss", "deg", "deg", "mb", "km", "m/s", "deg", "deg C",
		"deg C", "deg C", "deg C", "gm/m^3", "gm/m^3", "#/l", "#/cm^3",
		"cm^(2/3)/s"
	};

	IngestLog(EF_DEBUG,"attempting to read %i fields",nfields);

	/*
	 * Read the next two lines of the file
	 */
	if (!(fields_buf = GetLine(in,"reading field names")))
		exit(1);
	if (!(units_buf = GetLine(in,"reading units")))
		exit(1);

	/*
	 * Allocate space for the field ids
	 */
	ids = (FieldId *)malloc(nfields * sizeof(FieldId));
	if (!ids)
	{
		IngestLog(EF_PROBLEM,"not enough memory for field ids");
		exit(1);
	}

	num_ids = 0;
	for (i = 0; i < nfields + NREQ_FIELDS; ++i)
	{
		if (sscanf(fields_buf," %s %n", field_name, &n) != 1)
		{
			IngestLog(EF_PROBLEM,
				  "could only read %i field names", i);
			exit(1);
		}
		fields_buf += n;

		sscanf(units_buf," %n",&n);
		units_buf += n;
		if (!strncmp(units_buf, units_list[i], strlen(units_list[i])))
		{
			units = units_list[i];
		}
		else
		{
			IngestLog(EF_PROBLEM,
				  "expected units %s, units remaining: %s",
				  units_list[i], units_buf);
			exit(1);
		}
		units_buf += strlen(units);

		if (!fgets(desc,LINE_BUFFER_SIZE,parms))
		{
			IngestLog(EF_PROBLEM,
				  "could not read description for field %s, %s",
				  field_name, "continuing anyway...");
			strcpy(desc,"");
		}
		else if (strlen(desc) > 0)
		{
			if (desc[strlen(desc) - 1] == '\n')
				desc[strlen(desc) - 1] = '\0';
			else if (strlen(desc) == LINE_BUFFER_SIZE - 1)
				IngestLog(EF_PROBLEM,
					  "Line too long in parameters file: %s",
					  desc);
		}

		/*
		 * Have a field name and unit, get a field id, unless this is
		 * one of the time, lat, or lon fields which 
		 * we'll be storing implicitly with each sample
		 */
		if (streq(field_name,TIME_FIELD))
		{
			TimeField = i;
			IngestLog(EF_INFO,
				  "Time field, %s (%s), %s, in position %i",
				  field_name, units, desc, TimeField);
		}
		else if (streq(field_name,LAT_FIELD))
		{
			LatField = i;
			IngestLog(EF_INFO,
				  "Lat field, %s, (%s), %s, in position %i",
				  field_name, units, desc, LatField);
		}
		else if (streq(field_name,LON_FIELD))
		{
			LonField = i;
			IngestLog(EF_INFO,
				  "Lon field, %s, (%s), %s, in position %i",
				  field_name, units, desc, LonField);
		}
		else if (streq(field_name,ALT_FIELD))
		{
			AltField = i;
			IngestLog(EF_INFO,
				  "Alt field, %s, (%s), %s, in position %i",
				  field_name, units, desc, AltField);
		}
		else 
		{
			ids[num_ids] = F_DeclareField(field_name, desc, units);
			IngestLog(EF_DEBUG,"declared field %s (%s), %s, id = %i",
				  field_name, units, desc, ids[num_ids]);
			++num_ids;
		}
	}

	/*
	 * Make sure we found our time, lat, lon, and alt fields
	 */
	if (TimeField < 0)
		IngestLog(EF_PROBLEM,"time field, %s, not found!",TIME_FIELD);
	if (LatField < 0)
		IngestLog(EF_PROBLEM,"lat field, %s, not found!",LAT_FIELD);
	if (LonField < 0)
		IngestLog(EF_PROBLEM,"lon field, %s, not found!",LON_FIELD);
	if (AltField < 0)
		IngestLog(EF_PROBLEM,"alt field, %s, not found!",ALT_FIELD);
	if ((TimeField < 0) || (LatField < 0) || (LonField < 0) || (AltField < 0))
		exit(1);

	/*
	 * That should be it.  Return the necessary info. Note that we do
	 * not check that more field names may be on this line.  They are
	 * just ignored.  This is either lazy error checking or a feature.
	 */
	IngestLog(EF_INFO,"set to ingest %i of %i fields",
		  num_ids,nfields+NREQ_FIELDS);

	return(ids);
}



/*----------------------------------------------------------------
 * IngestSamples(DataChunk *dc, FILE *in, 
 *             int nfields, FieldId *fields, ZebTime *date)
 *
 * Read the rest of the file, adding the field values to each sample
 * of the data chunk.  Each line is a sample: the time and location
 * of the sample are taken from the TIME_FIELD, LAT_FIELD, and LON_FIELD.
 * Some correctness checks are made: the times of each sample must
 * be exactly one second apart, and the successive locations must be
 * within a certain tolerance of each other.  Bad values should have
 * already been flagged in the data chunk, so no special recognition of
 * them need be made here, unless a time or location field appears
 * bad.  Then we'll have problems...
 *
 * NOTE: 'nfields' is the number of field ids in 'fields'.  The number
 * of fields per line of the file will be nfields+NREQ_FIELDS.
 * 'fields' only contains ids for the fields on each line which will
 * be ingested.  That is,
 * all fields on the line except for the time, lat, and lon fields.
 *
 * The date is required to reconstruct a full time for each sample
 * from the hours, minutes, and seconds time given in the time field.
 */
void
IngestSamples(dc, in, nfields, fields, flight_date)
	DataChunk *dc;
	FILE *in;
	int nfields;
	FieldId *fields;
	ZebTime *flight_date;
{
	int i, fld;
	int nsample;
	float *values;
	static Location prev_locn = { 0, 0, 0 };
	static ZebTime prev_time = { 0, 0 };
	Location slocn;				/* Sample location */
	ZebTime stime;				/* Sample time */
	ZebTime start;
	ZebTime last_gap;
	int ngaps;
	int max_gap = 1;
	int min_spacing = -1, max_spacing = -1;
	char ctime[30];
	int nbad_locns = 0;

	values = (float *)malloc((nfields+NREQ_FIELDS)*sizeof(float));
	if (!values)
	{
		IngestLog(EF_PROBLEM,"could not allocate array of field values");
		exit(1);
	}

	last_gap.zt_MicroSec = 0;
	start.zt_MicroSec = 0;
	nsample = 0;
	ngaps = 0;
	while (!feof(in))
	{
		/*
		 * Just skip this line if we encounter an error.  Error messages
		 * will be logged from within the function.
		 */
		if (!ReadSampleLine(in, &stime, &slocn, 
				    values, nfields+NREQ_FIELDS, flight_date))
			continue;

		TC_EncodeTime(&stime, TC_Full, ctime);

		/* A quick handling of any pending messages */
		if (!(nsample % 100))
			while (msg_poll(0) != -1);

		/*
		 * Check for bad values in the location.  A location will
		 * be marked bad if lat, lon, AND alt are BAD_VALUE_FLAG (-99)
		 */
		if ((slocn.l_lat == BAD_VALUE_FLAG) ||
		    (slocn.l_lon == BAD_VALUE_FLAG) ||
		    (slocn.l_alt == BAD_VALUE_FLAG))
		{
			nbad_locns++;
			if (nbad_locns == 1)
				IngestLog(EF_PROBLEM,
					  "Bad locn found at %s",ctime);
			continue;
		}

		if (nbad_locns)
		{
			IngestLog(EF_PROBLEM,
				  "Good locn found at %s, %i bad locns skipped",
				  ctime, nbad_locns);
			nbad_locns = 0;
		}

		/*
		 * Quality control checking.  Check for gaps between sample times
		 * greater than 1 second and differences between successive lat/lon
		 * greater than 0.1 degree.  Time gaps of 5 or less seconds will
		 * be filled in with interpolated locations and bad values for fields.
		 * For larger gaps, the current DC is stored to a file and a new
		 * DC started.
		 */
		if (prev_time.zt_Sec)		/* This isn't the first time */
		{
			if (TC_Less(stime, prev_time))	/* Time regressed! */
				IngestLog(EF_PROBLEM,
					  "Time went backwards in sample %i, at %s !",
					  nsample, ctime);
			if (stime.zt_Sec - prev_time.zt_Sec > 1)
			{
				IngestLog(EF_DEBUG,
					  "Time gap of %i secs at sample %i, %s",
					  (stime.zt_Sec - prev_time.zt_Sec),
					  nsample, ctime);
				max_gap = Max(max_gap,
					      stime.zt_Sec - prev_time.zt_Sec);
				if (min_spacing < 0)
					min_spacing = stime.zt_Sec - start.zt_Sec;
				else
					min_spacing = Min(min_spacing,
							  stime.zt_Sec - 
							  last_gap.zt_Sec);
				if (max_spacing < 0)
					max_spacing = stime.zt_Sec - start.zt_Sec;
				else
					max_spacing = Max(max_spacing,
							  stime.zt_Sec -
							  last_gap.zt_Sec);
				ngaps++;
				last_gap.zt_Sec = stime.zt_Sec;

				/*
				 * Fill in gaps of 5 or less seconds with
				 * interpolated locations and sample times
				 */
				if (stime.zt_Sec - prev_time.zt_Sec <= 5)
				{
					InterpolateGap
						(dc, &prev_time, &prev_locn,
						 &stime, &slocn,
						 &nsample);
					/*
					 * Once the missing spots are filled in,
					 * we can continue with the current sample
					 */
				}
				else	/* We must start a whole new file */
				{
					IngestLog(EF_PROBLEM,
					  "Gap of %i seconds to big to interpolate",
					  (stime.zt_Sec - prev_time.zt_Sec));
					IngestLog(EF_PROBLEM,
					  "Must end file here and start another");
					StoreDataChunk(dc);
					dc = ReCreateDataChunk(dc);
					nsample = 0;
				}
			}
		}
		else	/* save the time of this first sample */
		{
			start.zt_Sec = stime.zt_Sec;
			last_gap.zt_Sec = stime.zt_Sec;
		}

		/*
		 * This is just info provided FUI, For the User's Information...
		 * Currently nothing is done with locns that appear to
		 * be in error because of large deltas.
		 */
		if (prev_locn.l_lat)		/* Not the first valid locn */
		{
			if (Abs(slocn.l_lat - prev_locn.l_lat) > 0.1)
				IngestLog(EF_DEBUG,
					  "sample %i, lat %f differs by %f degrees",
					  nsample, slocn.l_lat, 
					  slocn.l_lat - prev_locn.l_lat);
			if (Abs(slocn.l_lon - prev_locn.l_lon) > 0.1)
				IngestLog(EF_DEBUG,
					  "sample %i, lon %f differs by %f degrees",
					  nsample, slocn.l_lon, 
					  slocn.l_lon - prev_locn.l_lon);
		}

		/*
		 * We should now have a valid sample time and valid location
		 */
		IngestLog(EF_DEBUG | ((nsample % 500) ? 0 : EF_INFO),
			  "Sample %5i, %s, %.2f lat, %.2f lon, alt %5.3f km",
			  nsample, ctime, slocn.l_lat, slocn.l_lon, slocn.l_alt);

		/*
		 * Store all of the fields except time, lat, lon, and altitude
		 */
		for (i = 0, fld = 0; i < nfields+NREQ_FIELDS; ++i)
		{
			if ((i == TimeField) || (i == LatField) || 
			    (i == LonField)  || (i == AltField))
				continue;
			dc_AddScalar(dc, &stime, nsample, fields[fld],
				     &values[i]);
			IngestLog(EF_DEVELOP,"   field %6s (%10s) = %.2f",
				  F_GetName(fields[fld]),
				  F_GetUnits(fields[fld]),
				  values[i]);
			++fld;
		}
		dc_SetLoc(dc, nsample, &slocn);

		++nsample;
		prev_locn.l_lat = slocn.l_lat;
		prev_locn.l_lon = slocn.l_lon;
		prev_locn.l_alt = slocn.l_alt;
		prev_time.zt_Sec = stime.zt_Sec;
	}
	IngestLog(EF_INFO,"End of File, %i samples, last sample at %s",
		  nsample, ctime);
	IngestLog(EF_INFO,
		  "%i time gaps found, %.3f gaps per minute, 1 gap every %.0f secs",
		  ngaps, (float)ngaps/((float)(stime.zt_Sec - start.zt_Sec)/60.0),
		  (float)(stime.zt_Sec - start.zt_Sec)/(float)ngaps);
	IngestLog(EF_INFO,"Largest gap: %i seconds",max_gap);
	IngestLog(EF_INFO,"Minimum duration between gaps: %i seconds",min_spacing);
	IngestLog(EF_INFO,"Maximum duration between gaps: %i seconds",max_spacing);
}




/*
 * Calculate a sample time from a packed time
 * given the flight date (time 00:00:00).  The first time this function is called,
 * the static variable 'start' is set to the resultant time so that subsequent
 * rollovers in the date can be detected.  Hence this is not an all-purpose
 * converter; it expects to be called in the chronological order of the file.
 */
void
DateFromPackedTime(sample_time, flight_date, packed_time)
	ZebTime *sample_time;
	ZebTime *flight_date;
	unsigned long packed_time;
{
	int hours, minutes, seconds;
	static ZebTime start = { 0, 0 };	/* The start time of the data */

	/*
	 * Get the time, lat, and lon of this sample
	 */
	seconds = packed_time % 100;
	hours = packed_time / 10000;
	minutes = (packed_time - (hours * 10000)) / 100;
	
	/*
	 * Add the time of day to the flight date to get the sample time
	 */
	sample_time->zt_Sec = flight_date->zt_Sec + (hours * 3600) +
		(minutes * 60) + seconds;
	
	/*
	 * If the unpacked time is much less than the start time, then we
	 * probably ought to roll the date forward.
	 */
	if (!start.zt_Sec)		/* this sample time is the start */
		start.zt_Sec = sample_time->zt_Sec;
	else if ((sample_time->zt_Sec < start.zt_Sec) &&
		 (start.zt_Sec - sample_time->zt_Sec > 12*3600))
		sample_time->zt_Sec += 24*3600;

	sample_time->zt_MicroSec = 0;
}



/*
 * Read the next line of the file, fill in the values for each field, and
 * calculate the time and location of this sample from the time, lat, and
 * lon fields of the sample line.
 */
int
ReadSampleLine(in, sample_time, sample_locn, values, nfields, flight_date)
	FILE *in;
	ZebTime *sample_time;
	Location *sample_locn;
	float *values;
	int nfields;
	ZebTime *flight_date;
{
	static nline = 3;	/* Samples should start on line 4 of file */
	char *buf;
	int i, n;
	unsigned long packed_time;
	
	++nline;

	if (!(buf = GetLine(in, "reading a line of sample data")))
	{
		IngestLog(EF_PROBLEM,
			  "   skipping line %i", nline);
		return(0);
	}

	/*
	 * Try to read nfields values from this line
	 */
	for (i = 0; i < nfields; i++)
	{
		/*
		 * The time field will be the only field read as an int
		 */
		if (i == TimeField)
		{
			if (sscanf(buf," %lu %n",&packed_time, &n) != 1)
			{
				IngestLog(EF_PROBLEM,
					  "%s, %s, in position %i, %s %i",
					  "could not read time field", 
					  TIME_FIELD,
					  i, "skipping line", nline);
				break;
			}
		}
		else if (sscanf(buf," %f %n",&(values[i]),&n) != 1)
		{
			IngestLog(EF_PROBLEM,
				  "only got %i field values, expecting %i, %s %i",
				  i, nfields, "skipping line", nline);
			break;
		}
		buf += n;
	}
	
	/*
	 * Make sure we got all of the fields from the line, else return failure
	 */
	if (i < nfields)
		return(0);
	
	/* 
	 * Calculate our sample time given our flight date and the packed time
	 * read from this sample line
	 */
	DateFromPackedTime(sample_time, flight_date, packed_time);
	
	sample_locn->l_lat = values[LatField];
	sample_locn->l_lon = values[LonField];
	sample_locn->l_alt = values[AltField];	/* already in km */

	return(1);				/* Success! */
}



DataChunk *ReCreateDataChunk(src)
	DataChunk *src;
{
	/*
	 * Copy the structure of the class DCC_Scalar src DataChunk and not
	 * the data.
	 */
	int CopyAttribute(/* char *key, char *value */);
	DataChunk *dc;

	dc = dc_CreateDC(DCC_Scalar);
	dc->dc_Platform = src->dc_Platform;

	/*
	 * Put the fields of the old DataChunk into the new one
	 */
	dc_SetScalarFields(dc, dc_GetNField(src), dc_GetFields(src, NULL));

	/*
	 * Copy all of the global attributes into the new data chunk
	 */
	DestDC = src;
	dc_ProcessAttrs(src, NULL, CopyAttribute);
	
	/*
	 * Copy the bad value flag
	 */
	dc_SetBadval(dc, dc_GetBadval(src));

	dc_DestroyDC(src);
	return (dc);
}



int
CopyAttribute(key, value)
	char *key;
	char *value;
{
	dc_SetGlobalAttr(DestDC, key, value);
	return(0);
}

