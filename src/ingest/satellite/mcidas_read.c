#include "mcidas_read.h"
#define MAX_BAD_TRIES 20
#define CALB 0xc3c1d3c2

/* program to read SSEC (McIdas) tapes into SI format files */

int in_fd,count_scans;
int output_channel;
int latlon_flag;	/* whether to get & store lat/lon data */
unsigned int start_moment, end_moment;
char *match_file;
int verbose;	/* if true, print a lot of stuff out while processing*/
int updating;	/* if true, we are simply updating the headers of files
						created by previous versions of ssec_read */

char output_filename[63],ll_filename[63];
struct mc_area *area, *mc_rarea();
char cmonth[12][4] = {"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"};
int days[2][12] = {31,28,31,30,31,30,31,31,30,31,30,31,
                     31,29,31,30,31,30,31,31,30,31,30,31};

float gms_table[256] = {
349.438, 345.613, 342.456, 339.854, 337.711, 335.945, 334.488, 333.280,
332.275, 331.430, 330.713, 330.096, 329.556, 329.075, 328.637, 328.232,
327.850, 327.484, 327.127, 326.777, 326.430, 326.084, 325.737, 325.388,
325.038, 324.685, 324.331, 323.973, 323.614, 323.254, 322.891, 322.528,
322.164, 321.799, 321.433, 321.067, 320.700, 320.333, 319.966, 319.598,
319.230, 318.861, 318.492, 318.122, 317.751, 317.380, 317.007, 316.633,
316.258, 315.882, 315.504, 315.125, 314.745, 314.363, 313.980, 313.596,
313.210, 312.823, 312.434, 312.045, 311.653, 311.261, 310.868, 310.473,
310.077, 309.681, 309.283, 308.884, 308.484, 308.084, 307.682, 307.279,
306.876, 306.472, 306.066, 305.660, 305.253, 304.844, 304.435, 304.024,
303.613, 303.200, 302.786, 302.371, 301.954, 301.536, 301.116, 300.696,
300.273, 299.849, 299.424, 298.997, 298.568, 298.138, 297.706, 297.272,
296.836, 296.399, 295.960, 295.520, 295.077, 294.633, 294.187, 293.740,
293.291, 292.840, 292.387, 291.933, 291.477, 291.019, 290.560, 290.099,
289.636, 289.172, 288.706, 288.239, 287.769, 287.298, 286.826, 286.351,
285.875, 285.397, 284.917, 284.435, 283.951, 283.465, 282.978, 282.488,
281.996, 281.502, 281.005, 280.507, 280.006, 279.502, 278.997, 278.488,
277.977, 277.464, 276.948, 276.429, 275.907, 275.383, 274.855, 274.325,
273.792, 273.256, 272.717, 272.175, 271.630, 271.082, 270.531, 269.977,
269.419, 268.859, 268.295, 267.728, 267.157, 266.583, 266.006, 265.426,
264.842, 264.255, 263.664, 263.069, 262.471, 261.869, 261.264, 260.654,
260.041, 259.423, 258.801, 258.175, 257.544, 256.909, 256.269, 255.625,
254.975, 254.321, 253.661, 252.996, 252.325, 251.649, 250.967, 250.279,
249.585, 248.885, 248.178, 247.464, 246.744, 246.017, 245.283, 244.542,
243.749, 243.037, 242.274, 241.502, 240.722, 239.934, 239.138, 238.333,
237.519, 236.696, 235.863, 235.021, 234.169, 233.307, 232.434, 231.549,
230.654, 229.746, 228.826, 227.893, 226.946, 225.985, 225.009, 224.017,
223.009, 221.984, 220.941, 219.878, 218.796, 217.692, 216.567, 215.418,
214.245, 213.046, 211.820, 210.565, 209.280, 207.962, 206.611, 205.223,
203.796, 202.327, 200.813, 199.251, 197.634, 195.959, 194.219, 192.405,
190.511, 188.524, 186.433, 184.222, 181.874, 179.367, 176.678, 173.776,
170.626, 167.188, 163.413, 159.246, 154.621, 149.460, 143.676, 137.163
};

main(argc, argv)
int argc;
char *argv[];
{
	long int one=1;
	int i,retval;
	float orbit,current_orbit;
	int year, month, day;
	float time;
	char sat[7];
	char date[8];
	short iheadr[15];
	int prompt=FALSE;
	int get_data;
	char *progname = argv[0];
	char devname[120];
	long dum, status;
	int bad_tries;
	unsigned int start_jday, start_year, start_hour;
	unsigned int end_jday, end_year, end_hour;
	unsigned int hash_moment();
	char rootname[256],comment[256];
	char date_string[20],*unjulianate();
	int hour,min,itime;

/*	make sure our input buffer is the same as in the fortran common block*/
	verbose = FALSE;
	updating = FALSE;
	start_moment = 0;
	end_moment = 100000000;
	latlon_flag = TRUE;
	matching = FALSE;
	devname[0] = NULL;
	count_scans = FALSE;	/* flag whether to display current scanline */

/* parse arguments */
	while(argc > 1)
	{
		switch(argv[1][0])
		{
			case '-':
				switch(argv[1][1])
				{
					case 'c':
						count_scans = TRUE;
						break;
					case 'd':
						strcpy(devname,argv[2]);
						argv++;
						argc--;
						break;
					case 'l':
						latlon_flag = FALSE;/* don't get lat/lon data*/
						break;
					case 'm':
						matching = TRUE;
						match_file = argv[2];
						argv++;
						argc--;
						break;
					case 'p':
						prompt = TRUE;
						break;
					case 'v':
						verbose = TRUE;
						break;
					case 'u':
					case '?':
					default:
						print_usage(progname);
						exit(0);
						break;
				}
			break;
			default: /* if it wasn't a switch, it must be an error*/
				fprintf(stderr,"Argument: '%s' unrecognized\n",argv[1]);
				print_usage(progname);
				break;
		}
		argc--;
		argv++;
	}

	if(!devname[0])
	{
		printf("Enter input device (or file) name: ");
		scanf("%s",devname);
	}

	/* open input device(file) */
	if((area = mc_rarea(devname)) == NULL)
	{
		perror("mc_rarea");
		exit();
	}
	init();

	/*fill in the nav. structure and initialize mcidas nav. routines */
	one = 1;
	strcpy(area->nav->goes.type, "GOES");
	nvxini_(&one, area->nav);
	line_res = area->dir->lres;
	elem_res = area->dir->eres;
	start_line = area->dir->lcor;
	start_elem = area->dir->ecor;
	image_lines = area->dir->lsiz;
	image_elems = area->dir->esiz;
	elem_size = 1; 	/* assume data is all one byte*/
	out_hdr.data_type = BYTE;
	out_hdr.id = area->dir->satid;
	out_hdr.year = area->dir->ndate/1000;
	out_hdr.day = area->dir->ndate - out_hdr.year*1000;
	itime = area->dir->itime/1000 * 10;
	out_hdr.time = itime;
	hour = out_hdr.time/100;
	min = out_hdr.time - hour*100;

	strcpy(date_string, unjulianate(out_hdr.year,out_hdr.day));
	printf("date: %s\n",date_string);
	sprintf(rootname,"%s.%s.%04d",source[area->dir->satid],date_string,itime);
	sprintf(output_filename,"%s",rootname);
	sprintf(comment,"%s %s %02d:%02dGMT",source[area->dir->satid],date_string,hour,min);
	out_hdr.comment = comment;
	/* open si file to match */
	if(matching)
	{
		if((match_hdr = si_open(match_file,0)) == (SatImageHdr *)-1)
		{
			fprintf(stderr,"Could not open '%s'.\n",match_file);
			exit(-1);
		}
		out_hdr.bad_value = match_hdr->bad_value;
      out_hdr.num_scans = match_hdr->num_scans;
      out_hdr.samps_per_scan = match_hdr->samps_per_scan;
      out_hdr.lat_lon_filename = match_hdr->lat_lon_filename;
      /* allocate space for the entire image */
	}
	else
	{
		out_hdr.bad_value = BAD_VALUE;
		out_hdr.num_scans = image_lines;
		out_hdr.samps_per_scan = image_elems;
		sprintf(ll_filename,"%s.ll",rootname);
		out_hdr.lat_lon_filename = ll_filename;
/*    allocate space for the global lat & lon arrays */
      if((glat = (float *)malloc(out_hdr.samps_per_scan * sizeof(float))) <= 0)

      {
         perror("malloc");
         exit(-1);
      }
      if((glon = (float *)malloc(out_hdr.samps_per_scan * sizeof(float))) <= 0)

      {
         perror("malloc");
         exit(-1);
      }
	}
/* write SI output header*/
	load_table(&out_hdr);
	write_header(&out_hdr);
/* loop , write one scan at a time*/
	if(matching)
	{
		compute_match_data();
	}
	else
	{
		for(i=0; i<out_hdr.num_scans; i++)
			do_scanline(i);
	}
}
				
print_usage(progname)
char *progname;
{
	printf("Usage: %s [-d device_name] [-m match_file] [-s sensor #]... \n",progname);
	printf("\t[-b day year hour] [-e day year hour]");
	printf("Switches:\n");
	printf("-d device_name:	device_name is the input device or file\n");
	printf("-m match_file:	All output files will share lat/lon of match_file\n");
	printf("-s sensor #:	Sensor number to retrieve. 1=VIS, 8=IR, 10=IR(H2O)\n");
	printf("-b day year hour:	day,year, and hour specify the first image to get\n");
	printf("-e day year hour:	day,year, and hour specify the last image to get\n");
	printf("-l:	Do not retrieve lat,lon data.\n");
	printf("-p:	Prompt user whether to retrieve each image.\n");
}

end_routine()
{
	if(matching)
	{
		compute_match_data();
	/* close and re-open si file to match (same as lseek to beginning)*/
		si_close(match_hdr);
		if((match_hdr = si_open(match_file,0)) == (SatImageHdr *)-1)
		{
			fprintf(stderr,"Could not open '%s'.\n",match_file);
			exit(-1);
		}
	}

	/*si_close(&hdr[output_channel]);*/
	si_close(&out_hdr);
}

/* routine to compute the output data that matches a given lat/lon file */
compute_match_data()
{
	register int i,j,samps,max_lines;
	float *lat, *lon;
	float line, elem, dum;
	register int row,column;
	register float badval;
	float current_lat, current_lon;
	unsigned char *par;
	register unsigned char *p;
	register char *match_data = (char *)area->image;

	max_lines = image_lines;
	samps = image_elems;
	badval = match_hdr->bad_value;

	/* allocate space for par,lat,lon arrays*/
	if((par = (unsigned char *)malloc(match_hdr->samps_per_scan * sizeof(char))) <= 0)
	{
		perror("compute_match_data: malloc");
		return(-1);
	}
	if((lat = (float *)malloc(match_hdr->samps_per_scan * sizeof(float))) <= 0)
	{
		perror("compute_match_data: malloc");
		return(-1);
	}
	if((lon = (float *)malloc(match_hdr->samps_per_scan * sizeof(float))) <= 0)
	{
		perror("compute_match_data: malloc");
		return(-1);
	}
		
	/* loop. Each time through, read a scan of lat/lon data from the match file.
		For each point in the scan, find the row and column (line and element)
		indices for the closest image point to that lat,lon location. */
	printf("Scan 0000");
	for(i=0; i<match_hdr->num_scans; i++)
	{
		printf("%04d",i);
		si_read_scanline(match_hdr,NULL, lat,lon,NULL, NULL);
		p = par;
		for(j=0; j<match_hdr->samps_per_scan; j++)
		{
			/* compute line,elem indices from lat,lon coords */
			current_lat = lat[j];
			current_lon = lon[j];
			if((current_lat != badval) && (current_lon != badval))
			{
				current_lon = -current_lon;
				nvxeas_(&current_lat, &current_lon, &dum, &line, &elem, &dum);
			/* compute row,column indices from line,elem (ie divide by resolution*/
				row = (line - start_line + .5)/line_res;
				column = (elem - start_elem + .5)/elem_res;
			/*if((row < 0) || (column < 0))printf("row=%d, col=%d\n",row,column);*/
				if((row >= 0) && (row <= max_lines) &&
					(column >= 0) && (column <= samps))
					*p = match_data[row*samps + column]; 
				else
					*p = 0;
			}
			else
				*p = 0;
			p++;
		}
		/* write out the scanline of data */
		si_write_scanline(&out_hdr, par, NULL, NULL, NULL, NULL);
	}
	free(match_data);
}

/*mqmovb_(mbbyts, mbfarr, mbfoff, mbtarr, mbtoff)*/
/* routine to move num_bytes bytes from beginning at source[source_offset}
	to dest[dest_offset]*/
void
mqmovb_(num_bytes, source, source_offset, dest, dest_offset)
int *num_bytes, *source_offset, *dest_offset;
char *source, *dest;
{

/*	printf("mqmovb: src_off = %d, dest_off = %d\n",source_offset,dest_offset);*/
	if(*num_bytes <= 0)return;
	memcpy(&dest[*dest_offset], &source[*source_offset], *num_bytes);
}

/*RETURN 4 BYTES LOCATED AT 0-BASED OFFSET IN ARRAY
	 source AS AN INTEGER. TREAT THE 4 BYTES AS 32 BITS OF
	2'S-COMPLEMENT BINARY INTEGER.*/
int
mqword_(source,offset)
char *source;
int *offset;
{
	int *retptr;

	retptr = (int *)&source[*offset];

	return(*retptr);
}

/* routine to move nbytes bytes in source array to 'offset' in dest array */
void
movb_(nbytes, source, dest, dest_offset)
int *nbytes, *dest_offset;
char *source, *dest;
{
	if(*nbytes <= 0)return;

	memcpy(&dest[*dest_offset],source,*nbytes);
}

write_header(sihdr)
SatImageHdr *sihdr;
{
/*	write out header */
				printf("creating '%s'\n",output_filename);
				if(matching | !latlon_flag)
					si_write_header(output_filename,sihdr,0);
				else
					si_write_header(output_filename,sihdr,1);
}

/* routine to write a scanline of data */
do_scanline(linenum)
int linenum;
{
	unsigned char *data = &area->image[linenum * out_hdr.samps_per_scan];
		printf("%04d",linenum);
		if(latlon_flag)
		{
			navigate_scanline(linenum, out_hdr.samps_per_scan, glat, glon);
			/*si_write_scanline(&hdr[output_channel],dsbuf,glat,glon,0,0.0);*/
			si_write_scanline(&out_hdr,data,glat,glon,0,0.0);
		}
		else
			/*si_write_scanline(&hdr[output_channel],dsbuf,NULL,NULL,0,0.0);*/
			si_write_scanline(&out_hdr,data,NULL,NULL,0,0.0);
}

/* routine to create a lookup table. Only needed for IR data */
load_table(hdr)
SatImageHdr *hdr;
{
	int i;
	float *table;
	float goes_table[256];

	if(out_hdr.id == GMS_IR)
	{
		table = gms_table;
		printf("Gms table\n");
	}
	else
	{
		printf("GOES table\n");
		table = goes_table;

		for(i=0; i<176; i++)
			table[i] = 330.0 - (float)i/2.0;
		for(i=176; i<256; i++)
			table[i] = 418.0 - (float)i;
	}

	hdr->lookup_table = table;
	hdr->index_size = 8;
	hdr->table_size = 256;
	hdr->data_type = INDEXED;
/*		one space in the table must be used to flag 'bad' or missing values.
		If there are any values in the data that use this index, well, they
		will just be taken for bad. It's not this easy for VIS data.*/
	table[BAD_INDEX] = hdr->bad_value;
}

/* routine to initialize navigation for an area */
prnav_(data)
char *data;
{
	float line, elem;
	long int one=1;

/*	the nav data starts at the 24th byte of the NAV block */
	if(nvxini_(&one,&data[24]) != 0)
	{
		fprintf(stderr,"prnav: initialization unsuccessful\n");
		return(-1);
	}
	/* fill in the output header's private area with position info*/
	out_hdr.private_size = sizeof(PositionStruct);
	out_hdr.private = (char *)&sat_pos;
	subsat_pos_(&sat_pos.x, &sat_pos.y, &sat_pos.z, &sat_pos.lat, &sat_pos.lon);
	load_table(&out_hdr);
	write_header(&out_hdr);
}

navigate_scanline(scan_num,samps,lat,lon)
int scan_num,samps; /* the current scan line number and # of samps per scan*/
float *lat,*lon;
{
	float full_res_line, full_res_elem;
	float zero=0.0, xdum;
	register int i;

/*	we need to give nvxsae() FULL RES line & elem numbers to do the 
	transformation to lat/lon*/
	full_res_line = start_line + scan_num*line_res;
	for(i=0; i<samps; i++)
	{
		full_res_elem = start_elem + i*elem_res;
		if(nvxsae_(&full_res_line, &full_res_elem, &zero, &lat[i], &lon[i], &xdum) == -1)
		{
			lat[i] = BAD_VALUE;
			lon[i] = BAD_VALUE;
		}
/*	inversion done in NVXSAE
		else
			lon[i] = 360 - lon[i];/* I dunno why.  They all came out backwards.*/
	} 
}

/* return a single integer incorporating day, year, hour */
unsigned int
hash_moment(day, year, hour)
{
	/*yyyydddhh*/
	return(year*100000 + day*100 + hour);
}

/* routine returns true if the file descriptor is a tape device */
int 
input_is_tape(fd)
{
	struct stat buf;

	/* if the file is a regular file, return 0. else it must be a tape dev, so
		return 1*/

	fstat(fd,&buf);
	if((buf.st_mode & S_IFMT) == S_IFREG)
		return(FALSE);
	else
		return(TRUE);
}

init()
{
	strcpy(source[GOES1_VIS], "GOES1VIS");
	strcpy(source[GOES1_IR], "GOES1IR");
	strcpy(source[GOES2_VIS], "GOES2VIS");
	strcpy(source[GOES2_IR], "GOES2IR");
	strcpy(source[GOES3_VIS], "GOES3VIS");
	strcpy(source[GOES3_IR], "GOES3IR");
	strcpy(source[GOES4_VIS], "GOES4VIS");
	strcpy(source[GOES4_IR], "GOES4IR");
	strcpy(source[GOES5_VIS], "GOES5VIS");
	strcpy(source[GOES5_IR], "GOES5IR");
	strcpy(source[GOES6_VIS], "GOES6VIS");
	strcpy(source[GOES6_IR], "GOES6IR");
	strcpy(source[GOES7_VIS], "GOES7VIS");
	strcpy(source[GOES7_IR], "GOES7IR");
	strcpy(source[ERBE], "ERBE");
	strcpy(source[MET_VIS], "METVIS");
	strcpy(source[MET_IR], "METIR");
	strcpy(source[MET_H2O], "METH2O");
	strcpy(source[GMS_VIS], "GMSVIS");
	strcpy(source[GMS_IR], "GMSIR");
	strcpy(source[NOAA6], "NOAA6");
	strcpy(source[NOAA7], "NOAA7");
	strcpy(source[NOAA8], "NOAA8");
	strcpy(source[NOAA9], "NOAA9");
}

/* return a string specifying the date for the given julian day*/
char *
unjulianate(year,day)
int year,day;
{
	char string[20];
	int leap=1;
	int month=0;

	if((year % 4) == 0) /* probably a leap year. check if divisible by 100*/
	{
		if((year % 100) == 0)
		{
			 if((year % 400) != 0)leap = 0;
		}
	}
	else /* not a leap year*/
	{
		leap = 0;
	}

	while(day > days[leap][month])
		day -= days[leap][month++];

	sprintf(string,"%02d%s%02d",year,cmonth[month],day);
	return(string);
}
