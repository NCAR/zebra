/*
 * The GENPRO processing program.  Here we go through and boil GENPRO files
 * down into something reasonable.
 */
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
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

# include <fcntl.h>
# include <sys/file.h>
# include <ctype.h>
# include <stdio.h>
# include <netcdf.h>
# include "defs.h"
MAKE_RCSID ("$Id: gprotocdf.c,v 1.13 1994-02-01 09:05:40 granger Exp $")

/*
 * Netcdf stuff.
 */
# define MAXFLD 100
# define STRLEN	50
# define UNITLEN 20
# define DESCLEN 100
# define NAMELEN 50
# define BADVAL		(-32768.0)	/* Bad data flag		*/
# define LL_BADVAL	(0)
# define BAD_FACTOR	(0)
# define BAD_TERM	(-9999)
# define BAD_OFFSET	(-1)
# define BAD_VFIELD	(-1)
# define MAX(a,b)	(((a)>(b))?(a):(b))
# define STRNCPY(dst, src, n)	{ strncpy(dst, src, n); dst[n - 1] = '\0'; }

# define Isalnum(c)	(isalnum(c) || ((c) == '_'))
# define Tolower(c)	(((c) != '_') ? (tolower(c)) : (c))

typedef struct _GPField {
	char gpf_src[NAMELEN];	/* name of GP src field */
	char gpf_dst[NAMELEN];	/* name of netCDF dest	*/
	char gpf_desc[DESCLEN];	/* Descrpition of field	*/
	char gpf_units[UNITLEN];/* Units of field	*/
	float gpf_factor;	/* FACTOR from GP file	*/
	float gpf_term;		/* TERM for this field	*/
	int  gpf_offset;	/* Offset in GP record	*/
	int  gpf_ncvar;		/* netcdf variable num	*/
} GPField;

GPField Fields[MAXFLD];	/* Info for all of the fields	*/

int NField = 0;		/* number of elements in Fields[] */

# define SRC(i)		(Fields[i].gpf_src)
# define DST(i)		(Fields[i].gpf_dst)
# define DESC(i)	(Fields[i].gpf_desc)
# define UNITS(i)	(Fields[i].gpf_units)
# define FLDOFFSET(i)	(Fields[i].gpf_offset)
# define VFIELD(i)	(Fields[i].gpf_ncvar)
# define FACTOR(i)	(Fields[i].gpf_factor)
# define TERM(i)	(Fields[i].gpf_term)
# define INIT(i)	{ SRC(i)[0] = '\0'; \
			  DST(i)[0] = '\0'; \
			  DESC(i)[0] = '\0'; \
			  UNITS(i)[0] = '\0'; \
			  FLDOFFSET(i) = BAD_OFFSET; \
			  VFIELD(i) = BAD_VFIELD; \
			  FACTOR(i) = BAD_FACTOR; \
			  TERM(i) = BAD_TERM; \
			}

/*
 * 12/14/92 gjg -- As near as I can tell, the TERM and FACTOR fields in
 * the GENPRO header are used as follows to get the actual floating
 * point value of a datum:
 *
 * value = ( <unsigned int file datum> / FACTOR ) - TERM
 *
 * The FACTOR and TERM for a field are retrieved in ProcScale()
 * On a historical note, FACTOR used to be Scale, and TERM was always
 * assumed to be 1000.0 (in fact, so was Scale)
 */

int DTime;		/* The time (unlimited) dimension	*/
int VTime;		/* The time offset variable		*/
int VBTime;		/* The base time variable		*/
int NFile;		/* id for the netCDF file		*/
long NOut = 0;		/* Output record index			*/

/*
 * Local Genpro stuff.
 */
int AltOffset;		/* Altitude offset			*/
int LatOffset, LonOffset;	/* Ugly.  Please don't ask.	*/
int NTrashed = 0;
int Gp_fd;		/* The genpro file FD			*/
int Gp_lrlen;		/* Logical Record length;		*/
int Gp_prlen;		/* Physical Record length;		*/
int Gp_lperp;		/* Logical per physical record		*/
UItime Gp_Begin;	/* Begin time				*/
long Gp_Base;		/* Begin time as unix value		*/

int Time_off = 0;	/* The time offset to use		*/
int Time_tweak = 0;	/* Seconds to tweak time		*/
typedef enum { tf_NCAR, tf_UW } TimeFmt;
TimeFmt TFmt = tf_NCAR;

/*
 * The data record buffer.
 */
char D_buf[30000];
int Logical = 99999;	/* Current logical record.		*/

int Nrec = 0;		/* Number of comment records		*/
# define HDR_LEN 80

/*
 * Filetype stuff.
 */
int FileType = 0;
# define WYOMING	1
# define NCAR		2

/*
 * Quick white-space skipping.
 */
# define SKIP_WHITE(cp) while (*cp && (*cp == ' ' || *cp == '\t')) cp++;


/*
 * Prototypes
 */
void ProcScale (/* char *line */);
void ValidateFields ();



main (argc, argv)
int argc; 
char **argv;
{
/*
 * Basic sanity checking.
 */
	if (argc < 5)
	{
		printf ("Usage: %s -filetype infile outfile fields...\n", 
			argv[0]);
		exit (1);
	}
/*
 * Get the type of the input file.
 */
	get_file_type (argv + 1);
/*
 * Make our local field list.
 */
	make_fields (argv + 4, (argc - 4)/2);
/*
 * Figure out time offsets.
 */
	if (getenv ("TIME_OFFSET"))
		Time_off = atoi (getenv ("TIME_OFFSET"));
	else
		printf ("WARNING: Using default time offset\n");
	printf ("Using time offset of %d hours\n", Time_off);
	if (getenv ("TIME_TWEAK"))
		Time_tweak = atoi (getenv ("TIME_TWEAK"));
	if (Time_tweak)
		printf ("Tweaking times by %d seconds\n", Time_tweak);
/*
 * Open the input file.
 */
	GpOpen (argv[2]);
/*
 * Make sure we got all the info we need, e.g. scale factors and terms
 */
	ValidateFields();
/* 
 * Create the output file.
 */
	MakeNFile (argv[3]);
/*
 * Plow through the data.
 */
	Nrec = 0;
	Plow ();
	ncclose (NFile);
	printf ("Wrote %i samples.\n", NOut);
/*
 * Done.
 */
	if (NTrashed > 0)
		printf ("Warning: %d points dropped due to zero lat/lon\n",
			NTrashed);
	exit (0);
}




get_file_type (filetype)
char	**filetype;
/*
 * Get the type of the input file.
 */
{
	if (strncmp (filetype[0], "-wyoming", 
		     MAX(2,strlen(filetype[0]))) == 0)
		FileType = WYOMING;
	else if (strncmp (filetype[0], "-ncar", 
			  MAX(2,strlen(filetype[0]))) == 0)
		FileType = NCAR;
	else
	{
		printf ("Invalid file type: '%s'\n", filetype[0]);
		exit (0);
	}
	printf ("FileType: %s.\n", (FileType == NCAR) ? "ncar" : "wyoming" );
}




make_fields (fields, count)
char **fields;
int count;
/*
 * Put together our initial field list.
 */
{
	int i;
/*
 * Go through and do all the fields.
 */
	for (i = 0; i < count; i++)
	{
		INIT(i);
		STRNCPY (Fields[i].gpf_src, fields[0], NAMELEN);
		zapcase (Fields[i].gpf_src);
		STRNCPY (Fields[i].gpf_dst, fields[1], NAMELEN);
		fields += 2;
		NField++;
	}
/*
 * Add the positioning fields.
 */
	if (FileType == NCAR)
	{
		INIT(NField);
		LatOffset = NField;
		STRNCPY (Fields[NField].gpf_src, "alat", NAMELEN);
		STRNCPY (Fields[NField].gpf_dst, "lat", NAMELEN);
		++NField;

		INIT(NField);
		LonOffset = NField;
		STRNCPY (Fields[NField].gpf_src, "alon", NAMELEN);
		STRNCPY (Fields[NField].gpf_dst, "lon", NAMELEN);
		++NField;

		INIT(NField);
		AltOffset = NField;
		STRNCPY (Fields[NField].gpf_src, "palt", NAMELEN);
		STRNCPY (Fields[NField].gpf_dst, "alt", NAMELEN);
		++NField;
	}
	else if (FileType == WYOMING)
	{
		INIT(NField);
		LatOffset = NField;
		STRNCPY (Fields[NField].gpf_src, "latg", NAMELEN);
		STRNCPY (Fields[NField].gpf_dst, "lat", NAMELEN);
		++NField;

		INIT(NField);
		LonOffset = NField;
		STRNCPY (Fields[NField].gpf_src, "long", NAMELEN);
		STRNCPY (Fields[NField].gpf_dst, "lon", NAMELEN);
		++NField;

		INIT(NField);
		AltOffset = NField;
		STRNCPY (Fields[NField].gpf_src, "z", NAMELEN);
		STRNCPY (Fields[NField].gpf_dst, "alt", NAMELEN);
		++NField;
	}
}




GpOpen (file)
char *file;
/*
 * Open up this genpro file.
 */
{
	char hbuf[HDR_LEN], *cp;
/*
 * Actually open it.
 */
	if ((Gp_fd = open (file, 0)) < 0)
	{
		perror (file);
		exit (1);
	}
/*
 * Get and check the first header line.
 */
	get_rec (hbuf, HDR_LEN);
	if (strncmp (hbuf, " BEGINHD", 8))
	{
		printf ("Bad header line: '%s'\n", hbuf);
		exit (1);
	}
/*
 * Get the dates.
 */
	get_dates ();
/*
 * Find the logical record size.
 */
 	for (;;)
	{
		if (! get_rec (hbuf, HDR_LEN))
			GiveUp ("No 'LOGBIT' line in the file!");
	/*
	 * 5/92 jc	While we're at it, let's look for a comment
	 *		field and see if this is a UW tape; their
	 *		time format is different, of course....:-(
	 */
	 	if (! strncmp (hbuf, "/COMMENT", 8) &&
			! strncmp (hbuf + 12, "UW KING AIR", 11))
		{
			TFmt = tf_UW;
			printf ("This is a UW Funky tape\n");
		}
		if (! strncmp (hbuf, " LOGBIT", 7))
			break;
	}
	for (cp = hbuf + 8; ! isdigit (*cp); cp++)
		;
	sscanf (cp, "%d", &Gp_lrlen);
	Gp_lrlen /= 8;
/*
 * Log per phys.
 */
	if (! get_rec (hbuf, HDR_LEN))
		GiveUp ("EOF looking for DATLOG");
	for (cp = hbuf + 8; ! isdigit (*cp); cp++)
		;
	sscanf (cp, "%d", &Gp_lperp);
/*
 * Phys rec len.
 */
	if (! get_rec (hbuf, HDR_LEN))
		GiveUp ("EOF looking for DATSIZ");
	for (cp = hbuf + 8; ! isdigit (*cp); cp++)
		;
	sscanf (cp, "%d", &Gp_prlen);
	Gp_prlen /= 8;
	printf ("Records: logical %d, phys %d, %d log/phys\n", Gp_lrlen,
		Gp_prlen, Gp_lperp);
/*
 * Now scan forward to the variable definitions.
 */
 	for (;;)
	{
		if (! get_rec (hbuf, HDR_LEN))
			GiveUp ("No 'ORDVAR=TITLE' line in the file!");
		if (! strncmp (hbuf, " ORDVAR = TITLE", 15))
			break;
	}
/*
 * Process the variable lines.
 */
	for (;;)
	{
		if (! get_rec (hbuf, HDR_LEN))
			GiveUp ("EOF encountered in the header");
		if (strncmp (hbuf, " LETVAR", 7))
			break;
		ProcTitle (hbuf);
	}
/*
 * Do the units lines.
 */
	if (strncmp (hbuf, " ORDVAR = UNITS", 15))
		GiveUp ("No ORDVAR = UNITS, instead got %s", hbuf);
	for (;;)
	{
		if (! get_rec (hbuf, HDR_LEN))
			GiveUp ("EOF encountered reading units and offsets");
		if (strncmp (hbuf, " LETVAR", 7))
			break;
		ProcUnit (hbuf);
	}
	/*
	 * alt will actually be stored in km rather than m
	 */
	STRNCPY (Fields[AltOffset].gpf_units, "km", UNITLEN);
/*
 * Retrieve factors and terms for each field
 */
	if (strncmp (hbuf, " ORDVAR = CONKEY", 16))
		GiveUp ("No ORDVAR = CONKEY for reading scale factors: \n%s",
			hbuf);
	for (;;)
	{
		if (! get_rec (hbuf, HDR_LEN))
			GiveUp ("EOF encountered searching for scale factors");
		if (! strncmp (hbuf, " ENDHD", 6))
			break;
		ProcScale (hbuf);
	}
/*
 * NCAR files need this done, the "new" Wyoming files don't.
 */
	if (TFmt == tf_NCAR)
		while (Nrec % 10)
			get_rec (hbuf, HDR_LEN);
}



void
ValidateFields()
/*
 * Check that we have vital info for all of our Fields[]
 */
{
	int i;

	for (i = 0; i < NField; ++i)
	{
		if (TERM(i) == BAD_TERM)
			printf("TERM value missing for %s, offset %i\n",
			       SRC(i), FLDOFFSET(i));
		else if (FACTOR(i) == BAD_FACTOR)
			printf("FACTOR missing for field %s, offset %i\n",
			       SRC(i), FLDOFFSET(i));
		else if (FLDOFFSET(i) == BAD_OFFSET)
			printf("No offset found for %s, offset = %i\n",
			       SRC(i), FLDOFFSET(i));
		else
			continue;
		GiveUp("Giving up!");
	}
}




get_dates ()
/*
 * Pull the begin/end dates out of the file.
 */
{
	char hbuf[80], *strchr (), *begq, *endq;
	int d, mon, year;
	static char *months[] = { "junk", "JAN", "FEB", "MAR", "APR", "MAY",
		"JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC" };
/*
 * Find the PRDATE line.
 */
	for (;;)
	{
		if (! get_rec (hbuf, HDR_LEN))
			GiveUp ("(PRDATE) EOF encountered in the header");
		if (! strncmp (hbuf, "/PRDATE", 7))
			break;
	}
/*
 * Interpret it.
 */
	begq = strchr (hbuf, '\"');
	endq = strchr (begq + 1, '\"');
	*endq = '\0';
	sscanf (begq + 1, "%d", &d);	/* Got day */
	begq = strchr (endq + 1, '\"');
	endq = strchr (begq + 1, '\"');
	*endq = '\0';
	for (mon = 1; mon <= 12; mon++)
		if (! strcmp (begq + 1, months[mon]))
			break;
	d += mon*100;
	begq = strchr (endq + 1, '\"');
	endq = strchr (begq + 1, '\"');
	*endq = '\0';
	sscanf (begq + 1, "%d", &year);
	d += year*10000;
/*
 * Now we remember the date portion.
 */
	Gp_Begin.ds_yymmdd = d;
	printf ("Flight date %d\n", d);
/*
 * Get the next line, and hope to hell it's the PRTIME.  Skip it, in favor
 * of the BEGSNP line that follows.
 */
	get_rec (hbuf, HDR_LEN);
	if (! get_rec (hbuf, HDR_LEN))
		GiveUp ("(BEGSNP) EOF encountered in the header");
/*
 * Pick it apart.
 */
	Gp_Begin.ds_hhmmss = interpSNP (hbuf);
	pmu_dadd (&Gp_Begin.ds_yymmdd, &Gp_Begin.ds_hhmmss, 
		Time_off*10000);
/*
 * Same for the ending.
 */
	if (! get_rec (hbuf, HDR_LEN))
		GiveUp ("(BEGSNP) EOF encountered in the header");
# ifdef notdef
	Hdr.gph_end.ds_hhmmss = interpSNP (hbuf);
	pmu_dadd (&Hdr.gph_end.ds_yymmdd, &Hdr.gph_end.ds_hhmmss, 
		Time_off*10000);
# endif
}




interpSNP (line)
char *line;
/*
 * Interpret this SNP line.
 */
{
	char *strchr (), *cp;
	int hour, min, sec;

	cp = strchr (line, '(') + 1;
	while (! isdigit (*cp))
		cp++;
	sscanf (cp, "%d", &hour);
	
	cp = strchr (cp, ',') + 1;
	while (! isdigit (*cp))
		cp++;
	sscanf (cp, "%d", &min);

	cp = strchr (cp, ',') + 1;
	while (! isdigit (*cp))
		cp++;
	sscanf (cp, "%d", &sec);
	return (hour*10000 + min*100 + sec);
}





ProcTitle (line)
char *line;
/*
 * Process this title LETVAR line.
 */
{
	char *begq, *endq, *field, *fend, *strchr ();
	int i;
/*
 * Isolate the quotes.
 */
	if (! (begq = strchr (line, '"')))
		GiveUp ("Funky LETVAR: '%s'", line);
	begq++;
	if (! (endq = strchr (begq, '"')))
		GiveUp ("Funky LETVAR: '%s'", line);
	*endq = 0;
/*
 * Remove the (substantial) trailing blanks from this field.
 */
	field = endq + 2;
 	while (endq[-1] == ' ')
		*--endq = '\0';
/*
 * Now try to find the field name.
 */
 	if (! (field = strchr (field, ',')))
		GiveUp ("Funky LETVAR: '%s'", field);
	field++;
	SKIP_WHITE (field);
	for (fend = field; *fend && Isalnum(*fend); fend++)
		*fend = Tolower (*fend);
	*fend = '\0';
/*
 * Now see if we want this field.
 */
	for (i = 0; i < NField; i++)
		if (! strcmp (Fields[i].gpf_src, field))
			break;
	if (i >= NField)
		return;
	STRNCPY (Fields[i].gpf_desc, begq, DESCLEN);
	printf ("Found field %s: %s\n", SRC(i), DESC(i));
}




ProcUnit (line)
char *line;
/*
 * Deal with this UNIT line.
 */
{
	char *strchr (), *unit, *uend, *offset, *field, *fend;
	int i;
/*
 * Find the beginning of the units info.
 */
 	if (! (unit = strchr (line, '"')))
	{
		char cline[300];
		sprintf (cline, "Funky UNIT line: '%s'", line);
		GiveUp (cline);
	}
	unit++;
	SKIP_WHITE (unit);
	if (*unit == '"')	/* Empty units */
		unit -= 2;
/*
 * Now find the end.
 */
	for (uend = unit + 1; *uend && *uend != ' ' && *uend != '"'; uend++)
		;
	*uend = '\0';
/*
 * Skip fields up to the offset field.
 */
 	offset = uend;
	for (i = 0; i < 4; i++)
	 	if (! (offset = strchr (offset + 1, ',')))
			GiveUp ("Funky UNIT line: '%s'", line);
	offset++;
	SKIP_WHITE (offset);
/*
 * Now find the field name.
 */
	field = offset;
	for (i = 0; i < 3; i++)
	 	if (! (field = strchr (field + 1, ',')))
			GiveUp ("Funky UNIT line: '%s'", line);
	field++;
	SKIP_WHITE (field);
	for (fend = field; *fend && Isalnum (*fend); fend++)
		*fend = Tolower (*fend);
	*fend = '\0';
/*
 * Now see if we want this field.
 */
	for (i = 0; i < NField; i++)
		if (! strcmp (SRC(i), field))
			break;
	if (i >= NField)
		return;
/*
 * Grab the stuff.
 */
	STRNCPY (Fields[i].gpf_units, unit, UNITLEN);
	sscanf (offset, "%d", &(FLDOFFSET(i)));
	FLDOFFSET(i) /= 32;
	printf ("Field %s, units '%s', offset %d\n", SRC(i),
		UNITS(i), FLDOFFSET(i));
}




void
ProcScale (line)
char *line;
/*
 * Deal with this FACTOR and TERM line.
 * We expect lines of the form:
 * <conkey>, <sclkey>, <term>, <factor>, FOR, <fieldname>
 */
{
	char *strchr ();
	char *fend, *field;
	int conkey, sclkey;
	float term, factor;
	int i;

	if (sscanf (line, " LETVAR = %i , %i , %f , %f",
		    &conkey, &sclkey, &term, &factor) != 4)
		GiveUp("Could not parse TERM and FACTOR from line: '%s'", line);
/*
 * Now find the field name.
 */
	field = line;
	for (i = 0; i < 5; i++)
	 	if (! (field = strchr (field + 1, ',')))
			GiveUp ("Funky scale line: '%s'", line);
	field++;
	SKIP_WHITE (field);
	for (fend = field; *fend && Isalnum (*fend); fend++)
		*fend = Tolower (*fend);
	*fend = '\0';
/*
 * Now see if we want this field.
 */
	for (i = 0; i < NField; i++)
		if (! strcmp (SRC(i), field))
			break;
	if (i >= NField)
		return;
/*
 * Grab the stuff.
 */
	Fields[i].gpf_factor = factor;
	Fields[i].gpf_term = term;
	printf ("Field %s, factor = %6.2f, term = %6.1f\n",
		SRC(i), FACTOR(i), TERM(i));
}




get_rec (buf, len)
char *buf;
int len;
/*
 * Read a record.
 */
{
	Nrec++;
	return (read (Gp_fd, buf, len) == len);
}



char *
DataRec ()
/*
 * Get the next data record.
 */
{
	if (++Logical >= Gp_lperp)
	{
		if (! get_rec (D_buf, Gp_prlen))
			return (0);
		Logical = 0;
	}
	return (D_buf + (Logical * Gp_lrlen));
}


GiveUp (line, arg)
char *line, *arg;
/*
 * Print this line and quit.
 */
{
	printf (line, arg);
	printf ("\n");
	exit (1);
}





Plow ()
/*
 * Push through the data.
 */
{
	int *ip, i;
	UItime tv;
	float toff, data;
	static int nplow = 0;
	ZebTime	zt, last_zt;

	last_zt.zt_Sec = last_zt.zt_MicroSec = 0;
/*
 * Now work through the file.
 */
	tv = Gp_Begin;
	for (;;)
	{
	/*
	 * Get a record.
	 */
	 	if (! (ip = (int *) DataRec ()))
			break;
	/*
	 * Major source of ugliness.
	 */
# ifdef notdef
		if (ip[FLDOFFSET(LatOffset)]/FACTOR(LatOffset) 
		      == TERM(LatOffset) 			||
		    ip[FLDOFFSET(LonOffset)]/FACTOR(LonOffset) 
		      == TERM(LonOffset))
		{
			NTrashed++;
			continue;
		}
# endif
	/*
	 * Pull out the time.
	 */
		if (TFmt == tf_NCAR)
		{
			tv.ds_hhmmss = ((ip[0]/1000 - 1000) + Time_off)*10000 
			      + (ip[1]/1000 - 1000)*100 + (ip[2]/1000) - 1000;
			TC_UIToZt (&tv, &zt);
			if (Time_tweak)
			{
				zt.zt_Sec += Time_tweak;
				TC_ZtToUI (&zt, &tv);
			}
			if (zt.zt_Sec < last_zt.zt_Sec)
				continue;
			last_zt.zt_Sec = zt.zt_Sec;
		}
		else
		{
			tv.ds_yymmdd = ip[0];
			tv.ds_hhmmss = ip[1];
			TC_SysToFcc (TC_FccToSys (&tv) + Time_off*3600 
					+ Time_tweak, &tv);
		}
		toff = (float) (TC_FccToSys (&tv) - Gp_Base);
		ncvarput1 (NFile, VTime, &NOut, &toff);
	/*
	 * Package up the data.
	 */
	 	for (i = 0; i < NField; i++)
		{
			data = ip[FLDOFFSET(i)]/FACTOR(i) - TERM(i);
			if (i == AltOffset)
				data /= 1000.0;		/* m -> km */
			ncvarput1 (NFile, VFIELD(i), &NOut, &data);
		}
		NOut++;
# ifdef DEBUG
	/*
	 * Debuggery.
	 */
	 	if (++nplow > 500)
		{
			printf ("DEBUG EXIT -- SHOULD NOT BE HERE\n");
			return;
		}
# endif
	}
}





MakeNFile (name)
char *name;
/*
 * Make a netcdf file by this name.
 */
{
	int i;
	float bv = BADVAL;
	float llbv = LL_BADVAL;		/* lat/lon bad value */
/*
 * Create the actual file.
 */
	NFile = nccreate (name, NC_CLOBBER);
/*
 * Dimensions.  For now, just time -- everything else is scalar.
 */
	DTime = ncdimdef (NFile, "time", NC_UNLIMITED);
/*
 * Variables.  Start with times.
 */
	VBTime = ncvardef (NFile, "base_time", NC_LONG, 0, 0);
	VTime = ncvardef (NFile, "time_offset", NC_FLOAT, 1, &DTime);
/*
 * Now each data field.
 */
	for (i = 0; i < NField; i++)
	{
		VFIELD(i) = ncvardef (NFile, DST(i), NC_FLOAT, 1, &DTime);
		if (i == LatOffset || i == LonOffset)
			(void) ncattput (NFile, VFIELD(i), "missing_value",
					 NC_FLOAT, 1, &llbv);
		else
			(void) ncattput (NFile, VFIELD(i), "missing_value",
					 NC_FLOAT, 1, &bv);
		(void) ncattput (NFile, VFIELD(i), "units",
				 NC_CHAR, strlen(UNITS(i)) + 1, UNITS(i));
		(void) ncattput (NFile, VFIELD(i), "long_name",
				 NC_CHAR, strlen(DESC(i)) + 1, DESC(i));
	}
/*
 * That's it.  Finish defining, and put the base time in.
 */
	ncendef (NFile);
	Gp_Base = TC_FccToSys (&Gp_Begin);
	ncvarput1 (NFile, VBTime, 0, &Gp_Base);
	ncsync (NFile);
}
