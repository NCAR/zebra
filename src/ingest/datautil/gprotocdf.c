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
MAKE_RCSID ("$Id: gprotocdf.c,v 1.8 1992-11-03 17:57:52 kris Exp $")


/*
 * Netcdf stuff.
 */
# define MAXFLD 100
# define STRLEN	50
int VFields[MAXFLD];	/* The netcdf variables			*/
int DTime;		/* The time (unlimited) dimension	*/
int VTime;		/* The time offset variable		*/
int VBTime;		/* The base time variable		*/
int NFile;
int NOut = 0;		/* Output record index			*/
# define BADVAL	-32768.0	/* Bad data flag		*/

/*
 * Field names.
 */
char SrcFlds[MAXFLD][STRLEN];	/* Genpro field names		*/
char DstFlds[MAXFLD][STRLEN];	/* Equivalent netcdf names	*/
int NField = 0;

/*
 * Local Genpro stuff.
 */
float Scales[MAXFLD];	/* Scaling for each field		*/
int Foffsets[MAXFLD];	/* Offsets into the GP record		*/
int AltOffset;		/* Altitude offset			*/
int LatOffset, LonOffset;	/* Ugly.  Please don't ask.	*/
int NTrashed = 0;
int Gp_fd;		/* The genpro file FD			*/
int Gp_lrlen;		/* Logical Record length;		*/
int Gp_prlen;		/* Physical Record length;		*/
int Gp_lperp;		/* Logical per physical record		*/
time Gp_Begin;		/* Begin time				*/
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
 * Create the output file.
 */
	MakeNFile (argv[3]);
/*
 * Plow through the data.
 */
	Nrec = 0;
	Plow ();
	ncclose (NFile);
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
	if (strcmp (filetype[0], "-wyoming") == 0)
		FileType = WYOMING;
	else if (strcmp (filetype[0], "-ncar") == 0)
		FileType = NCAR;
	else
	{
		printf ("Invalid file type: '%s'\n", filetype[0]);
		exit (0);
	}
	printf ("FileType: %s.\n", filetype[0] + 1);
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
		strcpy (SrcFlds[i], fields[0]);
		zapcase (SrcFlds[i]);
		strcpy (DstFlds[i], fields[1]);
		Scales[i] = 1000.0;	/* xxx */
		fields += 2;
		NField++;
	}
/*
 * Add the positioning fields.
 */
	if (FileType == NCAR)
	{
		LatOffset = NField;
		strcpy (SrcFlds[NField], "alat");
		Scales[NField] = 1000.0;
		strcpy (DstFlds[NField++], "lat");
		LonOffset = NField;
		strcpy (SrcFlds[NField], "alon");
		Scales[NField] = 1000.0;
		strcpy (DstFlds[NField++], "lon");
		AltOffset = NField;
		strcpy (SrcFlds[NField], "palt");
		Scales[NField] = 1000.0;
		strcpy (DstFlds[NField++], "alt");
	}
	else if (FileType == WYOMING)
	{
		LatOffset = NField;
		strcpy (SrcFlds[NField], "latg");
		Scales[NField] = 10000.0;
		strcpy (DstFlds[NField++], "lat");
		LonOffset = NField;
		strcpy (SrcFlds[NField], "long");
		Scales[NField] = 10000.0;
		strcpy (DstFlds[NField++], "lon");
		AltOffset = NField;
		strcpy (SrcFlds[NField], "z");
		Scales[NField] = 1000.0;
		strcpy (DstFlds[NField++], "alt");
	}
}




sync ()
/*
 * Output the stuff to the file.
 */
{
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
		GiveUp ("No ORDVAR = UNITS");
	for (;;)
	{
		if (! get_rec (hbuf, HDR_LEN))
			GiveUp ("EOF encountered in the header");
		if (strncmp (hbuf, " LETVAR", 7))
			break;
		ProcUnit (hbuf);
	}
/*
 * Skip to the ENDHD.  Since all the scales still are, as far as I can
 * tell, 1000, I'm not going to bother with them here.
 */
	for (;;)
	{
		if (! strncmp (hbuf, " ENDHD", 6))
			break;
		get_rec (hbuf, HDR_LEN);
	}
/*
 * NCAR files need this done, the "new" Wyoming files don't.
 */
	if (TFmt == tf_NCAR)
		while (Nrec % 10)
			get_rec (hbuf, HDR_LEN);
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
	for (fend = field; *fend && isalnum (*fend); fend++)
		*fend = tolower (*fend);
	*fend = '\0';
/*
 * Now see if we want this field.
 */
	for (i = 0; i < NField; i++)
		if (! strcmp (SrcFlds[i], field))
			break;
	if (i >= NField)
		return;
	printf ("Found field %s: %s\n", field, begq);
# ifdef FIXME
	strcpy (Fields[i].gpf_desc, begq);
# endif
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
	for (fend = field; *fend && isalnum (*fend); fend++)
		*fend = tolower (*fend);
	*fend = '\0';
/*
 * Now see if we want this field.
 */
	for (i = 0; i < NField; i++)
		if (! strcmp (SrcFlds[i], field))
			break;
	if (i >= NField)
		return;
/*
 * Grab the stuff.
 */
# ifdef FIXME
	strcpy (Fields[i].gpf_unit, unit);
# endif
	sscanf (offset, "%d", Foffsets + i);
	Foffsets[i] /= 32;
	printf ("Field %s, unit '%s', offset %d\n", SrcFlds[i],
		"XXX", Foffsets[i]);
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
	time tv;
	float toff, data;
	static int nplow = 0;
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
		if (ip[Foffsets[LatOffset]]/Scales[LatOffset] == 1000 ||
		    ip[Foffsets[LonOffset]]/Scales[LonOffset] == 1000)
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
			if (Time_tweak)
			{
				ZebTime zt;
				TC_UIToZt (&tv, &zt);
				zt.zt_Sec += Time_tweak;
				TC_ZtToUI (&zt, &tv);
			}
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
			data = ip[Foffsets[i]]/Scales[i] - 1000.0;
			if (i == AltOffset)
				data /= 1000.0;	/* m -> km */
			ncvarput1 (NFile, VFields[i], &NOut, &data);
		}
		NOut++;
# ifdef notdef
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
		VFields[i] = ncvardef (NFile, DstFlds[i], NC_FLOAT, 1, &DTime);
		(void) ncattput (NFile, VFields[i], "missing_value",
			NC_FLOAT, 1, &bv);
		/* Units, desc, ...*/
	}
/*
 * That's it.  Finish defining, and put the base time in.
 */
	ncendef (NFile);
	Gp_Base = TC_FccToSys (&Gp_Begin);
	ncvarput1 (NFile, VBTime, 0, &Gp_Base);
	ncsync (NFile);
}
