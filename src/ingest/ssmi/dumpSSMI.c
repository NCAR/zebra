/*
 * Dump SSM/I Antenna Temperature tapes from Remote Sensing Systems
 */
/*
 *		Copyright (C) 1993 UCAR
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

# ifndef lint
	static char *rcsid = "$Id: dumpSSMI.c,v 1.1 1993-05-18 06:51:05 granger Exp $";
# endif

# include <time.h>
# include <errno.h>
# include <stdio.h>
# include <fcntl.h>

# include "ssmi_record.h"

/*
 * A macro to make function prototypes a little easier across both STDC and
 * non-STDC implementations.
 */
# ifdef __STDC__
#	define FP(stuff) stuff
# else
#	define FP(stuff) ()
# endif

# define TRUE	-1
# define FALSE	0
# define EPOCH_1987	536457600	/* 00:00 Jan. 1, 1987 in UNIX epoch */


static int	Fd;		/* file descriptor for our tape drive */
static SSMI_Rec	Lrec[16];	/* array of logical records (one phys. rec.) */
static int	Nlog;

static int	nextrec FP ((void));
static void	dump_lrec FP ((SSMI_Rec *));
static int	extract_12bit FP ((char *, int));




main (argc, argv)
int	argc;
char	**argv;
{
	char	tapedev[20];
	int	i;

	if (argc > 2)
	{
		
		printf ("Usage: %s [<tape_device>]\n", argv[0]);
		exit (1);
	}
	
	if (argc == 1)
	{
		printf ("Tape device: ");
		gets (tapedev);
	}
	else
		strcpy (tapedev, argv[1]);

	if ((Fd = open (tapedev, O_RDONLY)) < 0)
	{
		printf ("Error %d opening device '%s'\n", errno, tapedev);
		exit (1);
	}
/*
 * Dump the header
 */
	printf ("\n\nTAPE HEADER\n-----------\n\n");

	nextrec ();
	for (i = 0; i < Nlog; i++)
		printf ("%.79s\n", (char *)(Lrec + i));

	nextrec ();	/* Get rid of the EOF after the header */
/*
 * Data records
 */
	printf ("\n\nDATA RECORDS\n------------\n\n");
	while (nextrec ())
	{
		for (i = 0; i < Nlog; i++)
			dump_lrec (Lrec + i);
	}
}



static int
nextrec ()
/*
 * Read the next record from the tape into Lrec and put the number of
 * logical records into Nlog.  Return true for a good read, false for an
 * error or EOF.
 */
{
	int size = read (Fd, Lrec, sizeof (Lrec));
/*
 * Check for error or EOF
 */
	if (size <= 0)
	{
		if (size < 0)
			printf ("Error %d reading tape\n", errno);
		return (FALSE);
	}
	
	Nlog = size / sizeof (SSMI_Rec);
	return (TRUE);
}



static void
dump_lrec (rec)
SSMI_Rec *rec;
/*
 * Dump a logical record
 */
{
	int	i, t, clock;
	char	*lo_types[5] = {"19V", "19H", "37V", "37H", "22V"};
	char	*hi_types[2] = {"85V", "85H"};
/*
 * Scan time
 */
	clock = EPOCH_1987 + rec->time_sec;
	printf ("Scan time: %s\n", asctime (gmtime (&clock)));
/*
 * Low frequency data
 */
	for (t = 0; t < 5; t++)
	{
		printf ("%s:\n", lo_types[t]);
		for (i = 0; i < 64; i++)
		{
			printf ("%5d", extract_12bit (rec->lo_data[i], t));
			if (((i+1) % 16) == 0)
				printf ("\n");
		}
	}
/*
 * High frequency A-scan data
 */
	for (t = 0; t < 2; t++)
	{
		printf ("A-scan %s:\n", hi_types[t]);
		for (i = 0; i < 64; i++)
		{
			printf ("%5d", extract_12bit (rec->hi_data[i], t));
			printf ("%5d", extract_12bit (rec->hi_data[i], t+4));

			if (((i+1) % 8) == 0)
				printf ("\n");
		}
	}
/*
 * High frequency B-scan data
 */
	for (t = 0; t < 2; t++)
	{
		printf ("B-scan %s:\n", hi_types[t]);
		for (i = 0; i < 64; i++)
		{
			printf ("%5d", extract_12bit (rec->hi_data[i], t));
			printf ("%5d", extract_12bit (rec->hi_data[i], t+4));

			if (((i+1) % 8) == 0)
				printf ("\n");
		}
	}
	printf ("\n");
}



static int
extract_12bit (buf, which)
char	*buf;
int	which;
/*
 * Extract the which'th 12 bits from buf, returning the resulting value
 * as an int
 */
{
	int	val;

	buf += (which * 12) / 8;
	val = (unsigned char) buf[0] << 8 | (unsigned char) buf[1];
	val = (val >> ((which % 2) ? 0 : 4)) & 0xfff;
	return (val);
}
