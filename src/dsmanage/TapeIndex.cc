//
// Generate an index of a tar tape.
//
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

# ifdef hpux
# include <sys/sigevent.h>
# endif
# ifdef sgi
# define _BSD_TYPES
# include <sys/types.h>
# undef _BSD_TYPES
# endif

//# include <sys/select.h>
# include <stdio.h>
# include <stream.h>
# include <fcntl.h>
# include <errno.h>
# include <unistd.h>
# include <string.h>
# include <signal.h>
// # include <stdlib.h>

extern "C"
{
// #	include <tar.h>
#	include <defs.h>
#	include <ds_fields.h>
	void exit (int);
	extern int dfa_FindFormat (const char *);	// XXX
	extern int dfa_QueryDate (int, const char *, ZebTime *, ZebTime *, 
		int *);
}

# include "dsmanage.h"
# include "STable.h"
# include "Index.h"
# include "Tape.h"


RCSID ("$Id: TapeIndex.cc,v 1.16 2001-08-24 22:23:12 granger Exp $")


//
// The tape buffer.
//
const int TarBlockSize = 512;
const int BufSize = 128*TarBlockSize;
const int TarNameSize = 100;
const int SaveInterval = 50;


char TapeBuffer[BufSize];
char *TapeBlock = TapeBuffer;		// The current block
int DataLen = -1;			// How much data actually read

struct TarHeader
{
	char	th_name[TarNameSize];
	char	th_mode[8];
	char 	th_uid[8];
	char	th_gid[8];
	char	th_size[12];
	char	th_mtime[12];
	char	th_chksum[8];
	char	linkflag;
	char	linkname[TarNameSize];
};



//
// Local stuff.
//
char *GetTarBlock (Tape &, Tape *);
void ProcessTarFile (Tape &, Tape *, const TarHeader *, PlatformIndex &);
int DecodeNum (const char *);
int MakeTempFile ();
void ZapTempFile (int);
void ExtractPlat (const char *, char *);
void Interrupt (int);
volatile void Usage ();
void LoadPrefixFile (const char *);

//
// Kinda ugly....prefixes for layered platforms.
//
const int MaxPrefix = 20;
char *Prefixes[MaxPrefix];
int NPrefix = 0;




char *Scratch = ".";
char TmpFile[80];
//
// Kludge for interrupt handling.
//
char *IName;
PlatformIndex *PIndex;


int
main (int argc, char **argv)
{
	int flag;
	Tape *outtape = 0;
//
// Extern declarations below are only useful on SunOS 4.x systems, where
// these symbols are not in unistd.h
//
	extern char *optarg;
	extern int optind;
//
// Start by making sure we have our arguments
//
	sprintf (TmpFile, "%s/TapeIndex%x.tmp", Scratch, getpid ());
	while ((flag = getopt (argc, argv, "s:c:p:")) != -1)
	{
		switch (flag)
		{
		    case 'c':
		   	outtape = new Tape (optarg, TRUE);
			break;
		    case 's':
			sprintf (TmpFile, "%s/%s", optarg,
				"TapeIndex.tmp");
			break;
		    case 'p':
		    	LoadPrefixFile (optarg);
			break;
		    case '?':
			Usage ();
			break;
		}
	}
	if ((argc - optind) != 2)
		Usage ();
//
// Get our drive.
//
	Tape tape (argv[optind]);
	if (! tape.OK ())
		exit (1);
//
// We need a platform index to store the info.
//
	PlatformIndex index;
	PIndex = &index;
	IName = argv[optind + 1];
//
// Initialize field stuff
//
	F_Init ();
//
// Now it's time to pass through the data and get our stuff.
//
	TarHeader *tp;
	int ndone = 0;
	signal (SIGINT, Interrupt);
	while (tp = (TarHeader *) GetTarBlock (tape, outtape))
	{
		if ((++ndone % SaveInterval) == 0)
			index.save (argv[optind + 1]);
		ProcessTarFile (tape, outtape, tp, index);
	}
	index.save (argv[optind + 1]);
}



volatile void
Usage ()
//
// Complain.
//
{
	cerr << "Usage: tapeindex [-s scratch-dir] [-c copy-dev] " 
	     << "[-p prefix-file] "
	     << "input-tape index-file\n.";
	exit (1);
}



void
Interrupt (int)
//
// Deal with a ^c.
//
{
	cout << "Interrupt....\n";
	PIndex->save (IName);
	exit (1);
}








char *
GetTarBlock (Tape &t, Tape *outtape)
//
// Return back the next block.
//
{
//
// Move to the next block, and see if there is still enough data
// already read.
//
	TapeBlock += TarBlockSize;
	if ((TapeBlock - TapeBuffer) >= DataLen)
	{
	//
	// Pull in a new block from the tape.
	//
		if ((DataLen = t.getblock (TapeBuffer, BufSize)) <= 0)
		{
			if (outtape && (DataLen == TS_EOF ||DataLen == TS_EOT))
				outtape->WriteEof ();
			return (DataLen == TS_EOF ?
					GetTarBlock (t, outtape) : 0);
		}
	//
	// If we are outputting, write this block out the other side.
	//
		if (outtape)
			outtape->putblock (TapeBuffer, DataLen);
		TapeBlock = TapeBuffer;
	}
	return (TapeBlock);
}






void
ProcessTarFile (Tape &tape, Tape *outtape, const TarHeader *ttp,
		PlatformIndex &index)
//
// Deal with a file on the tar tape.
//
{
	TarHeader tp = *ttp;
//
// Check for the EOF block.
//
	int i;
	for (i = 0; i < 10; i++)
		if (tp.th_name[i])
			break;
	if (i >= 10)
	{
		cout << "Tar eof\n";
		DataLen = 0;		// Flush rest.
		return;
	}
//
// This is a legit file -- deal with it.  Zero-length files are not of
// interest however, even if they are not directories (the usual case).
//
	int size = DecodeNum (tp.th_size), nmoved;
	if (size == 0)
	{
		cout << "Ignore file " << tp.th_name << ".\n";
		return;
	}
	char plat[30];
	ExtractPlat (tp.th_name, plat);
//
// OK, pull in the file.
//
	cout << "File '" << tp.th_name << "', size " << size << 
			" plat " << plat << ".\n";
	int fd = MakeTempFile ();
	for (nmoved = 0; nmoved < size; nmoved += TarBlockSize)
	{
		char *data = GetTarBlock (tape, outtape);
		if (! data) // oops
		{
			ZapTempFile (fd);
			return;
		}
		write (fd, data, (nmoved + TarBlockSize) > size ?
				size - nmoved : TarBlockSize);
	}
//
// Get the dates out of the file.
//
	ZebTime begin, end;
	int junk, format = dfa_FindFormat (tp.th_name);
	if (format < 0 || ! dfa_QueryDate (format, TmpFile,&begin,&end,&junk))
		cerr << "QueryDate failure\n";
	else
	{
//
// Add the entry and we are done.
//
		IndexFile *ifl = new IndexFile (plat, tp.th_name, size,
				tape.filenum (), &begin, &end);
		index.add (plat, *ifl);
	}
	ZapTempFile (fd);
}




int
DecodeNum (const char *num)
//
// Decode one of these funky tar octal numbers.
//
{
	int ret = 0;

//
// Manual sez zero filled, but I see blanks...
//
	while (*num == ' ')
		num++;
	while (*num >= '0' && *num <= '7')
	{
		ret = (ret << 3) + (*num - '0');
		num++;
	}
	return (ret);
}





int
MakeTempFile ()
{
	int fd;

	if ((fd = open (TmpFile, O_RDWR|O_CREAT|O_TRUNC, 0666)) < 0)
		cerr << "Unable to create temp file\n";
	return (fd);
}




void
ZapTempFile (int fd)
{
	close (fd);
	unlink (TmpFile);
}





void
ExtractPlat (const char *file, char *plat)
//
// Pull out the platform name.
//
{
	const char *begin, *end;
//
// This is all made much more complicated by the bizarre naming
// scheme that was chosen for the RAPS project.
//
	strcpy (plat, "BoGuS");
//
// The final slash indicates the beginning of the file name itself.
//
	if ((begin = strrchr (file, '/')) == 0)
		return;
	begin++;
//
// Now we need to count back three periods.
//
	int nperiod = 0;
	for (end = begin + strlen (begin); end > begin && nperiod < 3; end--)
		if (*end == '.')
			nperiod++;
	strncpy (plat, begin, end - begin + 1);
	plat[end - begin + 1] = '\0';
//
// Now let's see if there is a prefix to apply to this platform name.
//
	int len;
	for (int i = 0; i < NPrefix; i++)
		if (! strncmp (Prefixes[i], plat, len = strlen (Prefixes[i])))
		{
			for (int j = strlen (plat) + 1; j > len; j--)
				plat[j] = plat[j - 1];
			plat[len] = '/';
		}
}





void
LoadPrefixFile (const char *fname)
//
// Pull in a prefix file.
//
{
//
// Open up the file.
//
	FILE *file = fopen (fname, "r");
	if (! file)
	{
		cerr << "Unable to open prefix file " << fname << "\n";
		exit (1);
	}
//
// Now read in prefixes.
//
	char line[80];
	while (fgets (line, 80, file))
	{
		line[strlen (line) - 1] = '\0';
		Prefixes[NPrefix] = new char[strlen (line) + 1];
		strcpy (Prefixes[NPrefix], line);
		NPrefix++;
	}
}
