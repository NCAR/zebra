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
	void exit (int);
	extern int dfa_FindFormat (const char *);	// XXX
	extern int dfa_QueryDate (int, const char *, ZebTime *, ZebTime *, 
		int *);
}

# include "dsmanage.h"
# include "STable.h"
# include "Index.h"
# include "Tape.h"


static char *rcsid = "$Id: TapeIndex.cc,v 1.1 1992-09-10 22:26:51 corbet Exp $";


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




char *GetTarBlock (Tape &);
void ProcessTarFile (Tape &, const TarHeader *, PlatformIndex &);
int DecodeNum (const char *);
int MakeTempFile ();
void ZapTempFile (int);
void ExtractPlat (const char *, char *);
void Interrupt ();



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
//
// Start by making sure we have our arguments
//
	if (argc == 4)
		Scratch = argv[3];
	else if (argc != 2)
	{
		cerr << "Usage: " << argv[0] <<
			" tape-drive out-index [scratch]\n.";
		exit (1);
	}
	sprintf (TmpFile, "%s/%s", Scratch, "TapeIndex.tmp");
//
// Get our drive.
//
	Tape tape (argv[1]);
	if (! tape.OK ())
		exit (1);
//
// We need a platform index to store the info.
//
	usy_init ();
	PlatformIndex index;
	PIndex = &index;
	IName = argv[2];
//
// Now it's time to pass through the data and get our stuff.
//
	TarHeader *tp;
	int ndone = 0;
	signal (SIGINT, Interrupt);
	while (tp = (TarHeader *) GetTarBlock (tape))
	{
		if ((++ndone % SaveInterval) == 0)
			index.save (argv[2]);
		ProcessTarFile (tape, tp, index);
	}
	index.save (argv[2]);
}




void
Interrupt ()
//
// Deal with a ^c.
//
{
	cout << "Interrupt....\n";
	PIndex->save (IName);
	exit (1);
}








char *
GetTarBlock (Tape &t)
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
		if ((DataLen = t.getblock (TapeBuffer, BufSize)) < 0)
			return (0);
		TapeBlock = TapeBuffer;
	}
	return (TapeBlock);
}






void
ProcessTarFile (Tape &tape, const TarHeader *ttp, PlatformIndex &index)
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
		char *data = GetTarBlock (tape);
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
	char *begin, *end;
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
}
