//
// The code which actually loads data from the disk.
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
# include <stdio.h>
# include <iostream>
# include <unistd.h>
# include <fcntl.h>
# include <sys/stat.h>

extern "C"
{
#	include <X11/Intrinsic.h>
#	include <defs.h>
	extern void ds_ForceRescan (int, int);
}
# include "dsmanage.h"
# include "dsmWindows.h"
# include "STable.h"
# include "Index.h"
# include "StatusWin.h"
//# include "container.h"
# include "dsPlatform.h"
# include "Tape.h"
# include "plcontainer.h"
MAKE_RCSID ("$Id: DLoad.cc,v 1.17 2002-12-18 00:24:12 granger Exp $")

using std::cerr;
using std::cout;
using std::endl;

//
// Import from main.
//
//IContainer<dsPlatform> *PList;

extern plContainer *PList;



//
// Forwards.
//
static void LoadFromCD (PlatformIndex *, const char *, StatusWindow &);
static void LoadFromTape (PlatformIndex *, const char *, StatusWindow &, int);
static void FixFName (const char *, const char *, int, char *);
static int FindNStrip (const char *, const char *);
static void MoveCDFile (const char *, const char *, int, const char *);
static int FindFirstFile (const PlatformIndex *index);
static void InsertSlash (char *string, const int where);
static void FlushTapeBlock ();
static int VerifyDir (const char *dirname);




void
DLoad (PlatformIndex *index, int istape, const char * tdev, int files,
		int bytes)
//
// Load in some data.
//
{
	int nf = 0, nb = 0;
//
// Make us a status window.
//
	StatusWindow swin (istape ? tdev : "CDROM", files, bytes);
	swin.popup ();
	Disp->sync ();
//
// Now do the work.
//
	if (istape)
		LoadFromTape (index, tdev, swin, files);
	else
		LoadFromCD (index, tdev, swin);
//
// Tell the daemon to update.
//
	sleep (5);	// Give daemon some time
	MakePlatformList ();
}







static void
LoadFromCD (PlatformIndex *index, const char *basedir, StatusWindow &sw)
//
// Bring in data from a CD.
//
{
    int nstrip = -1, plat, nf = 0, nbyte = 0;
    IndexFile *file;
    int dirverified;
//
// Pass through the list of platforms.
//
    for (plat = 0; plat < PList->ncontained (); plat++)
    {
	const dsPlatform &p = PList->nth (plat);
	int nfp = 0;
    //
    // If this platform is not selected, we blow it off.
    //
	if (! index->isMarked (p.name ()))
	    continue;

	char destdir[CFG_FILEPATH_LEN];
	ds_GetPlatDir (SrcId, p.index, destdir);
	dirverified = 0;
    //
    // Pass through the file list.
    //
	for (file = index->files (p.name ()); file; file = file->next ())
	{
	//
	// Only if it's marked.
	//
	    if (! file->isMarked ())
		continue;
	//
	// We need to move a file, so let's verify that the destination 
	// dir exists
	//
	    if (! dirverified && ! VerifyDir (destdir))
		break;

	    dirverified = 1;
	//
	// Only if the same file has not alreay been moved.
	//
	    IndexFile *chain = file->same();
	    for ( ; chain && (chain != file); chain = chain->same())
	    {
		char origdir[CFG_FILEPATH_LEN];
		ds_GetPlatDir (SrcId, ds_LookupPlatform (chain->plat()), 
			       origdir);
	    //
	    // Check for this platform among those already done
	    //
		int done;
		for (done = 0; done < plat; ++done)
		    if (! strcmp ((PList->nth (done)).name(), chain->plat()))
			break;
		if (done < plat && chain->isMarked() && 
		    index->isMarked(chain->plat()) &&
		    ! strcmp (destdir, origdir))
		{
#ifdef DEBUG
		    cout << p.name() << ": file " <<
			file->name() << " already "
			"moved to directory " <<
			origdir << "\n";
#endif
		    break;
		}
	    }
	    if (chain && (chain != file))
	    {
		nfp++;	// we still want this plat scanned
		continue;
	    }

	    if (nstrip < 0)
		nstrip = FindNStrip (basedir, file->name ());
	//
	// status.
	//
	    if (sw.status (nf, nbyte))
		return;
	    Disp->sync ();
	//
	// OK, pull it over.
	//
	    MoveCDFile (file->name (), basedir, nstrip, destdir);
	    nf++;
	    nfp++;
	    nbyte += file->size ();
	    Main->UpdateSpace ();
	}
    //
    // If we moved anything, tell the daemon that things have changed.
    //
	if (nfp > 0)
	    ds_ForceRescan (p.index, FALSE);
    }
}





static int
FindNStrip (const char *basedir, const char *fname)
//
// Find the number of path components to strip from "fname" before we
// can prepend "basedir" and find the real file.
//
{
	int i = 0;
	char testname[200];
	const char *slash = fname;

	while (slash = strchr (slash + 1, '/'))
	{
		i++;
		sprintf (testname, "%s/%s", basedir, slash + 1);
		if (! access (testname, R_OK))
			return (i);
	}
	cerr << "Can't find NSTRIP!\n";
	return (-1);
}





static void
MoveCDFile (const char *fname, const char *base, int nstrip, const char *dest)
//
// Relocate a CD-based file to disk.
//
{
	char realname[200];
	const int BSIZE = 16*4096;
	static char cbuf[BSIZE];
	int ifd, ofd, nread;
//
// Attempt to open the input file.
//
	FixFName (fname, base, nstrip, realname);
//	cout << "File " << realname;
	if ((ifd = open (realname, O_RDONLY)) < 0)
	{
		cerr << "Unable to open " << realname << ".\n";
		return;
	}
//
// Now the output.
//
	const char *slash = strrchr (fname, '/') + 1;
	sprintf (realname, "%s/%s", dest, slash);
//	cout << "\n\t-> " << realname << ".\n";
//	cout.flush ();
	if ((ofd = open (realname, O_WRONLY | O_CREAT, 0666)) < 0)
	{
		cerr << "Unable to create " << realname << ".\n";
		close (ifd);
		return;
	}
//
// Copy time.
//
	while ((nread = read (ifd, cbuf, BSIZE)) > 0)
		write (ofd, cbuf, nread);
//
// Done.
//
	close (ifd);
	close (ofd);
}





static void
FixFName (const char *fname, const char *newbase, int nstrip, char *dest)
//
// Fix up this name with the new base.
//
{
	strcpy (dest, newbase);
	const char *slash = fname;
	while (nstrip-- > 0)
		slash = strchr (slash + 1, '/');
	strcat (dest, slash);
}




//----------------------------------------
// Ugly tar stuff below here.  I should really make a "tartape" class
// to do this right, but what is here works.
//
// The tape buffer.
//
const int TarBlockSize = 512;
const int BufSize = 128*TarBlockSize;
const int TarNameSize = 100;

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
static int TarEOF (TarHeader *tp);
void ExtractPlat (const char *, char *);
void ExtractDirPlat (const char *, char *);
static void SkipTarFile (Tape &tape, TarHeader *tp);
static int FileWanted (const char *, const char *, const PlatformIndex *);
static int ExtractTapeFile (Tape &tape, char *plat, TarHeader *tp);





static void
LoadFromTape (PlatformIndex *index, const char *dev, StatusWindow &sw,
		int expect)
//
// Bring in data from a tape.
//
{
	int nstrip = -1, nf = 0, nbyte = 0, first;
	IndexFile *file;
	const char *destdir;
	char plat[30];
//
// Get a hold of our tape device.
//
	Tape tape (dev);
	if (! tape.OK ())
		return;
//
// Find out what the first tape file is, and go there.
//
	first = FindFirstFile (index);
	if (first < 0)
	{
		cout << "No files selected!\n";
		return;
	}
	cout << "First file is " << first << ".\n";
	if (first)
		tape.skip (first);
//
// Pass through the tape until we have everything.
//
	TarHeader *tp;
	while ((tp = (TarHeader *) GetTarBlock (tape)) && nf < expect)
	{
	//
	// Keep our widget up to date.
	//
		if (sw.status (nf, nbyte))
			return;
		Disp->sync ();
	//
	// Check for the special tar EOF marker.
	//
		if (TarEOF (tp))
		{
			FlushTapeBlock ();
			continue;
		}
	//
	// See if this is a file we want or not.  KLUGE: If the first platform
	// name we extract isn't good, try to build a platform name from the
	// directory path portion of the filename.
	//
		ExtractPlat (tp->th_name, plat);
		if (! index->files (plat))
			ExtractDirPlat (tp->th_name, plat);
			
		if (! FileWanted (plat, tp->th_name, index))
		{
			cout << "Skip " << tp->th_name << ".\n";
			SkipTarFile (tape, tp);
			continue;
		}
	//
	// Seems we want it.
	//
		nbyte += ExtractTapeFile (tape, plat, tp);
		nf++;
		Main->UpdateSpace ();
	}
	ds_ForceRescan (0, TRUE);
}






static int
FindFirstFile (const PlatformIndex *index)
//
// Find the first tape file that has interesting stuff.
//
{
	int first = 9999, plat;

	for (plat = 0; plat < PList->ncontained (); plat++)
	{
		const dsPlatform &p = PList->nth (plat);
		IndexFile *file = index->files (p.name ());

		for (; file; file = file->next ())
			if (file->isMarked () && file->filenum () < first)
				first = file->filenum ();
	}
	return (first == 9999 ? -1 : first);
}




static int
TarEOF (TarHeader *tp)
//
// Return TRUE if this is an EOF marker.
//
{
	int i;
//
// There must be a reason why this thing doesn't just look at name[0], but
// I sure as hell can't tell you what it is.
//
	for (i = 0; i < 10; i++)
		if (tp->th_name[i])
			return (0);
	return (1);
}




static int
FileWanted (const char *plat, const char *fname, const PlatformIndex *index)
//
// Return TRUE iff this is a user-selected file.
//
{
	IndexFile *file = index->files (plat);

	for (; file; file = file->next ())
		if (! strcmp (fname, file->name ()))
			return (file->isMarked ());
	return (0);
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
}




void
ExtractDirPlat (const char *file, char *plat)
//
// Try to build a platform name using the last two directories in "file"'s 
// path.  This is a klugy means to get around platform names that have 
// (exactly) one slash in them and the slash is removed when file names are
// built.  (E.g., platform "rass/kapinga" may have a file named something
// like "./rass/kapinga/rasskapinga.930201.000000".  ExtractPlat() above will
// return "rasskapinga" as the platform name.  ExtractDirPlat() will return
// "rass/kapinga")
//
{
	int	slash, nslash, len;
	const char	*begin, *end, *c;
//
// Grab the last two directories from "file"'s path
//
	nslash = 0;
	begin = end = file;

	for (c = file + strlen (file); c >= file; c--)
	{
		if (*c != '/')
			continue;
		
		nslash++;

		if (nslash == 1)
			end = c - 1;
		else if (nslash == 3)
		{
			begin = c + 1;
			break;
		}
	}
//
// Return what we got
//
	len = end - begin + 1;
	strncpy (plat, begin, len);
	plat[len] = '\0';
	
	return;
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
		if ((DataLen = t.getblock (TapeBuffer, BufSize)) <= 0)
			return (DataLen == TS_EOF ? GetTarBlock (t) : 0);
		TapeBlock = TapeBuffer;
	}
	return (TapeBlock);
}


static void
FlushTapeBlock ()
//
// Just dump anything else in memory.
//
{
	DataLen = 0; // will force new read
}






static void SkipTarFile (Tape &tape, TarHeader *tp)
//
// Just skip over this one.
//
{
	int size = DecodeNum (tp->th_size);

	while (size > 0)
	{
		(void) GetTarBlock (tape);
		size -= TarBlockSize;
	}
}







static int
ExtractTapeFile (Tape &tape, char *plat, TarHeader *tp)
//
// Load a file onto disk.
//
{
//
// Copy the header now, since it will get overwritten in the
// tape buffer if the file is big enough.
//
	TarHeader hdr = *tp;
	int size = DecodeNum (hdr.th_size);
//
// Find out where the file goes, and make the new file name.
//
	char destdir[CFG_FILEPATH_LEN];
	ds_GetPlatDir (SrcId, ds_LookupPlatform (plat), destdir);

	char fname[200], *cp = strrchr (hdr.th_name, '/');
	strcpy (fname, destdir);
	strcat (fname, cp);
	cout << "Extr " << hdr.th_name << " to " << fname << ".\n";
//
// Create the file.
//
	int fd = open (fname, O_WRONLY | O_CREAT, 0666);
	if (fd < 0)
	{
		perror (fname);
		SkipTarFile (tape, tp);
		return (0);
	}
//
// Move the data over.
//
	for (int nmoved = 0; nmoved < size; nmoved += TarBlockSize)
	{
		char *data = GetTarBlock (tape);
		write (fd, data, (nmoved + TarBlockSize) > size ?
				size - nmoved : TarBlockSize);
	}
//
// Done!
//
	close (fd);
	return (size);
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


static int
VerifyDir (const char *dirname)
//
// Verify that the given directory exists, creating it if necessary.
// Return zero if we fail to find or create the directory.
//
{
    struct stat statbuf;
    
    if (stat (dirname, &statbuf) < 0 && mkdir (dirname, 0777) < 0)
    {
	cout << "Failed to create directory " << dirname << endl;
	return 0;
    }
    return 1;
}
