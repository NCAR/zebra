//
// Data store file management.
//
// This is my first real C++ program.  It is probably not an outstanding
// example of the application of object-oriented techniques.
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

#ifdef hpux
# include <sys/sigevent.h>
#endif
# include <stdio.h>
# include <stream.h>
# include <string>

//
// Zeb includes.
//
# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <Platforms.h>

# include "STable.h"
# include "dsmanage.h"
# include "dsFile.h"
# include "dsPlatform.h"
# include "Index.h"
# include "plcontainer.h"

MAKE_RCSID ("$Id: dsmanage.cc,v 1.16 2001-08-27 20:00:17 granger Exp $");

extern "C" char *strcat (char *, const char *);
extern "C" char *strrchr (const char *, int);

//
// Global source id (we just use 0 for now)
//
int SrcId = 0;

//
//
// The platform list.
//
//IContainer<dsPlatform> *PList = 0;
plContainer *PList = 0;

//
// Forwards.
//
static int MsgHandler (Message *msg);
static void DSSetup (void);
static void ScanFiles (PlatformId pid);



int main (int argc, char **argv)
{
	msg_connect (MsgHandler, "dsmanager");	// Hook into msg system
	DSSetup ();				// Get data store going
	DisplaySetup (&argc, argv);		// Get display going
	DisplayAddInput (msg_get_fd(), (void (*)())msg_incoming);
	Run ();
//	msg_await ();
}



static int
MsgHandler (Message *msg)
//
// Deal with incoming messages.  The only kind we really expect here
// is the abandon ship variety.
//
{
        if (msg->m_proto == MT_MESSAGE)
        {
                struct mh_template *tmpl = (struct mh_template *) msg->m_data;
                if (tmpl->mh_type == MH_SHUTDOWN)
		{
			cout << "dsmanage: message manager shutdown\n";
                        exit (0);
		}
        }
        return (0);
#ifdef notdef
	cout << "Message handler\n";
	cout.flush ();
	return (1);
#endif
}




static void
DSSetup ()
//
// Initialize our connection to the data store.
//
{
//
// Hook into the data store.
//
	ds_Initialize ();
//
// Make the platform list.
//
	MakePlatformList ();
}




void
MakePlatformList ()
{
	int pid, nplat = ds_GetNPlat ();
	const Platform *p;
//
// If there is already a list, delete it and start over.
//
	if (PList)
		delete PList;
//
// Make the platform list.
//
	PList = new plContainer (nplat);
	for (pid = 0; pid < nplat; pid++)
	{
		p = dt_FindPlatform (pid);
		if (pi_Subplatform (p))
			continue;	// We forget these
		dsPlatform *dp = new dsPlatform (pi_Name (p), pid);
		PList->add (*dp);
		delete dp;	// Container copies it

		ScanFiles (pid);
	}
}




static void
ScanFiles (PlatformId pid)
//
// Pick up all the files for this platform.
//
{
    const DataFile *df;
    dsPlatform *dp = PList->index (pid);
//
// Now plow through the files. (For now, we work with the default writable
// source)
//
    for (df = ds_FirstFile (SRC_DEFAULT_W, pid); df; df = ds_NextFile (df))
    {
	dsFile dsf(df);
	dp->files.add (dsf);
    }
}





void
PEMakePLabel (char *buf, const dsPlatform& p)
//
// Create the label for this platform's button.
//
{
	int df;
	int nfiles = p.ndfile();
//
// Fill in the platform name.
//
	sprintf (buf, "%-15s", p.name ());
	buf += 15;
//
// If this platform has no files, just say so.
//
	if (nfiles <= 0)
	{
		strcpy (buf, "---- no data files ----");
		return;
	}
//
// Begin time of first file
//
	TC_EncodeTime (p.files.nth(0).begin(), TC_Full, buf);
	strcat (buf, "      ");
	buf += 22;
//
// End time of last file
//
	TC_EncodeTime (p.files.nth(nfiles - 1).end(), TC_Full, 
		       buf);
	strcat (buf, "     ");
	buf += 21;
//
// Files and space.
//
	sprintf (buf, "%3d  %6.2f", nfiles, p.space ());
}






void
FEMakeFLabel (char *buf, const dsFile& f)
//
// Create the label for this file's button.
//
{
//
// Fill in the file name.
//
	sprintf (buf, "%-28s", strrchr (f.name (), '/') + 1);
	buf += 28;
	buf[-1] = ' ';
//
// Begin time.
//
	TC_EncodeTime (f.begin(), TC_Full, buf);
	strcat (buf, "      ");
	buf += 24;
//
// End time.
//
	TC_EncodeTime (f.end(), TC_Full, buf);
	strcat (buf, "     ");
	buf += 24;
//
// Files and space.
//
	sprintf (buf, "%6.2f", f.size ()/1048576.0);
}




PlatformIndex *
MakeDSIndex ()
//
// Create an index of the local data store and return it.
//
{
    PlatformId pid;
    int file;
    PlatformIndex *index = new PlatformIndex;
//
// Plow through the platforms.
//
    for (pid = 0; pid < PList->ncontained (); pid++)
    {
	const dsPlatform &dsp = PList->nth (pid);
    //
    // Now all the files.
    //
	for (file = 0; file < dsp.ndfile(); file++)
	{
	    const dsFile &dsf = dsp.files.nth (file);
	    IndexFile *indf = new IndexFile (dsp.name(), dsf.name(), 
					     dsf.size(), 0, dsf.begin(), 
					     dsf.end());
	    index->add (dsp.name(), *indf);
	}
    }
//
// Return the index and we are done.
//
    return (index);
}






void
MakeLocalIndex (const char *fname)
//
// Create an index (in FILE) of the local data store.
//
{
	PlatformIndex *index = MakeDSIndex ();
//
// Now we have an index; save it to the file, and let the whole thing
// get destructed.
//
	index->save (fname);
	delete index;
}
