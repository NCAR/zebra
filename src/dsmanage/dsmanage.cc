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


//
// Zeb includes.
//
extern "C" {
#	include <defs.h>
#	include <message.h>
#	include <DataStore.h>
};

# include "BoolKludge.h"
#include <stdio.h>
# include <stream.h>
# include <String.h>
# include "STable.h"
//# include "container.h"
# include "dsmanage.h"
# include "DataDir.h"
# include "Index.h"
# include "plcontainer.h"

MAKE_RCSID ("$Id: dsmanage.cc,v 1.9 1995-12-05 20:25:16 corbet Exp $");

extern "C" void strcat (char *, const char *);
extern "C" char *strrchr (const char *, int);


//
// The platform list.
//
//IContainer<dsPlatform> *PList = 0;
plContainer *PList = 0;

//
// Other globals.
//
String DDir;		// The data directory.

//
// Forwards.
//
static int MsgHandler (Message *);
static void DSSetup ();
static void ScanFiles (int);



int main (int argc, char **argv)
{
	msg_connect (MsgHandler, "dsmanager");	// Hook into msg system
	usy_init ();				// User interface symbols
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
	DataSrcInfo dsi;
//
// Hook into the data store.
//
	ds_Initialize ();
//	cout << SHeader->sm_nPlatform << " platforms, " << 
//		SHeader->sm_nDTEUsed << " datafiles defined.\n";
//
// Assume that the data directory of the first platform is the directory
// for all, and save it aside.
//
// The machinations below should be replacable with one gsub() call with
// a regexp, but I couldn't make it work.
//
	ds_GetDataSource (0, 0, &dsi);
	DDir = dsi.dsrc_Where;

	int lastslash;
	int slash = lastslash = DDir.index ('/', 0);
	while ((slash = DDir.index ('/', slash + 1)) >= 0)
		lastslash = slash;
	DDir.del (lastslash, DDir.length() - lastslash);
	cout << "Data dir is '" << DDir << "'.\n";
//
// Make the platform list.
//
	MakePlatformList ();
}




void
MakePlatformList ()
{
	int plat, nplat = ds_GetNPlat ();;
	PlatformInfo pi;
//
// If there is already a list, delete it and start over.
//
	if (PList)
		delete PList;
//
// Make the platform list.
//
	PList = new plContainer (nplat);
	for (plat = 0; plat < nplat; plat++)
	{
		ds_GetPlatInfo (plat, &pi);
		if (pi.pl_SubPlatform)
			continue;	// We forget these
		dsPlatform *dp = new dsPlatform (pi.pl_Name, plat);
		PList->add (*dp);
		delete dp;	// Container copies it
		ScanFiles (plat);
	}
//	cout << "PList has " << PList->ncontained () << " entries.\n";
}




static void
ScanFiles (int ind)
//
// Pick up all the files for this platform.
//
{
	int dfindex;
	dsPlatform *dp = PList->index (ind);
	String name;
	DataSrcInfo dsi;
	DataFileInfo dfi;
//
// Pull over the source information, and take the first to
// be the one we are interested in.
//
	ds_GetDataSource (ind, 0, &dsi);
//
// Now plow through the files.
//
	for (dfindex = dsi.dsrc_FFile; dfindex; dfindex = dfi.dfi_Next)
	{
		ds_GetFileInfo (dfindex, &dfi);
		/* sprintf (name, "%s/%s", p->dp_dir, d->df_name); */
	//		name = String (dsi.dsrc_Where) + String ("/") +
	//			String (dfi.dfi_Name);
		name = String (dsi.dsrc_Where);
		name += "/";
		name += dfi.dfi_Name;
		dsFile *df = new dsFile (name.chars (), dfindex);
		dp->files.add (*df);
		delete df;
	}
//	cout << "Platform '" << p->dp_name << "' has " <<
//			dp->files.ncontained () << " files for " <<
//			dp->space() << "mb.\n";
}





void
DDInfo (const char **dir, float *space)
// XXX
// Return the data dir info.
//
{
	DataDir dd (DDir.chars ());
	*dir = DDir.chars ();
	*space = dd.FreeSpace ()/1024000.0;
}






void
PEMakePLabel (char *buf, const dsPlatform& p)
//
// Create the label for this platform's button.
//
{
	int df;
//
// Fill in the platform name.
//
	sprintf (buf, "%-15s", p.name ());
	buf += 15;
//
// If this platform has no files, just say so.
//
	if (p.files.ncontained () <= 0)
	{
		strcpy (buf, "---- no data files ----");
		return;
	}
//
// Begin time.
//
	df = p.files.nth (p.files.ncontained () - 1).index;
	DataFileInfo dfi;
	ds_GetFileInfo (df, &dfi);
	TC_EncodeTime (&dfi.dfi_Begin, TC_Full, buf);
	strcat (buf, "      ");
	buf += 22;
//
// End time.
//
	df = p.files.nth (0).index;
	ds_GetFileInfo (df, &dfi);
	TC_EncodeTime (&dfi.dfi_End, TC_Full, buf);
	strcat (buf, "     ");
	buf += 21;
//
// Files and space.
//
	sprintf (buf, "%3d  %6.2f", p.files.ncontained (), p.space ());
}






void
FEMakeFLabel (char *buf, const dsFile& f)
//
// Create the label for this file's button.
//
{
	int df;
//
// Fill in the file name.
//
	sprintf (buf, "%-28s", strrchr (f.name (), '/') + 1);
	buf += 28;
//
// Begin time.
//
	df = f.index;
	DataFileInfo dfi;
	ds_GetFileInfo (df, &dfi);
	TC_EncodeTime (&dfi.dfi_Begin, TC_Full, buf);
	strcat (buf, "      ");
	buf += 22;
//
// End time.
//
	TC_EncodeTime (&dfi.dfi_End, TC_Full, buf);
	strcat (buf, "     ");
	buf += 21;
//
// Files and space.
//
	sprintf (buf, "%6.2f", f.size ()/1024000.0);
}




PlatformIndex *
MakeDSIndex ()
//
// Create an index of the local data store and return it.
//
{
	int plat, file;
	PlatformIndex *index = new PlatformIndex;
	DataFileInfo dfi;
//
// Plow through the platforms.
//
	for (plat = 0; plat < PList->ncontained (); plat++)
	{
		const dsPlatform &dsp = PList->nth (plat);
	//
	// Now all the files.
	//
		for (file = 0; file < dsp.files.ncontained (); file++)
		{
			const dsFile &dsf = dsp.files.nth (file);
			ds_GetFileInfo (dsf.index, &dfi);
			IndexFile *indf = new IndexFile (dsp.name (),
				dsf.name (), dsf.size (), 0,
				&dfi.dfi_Begin,
				&dfi.dfi_End);
			index->add (dsp.name (), *indf);
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




const char *
GetPlatDir (const char *name)
//
// Return the data directory for this platform.
//
{
	PlatformId pid = ds_LookupPlatform ((char *) name);
	DataSrcInfo dsi;
	static char retbuf[256];	// XXX

	ds_GetDataSource (pid, 0, &dsi);
	strcpy (retbuf, dsi.dsrc_Where);
	return (retbuf);
}
