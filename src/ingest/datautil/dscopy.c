/*
 * Copy data from one platform to another.
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

# include <ui.h>
# include <config.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>

MAKE_RCSID ("$Id: dscopy.c,v 1.11 2005-07-29 21:42:00 granger Exp $")


# define MAX_TIMES 10000

/*
 * Keywords.
 */
# define KW_SOURCE	0
# define KW_DEST	1
# define KW_TIME	2
# define KW_FIELDS	3
# define KW_GO		4
# define KW_DIE		5


/*
 * Copy info.
 */
# define MAXFLD 40
PlatformId Source = BadPlatform, Dest = BadPlatform;
FieldId Fids[MAXFLD], RFids[MAXFLD];
int NField = 0, NRField = 0;
ZebTime Begin, End;
DataClass Class;	/* Class of data we move. */

zbool PreserveObs = TRUE;

char DimensionDetails[256] = "";

/*
 * Forwards.
 */
static void	Die FP ((void));
static int	Incoming FP ((Message *));
static int	Dispatcher FP ((int, struct ui_command *));
static void	Go FP ((void));
static ZebTime	*GetTimes FP ((int *));
static void	DoFields FP ((ZebTime *));





main (argc, argv)
int argc;
char **argv;
{
	char loadfile[100], pname[100];
	SValue v;
	Begin = ZT_EPOCH;
	End = ZT_END;
/*
 * Hook into the user interface.
 */
	fixdir_t ("DSCOPY_LF_DIR", GetLibDir(), "dscopy.lf", loadfile, ".lf");
	if (argc > 1)
	{
		ui_init (loadfile, FALSE, TRUE);
		v.us_v_ptr = argv[1];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
				SYMT_STRING, &v);
	}
	else
		ui_init (loadfile, TRUE, FALSE);
	SetupConfigVariables ();
	usy_c_indirect (usy_g_stbl ("ui$variable_table"), "preserveobs",
		&PreserveObs, SYMT_BOOL, 0);
	usy_c_indirect (usy_g_stbl ("ui$variable_table"), "dimensions",
		DimensionDetails, SYMT_STRING, sizeof(DimensionDetails));
/*
 * Hook into the message system.
 */
	sprintf (pname, "dscopy-%d", getpid ());
	msg_connect (Incoming, pname);
	ds_Initialize ();
/*
 * Go.
 */
	ui_get_command ("initial", "dscopy>", Dispatcher, 0);
	Die ();
}




static void
Die ()
/*
 * Time to shut down.
 */
{
	ui_finish ();
	exit (0);
}





static int
Dispatcher (junk, cmds)
int junk;
struct ui_command *cmds;
/*
 * Deal with a UI command.
 */
{
	int i;

	switch (UKEY (*cmds))
	{
	   case KW_DIE:
	   	Die ();
		break;

	   case KW_FIELDS:
		cmds++;
		for (i = 0; cmds->uc_ctype != UTT_END; i++, cmds++)
			RFids[NRField++] = F_Lookup (UPTR (*cmds));
		ui_warning ("Field selection doesn't work quite right yet");
		break;

	   case KW_TIME:
	   	TC_UIToZt (&UDATE(cmds[1]), &Begin);
		TC_UIToZt (&UDATE(cmds[2]), &End);
		break;

	   case KW_SOURCE:
	   	if ((Source = ds_LookupPlatform (UPTR (cmds[1]))) ==
				BadPlatform)
			ui_error ("Unknown Platform %s", UPTR (cmds[1]));
		break;

	   case KW_DEST:
	   	if ((Dest = ds_LookupPlatform (UPTR (cmds[1]))) == BadPlatform)
			ui_error ("Unknown Platform %s", UPTR (cmds[1]));
		break;

	   case KW_GO:
	   	Go ();
		break;
	}
	return (1);
}





static void
Go ()
/*
 * Actually do it.
 */
{
	ZebTime *obstimes;
	int ntime;
/*
 * Some checking.
 */
	if (Source == BadPlatform)
		ui_error ("No source platform specified");
	if (Dest == BadPlatform)
		ui_error ("No destination platform specified");
/*
 * Get the times for the source platform.
 */
	if ((obstimes = GetTimes (&ntime)) == 0)
	{
		ui_error ("No data available for source platform");
		Die ();
	}
# ifdef notdef
/*
 * Figure out the fields.
 */
	DoFields (obstimes);
	if (NField <= 0)
		ui_error ("No fields!");
# endif
/*
 * Figure out the class of data we will be moving.
 */
	switch (ds_PlatformDataOrg (Source))
	{
	   case Org1dGrid:
	   case Org2dGrid:
	   case Org3dGrid: 	Class = DCC_RGrid; break;
	   case OrgIRGrid:	Class = DCC_IRGrid; break;
	   case OrgScalar:	Class = DCC_Scalar; break;
	   case OrgOutline:	Class = DCC_Boundary; break;
	   case OrgCmpImage:
	   case OrgImage:	Class = DCC_Image; break;
	   case OrgNSpace:	Class = DCC_NSpace; break;
	}
/*
 * Do it.  Times come in descending order, so we reverse them here for the
 * hell of it.
 */
	for (ntime--; ntime >= 0; ntime--)
		CopyObservation (obstimes + ntime);
}






static ZebTime *
GetTimes (ntime)
int *ntime;
/*
 * Make a list of observation times.
 */
{
  ZebTime* Times = malloc(1024*sizeof(ZebTime));
  int nalloc = 1024;
  int nt, i;

  /* keep retrieving observation times until they all fit */
  while ((nt = ds_GetObsTimes (Source, &End, Times, nalloc, 0)) == nalloc)
  {
    nalloc += 1024;
    Times = realloc(Times, nalloc*sizeof(ZebTime));
  }
  *ntime = 0;
  for (i = 0; i < nt; ++i)
  {
    if (TC_Less (Times[i], Begin))
      break;
    ++(*ntime);
  }
  return (*ntime ? Times : 0);
}





static void
DoFields (t)
ZebTime *t;
/*
 * Figure out the fields.
 */
{
  FieldId* fp;
  int i;
/*
 * If they gave no fields, we do them all.
 */
	if (NRField <= 0)
	{
		NField = MAXFLD;
		ds_GetFields (Source, t, &NField, Fids);
	}
	fp = Fids;
	for (i = 0; i < NField; ++i)
	{
	  if (!strcmp(F_GetName(Fids[i]), "lat") 
	      || !strcmp(F_GetName(Fids[i]), "lon")
	      || !strcmp(F_GetName(Fids[i]), "alt"))
	    continue;
	  *fp++ = Fids[i];
	}
	NField = fp - Fids;
}





static int 
Incoming (msg)
Message *msg;
/*
 * Deal with an incoming message.
 */
{
	struct mh_template *tmpl = (struct mh_template *) msg->m_data;

	switch (msg->m_proto)
	{
	   case MT_MESSAGE:
		if (tmpl->mh_type == MH_DIE)
			Die ();
		else
			msg_ELog (EF_PROBLEM, "Weird MH msg %d",tmpl->mh_type);
		break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Unknown msg proto %d", msg->m_proto);
		break;
	}
	return (0);
}





CopyObservation (t)
ZebTime *t;
/*
 * Copy the observation containing this time.
 */
{
	static ZebTime times[MAX_TIMES];
	Location locs[MAX_TIMES];
	int nsample, samp, fld;
	DataChunk *dc;
	char atime[40];
	dsDetail details[10];
	int ndetail = 0;

	if (strlen(DimensionDetails) > 0)
	{
	  dc_NSFixedDetails(DimensionDetails, details, &ndetail);
	  ui_printf ("Setting %d details from %s\n", ndetail, 
		     DimensionDetails);
	}
	ds_SetDetail (DD_NC_ONE_TIME, details, ndetail++);
	TC_EncodeTime (t, TC_Full, atime);
	DoFields (t);
/*
 * Get the times available in this observation.
 */
	nsample = ds_GetObsSamples (Source, t, times, locs, MAX_TIMES);
	/* XXX XXX XXX */
	if (nsample <= 0)
	{
		nsample = 1;
		times[0] = *t;
	}
	/* XXX XXX XXX */
	ui_nf_printf ("Doing obs at %s, %d samples, %d fields: ", 
		      atime, nsample, NField);
	for (fld = 0; fld < NField; fld++)
		ui_nf_printf ("%s ", F_GetName (Fids[fld]));
	ui_printf ("\n");
/*
 * Now just do them.
 */
	for (samp = 0; samp < nsample; samp++)
	{
	        TC_EncodeTime (times+samp, TC_Full, atime);
		dc = ds_Fetch (Source, Class, times + samp, times + samp,
			Fids, NField, 0, 0);
		if (! dc)
		{
		  ui_printf("*** Failed to read sample at %s, skipping.\n", 
			    atime);
		  continue;
		}
		dc->dc_Platform = Dest;
# ifdef notdef
		if (samp == 0)
		{
		    char sname[80];
		    strcpy (sname, ds_PlatformName (Source));
		    dc_SetGlobalAttr (dc, "copied_from", sname);
		}
		ui_printf ("Samp %3d: %.2f\n", samp,
			dc_GetScalar (dc, 0, Fids[0]));
# endif
		ds_Store (dc, PreserveObs && samp == 0, details, ndetail);
		dc_DestroyDC (dc);
	}
}
