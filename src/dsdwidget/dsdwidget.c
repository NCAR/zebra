/*
 * Data store dump widget.
 */

/*		Copyright (C) 1987,88,89,90,91 by UCAR
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

# include <unistd.h>
# include <string.h>

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Box.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Viewport.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/List.h>
# include <X11/Shell.h>

# include <defs.h>
# include <zl_symbol.h>
# include <message.h>
# include <copyright.h>
# include <DataStore.h>
# include <Platforms.h>

RCSID ("$Id: dsdwidget.c,v 1.31 2004-01-15 20:08:53 burghart Exp $")


/*
 * Data structures for the platforms we know.
 */
char	*Names[MAXPLAT];
PlatformId PlatIds[MAXPLAT];
Widget	Entry[MAXPLAT];
int	NPlat = 0;

char *RemoteName = NULL;

struct dump_action {
	char *ext;
	char *cmd;
} DumpActions[] = 
{
	{ ".cdf",	"xncdump" },
	{ ".nc",	"xncdump" },
	{ ".rf",	"rfdump" },
	{ ".znf",	"zfdump -h" }
};
int NActions = XtNumber(DumpActions);

/*
 * Data structures for widgets and their contents.
 */
XtAppContext Appc;
Widget	Top, Box;

/*
 * Context structure for the datafile popup shells
 */
struct df_context {
	Widget	shell;		/* our popup shell */
	Widget	list;		/* the list widget */
	char 	**entries;	/* data file entries */
	char	**filenames;	/* complete file name for each entry */
	int	nent;		/* number of entries */
	PlatformId pid;		/* our pid */
};

/*
 * Translations for the list widget to make it look like commands
 */
static XtTranslations list_trans;
static char *atrans = "<Btn1Down>:	Set() \n\
           	       <Btn1Up>: 	Notify() Unset()";

static void	AddPlatforms (char *re, int sort);
static void	CreateDSDWidget (void);
static void	Die (void);
static void	GetTimes (const Platform*, ZebTime *, ZebTime *);
static void	SetEntry (int, ZebTime *, ZebTime *);
static int	MsgHandler (Message *);
static void	MsgInput (XtPointer junk, int *fd, XtInputId *morejunk);
static void	Update (void);

static void 	PopupDisplay (Widget w, XtPointer cdata, XtPointer call);
static void	DumpPlatform (struct df_context *dfc);
static void	DumpChain (struct df_context *dfc, const SourceInfo *si);
static Widget	CreateDisplayWidget FP((Widget parent, struct df_context *dfc,
					const Platform *p));
static void	RescanCallback FP((Widget w, XtPointer, XtPointer));
static void	ListCallback FP((Widget w, XtPointer, XtPointer));
static void	QuitDisplay FP((Widget w, XtPointer, XtPointer));



static void
usage (prog)
char *prog;
{
	printf("Usage: %s [-h][-a][-u][-t <title>] [regexp ...] \n",
	       prog);
	printf("If a regular expression is present, only those platforms\n");
	printf("whose names match the expression are displayed.  Any\n");
	printf("number or combination of expressions may be given.  If\n");
	printf("there are no strings to match, all of the platforms will\n");
	printf("be displayed, by default alphabetically\n");
	printf("   -h\tPrint this usage message.\n");
	printf("   -a\tAlphabetize the platforms for each matching string.\n");
	printf("   -u\tDon't alphabetize the platform names.\n");
	printf("   -t\tSpecify a title for the window.\n");
#ifdef notdef
	printf("   -c\tCommand which accepts selected file names.\n");
#endif
}


int
main (argc, argv)
int	argc;
char	**argv;
{
	char name[256];
	zbool sort;
	char *title;
	int c;
	int optind, i;

	sort = TRUE;
	title = name;
	optind = 1;
	while ((optind < argc) && (strlen(argv[optind]) == 2) &&
	       (argv[optind][0] == '-'))
	{
		c = argv[optind][1];
		switch (c)
		{
		   case 'h':
			usage(argv[0]);
			exit(0);
			break;
		   case 'a':
			sort = TRUE;
			break;
		   case 'u':
			sort = FALSE;
			break;
		   case 't':
			if (optind + 1 < argc)
				title = argv[++optind];
			else
			{
				printf ("-t option needs argument\n");
				usage (argv[0]);
				exit (2);
			}
			break;
#ifdef notdef
		   case 'c':
			if (optind + 1 < argc)
				DumpCommand = argv[++optind];
			else
			{
				printf ("-c option needs argument\n");
				usage (argv[0]);
				exit (2);
			}
			break;
#endif
		}
		for (i = 1; i < argc - optind; ++i)
			argv[i] = argv[i + optind];
		argc -= optind;
		optind = 1;
	}
/*
 * Hook into the message system and initialize data store.  We use our pid
 * in the name since it is very possible someone will want to run different
 * displays for different sets of platforms.
 */
	sprintf (name, "dsdwidget-%d", (int) getpid());
	if (! msg_connect (MsgHandler, name))
	{
		printf ("Unable to connect to message handler.\n");
		exit (1);
	}
	if (! ds_Initialize ())
	{
		printf ("Unable to initialize data store.\n");
		exit (1);
	}
/*
 * Hook into the window system.
 */
	Top = XtVaAppInitialize (&Appc, "dsdwidget", NULL, 0, &argc, argv,
                NULL, XtNtitle, title, NULL);
	list_trans = XtParseTranslationTable (atrans);
/*
 * Make our toplevel widget.
 */
	CreateDSDWidget ();
/*
 * Add platform labels and times to our widget.
 */
	if (! (RemoteName = getenv ("REMOTE_NAME")))
		RemoteName = "Remote";
#ifdef notdef
	if (! DumpCommand)
		DumpCommand = "ncdump -h";
#endif
	if (argc < 2)
		AddPlatforms (NULL, sort);
	for (optind = 1; optind < argc; ++optind)
		AddPlatforms (argv[optind], sort);
	if (NPlat == 0)
	{
		printf ("%s: No matches found!\n", argv[0]);
		exit (1);
	}
/*
 * Display the widget and wait for something to happen.
 */
        XtRealizeWidget (Top);
        XtAppAddInput (Appc, msg_get_fd (), (XtPointer) XtInputReadMask, 
		(XtInputCallbackProc) MsgInput, (XtPointer) 0);
        XtAppMainLoop (Appc);
	exit(0);
}


static int
MsgHandler (msg)
Message	*msg;
/*
 * Log a client event.
 */
{
/*
 * Everything is assumed to be a message handler event.
 */
        if (msg->m_proto == MT_MESSAGE)
        {
                struct mh_template *tmpl = (struct mh_template *) msg->m_data;
                if (tmpl->mh_type == MH_SHUTDOWN)
                        Die ();
        }
        return (0);
}


static void
MsgInput (XtPointer junk, int *fd, XtInputId *morejunk)
/*
 * Input is happening.
 */
{
	static int nexit = 0;

        if (msg_incoming (*fd))
        {
		if (++nexit > 50)
			Die ();
        }
}



static void
Die ()
/*
 * Exit gracefully.
 */
{
        msg_ELog (EF_INFO, "Exiting.");
        exit (0);
}




static void
GetTimes (const Platform *p, ZebraTime *begin, ZebraTime *end)
/*
 * Get the begin and end data times for a platform, without traversing
 * the entire file chain.  Use ds_FindDF and ds_FindAfter to get first
 * and last files in chain, without needing to read any files.
 */
{
	const DataFile *df;
/*
 * Check all sources at once for the first and last file available.
 */
	df = ds_FindDFBefore (SRC_ALL, pi_Id (p), &ZT_OMEGA);
	if (! df)
	{
		/* no data whatsoever */
		end->zt_Sec = end->zt_MicroSec = 0;	
		begin->zt_Sec = begin->zt_MicroSec = 0;	
	}
	else
	{
		*end = df->df_core.dfc_end;
		df = ds_FindDFAfter (SRC_ALL, pi_Id (p), &ZT_ALPHA);
		*begin = df->df_core.dfc_begin;
	}
}




static void
CreateDSDWidget ()
/*
 * Create the widget.
 */
{
        Arg args[5];
        int n;
        Widget button, die, info;
	Widget form, viewport;
/*
 * Start with a form, of course.
 */
        form = XtCreateManagedWidget ("form", formWidgetClass, Top, NULL, 0);
/*
 * The die button.
 */
        n = 0;
        XtSetArg (args[n], XtNlabel, "Exit");                   n++;
        XtSetArg (args[n], XtNfromHoriz, NULL);	                n++;
        XtSetArg (args[n], XtNfromVert, NULL);                  n++;
        die = XtCreateManagedWidget ("die", commandWidgetClass, form, args, n);
        XtAddCallback (die, XtNcallback, (XtCallbackProc) Die, 
		(XtPointer) 0);
/*
 * Button to update our information.
 */
        n = 0;
        XtSetArg (args[n], XtNlabel, "Update");			n++;
        XtSetArg (args[n], XtNfromHoriz, die);                  n++;
        XtSetArg (args[n], XtNfromVert, NULL);                  n++;
        button = XtCreateManagedWidget ("update", commandWidgetClass,
					form, args, n);
        XtAddCallback (button, XtNcallback, (XtCallbackProc)Update, 0);
/*
 * Button to request a global rescan of the data store
 */
        n = 0;
        XtSetArg (args[n], XtNlabel, "Global Rescan");		n++;
        XtSetArg (args[n], XtNfromHoriz, button);               n++;
        XtSetArg (args[n], XtNfromVert, NULL);                  n++;
        button = XtCreateManagedWidget ("rescan", commandWidgetClass,
					form, args, n);
        XtAddCallback (button, XtNcallback, (XtCallbackProc) RescanCallback, 
		       (XtPointer) -1);
/*
 * Instructional label
 */
        n = 0;
        XtSetArg (args[n], XtNfromHoriz, button);	n++;
        XtSetArg (args[n], XtNfromVert, NULL);		n++;
        info = XtCreateManagedWidget ("info", labelWidgetClass, form,
				      args, n);
/*
 * A big viewport.
 */
        n = 0;
        XtSetArg (args[n], XtNfromHoriz, NULL);         n++;
        XtSetArg (args[n], XtNfromVert, button);	n++;
        viewport = XtCreateManagedWidget ("viewport", viewportWidgetClass,
					  form, args, n);
/*
 * ...with a box in it.
 */
        n = 0;
        Box = XtCreateManagedWidget ("box", boxWidgetClass, viewport, args, n);
}



static void
Update ()
/*
 * Plow through the platforms and refresh the file times and widget 
 * entries for each.
 */
{
	int i;
	ZebraTime begin, end;

	msg_ELog (EF_INFO, "Updating platform info.");
	for (i = 0; i < NPlat; i++)
	{
		GetTimes (dt_FindPlatform (PlatIds[i]), &begin, &end);
		/* msg_ELog (EF_DEBUG, "Setting entry %d", i); */
		SetEntry (i, &begin, &end);
	}
}



static void
SetEntry (index, begin, end)
int index;
ZebraTime *begin, *end;
/*
 * Make a label for this platform.
 */
{
        char label[CFG_PLATNAME_LEN + 64], end_date[30], begin_date[30];
        Arg arg;
/*
 * Make the label.
 */
	/* msg_ELog (EF_DEBUG, "Setting entry for %s", Names[index]); */
	if ((end->zt_Sec == 0) && (begin->zt_Sec == 0))
	{
		sprintf (label, "%-25s  -- None --         ", Names[index]);
	}
	else 
	{
		TC_EncodeTime (end, TC_Full, end_date);
		TC_EncodeTime (begin, TC_Full, begin_date);
		sprintf (label, "%-17s  %22s -> %22s", Names[index],
			begin_date, end_date);
	}
	msg_ELog (EF_DEBUG, "Entry %s", label);
/*
 * Stash it into the widget.
 */
        XtSetArg (arg, XtNlabel, label);
        XtSetValues (Entry[index], &arg, 1);
}



static void
AddPlatforms (re, sort)
char *re;	/* NULL implies get them all 		*/
zbool sort;	/* true if we want them alphabetized 	*/
/*
 * Add platform names and data times to dsdwidget.
 */
{
	int i, nplat;
	ZebraTime begin, end;
	const Platform *p;
	PlatformId *platforms;

	platforms = ds_SearchPlatforms (re, &nplat, sort, FALSE);
	for (i = 0; i < nplat; i++)
	{
	/*
	 * Make sure we have room for another platform
	 */
		if (NPlat >= MAXPLAT)
		{
			msg_ELog (EF_PROBLEM, "Too many platforms: %d", NPlat);
			break;
		}
	/*
	 * Get the platform info.  Don't bother with subplatforms, since
	 * there is no info of interest there.
	 */
		p = dt_FindPlatform (platforms[i]);
	        Names[NPlat] = usy_pstring (pi_Name (p));
		PlatIds[NPlat] = platforms[i];
	/*
	 * Create a command button for a platform.
	 */
		GetTimes (p, &begin, &end);
        	Entry[NPlat] = XtCreateManagedWidget (pi_Name (p),
			commandWidgetClass, Box, NULL, 0);
		XtAddCallback (Entry[NPlat], XtNcallback, 
			       (XtCallbackProc) PopupDisplay, 
			       (XtPointer)(long) platforms[i]);

		SetEntry (NPlat, &begin, &end);
		NPlat++;
	}
	if (platforms)
		free (platforms);
}



/*ARGSUSED*/
static void 
PopupDisplay (w, cdata, call)
Widget w;
XtPointer cdata;
XtPointer call;
/*
 * Popup the window which displays the data files for a platform.
 */
{
	struct df_context *dfc = NEW (struct df_context);
	PlatformId pid = (PlatformId) cdata;
	const Platform *p = dt_FindPlatform (pid);
	
	dfc->pid = (PlatformId) (long)cdata;
	dfc->list = NULL;
	dfc->entries = NULL;
	dfc->filenames = NULL;
	dfc->nent = 0;
	/*
 	 * Generate the array of entries and the shell.
	 */
	DumpPlatform (dfc);
	CreateDisplayWidget (Top, dfc, p);

	/*
	 * Popup the shell and forget about it.
	 */
	XtPopup (dfc->shell, XtGrabNone);	
}



static void
DumpPlatform (struct df_context *dfc)
/*
 * Create list entries for all of this platform's datafiles
 */
{
	SourceInfo si;
	int s;
/*
 * Give the list an initial allocation, and don't reallocate except
 * when the number of entries crosses the granularity size.
 */
#	define GRAIN 25
	dfc->entries = (char **) malloc (GRAIN * sizeof(char *));
	dfc->filenames = (char **) malloc (GRAIN * sizeof(char *));
	dfc->nent = 0;
/*
 * Write data times to the list entries.
 */
	for (s = 0; ds_GetSourceInfo (s, &si); s++)
	    DumpChain (dfc, &si);
}



static void
DumpChain (struct df_context *dfc, const SourceInfo *si)
/*
 * Dump out a datafile chain.
 */
{
    const DataFile *df;
    char abegin[30], aend[30];
    char dest[256];
    const Platform *p = dt_FindPlatform (dfc->pid);

    /* Order them with most recent data first */
#ifdef notdef
    for (df = ds_FirstFile (si->src_Id, pi_Id (p)); df; 
	 df = ds_NextFile (df))
#endif
    for (df = ds_LastFile (si->src_Id, pi_Id (p)); df; 
	 df = ds_PrevFile (df))
    {
	TC_EncodeTime (&df->df_core.dfc_begin, TC_Full, abegin);
	sprintf (dest,"%-8s '%s' %s    ", si->src_Name, 
		 df->df_core.dfc_name, abegin);
	if (df->df_core.dfc_nsample > 1)
	{
	    TC_EncodeTime (&df->df_core.dfc_end, TC_Full, aend);
	    sprintf (dest+strlen(dest)-4, " -> %s [%hu]    ", aend, 
		     df->df_core.dfc_nsample);
	}

    /*
     * Add this entry to the list
     */
	++dfc->nent;
	if (! (dfc->nent % GRAIN))
	{
	    dfc->entries = (char **) 
		realloc (dfc->entries, (dfc->nent + GRAIN) * sizeof(char *));
	    dfc->filenames = (char **) 
		realloc (dfc->filenames, (dfc->nent + GRAIN) * sizeof(char *));
	}
	
	dfc->entries[ dfc->nent - 1 ] = usy_string (dest);
	dfc->filenames[ dfc->nent - 1 ] = usy_string (df->df_fullname);
    }
}




static Widget
CreateDisplayWidget (Widget parent, struct df_context *dfc, const Platform *p)
/*
 * Build a widget for displaying the full data for a platform.
 */
{
        int     n;
        Arg     args[15];
        Widget  quit, title, rescan, info;
	Widget 	form, popup;
	Widget  viewport, list;
	char 	label[256];
/*
 * Create the popup widget shell and the form widget within it that holds
 * everything.
 */
        n = 0;
        XtSetArg (args[n], XtNresize, True); 	n++;
        popup = XtCreatePopupShell ("platform_data",
                topLevelShellWidgetClass, parent, args, n);

        n = 0;
        XtSetArg (args[n], XtNborderWidth, 3); n++;
        form = XtCreateManagedWidget ("displayForm", formWidgetClass, popup,
				      args, n);
/*
 * Put platform info at top
 */
	sprintf (label, "Platform '%s'", pi_Name (p));
	if (pi_Mobile (p))
		strcat (label, " [mobile]");
        n = 0;
        XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
        XtSetArg (args[n], XtNfromVert, NULL);		n++;
        XtSetArg (args[n], XtNlabel, label);		n++;
        title = XtCreateManagedWidget ("title", labelWidgetClass, form,
				       args, n);
/*
 * Button to remove the popup
 */
        n = 0;
        XtSetArg (args[n], XtNfromHoriz, title);	n++;
        XtSetArg (args[n], XtNfromVert, NULL);		n++;
        XtSetArg (args[n], XtNlabel, "Done");		n++;
        quit = XtCreateManagedWidget ("quit", commandWidgetClass, form,
				      args, n);
        XtAddCallback (quit, XtNcallback, QuitDisplay, (XtPointer) dfc);
/*
 * Offer to rescan this platform.
 */
        n = 0;
        XtSetArg (args[n], XtNfromHoriz, quit);		n++;
        XtSetArg (args[n], XtNfromVert, NULL);		n++;
        XtSetArg (args[n], XtNlabel, "Rescan");		n++;
        rescan = XtCreateManagedWidget ("rescan", commandWidgetClass, form,
					args, n);
        XtAddCallback (rescan, XtNcallback, RescanCallback, 
		       (XtPointer)(long) dfc->pid);
/*
 * An informational label in the upper right
 */
        n = 0;
        XtSetArg (args[n], XtNfromHoriz, rescan);	n++;
        XtSetArg (args[n], XtNfromVert, NULL);		n++;
        info = XtCreateManagedWidget ("info", labelWidgetClass, form,
				      args, n);
/*
 * The viewport and list are only necessary if we have some entries.
 */
	if (dfc->nent)
	{
	/*
	 * The list widget must be contained in a viewport since the list will
	 * likely be long.
	 */
		n = 0;
		XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
		XtSetArg (args[n], XtNfromVert, title);		n++;
		viewport = XtCreateManagedWidget ("viewport", 
						  viewportWidgetClass,
						  form, args, n);
		n = 0;
		XtSetArg (args[n], XtNtranslations, list_trans); n++;
		XtSetArg (args[n], XtNlist, dfc->entries); n++;
		XtSetArg (args[n], XtNnumberStrings, dfc->nent); n++;
		list = XtCreateManagedWidget ("list", listWidgetClass,
					      viewport, args, n);
		XtAddCallback (list, XtNcallback, ListCallback, 
			       (XtPointer) dfc);
		dfc->list = list;
	}
	dfc->shell = popup;
	return (popup);
}



/*ARGSUSED*/
static void
RescanCallback (w, cdata, call_data)
Widget w;
XtPointer cdata;
XtPointer call_data;
{
	PlatformId pid = (PlatformId) (long)cdata;

	ds_ForceRescan (pid, (pid < 0));
}
	


/*ARGSUSED*/
static void
ListCallback (w, cdata, call_data)
Widget          w;
XtPointer       cdata;
XtPointer	call_data;
{
	XawListReturnStruct *lrs = (XawListReturnStruct *) call_data;
	struct df_context *dfc = (struct df_context *) cdata;
	char cmd[1024];
	char ext[32];
	char *period, *file;
	int i;

	msg_ELog (EF_DEBUG, "selected entry: %s", lrs->string);
/*
 * Get the full filename for the selected entry, and extract its extension
 */
	file = dfc->filenames[lrs->list_index];

	ext[0] = 0;
	if ((period = strrchr (file, '.')))
		strcpy (ext, period);
/*
 * Find the command to use for this extension
 */
	for (i = 0; i < NActions; ++i)
	{
		if (! strcmp (DumpActions[i].ext, ext))
			break;
	}
	if (i >= NActions)
	{
		msg_ELog (EF_INFO, "no action for extension '%s'", ext);
		return;
	}
/*
 * Finally, build and execute the command
 */
	sprintf (cmd, "%s %s &", DumpActions[i].cmd, file);
	msg_ELog (EF_DEBUG, "%s", cmd);
	system (cmd);
}




/*ARGSUSED*/
static void
QuitDisplay (w, cdata, call_data)
Widget          w;
XtPointer       cdata;
XtPointer	call_data;
/*
 * Remove (pop down) this display widget shell
 */
{
	struct df_context *dfc = (struct df_context *) cdata;
	int i;

	XtPopdown (dfc->shell);
	XtDestroyWidget (dfc->shell);
	for (i = 0; i < dfc->nent; ++i)
	{
		usy_rel_string (dfc->entries[i]);
		usy_rel_string (dfc->filenames[i]);
	}
	
	if (dfc->entries)
	{
		free (dfc->entries);
		free (dfc->filenames);
	}

	free (dfc);
}

