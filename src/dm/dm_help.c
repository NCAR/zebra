/*
 * This module provides help information using the Mosaic or
 * netscape browsers, preferrably netscape since only netscape has
 * been tested recently.
 */
# include <unistd.h>
# include <sys/types.h>
# include <sys/wait.h>
# include <stdio.h>
# include <signal.h>
# include <errno.h>
# include <string.h>
# include <ctype.h>

# include <ui.h>
# include <defs.h>
# include <message.h>
# include <dm.h>
# include "dm_vars.h"

# define URL_LEN (3 * CFG_FILEPATH_LEN)

/*
 * When the Mosaic browser is running, we keep it's PID here.
 */
static int MosPid = -1;

static int dm_FindURL FP ((char *, char *));
static void dm_StartBrowser FP ((char *));
static void dm_TweakMosaic FP ((char *));
static void dm_TweakNetscape FP ((char *));

/*
 * The temporary file through which we send commands to Mosaic.
 * Try to keep track of it and remove it when finished.
 */
static char TFile[ CFG_FILEPATH_LEN ] = { '\0' };

static char HelpPath[CFG_SEARCHPATH_LEN]; /* Where are the helpfiles */
static char BrowserPath[CFG_FILEPATH_LEN]; /* Browser executable */

/*
 * A variable to tell us if the browser is really netscape and not Mosaic.
 */
static zbool ReallyNetscape = FALSE;



void
dm_Init ()
{
	stbl vtable = usy_g_stbl ("ui$variable_table");

	usy_c_indirect (vtable, "helppath", HelpPath, SYMT_STRING,
			CFG_SEARCHPATH_LEN);
#ifdef MOSAIC_COMMAND
	strcpy (BrowserPath, MOSAIC_COMMAND);
#else
	strcpy (BrowserPath, "netscape");
#endif
	usy_c_indirect (vtable, "browserpath", BrowserPath, SYMT_STRING, 
			CFG_FILEPATH_LEN);
	usy_c_indirect (vtable, "mosaicpath", BrowserPath, SYMT_STRING, 
			CFG_FILEPATH_LEN);
/*
 * Give them a default helppath which includes a project subdirectory
 */
	sprintf (HelpPath, "%s/help,%s/help", GetProjDir (), GetLibDir ());
}


void
dm_Help (url)
char *url;
/*
 * Get a help window going with this URL.
 */
{
	char realurl[ URL_LEN ];
/*
 * Figure out what we are really looking at.
 */
	if (dm_FindURL (url, realurl))
	{
		msg_ELog (EF_DEBUG, "Found help entry %s at %s", url, realurl);
	} 
	else if (! dm_FindURL ("index", realurl))
	{
		msg_ELog (EF_PROBLEM, "Can't find help entry %s or %s", url,
			  "'index'");
		return;
	}
	else
	{
		msg_ELog (EF_INFO, "Help entry %s not found, using index, %s",
			  url, realurl);
	}
/*
 * Display it in our browser.  Check now to see which browser we're using,
 * hope we don't get confused, then run the appropriate tweaking.
 */
	ReallyNetscape = strstr (BrowserPath, "netscape") != 0;
	if (ReallyNetscape)
		dm_TweakNetscape (realurl);
	else
		dm_TweakMosaic (realurl);
}



void
dm_ExitHelp ()
/*
 * We could either kill Mosaic here if the user so desired it, or
 * just leave it running.  At the very least we clean up our temp file.
 */
{
	if (TFile[0])
	{
		unlink (TFile);
		TFile[0] = '\0';
	}
	MosPid = -1;
}



static int
dm_FindURL (url, realurl)
char *url, *realurl;
/*
 * Locate this URL and turn it into a full path name.
 */
{
	char temp[ URL_LEN ];
	char section[ URL_LEN ];
	char cwd[ URL_LEN ];
	char *c, *sharp;
/*
 * Be case-insensitive.
 */
	strcpy (temp, url);
	for (c = temp; *c; ++c) *c = tolower (*c);
/*
 * If this is some sort of network URL we don't mess with it.
 */
	if (! strncmp (temp, "http:", 5) || ! strncmp (temp, "gopher:", 7))
	{
		strcpy (realurl, url);
		return (TRUE);
	}
/*
 * OK, assume it's a file.  Copy it over and check for #'s, so we can trim
 * that part out.  Strip trailing spaces also.
 */
	c = temp + strlen(temp) - 1;
	while ((c >= temp) && isspace(*c))
		c--;
	*(++c) = '\0';
	if ((sharp = strchr (temp, '#')) != NULL)
	{
		*sharp = '\0';
		strcpy (section, sharp + 1);
	}
/*
 * The crux: can we find a file?
 */
	if (! FindFile (temp, HelpPath, realurl))
	{
		strcat (temp, ".html");
		if (! FindFile (temp, HelpPath, realurl))
			return (FALSE);
	}
	if (sharp)
	{
		strcat (realurl, "#");
		strcat (realurl, section);
	}
/*
 * The full URL (as opposed to a partial URL) explicitly specifies the
 * 'file' scheme, so that the file is found locally rather than appended 
 * to any current remote URL.  Likewise, it seems there's no way to keep
 * Mosaic from appending a file:<relative path> to the current file:<path>,
 * so we have to convert a relative path to absolute using our cwd.
 */
	if (realurl[0] == '/')	/* absolute */
		sprintf (temp, "file:%s", realurl);
	else
		sprintf (temp, "file:%s/%s", 
			 (char *)getcwd(cwd, (size_t)URL_LEN), realurl);
	strcpy (realurl, temp);
	return (TRUE);
}





static void
dm_StartBrowser (url)
char *url;
/*
 * Fire up a netscape or mosaic browser with this URL.
 */
{
	int i;
/*
 * Remove any existing temp file in case we're restarting
 */
	if (TFile[0])
	{
		unlink (TFile);
		TFile[0] = '\0';
	}
/*
 * Fire off a browser.
 */
	if ((MosPid = fork ()) == 0)
	{
		char *args[20];
	/*
	 * OK, we get to be the browser.  Clean up.
	 */
		for (i = 3; i < 20 /* xxx */ ; i++)
			close (i);
	/*
	 * Fix up args for the browser.
	 */
		i = 0;
		if (ReallyNetscape)
		{
			args[i++] = "netscape";
			args[i++] = "-ncols";
			args[i++] = "64";
			args[i++] = url;
		}
		else
		{
			args[i++] = "Mosaic";
			args[i++] = "-xrm";
			args[i++] = "useGlobalHistory:false";
			args[i++] = "-home";
			args[i++] = url;
		}
	/*
	 * Now see if we can make it go.
	 */
		args[i] = 0;
		if (BrowserPath[0])
		{
			execvp (BrowserPath, args);
			fprintf (stderr, "could not exec '%s'\n", BrowserPath);
		}
	/*
	 * Try some fallbacks according to the browser we were
	 * trying to start.
	 */
		if (ReallyNetscape)
		{
		    execvp ("netscape", args);
		    fprintf (stderr, "Exec of netscape failed!\n");
		}
		else
		{
		    execvp ("Mosaic", args);
		    execvp ("xmosaic", args);
		    fprintf (stderr, "Exec of Mosaic and xmosaic failed!\n");
		}
		_exit (1);
	}
}





static void
dm_TweakMosaic (url)
char *url;
/*
 * Get an existing Mosaic to show a new URL.  This module uses the "remote
 * control" mechanism described in:
 *
 * 	http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/remote-control.html
 *
 * Netscape, of course, uses a different protocol...
 */
{
	FILE *cfile;

	if (MosPid < 0)
	{
	    /* Mosaic hasn't been started yet, so start it. */
	    dm_StartBrowser (url);
	    return;
	}
/*
 * Create the control file if necessary.
 */
	if (! TFile[0])
		sprintf (TFile, "/tmp/Mosaic.%d", MosPid);
	if ((cfile = fopen (TFile, "w")) == NULL)
	{
		msg_ELog (EF_PROBLEM, "Unable to open %s", TFile);
		TFile[0] = '\0';
		return;
	}
	fprintf (cfile, "goto\n%s\n", url);
	fclose (cfile);
/*
 * Poke Mosaic.  If the poke fails and it looks like Mosaic exited,
 * try to start another one.
 */
	(void) waitpid (MosPid, (int *) 0, WNOHANG); /* zap zombies */
	if (kill (MosPid, SIGUSR1))
	{
		if (errno == ESRCH)
		{
			msg_ELog (EF_INFO, "Mosaic gone, restarting");
			dm_StartBrowser (url);
		}
		else
			msg_ELog (EF_PROBLEM, "Weird Mosaic poke status %s",
				  errno);
	}
}





static void
dm_TweakNetscape (url)
char *url;
/*
 * Poke the netscape browser.
 */
{
    char nscmd[URL_LEN];
    /*
     * Format up the funky command needed to make a currently running
     * netscape go to the new URL, and see if it works.  If it doesn't, try
     * starting our own.
     */
    sprintf (nscmd, "%s -raise -remote 'openURL(%s)'", BrowserPath, url);
    msg_ELog (EF_DEBUG, "%s", nscmd);
    if (system (nscmd) != 0)
    {
	msg_ELog (EF_INFO, "remote netscape failed, starting our own");
	dm_StartBrowser (url);
	return;
    }
}
