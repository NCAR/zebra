/*
 * This (now misnamed) module provides help information using the Mosaic or
 * netscape browsers.
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
 * When the browser is running, we keep it's PID here.
 */
static int MosPid = -1;

static int dm_FindURL FP ((char *, char *));
static void dm_StartMosaic FP ((char *));
static void dm_TweakMosaic FP ((char *));
static void dm_TweakNetscape FP ((char *));

/*
 * The temporary file through which we send commands to Mosaic.
 * Try to keep track of it and remove it when finished.
 */
static char TFile[ CFG_FILEPATH_LEN ] = { '\0' };

static char HelpPath[CFG_SEARCHPATH_LEN]; /* Where are the helpfiles */
static char MosaicPath[CFG_FILEPATH_LEN]; /* Mosaic executable */

/*
 * A variable to tell us if "Mosaic" is really netscape.
 */
static zbool ReallyNetscape = FALSE;



void
dm_Init ()
{
	stbl vtable = usy_g_stbl ("ui$variable_table");

	usy_c_indirect (vtable, "helppath", HelpPath, SYMT_STRING,
			CFG_SEARCHPATH_LEN);
#ifdef MOSAIC_COMMAND
	strcpy (MosaicPath, MOSAIC_COMMAND);
#else
	MosaicPath[0] = '\0';
#endif
	usy_c_indirect (vtable, "mosaicpath", MosaicPath, SYMT_STRING, 
			CFG_FILEPATH_LEN);
/*
 * Give them a default helppath which includes a project subdirectory
 */
	sprintf (HelpPath, "%s/help,%s/help", GetProjDir (), GetLibDir ());
}


void
dm_MosHelp (url)
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
 * Display it.
 */
	if (MosPid > 0)
		dm_TweakMosaic (realurl);
	else
		dm_StartMosaic (realurl);
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
 * If this is some sort of network URL we don't mess with it.
 */
	if (! strncmp (url, "http:", 5) || ! strncmp (url, "gopher:", 7))
	{
		strcpy (realurl, url);
		return (TRUE);
	}
/*
 * OK, assume it's a file.  Copy it over and check for #'s, so we can trim
 * that part out.  Strip trailing spaces also.
 */
	strcpy (temp, url);
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
dm_StartMosaic (url)
char *url;
/*
 * Fire up mosaic with this URL.
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
 * Fire off a browser.  Check now to see which it is, and hope we don't
 * get confused.
 */
	ReallyNetscape = strstr (MosaicPath, "netscape") != 0;
	if ((MosPid = fork ()) == 0)
	{
		char *args[10];
	/*
	 * OK, we get to be Mosaic.  Clean up.
	 */
		for (i = 3; i < 20 /* xxx */ ; i++)
			close (i);
	/*
	 * Fix up args for our browser.
	 */
		i = 0;
		if (ReallyNetscape)
		{
			args[i++] = "netscape";
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
		if (MosaicPath[0])
		{
			execvp (MosaicPath, args);
			fprintf (stderr, "could not exec '%s'\n", MosaicPath);
		}
		execvp ("Mosaic", args);
		execvp ("xmosaic", args);
		fprintf (stderr, "Exec of Mosaic and xmosaic failed!\n");
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
/*
 * Netscape?  Do it their way.
 */
	if (ReallyNetscape)
	{
		dm_TweakNetscape (url);
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
			dm_StartMosaic (url);
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
 * See if it died.
 */
	if (waitpid (MosPid, (int *) 0, WNOHANG) > 0)
	{
		msg_ELog (EF_INFO, "Netscape died, restarting");
		dm_StartMosaic (url);
		return;
	}
/*
 * It's still there.  Format up the funky command needed to make it go to the
 * new URL.
 */
	sprintf (nscmd, "%s -remote 'openURL(%s)'", MosaicPath, url);
	system (nscmd);
}
