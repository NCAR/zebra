/*
 * Provide help using Mosaic.
 */
# include <unistd.h>
# include <sys/types.h>
# include <sys/wait.h>
# include <stdio.h>
# include <signal.h>
# include <errno.h>
# include <string.h>
# include <ctype.h>

# include <defs.h>
# include <message.h>
# include <dm.h>
# include "dm_vars.h"

# define URL_LEN (2 * CFG_FILEPATH_LEN)

/*
 * When Mosaic is running, we keep it's PID here.
 */
static int MosPid = -1;

static int dm_FindURL FP ((char *, char *));
static void dm_StartMosaic FP ((char *));
static void dm_TweakMosaic FP ((char *));

/*
 * The temporary file through which we send commands to Mosaic.
 * Try to keep track of it and remove it when finished.
 */
static char TFile[ CFG_FILEPATH_LEN ] = { '\0' };

static char HelpPath[CFG_SEARCHPATH_LEN]; /* Where are the helpfiles */
static char MosaicPath[CFG_FILEPATH_LEN]; /* Mosaic executable */


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
 * The full URL includes file and localhost, so that the URL is
 * found locally rather than at any current remote server.
 */
	sprintf (temp, "file://localhost%s%s", 
		 (realurl[0] == '/') ? "" : "/", realurl);
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
	if ((MosPid = fork ()) == 0)
	{
		char *args[10];
	/*
	 * OK, we get to be Mosaic.  Clean up and fire it off.
	 */
		for (i = 3; i < 20 /* xxx */ ; i++)
			close (i);
		i = 0;
		args[i++] = "Mosaic";
		args[i++] = "-xrm";
		args[i++] = "useGlobalHistory: false";
		args[i++] = "-home";
		args[i++] = url;
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
 */
{
	FILE *cfile;
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
