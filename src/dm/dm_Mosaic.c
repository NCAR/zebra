/*
 * Provide help using Mosaic.
 */
# include <sys/types.h>
# include <sys/wait.h>
# include <stdio.h>
# include <signal.h>
# include <errno.h>

# include <defs.h>
# include <message.h>
# include "dm.h"
# include "dm_vars.h"


/*
 * When Mosaic is running, we keep it's PID here.
 */
static int MosPid = -1;

static int dm_FindURL FP ((char *, char *));
static void dm_StartMosaic FP ((char *));
static void dm_TweakMosaic FP ((char *));



void
dm_MosHelp (url)
char *url;
/*
 * Get a help window going with this URL.
 */
{
	char realurl[CFG_FILEPATH_LEN];
/*
 * Figure out what we are really looking at.
 */
	if (! dm_FindURL (url, realurl))
	{
		msg_ELog (EF_PROBLEM, "Can't find help entry %s", url);
		return;
	}
	msg_ELog (EF_DEBUG, "URL is '%s'", realurl);
/*
 * Display it.
 */
	if (MosPid > 0)
		dm_TweakMosaic (realurl);
	else
		dm_StartMosaic (realurl);
}





static int
dm_FindURL (url, realurl)
char *url, *realurl;
/*
 * Locate this URL and turn it into a full path name.
 */
{
	char temp[CFG_FILEPATH_LEN], *sharp, *strchr ();
	char section[CFG_FILEPATH_LEN];
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
 * that part out.
 */
	strcpy (temp, url);
	if (sharp = strchr (temp, '#'))
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
		execvp ("Mosaic", args);
		execvp ("xmosaic", args);
		fprintf (stderr, "Exec of Mosaic failed!\n");
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
	char fname[80];
/*
 * Create the control file.
 */
	sprintf (fname, "/tmp/Mosaic.%d", MosPid);
	if ((cfile = fopen (fname, "w")) == NULL)
	{
		msg_ELog (EF_PROBLEM, "Unable to open %s", fname);
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
