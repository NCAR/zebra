/* 12/87 jc */
# include "param.h"
# ifdef DEV_COMTAL
/*
 * Simply a repackaging of Comtal tablet communications routines.
 */
# include <iodef.h>
# include <ssdef.h>
# include <stdio.h>



zb_dimode ()
/*
 * Enter dump image mode.
 */
{
	zb_scom ('D');
}




zb_nodimode ()
/*
 * Leave dump image mode.
 */
{
	zb_scom ('U');
}




/*
 * Character definitions.
 */
# define C_ENQ	'\005'		/* Enquire character */
# define C_ACK	'\006'		/* Acknowledge character */
# define C_SCR	'S'		/* "get screen info" command	*/
# define C_WTHRU 'W'		/* Write through command. */
# define C_CESC	'\t'		/* What the COMTAL calls ESC */
# define C_ESC  '\033'		/* What the rest of the world calls ESC */
# define C_EOT  '\032'		/* EOT indicator */
# define C_DEL	'\177'		/* DELETE character */
# define C_BS	'\b'		/* Backspace character */

/*
 * Delay time before sending C_SCR.
 */
# define DELAY	"0 00:00:00.05"



# define ___ 0
# define s_error(s) (((s) & 0x1) == 0)


int
zb_info (zoom, image, x, y)
int *zoom, *image, *x, *y;
/*
 * Return the comtal origin/zoom info.
 */
{
	int status, ef, t_buf[2];
	short iosb[4], channel;
	char tab_str[80];
/*
 * Get a channel to the COMTAL tablet controller.
 */
	status = sys$assign (descr ("COMTAL_TABLET"), &channel, ___, ___);
	if (s_error (status))
	{
		ui_error (" Error assigning to COMTAL tablet");
		errmes (&status);
		return (FALSE);
	}
/*
 * Get an event flag.
 */
	lib$get_ef (&ef);
/*
 * Send out the enquire character.
 */
	status = sys$qiow (ef, channel,
		IO$_TTYREADPALL | IO$M_NOECHO | IO$M_TIMED | IO$M_PURGE,
		iosb, ___, ___, tab_str, 1, 10, ___, &C_ENQ, 1);
	if (s_error (status))
	{
		ui_error (" Enquire QIO status error");
		errmes (&status);
		return (FALSE);
	}
	if (iosb[0] == SS$_TIMEOUT)
	{
		ui_error (" Graphic tablet timeout!  Try reseting tablet...");
		return (FALSE);
	}
	else if (s_error (iosb[0]))
	{
		ui_error (" Tablet ack read error");
		status = iosb[0];
		errmes (&status);
	}
	/*
	printf ("Tablet ack return: %d bytes, char is %X\n", iosb[1],
		tab_str[0]);
	if (tab_str[0] != C_ACK)
		printf ("HELP! tablet returned %x\n", tab_str[0]);
	*/
/*
 * Delay before sending the screen info command.
 */
	sys$bintim (descr (DELAY), t_buf);
	status = sys$setimr (ef, t_buf, ___, ___);
	if (s_error (status))
	{
		ui_error (" SETIMR error return");
		errmes (&status);
		return (FALSE);
	}
	sys$waitfr (ef);
/*
 * Get the info.
 */
	status = sys$qiow (ef, channel,
		IO$_TTYREADPALL | IO$M_NOECHO | IO$M_TIMED | IO$M_PURGE,
		iosb, ___, ___, tab_str, 80, 10, ___, &C_SCR, 1);
	if (s_error (status))
	{
		ui_error (" Screen QIO status error");
		errmes (&status);
		return (FALSE);
	}
	if (iosb[0] == SS$_TIMEOUT)
	{
		ui_error (" Graphic tablet info timeout!  Try reseting tablet...");
		return (FALSE);
	}
	else if (s_error (iosb[0]))
	{
		ui_error (" Tablet info read error");
		status = iosb[0];
		errmes (&status);
	}
	tab_str[iosb[1]] = '\0';
/*
 * Try to decode the info.
 */
	if (sscanf (tab_str, "(%d,%d,%d,%d)", image, x, y, zoom) != 4)
	{
		ui_error (" Ui_Error parsing tablet info: ");
		ui_error (tab_str);
	}
/*
 * Clean up.
 */
	lib$free_ef (&ef);
	sys$dassgn (channel);
	return (TRUE);
}





int
zb_scom (c)
char c;
/*
 * Perform a comtal tablet simple (one character) command.
 */
{
	int status, ef, t_buf[2];
	short iosb[4], channel;
	char tab_str[80];
/*
 * Get a channel to the COMTAL tablet controller.
 */
	status = sys$assign (descr ("COMTAL_TABLET"), &channel, ___, ___);
	if (s_error (status))
	{
		ui_error (" Error assigning to COMTAL tablet");
		errmes (&status);
		return (FALSE);
	}
/*
 * Get an event flag.
 */
	lib$get_ef (&ef);
/*
 * Send out the enquire character.
 */
	status = sys$qiow (ef, channel,
		IO$_TTYREADPALL | IO$M_NOECHO | IO$M_TIMED | IO$M_PURGE,
		iosb, ___, ___, tab_str, 1, 3, ___, &C_ENQ, 1);
	if (s_error (status))
	{
		ui_error (" Enquire QIO status error");
		errmes (&status);
		return (FALSE);
	}
	if (iosb[0] == SS$_TIMEOUT)
	{
		ui_error (" Graphic tablet timeout!  Try reseting tablet...");
		return (FALSE);
	}
	else if (s_error (iosb[0]))
	{
		ui_error (" Tablet ack read error");
		status = iosb[0];
		errmes (&status);
	}
	/*
	printf ("Tablet ack return: %d bytes, char is %X\n", iosb[1],
		tab_str[0]);
	*/
/*
 * Delay before sending the command.
 */
	sys$bintim (descr (DELAY), t_buf);
	status = sys$setimr (ef, t_buf, ___, ___);
	if (s_error (status))
	{
		ui_error (" SETIMR error return");
		errmes (&status);
		return (FALSE);
	}
	sys$waitfr (ef);
/*
 * Get the info.
 */
	status = sys$qiow (ef, channel,
		IO$_TTYREADPALL | IO$M_NOECHO | IO$M_TIMED | IO$M_PURGE,
		iosb, ___, ___, tab_str, 80, 10, ___, &c, 1);
	if (s_error (status))
	{
		ui_error (" Command QIO status error");
		errmes (&status);
		return (FALSE);
	}
	if (iosb[0] == SS$_TIMEOUT)
	{
		ui_error (" Graphic tablet cmd timeout!  Try reseting tablet...");
		return (FALSE);
	}
	else if (s_error (iosb[0]))
	{
		ui_error (" Tablet cmd read error");
		status = iosb[0];
		errmes (&status);
	}
/*
 * The return character should be the same as what we sent.
 */
	if (c != tab_str[0])
		ui_error (" WARNING: comtal tablet cmd/return mismatch");
/*
 * Clean up.
 */
	lib$free_ef (&ef);
	sys$dassgn (channel);
	return (TRUE);
}



zb_wcmd (s)
char *s;
/*
 * Write a command directly through to the COMTAL.
 */
{
	int status, ef, t_buf[2];
	char c;
	short iosb[4], channel;
	char tab_str[80];
/*
 * Get a channel to the COMTAL tablet controller.
 */
	status = sys$assign (descr ("COMTAL_TABLET"), &channel, ___, ___);
	if (s_error (status))
	{
		ui_error (" Error assigning to COMTAL tablet");
		errmes (&status);
		return (FALSE);
	}
/*
 * Get an event flag.
 */
	lib$get_ef (&ef);
/*
 * Send out the enquire character.
 */
	status = sys$qiow (ef, channel,
		IO$_TTYREADPALL | IO$M_NOECHO | IO$M_TIMED | IO$M_PURGE,
		iosb, ___, ___, tab_str, 1, 10, ___, &C_ENQ, 1);
	if (s_error (status))
	{
		ui_error (" Enquire QIO status error");
		errmes (&status);
		return (FALSE);
	}
	if (iosb[0] == SS$_TIMEOUT)
	{
		ui_error (" Graphic tablet timeout!  Try reseting tablet...");
		return (FALSE);
	}
	else if (s_error (iosb[0]))
	{
		ui_error (" Tablet ack read error");
		status = iosb[0];
		errmes (&status);
	}
	/*
	printf ("Tablet ack return: %d bytes, char is %X\n", iosb[1],
		tab_str[0]);
	*/
/*
 * Delay before sending the command.
 */
	sys$bintim (descr (DELAY), t_buf);
	status = sys$setimr (ef, t_buf, ___, ___);
	if (s_error (status))
	{
		ui_error (" SETIMR error return");
		errmes (&status);
		return (FALSE);
	}
	sys$waitfr (ef);
/*
 * Send the write-through command.
 */
	status = sys$qiow (ef, channel,
		IO$_TTYREADPALL | IO$M_NOECHO | IO$M_TIMED | IO$M_PURGE,
		iosb, ___, ___, tab_str, 80, 10, ___, &C_WTHRU, 1);
	if (s_error (status))
	{
		ui_error (" Write-through QIO status error");
		errmes (&status);
		return (FALSE);
	}
	if (iosb[0] == SS$_TIMEOUT)
	{
		ui_error (" Graphic tablet write-through timeout!  Try reseting tablet...");
		return (FALSE);
	}
	else if (s_error (iosb[0]))
	{
		ui_error (" Tablet write-through read error");
		status = iosb[0];
		errmes (&status);
	}
/*
 * The return character should be the same as what we sent.
 */
	if (tab_str[0] != C_WTHRU)
		ui_error (" WARNING: comtal tablet cmd/return mismatch");
/*
 * Copy characters over.
 */
	for (;;)
	{
		if (c != C_EOT)
			c = *s++;
	/*
	 * Remap certain characters.
	 */
		switch (c)
		{
		  case C_ESC:
			c = C_CESC;
			break;
		  case C_DEL:
			c = C_BS;
			break;
		}
	/*
	 * Send the character to the tablet.  Note that we send things one
	 * character at a time, to avoid burying the Z80.
	 */
		status = sys$qiow (ef, channel, IO$_WRITEVBLK | IO$M_NOFORMAT,
			iosb, ___, ___, &c, 1, ___, ___, ___, ___);
		if (s_error (status))
		{
			ui_error (" Character send error\n");
			errmes (&status);
			break;
		}
		else if (s_error (iosb[0]))
		{
			ui_error (" Character send IOSB error");
			status = iosb[0];
			errmes (&status);
			break;
		}
	/*
	 * If we are at the end of our string, send the EOT char.
	 */
		if (c == C_EOT)
			break;
		else if (! *s)
			c = C_EOT;
	}
/*
 * Clean up.
 */
	lib$free_ef (&ef);
	status = sys$dassgn (channel);
	if (s_error (status))
	{
		ui_error (" TABLET channel dassgn error");
		errmes (&status);
	}
	return (TRUE);
}




# endif /* DEV_COMTAL */
