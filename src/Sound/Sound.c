/*
 * Send sound files to the audio port.
 */
static char *rcsid = "$Id: Sound.c,v 2.1 1991-09-13 15:01:58 corbet Exp $";
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
# include <sys/types.h>
# include <unistd.h>
# include <fcntl.h>
# include <errno.h>
# include "defs.h"
# include "copyright.h"
# include "../config/config.h"
# include "../include/message.h"
# include "../include/timer.h"


int IMessage ();
int Enabled = FALSE;
int SoundDev = -1;
stbl Sounds;

# define MAXSAVE 20000		/* Biggest sound we actually save	*/

# define CLOSEDELAY 2*INCFRAC	/* Keep device open this long		*/
/*
 * Structure for saved sounds.
 */
typedef struct 
{
	int ss_len;		/* How long it is.			*/
	char *ss_data;		/* The sound data			*/
} SavedSound;



main ()
{
/*
 * Hook into the message system.
 */
	if (! msg_connect (IMessage, "Sound"))
	{
		printf ("Unable to connect to message handler\n");
		exit (1);
	}
/*
 * Symbol table setup.
 */
 	usy_init ();
	Sounds = usy_c_stbl ("sounds");
/*
 * Wait for things.
 */
	msg_await ();
}




IMessage (msg)
struct message *msg;
{
	if (msg->m_proto == MT_SOUND)
	{
		if (msg->m_len == 1)
			Enabled = *msg->m_data;
		else if (Enabled)
			DoSound (msg->m_data);
	}
	else if (msg->m_proto == MT_MESSAGE)
	{
		struct mh_template *tm = (struct mh_template *) msg->m_data;
		if (tm->mh_type == MH_SHUTDOWN)
			exit (0);
	}
	return (0);
}




DoSound (file)
char *file;
{
	char fname[120];
	static char sbuf[2000];
	int fd, dev, len;
/*
 * Make sure we have a device.
 */
	if (! OpenDev ())
		return;
/*
 * If the sound is already in our table, just blast it out.
 */
	if (DoLoadedSound (file))
		return;
/*
 * Open up the file.
 */
	sprintf (fname, "%s/audio/%s.au", LIBDIR, file);
	if ((fd = open (fname, O_RDONLY, 0)) < 0)
	{
		msg_ELog (EF_INFO, "Error %d opening sound file '%s'", errno,
			fname);
		return;
	}
	SaveSound (file, fd);
/*
 * Now send out the sound.
 */
	while ((len = read (fd, sbuf, 2000)) > 0)
		write (SoundDev, sbuf, len);
	close (fd);
}





DoLoadedSound (sound)
char *sound;
/*
 * If this sound is already loaded, send it out without opening the file
 * again.
 */
{
	int type;
	SValue v;
	SavedSound *s;
/*
 * See if it's there.
 */
	if (! usy_g_symbol (Sounds, sound, &type, &v))
		return (FALSE);
/*
 * OK, it is.  Ship it out to the device.
 */
	s = (SavedSound *) v.us_v_ptr;
	write (SoundDev, s->ss_data, s->ss_len);
}






SaveSound (name, fd)
char *name;
int fd;
/*
 * Consider saving this sound.
 */
{
	int len;
	SavedSound *s;
	SValue v;
/*
 * See how long the file is.  If it's over our max, we just pull it from
 * the file each time.
 */
	len = lseek (fd, 0, SEEK_END);
	(void) lseek (fd, 0, SEEK_SET);		/* back to beginning	*/
	if (len > MAXSAVE)
		return;
/*
 * OK, we'll keep it.  Allocate our memory, and pull it in.
 */
	s = ALLOC (SavedSound);
	s->ss_data = malloc (len);
	s->ss_len = len;
	read (fd, s->ss_data, len);
	(void) lseek (fd, 0, SEEK_SET);
/*
 * Stash it into our symbol table.
 */
	v.us_v_ptr = (char *) s;
	usy_s_symbol (Sounds, name, SYMT_POINTER, &v);
}




void
TimeOut ()
/*
 * Close the audio device.
 */
{
	close (SoundDev);
	SoundDev = -1;
}





OpenDev ()
/*
 * Open up the sound device.
 */
{
	static int First = TRUE;
/*
 * Get the device.
 */
	if (SoundDev < 0 && (SoundDev = open ("/dev/audio", O_WRONLY, 0)) < 0)
	{
		if (First)
			msg_ELog (EF_INFO, "Error %d on /dev/audio", errno);
		First = FALSE;
       		return (FALSE);
	}
/*
 * Set up a timeout to close it again.
 */
	tl_AllCancel ();	/* Lazy */
	tl_AddRelativeEvent (TimeOut, 0, CLOSEDELAY, 0);
	return (TRUE);
}
