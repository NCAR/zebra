/*
 * Interface to the sound generator.
 */
static char *rcsid = "$Id: Sound_lib.c,v 2.1 1991-09-13 15:01:58 corbet Exp $";
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
# ifdef SUNOS_4_1
# include <unistd.h>
# else
# include <sys/file.h>
# endif
# include "defs.h"
# include "../include/message.h"






DoSound (sound)
char *sound;
/*
 * Fire off a sound.
 */
{
	if (access ("/dev/audio", F_OK))
		return;
	msg_send ("Sound", MT_SOUND, FALSE, sound, strlen (sound) + 1);
}
