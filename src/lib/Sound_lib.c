/*
 * Interface to the sound generator.
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
