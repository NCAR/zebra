/*
 * Replace McIDAS's sendtext, just writing to the event logger instead
 */
# include <message.h>

#if defined(UNDERSCORE)
  void sendtext_(char *, long  *, long *, short );
#else
  void sendtext(char * , long *, long *, short );
#endif

#if defined(UNDERSCORE)
  void sendtext_(char *ctext, long  *window, long *color, short lentext)
#else
  void sendtext(char *ctext , long *window, long *color, short lentext)
#endif

/*
   Write ctext to current window 
*/
{
	char	buf[256];
	int	reallen = lentext - 13;
	strncpy (buf, ctext + 12, reallen);
	buf[reallen] = '\0';
	msg_ELog (EF_DEBUG, buf);
}
