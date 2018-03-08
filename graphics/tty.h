/* 5/87 jc */
/*
 * The terminal tag structure.
 */
# define TTYBUFLEN	1024		/* Device buffer length		*/
struct tty_tag
{
	int	tty_channel;		/* Our channel/fd		*/
	int	tty_ef;			/* Our event flag		*/
	char	tty_buf[TTYBUFLEN];	/* Data buffer			*/
	char	*tty_bufp;		/* Pointer into tty_buf		*/
};
	
