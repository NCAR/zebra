/*
 * Summagraphics graphics tablet interface module
 *
 * (adapted from Forrest's gtab.c)
 */

# include <errno.h>
# include <fcntl.h>
# ifdef AIXV3
# include <termios.h>
# else
# include <sys/termios.h>
# endif

/*
 * Beware of /dev/ttya with getty running on it
 */
# define TABLET_PORT	"/dev/ttya"

struct termios	Term_io;
int	Port;




tab_point (button, x, y)
int *button, *x, *y;
/*
 * Get the next button click from the tablet, returning the button 
 * number (top=1, right=2, bottom=4, left=8), x position, and y position.
 * Return button number as zero if we get a bad point.
 */
{
	char	string[20];
/*
 * Flush the buffer, then read
 */
	ioctl (Port, TCFLSH, TCIFLUSH);
	read (Port, string, 20);
/*
 * The format used by the tablet is @B+XXXX+YYYY<RET> where B is the button
 * number, XXXX is the x position, and YYYY is the y position.
 */
	if (sscanf (string, "@%x+%x+%x", button, x, y) != 3)
		*button = 0;
}




int
tab_init ()
/*
 * Initialize the Summagraphics tablet on TABLET_PORT.  Return zero if
 * we fail
 */
{
/*
 * Open the port
 */
	if ((Port = open (TABLET_PORT, O_RDWR)) < 0)
		return (0);
/*
 * get the termio characterstics for the port
 */
	if ((ioctl (Port, TCGETS, &Term_io)) < 0)
		return (0);
/*
 * Set 9600 baud, 7 data bits, even parity, 2 stop bits
 */
	Term_io.c_cflag = (Term_io.c_cflag & ~CBAUD) | B9600;
	Term_io.c_cflag = (Term_io.c_cflag & ~CSIZE) | CS7;
	Term_io.c_cflag |= PARENB;
	Term_io.c_cflag &= ~PARODD;
	Term_io.c_cflag = Term_io.c_cflag | CSTOPB;
/*
 * Set up miscellaneous port parameters
 */
	Term_io.c_cflag |= CRTSCTS;	/* enable RTS/CTS flow control */

	Term_io.c_iflag &= ~(ICRNL|INLCR); /* do not map cr to nl on input */
	Term_io.c_oflag &= ~OPOST;	/* defeat output post processing */

	Term_io.c_lflag &= ~ISIG;	/* turn off signals */
	Term_io.c_lflag &= ~ICANON;	/* non-canonical mode */
	Term_io.c_lflag &= ~ECHO;	/* turn off echo */

	Term_io.c_cc[VMIN] = 12;	/* important read time modes */
	Term_io.c_cc[VTIME] = 1;	/* check the termio man page */
/*
 * Store the newly set termio characterstics for the port
 */
	if (ioctl (Port, TCSETS, &Term_io) < 0)
		return (0);
	else
		return (1);
}
