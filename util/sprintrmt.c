/* sprintrmt(buf,arg) does what sprintf(buf,"%r",arg) used to
 * 9/93 - calls to this routine were removed from ui, suds, robot which now 
 * use the variable argument C routines.  Any other programs still calling
 * this routine should follow suit
 */

# ifdef VMS
	void
	sprintrmt(buf, fmt, arg)
	char   *buf, *fmt;
	register char **arg;
	{	/* this silly routine should really be calling _doprnt */
		register char  *t1,
	                   *t2, *t3;
		int sprintf ();

		t1 = *--arg;
		*arg = fmt;
		t2 = *--arg;
		*arg = buf;
		t3 = *--arg;
		*arg = (char *) 255;
		lib$callg (arg, sprintf);
		*arg++ = t3;
		*arg++ = t2;
		*arg++ = t1;
	}
# else
	void
	sprintrmt(buf, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
	char   *buf, *fmt;
	int	a1, a2, a3, a4, a5, a6, a7, a8, a9, a10;
	{
		sprintf (buf, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
	}
# endif
