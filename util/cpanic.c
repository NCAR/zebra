/*
 * c_panic:
 *
 * A panic call for C programs that does a sprintf first...
 */
# ifdef VMS
#	define ERRARGS		args
#	define SPRINTARGS	&args
# else
#	define ERRARGS		a1, a2, a3, a4, a5
#	define SPRINTARGS	a1, a2, a3, a4, a5
# endif


c_panic (fmt, ERRARGS)
char *fmt;
int ERRARGS;
{
	char line[133];
	extern ftl_panic ();

	sprintrmt (line, fmt, SPRINTARGS);
# ifdef VMS
	lib$signal (ftl_panic, 1, descr (line));
# else
	printf ("C_PANIC: %s\n", line);
	abort ();
# endif
}
