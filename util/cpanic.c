/*
 * c_panic:
 *
 * A panic call for C programs that does a sprintf first...
 */
void
c_panic (fmt, a1, a2, a3, a4, a5)
char *fmt;
int a1, a2, a3, a4, a5;
{
	char line[133];
	extern ftl_panic ();

	sprintrmt (line, fmt, a1, a2, a3, a4, a5);
	printf ("C_PANIC: %s\n", line);
	abort ();
}
