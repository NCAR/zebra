/*
 * Low level event definitions.
 */

# ifdef __STDC__
	void lle_AddFD (int fd, void (*proc) ());
# else
	void lle_AddFD ();
# endif
