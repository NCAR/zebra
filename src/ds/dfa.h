/*
 * Internal DFA declarations.
 */


# ifdef __STDC__
	void	dfa_AddOpenFile (int, int, void *);
	void	dfa_ForceClose (int);
	int	dfa_OpenFile (int, int, void **);
# else
	void	dfa_AddOpenFile ();
	void	dfa_ForceClose ();
	int	dfa_OpenFile ();
# endif
