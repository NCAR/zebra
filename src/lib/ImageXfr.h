/*
 * Image transfer global stuff.
 */

# ifdef __STDC__
	struct _ix_desc * IX_HookIn (int, char *, int *, int *, int *,char **);
	struct _ix_desc * IX_Create (int, int, int, int, int, char **);
	int	IX_GetWriteFrame (struct _ix_desc *, char **);
	void	IX_SendFrame (struct _ix_desc *, int, time *, RGrid *,
			Location *, ScaleInfo *, int, int, int, int);
	int	IX_GetReadFrame (struct _ix_desc *, int, char **, time *,
			RGrid *, Location *, ScaleInfo *, int *, int *,
			int *, int *);
	void	IX_ReleaseFrame (struct _ix_desc *, int);
	void	IX_Detach (struct _ix_desc *);
	void	IX_LockMemory (struct _ix_desc *);
# else
	struct _ix_desc * IX_HookIn ();
	struct _ix_desc * IX_Create ();
	int	IX_GetWriteFrame ();
	void	IX_SendFrame ();
	int	IX_GetReadFrame ();
	void	IX_ReleaseFrame ();
	void	IX_Detach ();
	void	IX_LockMemory ();
# endif

