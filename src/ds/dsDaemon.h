/*
 * Data store daemon-specific definitions.
 */
/* $Id: dsDaemon.h,v 1.1 1990-11-02 08:58:15 corbet Exp $ */
/*
 * The platform and data tables, via pointer.
 */
Platform *PTable;
DataFile *DFTable;

/*
 * The default data directory.
 */
char DefDataDir[80];


# ifdef __STDC__
	void InitSharedMemory (void);
	Platform *dt_NewPlatform (char *);
	Platform *dt_FindPlatform (char *, int);
	DataFile *dt_NewFile (void);
	void dt_FreeDFE (DataFile *);
	void dt_AddToPlatform (Platform *, DataFile *, int);
	void dc_DefPlatform (char *);
# else
	void InitSharedMemory ();
	Platform *dt_NewPlatform ();
	Platform *dt_FindPlatform ();
	DataFile *dt_NewFile ();
	void dt_FreeDFE ();
	void dt_AddToPlatform ();
	void dc_DefPlatform ();
# endif
