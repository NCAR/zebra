/*
 * $Id: dsPrivate.h,v 1.1 1990-10-22 16:04:11 corbet Exp $
 *
 * Data store information meant for DS eyes only.
 */

/*
 * The file types.
 */
typedef enum {
	FTUnknown = 0,
	FTNetCDF = 1,
	FTMDA = 2,
	/* ... */
} FileType;


