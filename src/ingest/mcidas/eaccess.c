/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 EACCESS.C 7-Sep-93,10:46:02,`RUSSD' Initial release to AIX (4069)       */
/* 2 EACCESS.C 5-Oct-93,8:11:16,`USER' Released for McIDAS-X Only            */
/* 3 EACCESS.C 1-Mar-94,10:03:26,`RUSSD' Standardize function names          */
/* 4 EACCESS.C 31-Mar-94,22:15:22,`BARRYR' Add proprietary statement         */
/* 5 EACCESS.C 31-Mar-94,23:44:10,`BARRYR' Add proprietary statement         */
/* 6 EACCESS.C 2-May-94,15:57:34,`USER' Released for McIDAS-X Only           */
/* 7 EACCESS.C 9-May-95,18:26:16,`DWS' MCPATH phase 3 (5429)                 */
/* 8 EACCESS.C 6-Jun-95,12:06:52,`RICKK' Updated revision history cards      */
/*      moving from aix to com.                                              */
/* 9 EACCESS.C 6-Jun-95,14:49:54,`USER' Released for McIDAS-X Only           */
/**** McIDAS Revision History *** */

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

#include "mcidas.h"
#include "mcidasp.h"
#include "m0file.h"


/*
*| Name:
*|      eaccess - determine file accessibility
*|
*| Interface:
*|      #include <sys/types.h>
*|      #include <sys/stat.h>
*|      #include <unistd.h>
*|      #include "mcidas.h"
*|
*|      int
*|      eaccess(const char *path, int amode, int type)
*|
*| Input:
*|      path    - pathname to check
*|      amode   - bit mask of permissions to check
*|      type    - type of file to check
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|       0      - success, file is accessible in the desired way
*|      -1      - failure, file is not accessible in the desired way
*|
*| Remarks:
*|      This function indicates whether the given pathname is of the
*|      given type and is accessible in the given way (amode), using the
*|      effective uid and gid of the process.  By contrast, the access()
*|      system call uses the real uid and gid, which don't necessarily
*|      reveal what the process can actually do.
*|
*|      The amode argument is a bitwise OR of the permissions to be
*|      checked: R_OK, W_OK, X_OK, F_OK, defined in <unistd.h>.
*|
*|      The type argument is ignored if zero, but otherwise causes
*|      eaccess() to check whether the path is one of S_IFREG, S_IFDIR,
*|      S_IFBLK, S_IFCHR, S_IFIFO, S_IFSOCK, S_IFLNK, etc., defined
*|      in <sys/stat.h>.  If type is nonzero and the path is not of
*|      the specified type, eaccess() fails.  Note that eaccess() will
*|      follow symbolic links unless type == S_IFLNK.
*|
*|      This function sets errno on failure.
*|
*| Categories: 
*|      file 
*/

int 
eaccess(const char *path, int amode, int type)
{
	struct stat     st;

#ifdef S_IFLNK
	if (type == S_IFLNK)
	{
		if (lstat(path, &st) == -1)
			return -1;
	}
	else
#endif
	if (stat(path, &st) == -1)
	{
		return -1;
	}
	if (type != 0 && (st.st_mode & S_IFMT) != type)
	{
		/* what is an appropriate return value? */
		errno = EACCES;
		return -1;
	}

	return M0eaccess0(&st, amode);
}


/*
*| Name:
*|      M0eaccess0 - determine file accessibility based on stat buffer
*|
*| Interface:
*|      #include <unistd.h>
*|      #include "m0file.h"
*|
*|      int
*|      M0eaccess0(const struct stat *sb, int amode)
*|
*| Input:
*|      sb      - buffer initialized by a call to stat()
*|      amode   - bit mask of permissions to check
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|       0      - success, file is accessible in the desired way
*|      -1      - failure, file is not accessible in the desired way
*|
*| Remarks:
*|      This function indicates whether the given stat buffer is from
*|      a file that is accessible in the given way (amode), using the
*|      effective uid and gid of the process.  By contrast, the access()
*|      system call uses the real uid and gid, which don't necessarily
*|      reveal what the process can actually do.
*|
*|      The amode argument is a bitwise OR of the permissions to be
*|      checked: R_OK, W_OK, X_OK, F_OK, defined in <unistd.h>.
*|
*|      This function sets errno on failure.
*|
*| Categories: 
*|      file 
*/

#ifdef __EMX__
int 
M0eaccess0(const struct stat *sb, int amode)
{
	if (amode == 0)
		return 0;

	/* check read permission if requested */

	if ((amode & R_OK) && !(sb->st_mode & S_IREAD))
	{
		errno = EACCES;
		return -1;
	}

	/* check write permission if requested */

	if ((amode & W_OK) && !(sb->st_mode & S_IWRITE))
	{
		errno = EACCES;
		return -1;
	}

	/* check execute permission if requested */

	if ((amode & X_OK) && !(sb->st_mode & S_IEXEC))
	{
		errno = EACCES;
		return -1;
	}

	return 0;
}
#else
int 
M0eaccess0(const struct stat *sb, int amode)
{
	static int	first = 1;
	static int      uid;
	static int      gid;

	int             mask;

	if (first)
	{
		first = 0;
		uid = geteuid();
		gid = getegid();
	}

	if (amode == 0)
		return 0;

#define MKMASK(um,gm,om)					\
		if(uid == sb->st_uid)				\
		{						\
			mask = (um);				\
		}						\
		else						\
		if(gid == sb->st_gid || ingroupset(sb->st_gid))	\
		{						\
			mask = (gm);				\
		}						\
		else						\
		{						\
			mask = (om);				\
		}
	

	/* check read permission if (a) requested, and (b) nonroot */

	if ((amode & R_OK) && uid)
	{
		MKMASK(S_IRUSR, S_IRGRP, S_IROTH);

		if (!(sb->st_mode & mask))
		{
			errno = EACCES;
			return -1;
		}
	}

	/* check write permission if (a) requested, and (b) nonroot */

	if ((amode & W_OK) && uid)
	{
		MKMASK(S_IWUSR, S_IWGRP, S_IWOTH);

		if (!(sb->st_mode & mask))
		{
			errno = EACCES;
			return -1;
		}
	}

	/* check execute permission if requested */

	if (amode & X_OK)
	{
		if(uid == 0)
		{
			mask = (S_IXUSR | S_IXGRP | S_IXOTH);
		}
		else
		{
			MKMASK(S_IXUSR, S_IXGRP, S_IXOTH);
		}

		if (!(sb->st_mode & mask))
		{
			errno = EACCES;
			return -1;
		}
	}

	return 0;
}
#endif
