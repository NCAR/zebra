/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 M0REDIR.C 1-May-95,16:25:48,`DWS' REDIRECT entry resolver (5402)        */
/* 2 M0REDIR.C 9-May-95,18:18:40,`DWS' MCPATH phase 3 (5429)                 */
/* 3 M0REDIR.C 6-Jun-95,15:07:00,`USER' Released                             */
/* 4 M0REDIR.C 19-Feb-96,15:58:36,`DWS' reglue: modified file                */
/* 5 M0REDIR.C 20-Feb-96,11:58:04,`USER' Released                            */
/* 6 M0REDIR.C 22-Mar-96,11:52:42,`DWS' reglue: modified file                */
/* 7 M0REDIR.C 25-Mar-96,13:56:14,`USER' Released                            */
/**** McIDAS Revision History *** */

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#include "mcidas.h"
#include "mcidasp.h"
#include "m0file.h"

/**
*| Name:
*|	M0redirect - return REDIRECT path for a file name
*|
*| Interface:
*|	#include "mcidasp.h"
*|
*|	const char *
*|	M0redirect(const char *file)
*|
*| Input:
*|	file	- name of a file
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	  0	- no redirection for the given file
*|	<>0	- a pointer to the name of the directory for the file.
*|
*| Remarks:
*|	Redirect only is checked, not MCPATH.
*|
*|	If the file name is not a base name, M0redirect() returns a null
*|	pointer and does not check the redirect table.
*|
*|	The return pointer may point to a static data area overwritten
*|	on each call.
*|
*| Categories:
*|	FILE
*/

const char     *
M0redirect(const char *file)
{
	const M0Redirect     *list;
	const M0Redirect     *cur;

	/*
	 * save[] holds any directory name we find for the given
	 * file. It is static so that it remains valid after we
	 * return.
	 */
	static char     save[1024];

	if(!file || !*file)
		return (char *)0;

	if(!M0isbasename(file))
		return (char *)0;

	list = M0getredirects();

	if(!list)
		return (char *)0;

	cur = M0redirect_match(list, file);

	if(cur)
	{
		strcpy(save, M0getreddir(cur));
	}

	return cur ? save : (char *) 0;
}

/**
*| Name:
*|	M0redirect_match - return REDIRECT entry that matches a file
*|
*| Interface:
*|	#include "m0file.h"
*|
*|	const M0Redirect *
*|	M0redirect_match(const M0Redirect *list, const char *file)
*|
*| Input:
*|	list	- pointer to a list of REDIRECT entries
*|	file	- name of a file
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	  0	- no redirection for the given file
*|	<>0	- a pointer to the REDIRECT entry for the file
*|
*| Remarks:
*|	Redirect only is checked, not MCPATH.
*|
*| Categories:
*|	file
*/

const M0Redirect *
M0redirect_match(const M0Redirect *list, const char *filename)
{
	for(; list; list = M0getrednext(list))
	{
		if (M0sh_match(M0getredpat(list), filename))
			return list;
	}
	return 0;
}

/**
 * FORMAT OF THE REDIRECT TABLE IN SHARED MEMORY
 *
 * A redirection entry consists of a pattern and a directory name.
 * If a filename matches the pattern, the system looks for the file
 * in the corresponding directory.
 *
 * The memory used to store the redirect entries is organized in the
 * following way.
 *
 * The first four bytes hold an integer N that records the number of
 * redirect entries in the shared memory.
 *
 * Following these four bytes are N entries of 100 bytes each.
 * Each entry looks like this:
 *
 *  4 bytes: integer      cfilelen      ! length of cfile used
 *  4 bytes: integer      cextlen       ! length of cext used
 *  4 bytes: integer      cpathlen      ! length of cpath used
 * 12 bytes: character*12 cfile         ! part of pattern before '.'
 *  4 bytes: character*4  cext          ! part of pattern '.' and after
 * 72 bytes: character*72 cpath         ! directory name
 *
 * The strings are stored in blank-padded Fortran form.  The pattern
 * is the concatenation of the contents of cfile and cext.  (cext
 * includes the '.'.)
 *
 * Note that redirect.pgm allows for up to 300 entries.  However, since
 * the shared memory segment is allocated to be 300*100 = 30000 bytes
 * long, it is not big enough for all 300 entries.  (30000 does not
 * include the four bytes to hold N.)
 */

/*
 * This is the structure of a REDIRECT entry as presented to C code.
 *
 * Because of its location in this file, the definition of the contents
 * of this structure is hidden from the code above it in this file, and
 * from code in any other files.  Other files may only have pointers to
 * this structure.  The access functions M0getredpat(), M0getreddir(),
 * and M0getrednext() allow allow other files to examine the contents of
 * a REDIRECT entry.
 *
 * This organization allows us to remove the fixed limits on the shared
 * memory structure holding the REDIRECT entries without having to
 * change any C code except for the code below this point in this file.
 *
 * We use fixed-size arrays in the current structure to simplify memory
 * management, but since all access is via functions, this can easily
 * be changed to use dynamically allocated storage if required in the
 * future.
 */

struct _M0Redirect
{
	char            pattern[17];	/* 16 + 1 for the NUL */
	char            directory[73];	/* 72 + 1 for the NUL */
	struct _M0Redirect *next;
};

/**
*| Name:
*|      M0getredpat - get the pattern of a REDIRECT entry
*|
*| Interface:
*|      #include "m0file.h"
*|
*|      const char *
*|      M0getredpat(const M0Redirect *red)
*|
*| Input:
*|      red     - a REDIRECT entry
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|        0     - (should never happen)
*|      <>0     - a pointer to the pattern part of the REDIRECT entry
*|
*| Remarks:
*|      none
*|
*| Categories:
*|      file
*/

const char *
M0getredpat(const M0Redirect *red)
{
	return red->pattern;
}

/**
*| Name:
*|      M0getreddir - get the directory of a REDIRECT entry
*|
*| Interface:
*|      #include "m0file.h"
*|
*|      const char *
*|      M0getreddir(const M0Redirect *red)
*|
*| Input:
*|      red     - a REDIRECT entry
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|        0     - (should never happen)
*|      <>0     - a pointer to the directory part of the REDIRECT entry
*|
*| Remarks:
*|      none
*|
*| Categories:
*|      file
*/

const char *
M0getreddir(const M0Redirect *red)
{
	return red->directory;
}

/**
*| Name:
*|      M0getrednext - return pointer to next REDIRECT entry
*|
*| Interface:
*|      #include "m0file.h"
*|
*|      const M0Redirect *
*|      M0getrednext(const M0Redirect *red)
*|
*| Input:
*|      red     - current REDIRECT entry
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|        0     - there is no next REDIRECT entry
*|      <>0     - a pointer to the next REDIRECT entry
*|
*| Remarks:
*|      none
*|
*| Categories:
*|      file
*/

const M0Redirect *
M0getrednext(const M0Redirect *red)
{
	return red->next;
}

/**
** Name:
**      freeredirects - return pointer to next REDIRECT entry
**
** Interface:
**      static void
**      freeredirects(M0Redirect *plist)
**
** Input:
**      plist   - pointer to first REDIRECT entry in the list
**
** Input and Output:
**      none
**
** Output:
**      none
**
** Return values:
**      none
**
** Remarks:
**      This routine frees the given list of REDIRECT entries.
**
** Categories:
**      none
*/

static void
freeredirects(M0Redirect * plist)
{
	if (plist)
		free(plist);
}

/**
*| Name:
*|      M0getredirects - return linked list of REDIRECT entries
*|
*| Interface:
*|      #include "m0file.h"
*|
*|      const M0Redirect *
*|      M0getredirects(void)
*|
*| Input:
*|      none
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|        0     - there are no REDIRECT entries
*|      <>0     - a pointer to the first entry in a linked list of
*|                the active REDIRECT entries
*|
*| Remarks:
*|      The function should be called whenever access to the REDIRECT
*|      entries is desired.  Since REDIRECT entries may change
*|      dynamically, the return value from this function should not be
*|      cached or stored long-term.
*|
*| Categories:
*|      file
*/

const M0Redirect     *
M0getredirects(void)
{
	int             i;
	Fint            i0 = 0;	/* constant for m0volrd_() */
	Fint            i4 = 4;	/* constant for m0volrd_() */
	Fint            num_paths;	/* number of records in
					 * REDIRECT table */

	static M0Redirect     *ans = 0;
	static int anscount = 0;	/* number of entries in ans */

	/* initialize */
	m0volrd_(&i4, &i0, &num_paths);
	if (num_paths == 0)
		return (M0Redirect *) 0;

	/*
	 * If we do not already have a chunk for the correct
	 * number of entries, then free the previous chunk (if
	 * needed), and allocate a new chunk with the appropriate
	 * number of entries and set the pointers up.
	 */

	if (anscount != num_paths)
	{
		int             j;

		if(ans)
		{
			freeredirects(ans);
			ans = (M0Redirect *) 0;
			anscount = 0;
		}

		ans = malloc(sizeof(*ans) * num_paths);
		if (!ans)
			return (M0Redirect *) 0;
		anscount = num_paths;

		for (j = 0; j < anscount - 1; j++)
			ans[j].next = &ans[j + 1];
		ans[anscount - 1].next = 0;
	}

	for (i = 0; i < anscount; i++)
	{

		/*
		 * The following maps a record in the REDIRECT data
		 * structure
		 */
		struct
		{
			Fint            flen;	/* length of cfile */
			Fint            elen;	/* length of cext  */
			Fint            plen;	/* length of cpath */
			char            cfile[12];
			char            cext[4];
			char            cpath[72];
		}               vol_record;

		/* length of REDIRECT record */
		Fint            record_len = sizeof(vol_record);

		Fint            record_pointer;
		int             off;
		int             len;

		/*
		 * Make sure that a fundamental assumption of the
		 * implementation holds.
		 */

		assert(sizeof(vol_record) == 100);

		/* acquire the next record from the REDIRECT memory */

		record_pointer = 4 + i * (Fint)sizeof(vol_record);
		m0volrd_(&record_len, &record_pointer, &vol_record);

		/* set ans[i].pattern, ans[i].directory */

		off = 0;
		len = vol_record.flen;
		memcpy(ans[i].pattern + off, vol_record.cfile, len);
		off += len;
		len = vol_record.elen;
		memcpy(ans[i].pattern + off, vol_record.cext, len);
		off += len;
		ans[i].pattern[off] = '\0';

		assert(off < sizeof(ans[0].pattern));

		off = 0;
		len = vol_record.plen;
		memcpy(ans[i].directory + off, vol_record.cpath, len);
		off += len;
		ans[i].directory[off] = '\0';

		assert(off < sizeof(ans[0].directory));
	}

	return ans;
}
