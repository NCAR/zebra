/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 M0FILE.H 9-May-95,18:16:20,`DWS' MCPATH phase 3 (5429)                  */
/* 2 M0FILE.H 11-May-95,16:17:58,`DWS' added M0flistadd() prototype          */
/* 3 M0FILE.H 6-Jun-95,15:06:44,`USER' Released                              */
/**** McIDAS Revision History *** */

/*
 * This header file is for private utilities in the file-access
 * subsystem of the McIDAS library.
 */

#ifndef    _M0FILE_H
#define    _M0FILE_H

#include <sys/types.h>
#include <sys/stat.h>
#include <stddef.h>			/* size_t */

typedef struct _M0flent M0flent;		/* file list entry */
typedef struct _M0flist M0flist;		/* file list */

/* eaccess.c */

extern int
M0eaccess0(const struct stat *sb, int amode);

/* m0redir.c */

typedef struct _M0Redirect M0Redirect;

extern const M0Redirect *
M0redirect_match(const M0Redirect *list, const char *filename);

extern const
M0Redirect *M0getredirects(void);

extern const
char *M0getredpat(const M0Redirect *r);

extern const
char *M0getreddir(const M0Redirect *r);

extern const
M0Redirect *M0getrednext(const M0Redirect *r);

/* m0file.c */

extern M0flist          *
M0flistmake(void);

extern void
M0flistfree(M0flist *fl);

extern void
M0flistadd(M0flist *fl, M0flent *pent);

extern int
M0flistdiradd(M0flist *fl, const char *method, const char *dirname, int (*wanted)(const char *));

extern void
M0flistsort(M0flist *fl);

extern size_t
M0flistcount(const M0flist *fl);

extern M0flent *
M0flistent(const M0flist *fl, size_t i);

extern M0flent          *
M0flentmake(const char *method, const char *dirname, const char *filename);

extern void
M0flentfree(M0flent *f);

extern const char          *
M0flentdir(M0flent *f);

extern const char          *
M0flentfile(M0flent *f);

extern const char          *
M0flentpath(M0flent *f);

extern const char          *
M0flentmeth(const M0flent *f);

extern unsigned
M0flentnum(const M0flent *f);

extern unsigned
M0flentvis(const M0flent *f);

extern void
M0flentsetvis(M0flent *f, unsigned v);

/* m0fmap.c */

extern void
M0fmap(const char *pattern, int listbits);

extern void
m0fmap_(const char *pattern, Fint *listbits, FsLen patternlen);

extern void
M0flistdump(M0flist *fl, int all);

extern void
M0namespace(M0flist *fl, const char *pat, int flags);

#endif  /* _M0FILE_H */
