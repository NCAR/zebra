/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 MCIDASP.H 3-Apr-95,15:19:22,`JUDYK' McIDAS Library Private Header File  */
/* 2 MCIDASP.H 10-Apr-95,8:59:12,`RICKK' Added prototypes for mcpath.        */
/* 3 MCIDASP.H 6-Jun-95,8:53:36,`RICKK' Updated for argument fetchers, McPATH*/
/*      and ADDE                                                             */
/* 4 MCIDASP.H 6-Jun-95,15:08:32,`USER' Released                             */
/* 5 MCIDASP.H 15-Jan-96,14:02:42,`RICKK' updated for mcidas version *.103   */
/* 6 MCIDASP.H 15-Jan-96,14:52:36,`USER' Released                            */
/* 7 MCIDASP.H 19-Feb-96,14:49:22,`DWS' reglue: modified file                */
/* 8 MCIDASP.H 20-Feb-96,11:59:38,`USER' Released                            */
/* 9 MCIDASP.H 25-Mar-96,14:20:54,`RICKK' Updated for reglue                 */
/* 10 MCIDASP.H 25-Mar-96,15:22:36,`USER' Released                           */
/* 11 MCIDASP.H 17-Apr-96,10:37:10,`RICKK' Updated for reglue and pt commands*/
/* 12 MCIDASP.H 17-Apr-96,15:07:14,`USER' Released                           */
/* 13 MCIDASP.H 11-Jul-96,13:18:12,`DAVEST' Added assert switch              */
/* 14 MCIDASP.H 29-Jul-96,12:57:38,`USER' Released                           */
/* 15 MCIDASP.H 2-Aug-96,6:19:58,`DAVEST' Added function protocals for addefu*/
/* 16 MCIDASP.H 13-Aug-96,13:32:24,`DAVEST' Added protocals for new obs      */
/* 17 MCIDASP.H 27-Sep-96,14:08:18,`DAVEST' Added function prototype for Inq.*/
/*      6796 (6769)                                                          */
/* 18 MCIDASP.H 7-Oct-96,11:29:08,`USER' Released                            */
/* 19 MCIDASP.H 11-Oct-96,11:24:44,`DAVEST' Added prototypes for nav (7018)  */
/* 20 MCIDASP.H 14-Oct-96,9:11:46,`DAVEST' Added prototype for 6997          */
/* 21 MCIDASP.H 21-Oct-96,18:24:06,`USER' Released                           */
/**** McIDAS Revision History *** */

/*******************************************************************\
          McIDAS Library Private Header File
\*******************************************************************/

#ifndef _MCIDASP_H
#define _MCIDASP_H

#ifndef _MCIDAS_H
#include "mcidas.h"
#endif

#include "servacct.h"

typedef struct _M0buf M0buf; /* for the M0buf*() routines */

typedef struct _M0hist M0hist; /* for the M0hist*() routines */


/*******************************************************************
 *              Category: CALIBRATION
 *                        (calibration, units conversion)
 *
 */


/*******************************************************************
 *              Category: CONVERTER
 *                        (parsing, byte movers, unit converter)
 *
 * lbrepl_()    - Expand character string by substituting appropriate
 *                definitions from the system string table.
 *
 * mclocase_()  - Convert a string to all lower case letters.
 *
 * mcupcase_()  - Convert a string to all upper case letters.
 *
 */

extern Fint
lbrepl_(const char cmark[1], const char *cin, char *cout, FsLen siz_cmark, FsLen siz_cin, FsLen siz_cout);

extern void
mclocase_(char *string, FsLen string_len);

extern void
mcupcase_(char *string, FsLen string_len);


/*******************************************************************
 *              Category: DAY/TIME
 *                        (day, date, time converters, reformatting)
 *
 */


/*******************************************************************
 *              Category: DISPLAY
 *                        (xwindows, enhancements, cursor/pointer)
 *
 */


/*******************************************************************
 *              Category: EVENT
 *                        (scheduler)
 *
 */


/*******************************************************************
 *              Category: FILE
 *                        (lw, general file i/o, database i/o)
 *
 * kilcom_() - close open communication fd's
 *
 * M0cacheflush() - close open fd's, delete files of 0 length
 *
 * lwmop_() - close open fd's, delete files of 0 length
 *
 * M0cacheopen() - return file descriptor for a named file
 *
 * m0cacheopen_() - return file descriptor for a named file
 *
 * m0filesize_() - Returns size of file, given its file descriptor.
 *
 * m0sysclose_() - Close a file.
 *
 * m0sysread_() - Read a block from a file.
 *
 * m0syswrite_() - Write a block to a file.
 *
 * lbi_() - read file (byte oriented)
 *
 * lwi_() - read file (word oriented)
 *
 * lbo_() - write file (byte oriented)
 *
 * lwo_() - write file (word oriented)
 *
 * lwd_() - remove a file
 *
 * M0fgetline() - Returns a pointer to a character string
 *      containing the next line from the data stream.
 */

extern void
kilcom_(void);

extern void
M0cacheflush(void);

extern void
lwmop_(void);

extern Fint
m0cacheopen_(char *name, FsLen namelen);

extern int
M0cacheopen(const char *filename);

extern Fint
m0filesize_(Fint *filedesc);

extern void
m0sysclose_(Fint *filedesc);

extern Fint
m0sysread_(Fint *filedesc, void *buf, Fint *nbytes);

extern Fint
m0syswrite_(Fint *filedesc, void *buf, Fint *nbytes);

extern Fint
lbi_(const char *file, const Fint *start, const Fint *count, void *buf, FsLen len);

extern Fint
lwi_(const char *file, const Fint *start, const Fint *count, void *buf, FsLen len);

extern Fint
lbo_(const char *file, const Fint *start, const Fint *count, const void *buf, FsLen len);

extern Fint
lwo_(const char *file, const Fint *start, const Fint *count, const void *buf, FsLen len);

extern Fint
lwd_(char *file, FsLen siz_file);

extern char *
M0fgetline(FILE *fileptr, size_t *lenptr);


/*******************************************************************
 *              Category: GRAPHIC
 *                        (plot package, utilities, color levels)
 *
 * m0wrtgif() - Create a GIF file.
 *
 * mcframetogif_() - Write a .gif file of a given frame.
 *
 */


extern int 
m0wrtgif(char *filename, int width, int height, int bitsperpixel, int *red, int *green, int *blue, int (*nextpixel)(int, int));

extern Fint
mcframetogif_(Fint *bframe, char *ffilename, FsLen ffilename_len);


/*******************************************************************
 *              Category: GRID
 *              (grid data, decode, store, list, retrieve, manipulate)
 *
 */


/*******************************************************************
 *              Category: IMAGE
 *   (image/area data, ingest, store, retrieve, display, manipulate)
 *
 */


/*******************************************************************
 *              Category: INGEST/DECODE
 *                        (ingest and decode of data)
 *
 */


/*******************************************************************
 *              Category: MET/SCIENCE
 *                        (science algorithms, unit converter)
 *
 */


/*******************************************************************
 *              Category: NAVIGATION
 *                        (lat/lon, stations)
 *
 */


/*******************************************************************
 *              Category: PT_SRC
 * (point source/md files data, decode, store, retr ieve, plot, manipulate)
 *
 */


/*******************************************************************
 *              Category: SYS_CONFIG
 *                        (uc, syskey, mcpath, itrmch)
 *
 * luc_() - Return a value from User Common.
 *
 * puc_() - Put an integer value into User Common.
 *
 * m0volrd_() - read from REDIRECT tables in memory
 *
 * m0volwr_() - write to REDIRECT tables in memory
 *
 * M0redirect() - locate a file in the REDIRECT entries
 *
 * M0mcpath() - locate a file in the MCPATH directories
 * 
 * M0defpath() - determins the default pathname of a file
 *
 * volnam_() - determines the system pathname of a McIDAS file
 * 
 * M0sh_match() - tests for a /bin/sh-style pattern match
 *
 * M0sh_pattern() - tests for a /bin/sh-style pattern
 *
 * pathcat() - Return a pointer to the concatenation of the two
 *      given strings separated by a '/'.
 * 
 * pathtok() - Parse the given colon-separated path string.  
 * 
 * pathvec() - Return the vector of strings constructed from
 *      the given colon-separated path string.
 *
 * dirpathvec() - Return the vector of directory names constructed from
 *      the given vector of strings.
 *
 * envdirpathvec() - Return the vector of directory names constructed
 *      from the value of the given environment variable
 *
 * pathvecsearch() Searche for a file named 'name' of the given type
 *      along the given path vector, accessible with the given mode.
 *
 * MCPATHvec() - Return a vector of the directories listed in the
 *      MCPATH environment variable.
 *
 * PATHvec() - Return a vector of the directories listed in the
 *      PATH environment variable.
 * 
 * M0isbasename() - tests whether a pathname is a simple filename
 * 
 * initblok_() - initialized the McIDAS environment
 */

extern Fint
luc_(Fint *index);

extern void 
puc_(Fint *value, Fint *index);

extern void
m0volrd_(Fint *bytes, Fint *offset, void *location);

extern void
m0volwr_(Fint *bytes, Fint *offset, void *location);

extern const char *
M0redirect(const char *filename);

extern const char *
M0defpath(const char *filename);

extern const char *
M0mcpath(const char *filename);

extern Fint
volnam_(char *file, char *path, FsLen nfile, FsLen npath);

extern int 
M0sh_match(const char *pattern, const char *text);

extern int 
M0sh_pattern(const char *pattern);

extern const char *
pathcat(const char *prefix, const char *suffix);

extern char *
pathtok(char *str);

extern char **
pathvec(const char *pathstr);

extern char **
dirpathvec(char **srcvec);

extern char **
envdirpathvec(const char *name);

extern const char *
pathvecsearch(char **pathvec, const char *name, int amode, int type);

extern char **
PATHvec(void);

extern char **
MCPATHvec(void);

extern int 
M0isbasename(const char *pathname);

extern void
initblok_ (Fint2 *);

/*******************************************************************
 *              Category: SYSTEM
 *                        (communications, servers, OS interfaces)
 *
 * M0lock() - Set global system lock.
 *
 * lock_() - Set global system lock.
 *
 * M0unlock() - Unlock a system lock set by 'lock'.
 *
 * unlock_() - Unlock a system lock set by 'lock'.
 *
 * M0uclock() - lock based on a byte offset in positive UC
 *
 * M0ucunlock() - unlock a byte offset in positive UC locked by M0uclock
 *
 * connct_() - Sets up and calls function 'connect' for TCP/IP.
 *
 * selects_() - Determine if any data ready for reading on socket.
 *
 * canexec() - Test whether file is executable.
 *
 * ingroupset() - Returns true if the given gid is in the group
 *      set of this process, false otherwise.  
 *
 * sencmd_() - Send command to operating system and route output to
 *      the current text window.
 *
 * keyin_() - run keyin asynchronously with autodot
 *
 * skeyin_() - run keyin synchronously without autodot
 *
 * keyin1_() - run keyin asynchronously without autodot
 *
 * kilpid_() - Kills a specific process ID and prints a message.
 *
 * M0is_display_remote() - Tests whether the X server is remote
 *
 * M0version() - Returns the version string from VERSION.TXT
 *
 * M0window_titles() - appends the window and icon title strings to
 *                        the given buffers
 *
 * m0cxread_() - reads data from the server
 *
 * m0sxread_() - reads data from the client
 *
 * m0sxsend_() - writes data to the client
 *
 * m0cxreq_() - transmit request to server for inbound data flow
 *
 * m0vserv_() - validate a client request
 *
 * m0sxlogi_() - initialize a servacct data structure for accounting
 *
 * m0cxfin_() - end a transaction after all data has been read
 *
 * m0cxerms() - output the error message sent by the server
 */

extern void
M0lock(const char *name);

extern void
lock_(const char *name, FsLen len);

extern void
M0unlock(const char *name);

extern void
unlock_(const char *name, FsLen len);

extern int
M0uclock(off_t byte, int blocking);

extern int
M0ucunlock(off_t byte);

extern Fint
connct_(Fint *addr, Fint *s);

extern Fint 
selects_(Fint *ns);

extern int 
canexec(const char *name);

extern int 
ingroupset(int gid);

extern void 
sencmd_(char *cmd_arg, FsLen length);

extern void
keyin_(const char *cmd, FsLen len);

extern Fint
skeyin_(const char *cmd, FsLen len);

extern void
keyin1_(const char *cmd, FsLen len);

extern void 
kilpid_(Fint *pid);

extern int
M0is_display_remote(const char *display_name);

extern const char *
M0version(void);

extern void
M0window_titles(M0buf *win_buf, M0buf *icn_buf);

extern Fint
m0cxread_(Fint *, Fint *);

extern Fint
m0sxread_(Fint *, Fint *);

extern Fint
m0sxsend_(Fint *, Fint *);

extern Fint
m0cxreq_ (char *, char *, Fint *, Fint *, Fint *, FsLen, FsLen);

extern void
m0vserv_ (servacct *);

extern void
m0sxlogi_ (servacct *);

extern void
m0cxfin_ (void);

extern void
m0cxerms_ (void);

/*******************************************************************
 *              Category: TEXT
 *                        (textual data, ingest, list, retrieve, manipulate)
 *
 */


/*******************************************************************
 *              Category: USER_INTERFACE
 *                        (menus, messages , strings, command_line)
 *
 * M0sspout()	- low level text I/O to McIDAS standard destination
 *
 * M0espout()	- low level text I/O to McIDAS error destination
 *
 * M0dspout()	- low level text I/O to McIDAS debug destination
 *
 * M0espout0()	- low level text I/O to McIDAS error destination
 *
 * M0dspout0()	- low level text I/O to McIDAS debug destination
 * 
 * M0tcolget()  - return current program text color
 * 
 * M0tcolset()  - set current program text color
 * 
 * M0twinget()  - return current program text window
 * 
 * M0twinset()  - set current program text window
 * 
 * askit_() - Solicits a typed-in response from the user.
 *
 * beep_() - Make an audible tone on the workstations speaker.
 *
 * ddest_()     - Print to McIDAS debug destination.
 *
 * edest_()     - Print to McIDAS error destination.
 *
 * sdest_()     - Print to McIDAS standard destination.
 *
 * m0split() - split dataset name and lookup alias
 *
 * m0sxresolv() - resolve names on a server
 *
 * m0sxdone() - bind off a DDE transaction, after all data has been sent
 *
 * m0sxtrce() - writes to a trace file
 */

extern void
M0sspout(const char *text, size_t textlen);

extern void
M0espout(const char *text, size_t textlen);

extern void
M0dspout(const char *text, size_t textlen);

extern void
M0espout0(const char *text, size_t textlen);

extern void
M0dspout0(const char *text, size_t textlen);

extern int
M0tcolget(void);

extern int
M0tcolset(int color);

extern int
M0twinget(void);

extern int
M0twinset(int window);

extern void 
askit_(char *question, char *cstring, Fint *ret, FsLen qlen, FsLen clen);

extern void
beep_(Fint *freq, Fint *time);

extern void
ddest_(const char *msg, const Fint *num, FsLen siz_msg);

extern void
edest_(const char *msg, const Fint *num, FsLen siz_msg);

extern void
sdest_(const char *msg, const Fint *num, FsLen siz_msg);

extern Fint
m0split_ (char *, char *, FsLen, FsLen);

extern Fint
m0sxresolv_(char *, char *, FsLen, FsLen);

extern void
m0sxdone_(Fint *);

extern void
m0sxtrce_ (char *, FsLen);

/*****************************************************************n
 *              Category: UTILITY
 * (functions useful for programming, substringing, byte manipulation, etc)
 *
 * M0bufalloc()   - allocate a dynamic buffer
 * M0buffree()    - free a dynamic buffer
 * M0bufrealloc() - set the length of a dynamic buffer
 * M0bufsplice()  - splice data into a dynamic buffer
 * M0buflen()     - get the length of a dynamic buffer
 * M0bufptr()     - return a pointer to the data in a dynamic buffer
 * M0bufcpy()     - set the contents of a dynamic buffer
 * M0bufcat()     - append to the contents of a dynamic buffer
 *
 * M0handalloc() - Allocate a new handle from the pool of available handles.
 * M0handfree()  - Free the given handle from the pool of available handles.
 * M0handget()   - Get the pointer assigned to the given handle.
 * M0handput()   - Put the pointer to the given assigned handle.
 *
 * M0histalloc()   - allocate a history object
 * M0histfree()    - free a history object
 * M0histrealloc() - set the length of a history object
 * M0histadd()     - add a line to a history object
 * M0histlen()     - get the number of lines in a history object
 * M0histget()     - return a pointer to a line in a history object
 *
 * M0keycmp()    - Compare given string with given "key.str" match string.
 *
 * M0strtoken()  - Get ptr to begin, end, and length of first token in string.
 *
 * btest_() - tests a bit of an integer value
 * iand_()  - performs a logical AND
 * ibclr_() - clears one bit of an integer value to zero
 * ibset_() - sets one bit of an integer value to one
 * ieor_()  - performs a logical exclusive OR
 * int4_()  - return an integer value from an integer*2 argument
 * ior_()   - performs a logical OR
 * ishft_() - performs a logical shift
 */

extern M0buf *
M0bufalloc(void);

extern void
M0buffree(M0buf *buf);

extern void
M0bufrealloc(M0buf *buf, size_t nbytes);

extern void
M0bufsplice(M0buf *dst, size_t dstoff, size_t dstlen,
	    const void *src, size_t srclen);

extern size_t
M0buflen(M0buf *buf);

extern void *
M0bufptr(M0buf *buf);

extern void
M0bufcpy(M0buf *dst, const void *src, size_t nbytes);

extern void
M0bufcat(M0buf *dst, const void *src, size_t nbytes);


extern int
M0handalloc(void);

extern int
M0handfree(int handle);

extern int
M0handget(int handle, void **ret_ptr);

extern int
M0handput(int handle, const void *given_ptr);


extern M0hist *
M0histalloc(size_t nlines);

extern void
M0histfree(M0hist *hist);

extern void
M0histrealloc(M0hist *hist, size_t nlines);

extern void
M0histadd(M0hist *hist, const char *src, size_t nbytes);

extern size_t
M0histlen(M0hist *hist);

extern const char *
M0histget(M0hist *hist, size_t lineno, size_t *nbytes);


extern int
M0keycmp(const char *keystr, const char *given_str);

extern size_t
M0strtoken(const char *str, const char **btok, const char **etok);


extern Fint
btest_(Fint *i, Fint *pos);

extern Fint
iand_(Fint *i, Fint *j);

extern Fint
ibclr_(Fint *i, Fint *pos);

extern Fint
ibset_(Fint *i, Fint *pos);

extern Fint
ieor_(Fint *i, Fint *j);

extern Fint
int4_(Fint2 *val);

extern Fint
ior_(Fint *i, Fint *j);

extern Fint
ishft_(Fint *i, Fint *shift);

extern void
M0sxSetTraceOn (void);

extern void
M0sxSetTraceOff (void);

extern void
m0sxsettraceon_ (void);

extern void
m0sxsettraceoff_ (void);

extern M0sxGetClientRequest (servacct *, char **);

extern int
M0sxdatasetinfo (char *,
                 char **, char **,
                 char **, char **,
                 char **, char **,
                 int *, int *, int *);

extern Fint
m0fndzon_
(char *, Fint *, Fint *, Fint *, Fint *, Fint *, Fint *,
Fint *, Fint *, char *, char *, Fint *, Fint *, FsLen, FsLen,
FsLen);

extern Fint
m0lllcty_ (const char *, Fint *, Fint *, Fint *, double *, double
*, FsLen);

extern Fint
m0lfhcty_ (const char *, Fint *, FsLen);

extern Fint
m0gcfcty_
(const char *, Fint *, char *, char *, Fint *, Fint *, FsLen,
FsLen, FsLen);

extern void
M0FlipObsTextHeader (int *);

extern int
M0ObTextErrorString (int, char **);

extern int
M0isrtdataset (char *);

extern Fint
m0mxnvnum_(void);

extern Fdouble
mcearthradius_(void);

extern Fint
mcmxanum_(void);

extern void
mcreadnav_(Fint *, Fint *, Fint *);

extern Fint
itvll_(Fint *, Fint *, Fint *, Fint *, Fint *, Freal *, Freal *, Fint *);

extern Fint
illtv_ (Fint *, Fint *, Freal *, Freal *, Fint *, Fint *, Fint *, Fint *);

extern void
dshon_(void);

extern void
dshoff_(void);

extern void
qgdash_(Fint *);

extern Fdouble
mcearthradius_(void);

extern void
readd_(Fint *, Fint *);

extern void
mcreadnav_(Fint *, Fint *, Fint *);

extern Fint
nvprep_(Fint *, Fint *);

extern Freal
geolat_(Freal *, Fint *);

extern Fint
nv1ini_(Fint *, Fint *);

extern Fint
nv2ini_(Fint *, Fint *);

extern Fint
nv3ini_(Fint *, Fint *);

extern Fint
nv1sae_(Freal *, Freal *, Freal *, Freal *, Freal *, Freal *);

extern Fint
nv2sae_(Freal *, Freal *, Freal *, Freal *, Freal *, Freal *);

extern Fint
nv3sae_(Freal *, Freal *, Freal *, Freal *, Freal *, Freal *);

extern Fint
nv1eas_(Freal *, Freal *, Freal *, Freal *, Freal *, Freal *);

extern Fint
nv2eas_(Freal *, Freal *, Freal *, Freal *, Freal *, Freal *);

extern Fint
nv3eas_(Freal *, Freal *, Freal *, Freal *, Freal *, Freal *);

extern Fint
nv1opt_(Fint *, Freal *, Freal *);

extern Fint
nv2opt_(Fint *, Freal *, Freal *);

extern Fint
nv3opt_(Fint *, Freal *, Freal *);

extern void
M0fixnewline(char *);

#endif  /* _MCIDASP_H  */

#ifdef M0ASSERT_ON

#undef NDEBUG
#include <assert.h>
#define M0ASSERT(EX)  assert(EX)

#else /* !M0ASSERT_ON */

#define M0ASSERT(EX)  ((void)0)

#endif /* M0ASSERT_ON */
