/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 ARGUTIL.C 4-Apr-95,10:13:56,`JUDYK' Arg-Fetching Utility/Maint Functions*/
/* 2 ARGUTIL.C 20-Apr-95,14:31:34,`JUDYK' int mcargfree_ TO Fint4 mcargfree_ */
/* 3 ARGUTIL.C 21-Apr-95,12:44:18,`JUDYK' chg mcargnam_ to ret kw if neg stat*/
/* 4 ARGUTIL.C 24-Apr-95,16:15:58,`JUDYK' Mcargkey: move array null to begin.*/
/* 5 ARGUTIL.C 2-May-95,16:41:12,`JUDYK' chg strlen to STRLEN & () to (void) */
/* 6 ARGUTIL.C 4-May-95,13:46:48,`JUDYK' fix neg stat return if #kw=dim and "*/
/* 7 ARGUTIL.C 8-May-95,22:54:32,`JUDYK' add const                           */
/* 8 ARGUTIL.C 11-May-95,20:12:50,`JUDYK' add more const                     */
/* 9 ARGUTIL.C 30-May-95,18:46:08,`JUDYK' update interface doc, Fint4 to Fint*/
/* 10 ARGUTIL.C 6-Jun-95,14:58:50,`USER' Released                            */
/* 11 ARGUTIL.C 28-Jun-95,17:22:32,`JUDYK' recode Mcargcmd, add M0keytxt     */
/* 12 ARGUTIL.C 15-Jan-96,14:48:08,`USER' Released                           */
/* 13 ARGUTIL.C 19-Feb-96,15:37:02,`DWS' reglue: modified file               */
/* 14 ARGUTIL.C 20-Feb-96,11:50:00,`USER' Released                           */
/* 15 ARGUTIL.C 22-Mar-96,11:45:56,`DWS' reglue: modified file               */
/* 16 ARGUTIL.C 25-Mar-96,13:49:30,`USER' Released                           */
/**** McIDAS Revision History *** */

/*==============================argutil.c=====================================*/
/*
        argutil.c - Argument-Fetching Utility Functions
                    ARGAPI functions pertaining to the arg-fetching structure.
*/
/*----------------------------------------------------------------------------*/

#include <ctype.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "mcidas.h"
#include "mcidasp.h"
#include "m0arg.h"

/*==============================TYPEDEFS, STRUCTURES and UNIONS===============*/
/* Define typedefs, structures and unions                                     */

#ifndef    _McKwArg
#  define  _McKwArg
   typedef struct McKwArg {    /* define keyword argument structure           */
      const char **  arg;      /* array of string arguments for a keyword     */
      int            narg;     /* # arguments for keyword (incl. 1 for name)  */
   }        McKwArg;
#endif  /* _McKwArg */

#ifndef    _McArg
#  define  _McArg
   typedef struct McArg {      /* define argument-fetching structure          */
      int        nkw;          /* # keyword structures (dimension size of kw) */
      McKwArg *  kw;           /* keyword arg structures array, as they occur */
   }        McArg;
#endif  /* _McArg */

/*              McIDAS Argument-Fetching Data Structure (typedef McArg)
_______         -------------------------------------------------------
|     |
| arg |
|__|__|
   |
___V____
|      |
| nkw  | # keywords for this command line (including 1 for positional args and
|______|                                   1 for quote field)
|      | Malloc'ed Array of
|      | Keyword Argument
|      | Structures
|      | (typedef McKwArg) 
|      |  ________
| kw   |->|      |  Malloc'ed Array of Positional Argument Strings:
|      |  |      |  ____________________________________________________________
|      |  |      |  | kw[0]    | kw[0]    | kw[0]    |   | kw[0]               |
|      |  | arg  |->|  .arg[0] |  .arg[1] |  .arg[2] |...|  .arg[kw[0].narg-1] |
|      |  |      |  |_|________|_|________|_|________|___|_|___________________|
|      |  |      |    V          V          V              V
|      |  |      |    malloc()ed malloc()ed malloc()ed     malloc()ed
|      |  |      |    command    1st        2nd            last
|      |  |      |    name       positional positional     positional
|      |  |______|               argument   argument       argument
|      |  |      |
|      |  | narg | # positional arguments (plus 1)
|      |  |______|
|      |  ___|____
|      |  |      |  Malloc'ed Array of First Keyword's Argument Strings:
|      |  |      |  ____________________________________________________________
|      |  |      |  | kw[1]    | kw[1]    | kw[1]    |   | kw[1]               |
|      |  | arg  |->|  .arg[0] |  .arg[1] |  .arg[2] |...|  .arg[kw[1].narg-1] |
|      |  |      |  |_|________|_|________|_|________|___|_|___________________|
|      |  |      |    V          V          V              V
|      |  |      |    malloc()ed malloc()ed malloc()ed     malloc()ed
|      |  |      |    first      1st        2nd            last
|      |  |      |    keyword    keyword    keyword        keyword
|      |  |______|    name       argument   argument       argument
|      |  |      |
|      |  | narg | # arguments for this keyword (plus 1)
|      |  |______|
|      |  ___|____
|      |  |      |  Malloc'ed Array of Second Keyword's Argument Strings:
|      |  |      |  ____________________________________________________________
|      |  |      |  | kw[2]    | kw[2]    | kw[2]    |   | kw[2]               |
|      |  | arg  |->|  .arg[0] |  .arg[1] |  .arg[2] |...|  .arg[kw[2].narg-1] |
|      |  |      |  |_|________|_|________|_|________|___|_|___________________|
|      |  |      |    V          V          V              V
|      |  |      |    malloc()ed malloc()ed malloc()ed     malloc()ed
|      |  |      |    second     1st        2nd            last
|      |  |      |    keyword    keyword    keyword        keyword
|      |  |______|    name       argument   argument       argument
|      |  |      |
|      |  | narg | # arguments for this keyword (plus 1)
|      |  |______|
|      |     |
|      |     :        :          :          :              :
|      |     :        :          :          :              :
|      |  ___|____
|      |  |      |  Malloc'ed Array of Last Keyword's Argument Strings:
|      |  |      |  ____________________________________________________________
|      |  |      |  | kw[arg->nkw-1] | kw[arg->nkw-1] |   | kw[arg->nkw-1]     |
|      |  | arg  |->|  .arg[0]       |  .arg[1]       |...|  .arg[kw["].narg-1]|
|      |  |      |  |_|______________|_|______________|___|_|__________________|
|      |  |      |    V                V                    V
|      |  |      |    malloc()ed       malloc()ed           malloc()ed
|      |  |      |    last             1st                  last
|      |  |      |    keyword          keyword              keyword
|      |  |______|    name             argument             argument
|      |  |      |
|      |  | narg | # arguments for last keyword (plus 1)
|______|  |______|

*/

/*==============================PROTOTYPES====================================*/
/* Define function prototypes                                                 */

/*******************************************************************
 *              Category:  USER_INTERFACE
 *
 * M0arggetkw()  - Get given keyword's first occurring keyword arg structure.
 *
 * M0argptr()    - Get pointer to argument-fetching structure for given handle.
 *
 * M0argstrtab() - Get given keyword's system string table arg-fetching data.
 *
 */

extern int
M0arggetkw(int arg_handle, const char *keyword, const McKwArg **kw);

extern McArg *
M0argptr(int arg_handle);

extern McKwArg*
M0argstrtab(const char *keyword);

/*==============================M0argalloc====================================*/
/*
*| Name:
*|      M0argalloc - Allocate a new argument-fetching structure and handle.
*|
*| Interface:
*|      #include "m0arg.h"
*|
*|      int
*|      M0argalloc(void)
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
*|      >  0     - Handle to newly allocated argument-fetching structure.
*|      <  0     - failure statuses
*|
*| Remarks:
*|      Use Mcargfree() to free the structure.
*|
*|      The newly allocated argument-fetching structure is initialized to zeros,
*|      and contains one "keyword" structure for the positional arguments.
*|
*| Categories:
*|      USER_INTERFACE
*/

int                                   /* returned arg-fetching handle         */
M0argalloc(void)                      /* Allocate Argument-Fetching Structure */
{
   McArg      *  arg;                 /* argument-fetching structure pointer  */
   const McArg   arg0 = {0};          /* zero argument-fetching structure     */
   int           handle;              /* newly assigned handle                */
   int           stat;                /* working status code                  */

   MALLOC(arg, sizeof(*arg));         /* allocate new structure               */

   *arg = arg0;                                  /* init to zeros             */

   if ((handle = M0handalloc()) < 0) {           /* assign new arg handle     */
      free(arg);
      return handle;
   }

   stat = M0handput(handle, arg);                /* set handle to new struct  */
   ++handle;                                     /* adjust handle by 1        */
   if (stat < 0) {               
      Mcargfree(handle);
      return stat;
   }

   if ((stat = M0argaddkw(handle, NULL)) < 0) {  /* add positional kw struct  */
      Mcargfree(handle);
      return stat;
   }

   return handle;                                /* return arg handle         */
}
/*==========================end M0argalloc====================================*/

/*==============================M0argaddkw====================================*/
/*
*| Name:
*|      M0argaddkw - Add new keyword to given argument-fetching handle.
*|
*| Interface:
*|      #include "m0arg.h"
*|
*|      int
*|      M0argaddkw(int arg_handle, const char* newkw)
*|
*| Input:
*|      arg_handle  - Given argument-fetching handle to be updated, or
*|                    0 to update the current McIDAS command's structure.
*|      newkw       - Given pointer to the malloc()ed keyword name to add.
*|                    Use a malloc()ed "\"" to add the quote field "keyword".
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      >= 0        - successful  (# of keywords)
*|      <  0        - failure statuses
*|
*| Remarks:
*|      THE GIVEN KEYWORD NAME, 'newkw', MUST BE A MALLOC()ED POINTER.
*|
*|      To add the quote field, the given keyword name MUST be "\"".
*|
*|      The special "0th positional keyword", is created by M0argalloc -
*|      do not attempt to create this yourself.
*|
*| Categories:
*|      USER_INTERFACE
*/

int                                  /* returned status code                  */
M0argaddkw(                          /* Argument-Fetching Add Keyword         */
   int            arg_handle,        /* given argument-fetching handle or 0   */
   const char  *  newkw)             /* given malloc()ed keyword name to add  */
{
   int            nkw    = 0;        /* # of current keywords                 */
   int            i      = 0;        /* index to new keyword structure        */
   const McKwArg  kwarg0 = {0};      /* used to init new keyword structure    */
   McArg       *  arg    = M0argptr(arg_handle);      /* get arg-fetching ptr */

   if (!arg)  return -1;             /* no arg structure ?                    */

   i   = arg->nkw;                   /* index to new keyword structure        */
   nkw = arg->nkw + 1;               /* increment number of keywords          */

                                     /* make room for new kw structure at end */
   REALLOC(arg->kw, nkw * sizeof(*arg->kw));       

   arg->kw[i] = kwarg0;              /* init new kw structure to zeros        */

/* if 0th "positional keyword", just allocate the zero init'ed structure      */
/* if not the "positional keyword", add given keyword name as position=0 arg  */

   if (i > 0 || !POSITIONAL_KEYWORD(newkw)) {
      MALLOC(arg->kw[i].arg, sizeof(*arg->kw[i].arg));       
      arg->kw[i].arg[0] = newkw;
      arg->kw[i].narg   = 1;
   }

   arg->nkw = nkw;                   /* save incremented # of keywords        */

   return nkw;                       /* return incremented # of keywords      */
}
/*==========================end M0argaddkw====================================*/

/*==============================M0argadd======================================*/
/*
*| Name:
*|      M0argadd - Add new keyword argument to given argument-fetching handle.
*|
*| Interface:
*|      #include "m0arg.h"
*|
*|      int
*|      M0argadd(int arg_handle, const char* kwname, const char* newarg)
*|
*| Input:
*|      arg_handle  - Given argument-fetching handle to be updated, or
*|                    0 to update the current McIDAS command's structure.
*|      kwname      - Given keyword name, or null/blank(s) for positional arg,
*|                    or "\"" for the quote field.
*|      newarg      - Given pointer to the malloc()ed argument string to add.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      >= 0        - successful  (# of arguments for given keyword)
*|      <  0        - failure statuses
*|
*| Remarks:
*|      THE GIVEN ARGUMENT, 'newarg', MUST BE A MALLOC()ED POINTER.
*|
*|      The given 'kwname' must exactly match a keyword already added to the
*|      structure using M0argaddkw.  The given 'newarg' is added to the end
*|      of arguments for the last matching keyword in the structure.
*|
*| Categories:
*|      USER_INTERFACE
*/

int                                  /* returned status code                  */
M0argadd(                            /* Argument-Fetching Add Argument        */
   int           arg_handle,         /* given argument-fetching handle or 0   */
   const char *  kwname,             /* given keyword name, or NULL           */
   const char *  newarg)             /* given malloc()ed argument to add      */
{
   int           narg;               /* number of arguments for given keyword */
   int           i   = 0;            /* index of matching keyword             */
   McArg      *  arg = M0argptr(arg_handle);      /* get arg-fetching ptr     */

   if (!arg  ||  arg->nkw <= 0)  return -1;       /* no keywords ?            */

   if (!POSITIONAL_KEYWORD(kwname)) {             /* not positional keyword ? */
      for (i = arg->nkw - 1;  i > 0;  i--) {      /* find last matching kw    */
         if (arg->kw[i].narg <= 0)  continue;
         if (!strcmp(kwname, *arg->kw[i].arg))  break;
      }
      if (i <= 0)  return -1;                     /* not found ?              */
   }

   narg = arg->kw[i].narg + 1;                    /* increment # of args      */

                                                  /* make room for new arg    */
   REALLOC(arg->kw[i].arg, narg * sizeof(*arg->kw[i].arg));       

   arg->kw[i].arg[arg->kw[i].narg] = newarg;      /* add new arg to end       */
   arg->kw[i].narg                 = narg;

   return narg;                                   /* return # of args         */
}
/*==========================end M0argadd======================================*/

/*==============================M0argput======================================*/
/*
*| Name:
*|      M0argput - Put given argument to given arg-fetching keyword position.
*|
*| Interface:
*|      #include "m0arg.h"
*|
*|      int
*|      M0argput(int arg_handle, const char* keyword, int position, 
*|               const char* arg)
*|
*| Input:
*|      arg_handle  - Given arg-fetching handle, or 0 for McIDAS command.
*|      keyword     - Given name of the desired keyword, in key.word format,
*|                    where key=minimum keyword name, .word=maximum name,
*|                    or null/blanks to indicate a positional argument,
*|                    or "\"" for the quote field.
*|      position    - Given argument position within the keyword, or
*|                    positional argument's placement.
*|                    1 indicates 1st positional arg or 1st arg for keyword.
*|                    0 puts either the program name (for null keyword), or
*|                    the entered keyword name.
*|      arg         - Given argument string to put.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      >= 0        - successful  (# of arguments for given keyword)
*|      <  0        - failure statuses
*|
*| Remarks:
*|      Makes malloc()ed copy of given arg (and keyword, if neccessary).
*|      Does not free any old existing argument.
*|
*|      If keyword not found, then adds it to the end of structure.
*|
*|      If position not found, then adds missing args up to the given position,
*|      otherwise updates the existing position.
*|
*| Categories:
*|      USER_INTERFACE
*/

int                                        /* returned status code            */
M0argput(                                  /* Argument-Fetching Update        */
   int              arg_handle,            /* given arg-fetching handle or 0  */
   const char    *  keyword,               /* given key.word name or null     */
   int              position,              /* given position within keyword   */
   const char    *  arg)                   /* given arg to put                */
{
   int              stat;                  /* status code                     */
   const McKwArg *  kw;                    /* ptr to given keyword's arg data */
   char          *  newarg;                /* malloc() of given arg           */
/*----------------------------------------------------------------------------*/

   if (position < 0) {                     /* validate given parameter        */
      PROG_ERROR(parameter,                /* print standard diagnostic       */
         "invalid parm for position = %d", position);
      return PARM_RETERR;                  /* return invalid parm status      */
   }

                                           /* get given keyword's arg data    */
   if ((stat = M0arggetkw(arg_handle, keyword, &kw)) < 0)  return stat;

/*----------------------------------------------------------------------------*/

   if (stat != ARG_KEYIN) {                /* create new keyword if not found */

      char *  newkw;
      char    keyword_minmax_separator[] = { KEYWORD_MINMAX_SEPARATOR, '\0' };
      size_t  lenkw = strcspn(keyword, keyword_minmax_separator);

      MALLOC(newkw, lenkw + 1);            /* ensure malloc()ed keyword name  */
      memcpy(newkw, keyword, lenkw);
      newkw[lenkw] = '\0';

      if ((stat = M0argaddkw(arg_handle, newkw)) < 0) {    /* add new keyword */
         free(newkw);
         return stat;
      }

      if ((stat = M0arggetkw(arg_handle, keyword, &kw)) < 0)  return stat;

   }

/*----------------------------------------------------------------------------*/

   MALLOC(newarg, STRLEN(arg) + 1);        /* ensure malloc()ed argument      */
   strcpy(newarg, arg);

   if (position < kw->narg) {              /* update existing keyword arg     */
      kw->arg[position] = newarg;
   }

   else {                                  /* create new keyword arg          */
      int  i;

      /* add (position - kw->narg) copies of MISSING_ARG_VALUE */

      for (i = kw->narg;  i < position;  i++) {
	 char *newmiss;

         MALLOC(newmiss, STRLEN(MISSING_ARG_VALUE) + 1);
         strcpy(newmiss, MISSING_ARG_VALUE);

         if ((stat = M0argadd(arg_handle, keyword, newmiss)) < 0) {
            free(newmiss);
            free(newarg);
            return stat;
         }
      }
      if ((stat = M0argadd(arg_handle, keyword, newarg)) < 0) {
         free(newarg);
         return stat;
      }
   }

   return kw->narg;                        /* return # of keyword args        */
}
/*==========================end M0argput======================================*/

/*==============================m0argput_=====================================*/
/*
*| Name:
*|      m0argput - Put given argument to given arg-fetching keyword position.
*|
*| Interface:
*|      integer function
*|      m0argput(integer arg_handle, character*(*) keyword, integer position, 
*|               character*(*) arg)
*|
*| Input:
*|      arg_handle  - Given arg-fetching handle, or 0 for McIDAS command.
*|      keyword     - Given name of the desired keyword, in key.word format,
*|                    where key=minimum keyword name, .word=maximum name,
*|                    or null/blanks to indicate a positional argument,
*|                    or "\"" for the quote field.
*|      position    - Given argument position within the keyword, or
*|                    positional argument's placement.
*|                    1 indicates 1st positional arg or 1st arg for keyword.
*|                    0 puts either the program name (for null keyword), or
*|                    the entered keyword name.
*|      arg         - Given argument string to put.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      >= 0        - successful  (# of arguments for given keyword)
*|      <  0        - failure statuses
*|
*| Remarks:
*|      Makes malloc()ed copy of given arg (and keyword, if neccessary).
*|      Does not free any old existing argument.
*|
*|      If keyword not found, then adds it to the end of structure.
*|
*|      If position not found, then adds missing args up to the given position,
*|      otherwise updates the existing position.
*|
*| Categories:
*|      USER_INTERFACE
*/

Fint                                       /* returned status code            */
m0argput_(                                 /* Argument-Fetching Update        */
   const Fint *  arg_handle,               /* given arg-fetching handle or 0  */
   const char *  keyword,                  /* given key.word name or null     */
   const Fint *  position,                 /* given position within keyword   */
   const char *  arg,                      /* given arg to put                */
   FsLen         siz_keyword,              /* character*(*) size of 'keyword' */
   FsLen         siz_arg)                  /* character*(*) size of 'arg'     */
{
   int           status;                   /* returned status code            */
                                      /* convert fortran char*(*) to C string */
   char       *  keyword_ = fsalloc(keyword, siz_keyword);
   char       *  arg_     = fsalloc(arg,     siz_arg);

   if (keyword_ && arg_) {

      status = M0argput(*arg_handle, keyword_, *position, arg_);

   }

   free(keyword_);                         /* free working C string           */
   free(arg_);                             /* free working C string           */

   if (!keyword_ || !arg_)  return MALLOC_RETERR;

   return status;                          /* return the status code          */
}
/*==========================end m0argput_=====================================*/

/*==============================M0argptr======================================*/
/*
*| Name:
*|      M0argptr - Get pointer to argument-fetching structure for given handle.
*|
*| Interface:
*|      McArg*
*|      M0argptr(int arg_handle)
*|
*| Input:
*|      arg_handle  - Given arg-fetching handle, or 0 for McIDAS command.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      !NULL       - Pointer to argument-fetching structure as follows:
*|                    .nkw      - Number of keywords.
*|                    .kw       - Array of keyword structures:
*|                       .arg   - Array of arg strings for specific keyword.
*|                       .narg  - Number of arguments for specific keyword.
*|       NULL       - failure
*|
*| Remarks:
*|      none
*|
*| Categories:
*|      USER_INTERFACE
*/

McArg *                                   /* returned arg-fetching pointer    */
M0argptr(                                 /* Get Argument-Fetching Pointer    */
   int     arg_handle)                    /* given arg-fetching handle or 0   */
{
   void *  ptr;
                                          /* adjust given or cmd handle by -1 */
   M0handget((arg_handle ? arg_handle : M0cmdget()) - 1, &ptr);

   return (McArg*)ptr;                    /* return arg-fetching pointer      */
}
/*==========================end M0argptr======================================*/

/*==============================M0argget======================================*/
/*
*| Name:
*|      M0argget - Get pointer to desired argument, for given keyword, position,
*|                 and argument-fetching handle.
*|
*| Interface:
*|      #include "m0arg.h"
*|
*|      int
*|      M0argget(int arg_handle, const char* keyword, int position, 
*|               const char** value)
*|
*| Input:
*|      arg_handle  - Given arg-fetching handle, or 0 for McIDAS command.
*|      keyword     - Given name of the desired keyword, in key.word format,
*|                    where key=minimum keyword name, .word=maximum name,
*|                    or null/blanks to indicate a positional argument,
*|                    or "\"" for the quote field.
*|      position    - Given argument position within the keyword, or
*|                    positional argument's placement.
*|                    1 indicates 1st positional arg or 1st arg for keyword.
*|                    0 gets either the program name (for null keyword), or
*|                    the entered keyword name.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      value       - Pointer to desired argument string, or NULL.
*|
*| Return values:
*|            0     - argument not found, use the default
*|         1000     - successful fetch of command line argument
*|         2000     - successful fetch of system string table argument
*|      < -9999     - failure statuses
*|
*| Remarks:
*|      Returns pointer to "global", malloc()ed,  argument, which must not be
*|      changed by calling program.  If the calling program needs to change this
*|      arg value, it should be copied to a work area.
*|
*| Categories:
*|      USER_INTERFACE
*/

int                                        /* returned status code            */
M0argget(                                  /* Fetch Desired Argument Value    */
   int              arg_handle,            /* given arg-fetching handle       */
   const char    *  keyword,               /* given key.word name or null     */
   int              position,              /* given position within keyword   */
   const char   **  value)                 /* return ptr to global arg value  */
{
   int              status;                /* status code                     */
   const McKwArg *  kw;                    /* ptr to given keyword's arg data */
/*----------------------------------------------------------------------------*/
   if (!value) {                           /* validate given parameter        */
      PROG_ERROR(parameter,                /* print standard diagnostic       */
         "missing char **value - can't get arg for keyword = %s", keyword);
      return PARM_RETERR;                  /* return invalid parm status      */
   }

   *value = NULL;                          /* init returned value to NULL     */

   if (position < 0) {                     /* validate given parameter        */
      PROG_ERROR(parameter,                /* print standard diagnostic       */
         "invalid parm for position = %d", position);
      return PARM_RETERR;                  /* return invalid parm status      */
   }
/*----------------------------------------------------------------------------*/
                                           /* get given keyword's arg data    */
   status = M0arggetkw(arg_handle, keyword, &kw);

   if (!kw)  return status;                /* couldn't retrieve keyword ?     */

/*----------------------------------------------------------------------------*/
/* Try to get argument value from text line -                                 */
/* if keyword exists, arg exists within keyword, AND not missing arg          */

   if (position < kw->narg  &&  strcmp(kw->arg[position], MISSING_ARG_VALUE)) {

      *value = kw->arg[position];          /* return ptr to global arg value  */
      return status;                       /* return "keyedin arg" status     */

   }

   return ARG_DEF;                         /* desired argument not found      */
}
/*==========================end M0argget======================================*/

/*==============================M0arggetkw====================================*/
/*
*| Name:
*|      M0arggetkw - Get given handle's first occurring keyword arg structure.
*|
*| Interface:
*|      int
*|      M0arggetkw(int arg_handle, const char* keyword, const McKwArg** kw)
*|
*| Input:
*|      arg_handle  - Given arg-fetching handle, or 0 for McIDAS command.
*|      keyword     - Given name of the desired keyword, in key.word format,
*|                    where key=minimum keyword name, .word=maximum name,
*|                    or null/blanks to indicate a positional argument,
*|                    or "\"" for the quote field.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      kw          - Pointer to desired keyword's arg structure, or NULL:
*|        .arg      - Array of argument strings for given keyword.
*|        .narg     - Number of arguments for given keyword.
*|
*| Return values:
*|            0     - given keyword not found
*|         1000     - successful fetch of command line keyword
*|         2000     - successful fetch of system string table keyword
*|      < -9999     - failure statuses
*|
*| Remarks:
*|      none
*|
*| Categories:
*|      USER_INTERFACE
*/

int                                       /* returned status code             */
M0arggetkw(                               /* Fetch Keyword's Argument Data    */
   int               arg_handle,          /* given arg-fetching handle, or 0  */
   const char     *  keyword,             /* given key.word name to retrieve  */
   const McKwArg **  kw)                  /* return keyword argument struct   */
{
   register          i;                   /* loop counter                     */
   const McArg    *  arg = M0argptr(arg_handle);  /* get arg-fetching pointer */
/*----------------------------------------------------------------------------*/
   if (!kw) {                             /* validate given parameter         */
      PROG_ERROR(parameter,               /* print standard diagnostic        */
                 "missing McKwArg **kw - can't get keyword = %s", keyword);
      return PARM_RETERR;                 /* return invalid parm status       */
   }

   *kw = NULL;
/*----------------------------------------------------------------------------*/
/* Find first command line keyword that matches with "key" up thru "keyword". */

   if (arg) {                             /* arg-fetching data there ?        */

      if (POSITIONAL_KEYWORD(keyword)) {  /* positional keyword ?             */
         *kw = arg->kw;                   /* return positional arg structure  */
         return ARG_KEYIN;                /* return successful status         */
      }

      for (i=1;  i < arg->nkw;  i++) {    /* compare to all command keywords  */
         if (!arg->kw[i].arg)  continue;
         if (M0keycmp(keyword, *arg->kw[i].arg) >= -1) {
            *kw = arg->kw + i;            /* return found keyword structure   */
            return ARG_KEYIN;             /* return successful status         */
         }
      }

   }
/*----------------------------------------------------------------------------*/
/* Otherwise get system string table keyword arguments (when implemented).    */

   if (arg == M0argptr(MCHAND)) {         /* is this command line fetching ?  */

      *kw = M0argstrtab(keyword);         /* return string table keyword args */

      if (*kw)  return ARG_STRTAB;        /* return "string table" status     */

   }
/*----------------------------------------------------------------------------*/

   return ARG_DEF;                        /* return keyword not found status  */
}
/*==========================end M0arggetkw====================================*/

/*==============================M0keycmp======================================*/
/*
*| Name:
*|      M0keycmp - Compare given string with given "key.str" match string.
*|                 (case insensitive)
*|
*| Interface:
*|      #include "mcidasp.h"
*|
*|      int
*|      M0keycmp(const char* keystr, const char* given_str)
*|
*| Input:
*|      "key.str"  - Given key string to match with,
*|                   where key=minimum match >=1 char, .str=up to maximum match.
*|                   "key.str" matches with "key" up thru "keystr"
*|                   "keystr" and "keystr." match only with "keystr"
*|      given_str  - Given string to match.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|            0     - successful match
*|           -1     - keystr = NULL/blanks
*|           -2     - unsuccessful match
*|      < -9999     - failure statuses
*|
*| Remarks:
*|      Examples:
*|         M0keycmp("A.REA", "A")       =  0     M0keycmp("A.REA", "AR")   =  0
*|         M0keycmp("A.REA", "ARE")     =  0     M0keycmp("A.REA", "AREA") =  0
*|         M0keycmp("A.REA", "AREANUM") = -2     M0keycmp("A.REA", "MD")   = -2
*|         M0keycmp("AREA",  "AREA")    =  0     M0keycmp("AREA",  "A")    = -2
*|         M0keycmp("",      "")        = -1     M0keycmp(".AREA", "A")    = -2
*|
*| Categories:
*|      USER_INTERFACE
*/

int                                       /* returned status code             */
M0keycmp(                                 /* Compare To A Key String          */
   const char *  keystr,                  /* given "key.str" match name       */
   const char *  given_str)               /* given string trying to match     */
{
   char       *  key;                     /* uppercase work copy of keystr    */
   char       *  str;                     /* uppercase work copy of given_str */
   const char *  dot;                     /* pointer to '.'                   */
                                          /* keystr length w/o trail blanks   */
   int           lenkey = fslen(keystr, (FsLen)STRLEN(keystr));
                                          /* given_str len w/o trail blanks   */
   int           lenstr = fslen(given_str, (FsLen)STRLEN(given_str));
   ptrdiff_t     minlen = lenkey;         /* init minimum acceptable key len  */
   int           status = -2;             /* init status to unsuccessful match*/
/*----------------------------------------------------------------------------*/

   if (lenkey <= 0)  return -1;           /* return if keystr = NULL/blanks   */

                                          /* if '.' found within given key    */
   dot = strchr(keystr, KEYWORD_MINMAX_SEPARATOR);
   if (dot) {
      minlen = dot - keystr;              /* re-compute minimum key length    */
   }

                                          /* else if keystr is "key*" form,   */
   else if (keystr[lenkey-1] == KEYWORD_MATCH_BEGIN) {
      lenstr = lenkey - 1;                /*  find 1st match >= this len      */
      minlen = 0;                         /*  turn off min key length check   */
   }

   if (lenstr < minlen)  return status;   /* if min not there, return failure */

/*----------------------------------------------------------------------------*/
/* Make UPPERCASE, no trailing blanks, working copy of keystr & given_str,
   and try to match GIVEN_STR with "KEY" up thru "KEYSTR".                    */

   MALLOC(key, lenkey + 1);                    /* alloc new memory for keystr */
   *key = '\0';                                /* copy & convert to uppercase */
   mcupcase_(strncat(key, keystr, lenkey), lenkey);

   if (dot && minlen)
      strcpy(key+minlen, key+minlen+1);        /* make key.str into keystr    */

   MALLOC(str, lenstr + 1);                    /* alloc memory for given_str  */
   *str = '\0';                                /* copy & convert to uppercase */
   mcupcase_(strncat(str, given_str, lenstr), lenstr);

   if (!strncmp(key, str, lenstr)) status = 0; /* set status to successful    */

   free(key);                                  /* free the uppercase copy     */
   free(str);                                  /* free the uppercase copy     */

   return status;                              /* return status code          */
}
/*==========================end M0keycmp======================================*/

/*==============================m0keycmp_=====================================*/
/*
*| Name:
*|      m0keycmp - Compare given string with given "key.str" match string.
*|                 (case insensitive)
*|
*| Interface:
*|      integer function
*|      m0keycmp(character*(*) keystr, character*(*) given_str)
*|
*| Input:
*|      "key.str"  - Given key string to match with,
*|                   where key=minimum match >=1 char, .str=up to maximum match.
*|                   "key.str" matches with "key" up thru "keystr"
*|                   "keystr" and "keystr." match only with "keystr"
*|      given_str  - Given string to match.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|            0     - successful match
*|           -1     - unsuccessful match
*|           -2     - keystr = blanks
*|      < -9999     - failure statuses
*|
*| Remarks:
*|      Examples:
*|         m0keycmp('A.REA', 'A')       =  0     m0keycmp('A.REA', 'AR')   =  0
*|         m0keycmp('A.REA', 'ARE')     =  0     m0keycmp('A.REA', 'AREA') =  0
*|         m0keycmp('A.REA', 'AREANUM') = -1     m0keycmp('A.REA', 'MD')   = -1
*|         m0keycmp('AREA',  'AREA')    =  0     m0keycmp('AREA',  'A')    = -1
*|         m0keycmp(' ',     ' ')       = -2     m0keycmp('.AREA', 'A')    = -1
*|
*| Categories:
*|      USER_INTERFACE
*/

Fint                                  /* returned status code                 */
m0keycmp_(                            /* fortran jacket for M0keycmp          */
   const char *  keystr,              /* given "key.str" match name           */
   const char *  given_str,           /* given string trying to match         */
   FsLen         siz_keystr,          /* character*(*) size of 'keystr'       */
   FsLen         siz_given_str)       /* character*(*) size of 'given_str'    */
{
   int           status;              /* returned status code                 */
                                      /* convert fortran char*(*) to C string */
   char       *  keystr_    = fsalloc(keystr,    siz_keystr);
   char       *  given_str_ = fsalloc(given_str, siz_given_str);

   if (keystr_ && given_str_) {

      status = M0keycmp(keystr_, given_str_);

   }

   free(keystr_);                     /* free working C string                */
   free(given_str_);                  /* free working C string                */

   if (!keystr_ || !given_str_)  return MALLOC_RETERR;

   return status;                     /* return the status code               */
}
/*==========================end m0keycmp_=====================================*/

/*==============================M0argstrtab===================================*/
/*
*| Name:
*|      M0argstrtab - Get given keyword's system string table arg-fetching data
*|                    (when implemented).
*|
*| Interface:
*|      McArgKw *
*|      M0argstrtab(const char* keyword)
*|
*| Input:
*|      keyword     - Given name of keyword for fetching string table args.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      !NULL       - Pointer to string table keyword's arg-fetching data.
*|      NULL        - failure - keyword not found, or other error
*|
*| Remarks:
*|      Returns pointer to a static malloc()ed structure known only to this
*|      function.  This function dynamically builds a system string table
*|      arg-fetching structure, containing the looked up keywords.
*|      If a keyword has already been looked up, then its previously built
*|      structure is returned.  Otherwise, lbrepl() is used to retrieve the
*|      given keyword's string table arguments, and a newly parsed keyword
*|      structure is added to the existing string table arg handle.
*|
*|      This functionality is implementation specific.
*|      The arg-fetching string table lookup switch must be turned on
*|      (by calling M0argstrtabset(1)), and 
*|      auto-context-table-search (uc -30) must be turned on.
*|
*| Categories:
*|      USER_INTERFACE
*/

static int  argstrtab_lookup = ARGSTRTAB_LOOKUP;  /* string tab lookup switch */

#define  LBREPL_MAXLEN  256               /* max buffer size within lbrepl_() */

McKwArg *
M0argstrtab(                              /* Fetch String Table Keyword Args  */
   const char *  keyword)                 /* given keyword name               */
{
   extern int    argstrtab_lookup;        /* arg string table lookup switch   */
   McKwArg    *  kw        = NULL;        /* return str tab kw arg structure  */
   McArg      *  arg;                     /* ptr to str tab arg structure     */
   McArg      *  newarg;                  /* ptr to new str tab arg structure */
   static int    starg     = -1;          /* string table arg-fetching handle */
   int           lbreplarg = -1;          /* new str tab keyword's arg handle */
   int           i;                       /* loop counter                     */
   int           len_lbrepltxt;           /* length of lbrepl() str tab text  */
   size_t        sizkey;                  /* size of newkey keyword           */
   size_t        lenkey;                  /* length of newkey keyword         */
   char       *  newkey   = NULL;         /* make new string table keyword    */
   char          lbrepltxt[LBREPL_MAXLEN];/* string table output text         */
   char       *  p;                       /* work pointer                     */

   if (!argstrtab_lookup)  return kw;     /* is string table lookup flag on ? */

   if (Mcluc(-30) <= 0)    return kw;     /* is string table uc on ?          */

   if (!keyword)           return kw;     /* not a keyword ?                  */

   sizkey = strcspn(keyword, " ");

   if (!sizkey)            return kw;     /* keyword all blanks ?             */
                                          /* initialize string tab arg handle */
   if (starg < 0)  starg = Mcargparse("", NULL, NULL);

                                          /* is keyword already there ?       */
   else if (M0arggetkw(starg, keyword, (const McKwArg**)&kw) > 0)  return kw;

   arg = M0argptr(starg);
   if (!arg)  return kw;

/*----------------------------------------------------------------------------*/
/* Get string table keyword, and add its parsed args to str tab arg handle.   */

   if (keyword[sizkey-1] == KEYWORD_MATCH_BEGIN)  --sizkey;

   MALLOC_RC(newkey, sizkey + 2, kw);     /* make string table keyword copy   */
   strncat(strcpy(newkey, "#"), keyword, sizkey);
   mcupcase_(newkey+1, (FsLen)sizkey);    /* convert to uppercase             */

   if ((p = strchr(newkey, KEYWORD_MINMAX_SEPARATOR)))  *p = '\0';
   lenkey = STRLEN(newkey);

                                          /* get string table text            */
   while (lbrepl_(newkey, newkey, lbrepltxt, 1, (FsLen)lenkey, LBREPL_MAXLEN) < 0) {
      if (lenkey >= sizkey) {
         free(newkey);
         return kw;
      }
      newkey[lenkey]   = newkey[lenkey+1];
      newkey[lenkey+1] = '\0';
      lenkey = STRLEN(newkey);
   }

   len_lbrepltxt = fslen(lbrepltxt, LBREPL_MAXLEN);
   lbrepltxt[len_lbrepltxt] = '\0';

   memmove(newkey, newkey+1, lenkey);     /* shift keyword over the '#' char  */

                                          /* parse string table text          */
   if ((lbreplarg = Mcargparse(lbrepltxt, NULL, NULL)) < 0) {
      free(newkey);
      Mcargfree(lbreplarg);
      return kw;
   }
                                          /* get ptr to parsed str tab text   */
   if (!(newarg = M0argptr(lbreplarg)) || !newarg->kw) {
      free(newkey);
      Mcargfree(lbreplarg);
      return kw;
   }

   if (M0argaddkw(starg, newkey) < 0) {   /* add keyword to str tab handle    */
      free(newkey);
      Mcargfree(lbreplarg);
      return kw;
   }
                                          /* add each arg to str tab handle   */
   for (i = 0;  i < newarg->kw[0].narg;  i++) {
      if (M0argadd(starg, newkey, newarg->kw[0].arg[i]) < 0) {
         free(newkey);
         Mcargfree(lbreplarg);
         return kw;
      }
   }

   newarg->kw[0].narg = 0;                /* don't free str tab arg strings ! */
   Mcargfree(lbreplarg);                  /* free the parsed str tab text     */

                                          /* get ptr to newly added keyword   */
   M0arggetkw(starg, keyword, (const McKwArg**)&kw);
   return kw;                             /* return ptr to keyword data       */
}
/*==========================end M0argstrtab===================================*/

/*==============================M0argstrtabset================================*/
/*
*| Name:
*|      M0argstrtabset - Set the arg-fetching string table lookup switch.
*|
*| Interface:
*|      #include "m0arg.h"
*|
*|      void   
*|      M0argstrtabset(int set)
*|
*| Input:
*|      set  - Given string table lookup switch setting, as follows:
*|                >0 = turn on
*|                 0 = turn off
*|                <0 = set to system defined default
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      none
*|
*| Remarks:
*|      This switch is looked at in M0argstrtab().
*|
*| Categories:
*|      USER_INTERFACE
*/

void
M0argstrtabset(                          /* Set Arg String Table Lookup       */
   int         set)                      /* given switch setting              */
{
   extern int  argstrtab_lookup;         /* arg string table lookup switch    */

   argstrtab_lookup = (set >= 0) ? (set) : (ARGSTRTAB_LOOKUP);

}
/*==========================end M0argstrtabset================================*/

/*==============================m0argstrtabset_===============================*/
/*
*| Name:
*|      m0argstrtabset - Set the arg-fetching string table lookup switch.
*|
*| Interface:
*|      subroutine
*|      m0argstrtabset(integer set)
*|
*| Input:
*|      set  - Given string table lookup switch setting, as follows:
*|                >0 = turn on
*|                 0 = turn off
*|                <0 = set to system defined default
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      none
*|
*| Remarks:
*|      This switch is looked at in M0argstrtab().
*|
*| Categories:
*|      USER_INTERFACE
*/

void
m0argstrtabset_(                         /* Set Arg String Table Lookup       */
   const Fint *  set)                    /* given switch setting              */
{
   M0argstrtabset(*set);
}
/*==========================end m0argstrtabset_===============================*/

/*==============================Mcargfree=====================================*/
/*
*$ Name:
*$      Mcargfree - Free parsed arg-fetching structure for the given handle.
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      int
*$      Mcargfree(int arg_handle)
*$
*$ Input:
*$      arg_handle  - Given argument-fetching handle to be freed, or
*$                    0 to free the current McIDAS command's structure.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$        0         - success
*$      < 0         - failure
*$
*$ Remarks:
*$      Frees the arg handle, the arg structure, all of the keyword structures,
*$      and all of the argument strings.
*$
*$ Categories:
*$      USER_INTERFACE
*/

int
Mcargfree(                                /* Free Argument-Fetching Structure */
   int        arg_handle)                 /* given argument-fetching handle   */
{               
   McArg   *  arg = M0argptr(arg_handle); /* get arg-fetching structure ptr   */
   register   i;                          /* loop counter                     */

   if (!arg)  return -1;                  /* return if illegal arg_handle     */

   for (i = 0; i < arg->nkw; i++) {       /* free each keyword's arg info     */
      Mcfreearr((char**)arg->kw[i].arg, arg->kw[i].narg);
   }

   free(arg->kw);                         /* free array of McKwArg structures */
   free(arg);                             /* free McArg structure             */

   return M0handfree(arg_handle - 1);     /* free the arg-fetching handle     */
}
/*==========================end Mcargfree=====================================*/

/*==============================mcargfree_====================================*/
/*
*$ Name:
*$      mcargfree - Free parsed arg-fetching structure for the given handle.
*$
*$ Interface:
*$      integer function
*$      mcargfree(integer arg_handle)
*$
*$ Input:
*$      arg_handle  - Given argument-fetching handle to be freed.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$        0         - success
*$      < 0         - failure
*$
*$ Remarks:
*$      Frees the arg handle, the arg structure, all of the keyword structures,
*$      and all of the argument strings.
*$
*$ Categories:
*$      USER_INTERFACE
*/

Fint
mcargfree_(const Fint * arg_handle)       /* fortran jacket for Mcargfree     */
{
   return Mcargfree(*arg_handle);
}
/*==========================end mcargfree_====================================*/

/*==============================Mcargdump=====================================*/
/*
*$ Name:
*$      Mcargdump - Display parsed arg-fetching to McIDAS debug destination.
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      void
*$      Mcargdump(int arg_handle)
*$
*$ Input:
*$      arg_handle  - Given argument-fetching handle to be displayed, or
*$                    0 to display the current McIDAS command's structure.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      USER_INTERFACE
*$
*/

void
Mcargdump(                                        /* Dump Arg-Fetching Data   */
   int            arg_handle)                     /* given arg handle         */
{
   const McArg *  arg = M0argptr(arg_handle);     /* get arg-fetching ptr     */
   register       i, j;                           /* loop counters            */

   if (!arg)  return;
                                                  /* display command line     */
   Mcdprintf("\nCommand = %.*s\n", (SIZDEST-11), Mcargcmd(arg_handle));

                                                  /* display # of keywords    */
   Mcdprintf("%57s #keywords=%d\n", "", arg->nkw);

   for (i = 0;  i < arg->nkw;  i++) {             /* display all keyword args */
      Mcdprintf("Keyword = %-50.50s (#args=%d)\n",
         (arg->kw[i].arg ? *arg->kw[i].arg : ""), MAX(0, arg->kw[i].narg-1));
      for (j = 1;  j < arg->kw[i].narg;  j++) {
         Mcdprintf("    Arg = %-50.50s (      %d)\n", arg->kw[i].arg[j], j);
      }
   }

   Mcdprintf("\n");
}
/*==========================end Mcargdump=====================================*/

/*==============================mcargdump_====================================*/
/*
*$ Name:
*$      mcargdump - Display parsed arg-fetching to McIDAS debug destination.
*$
*$ Interface:
*$      subroutine
*$      mcargdump(integer arg_handle)
*$
*$ Input:
*$      arg_handle  - Given argument-fetching handle to be displayed, or
*$                    0 to display the current McIDAS command's structure.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      USER_INTERFACE
*$
*/

void
mcargdump_(const Fint * arg_handle)               /* Mcargdump fortran jacket */
{
   Mcargdump(*arg_handle);
}
/*==========================end mcargdump_====================================*/

/*==============================Mcargcmd======================================*/
/*
*$ Name:
*$      Mcargcmd - Build and return a McIDAS command line for the given handle.
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      char*  
*$      Mcargcmd(int arg_handle)
*$
*$ Input:
*$      arg_handle  - Given arg-fetching handle or 0 for current McIDAS command.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      Malloc()ed McIDAS command line, or NULL.
*$
*$ Remarks:
*$      Builds a McIDAS command line, from the parsed arg-fetching structure.
*$      The returned text will be formatted in McIDAS command line syntax.
*$
*$ Categories:
*$      USER_INTERFACE
*/

char *                                           /* return built command line */
Mcargcmd(                                        /* Build McIDAS Command Line */
   int            arg_handle)                    /* given arg-fetching handle */
{
   const McArg *  arg    = M0argptr(arg_handle); /* get arg-fetching ptr      */
   char       **  keytxt;                        /* malloc()ed kw text array  */
   char        *  txtlin;                        /* ptr to malloc()ed command */
   int            lentxt = 0;                    /* malloc()ed command length */
   int            i;                             /* loop counter              */

   if (!arg)  return NULL;                       /* arg data not there ?      */

/* recreate text line from the arg-fetching data in format:
   cmdnam posarg posarg kw=arg arg kw=arg arg "quote field
   args with embedded McIDAS syntax are enclosed within single quotes         */

   MALLOC_RC(keytxt, arg->nkw * sizeof(*keytxt), NULL);

   for (i = 0; i < arg->nkw; i++) {
      keytxt[i] = M0keytxt(arg_handle, i);
      lentxt   += strlen(keytxt[i]);
   }

   MALLOC_RC(txtlin, lentxt + 1, NULL);
   *txtlin = '\0';

   for (i = 0; i < arg->nkw; i++) {
      if (!arg->kw[i].arg  ||  strcmp(*arg->kw[i].arg, QFLD_KEYWORD))
         strcat(txtlin, keytxt[i]);
   }

   for (i = 0; i < arg->nkw; i++) {
      if (arg->kw[i].arg  &&  !strcmp(*arg->kw[i].arg, QFLD_KEYWORD))
         strcat(txtlin, keytxt[i]);
   }

   Mcfreearr(keytxt, arg->nkw);

   return txtlin;
}
/*==========================end Mcargcmd======================================*/

/*==============================mcargcmd_=====================================*/
/*
*$ Name:
*$      mcargcmd - Build and return a McIDAS command line for the given handle.
*$
*$ Interface:
*$      character*(*) function
*$      mcargcmd(integer arg_handle)
*$
*$ Input:
*$      arg_handle  - Given arg-fetching handle, or 0 for McIDAS command.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      McIDAS command line, or NULL.
*$
*$ Remarks:
*$      Builds a McIDAS command line, from the parsed arg-fetching structure.
*$      The returned text will be formatted in McIDAS command line syntax.
*$
*$ Categories:
*$      USER_INTERFACE
*/

void
mcargcmd_(                                  /* fortran jacket for Mcargcmd    */
   char       *  cmdlin,                    /* return current command line    */
   FsLen         siz_cmdlin,                /* character*(*) size of 'cmdlin' */
   const Fint *  arg_handle)                /* given arg-fetching handle      */
{
   char       *  cmdlin_ = NULL;            /* declare working C string       */

   cmdlin_ = Mcargcmd(*arg_handle);         /* call actual C function         */

   Mcstrtofs(cmdlin, cmdlin_, siz_cmdlin);  /* global cmdlin to fort char*(*) */

   free(cmdlin_);
}
/*==========================end mcargcmd_=====================================*/

/*==============================M0keytxt======================================*/
/*
*|
*| Name:
*|      M0keytxt - Reformat the given nth arg-fetching keyword into McIDAS text.
*|
*| Interface:
*|      #include "m0arg.h"
*|
*|      char*
*|      M0keytxt(int arg_handle, int nthkey)
*|
*| Input:
*|      arg_handle  - Given arg-fetching handle or 0 for current McIDAS command.
*|      nthkey      - Given position of arg-fetching keyword within structure.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      Pointer to malloc()ed re-creation of keyword arg text in McIDAS syntax,
*|      or NULL if failure.
*|
*| Remarks:
*|      Returns text representation of the given 'nthkey' keyword within the
*|      parsed arg-fetching structure.  The returned text is formatted in
*|      McIDAS command line syntax.
*|
*| Categories:
*|      USER_INTERFACE
*|
*/

char *                                           /* return arg keyword text   */
M0keytxt(                                        /* Format Arg Keyword Text   */
   int              arg_handle,                  /* given arg-fetching handle */
   int              nthkey)                      /* given keyword position    */
{
   const McArg   *  arg                = M0argptr(arg_handle);
   const McKwArg *  kw;
   int              i;
   int              lentxt             = 0;
   char          *  keytxt;
   char             sep                = nthkey ? *KEYWORD_SEPARATORS : ' ';
   char             quoting_delims[3]  = "  ";

   if (!arg)  return NULL;
   if (nthkey < 0  ||  nthkey >= arg->nkw)  return NULL;

   kw = &arg->kw[nthkey];

   quoting_delims[0] = *QUOTING_DELIMS_LEFT;
   quoting_delims[1] = *QUOTING_DELIMS_RIGHT;

/* first, run thru args to compute the malloc size                            */
   for (i = 0; i < kw->narg; i++) {
      if (!kw->arg[i]  ||  !strcmp(kw->arg[i], MISSING_ARG_VALUE)) {
         lentxt += STRLEN(MISSING_ARGUMENT);
      }
      else {
         const char *p, *p2;
/*       case insensitive missing argument token?                             */
         for (p = kw->arg[i], p2 = MISSING_ARGUMENT;  *p && *p2;  p++, p2++) 
            if ((islower(*p)  ? _toupper(*p)  : *p)   !=
                (islower(*p2) ? _toupper(*p2) : *p2)) break;
         if ((!*p && !*p2) || strpbrk(kw->arg[i], MCIDAS_SYNTAX_CHARS)) {
            const char * p3 = kw->arg[i] - 1;  
            while ((p3 = strpbrk(p3 + 1, quoting_delims)))  ++lentxt;
            lentxt += 2;
         }
         lentxt += strlen(kw->arg[i]);
      }
      ++lentxt;
   }

   MALLOC_RC(keytxt, lentxt + 1, NULL);
   *keytxt = '\0';

   if (kw->arg  &&  !strcmp(*kw->arg, QFLD_KEYWORD)) {
      for (i = 1; i < kw->narg; i++) {
         sprintf(strchr(keytxt, '\0'), "%s%s", QFLD_KEYWORD, kw->arg[i]);
      }
      return keytxt;
   }

   for (i = 0; i < kw->narg; i++) {
      if (!kw->arg[i]  ||  !strcmp(kw->arg[i], MISSING_ARG_VALUE)) {
         sprintf(strchr(keytxt, '\0'), "%s%c", MISSING_ARGUMENT, sep);
      }
      else {
         const char *p, *p2;
/*       case insensitive missing argument token?                             */
         for (p = kw->arg[i], p2 = MISSING_ARGUMENT;  *p && *p2;  p++, p2++) 
            if ((islower(*p)  ? _toupper(*p)  : *p)   !=
                (islower(*p2) ? _toupper(*p2) : *p2)) break;
         if ((!*p && !*p2) || strpbrk(kw->arg[i], MCIDAS_SYNTAX_CHARS)) {
            char * p3 = strchr(keytxt, '\0');
            sprintf(p3, "%c%s%c%c", *QUOTING_DELIMS_LEFT,  kw->arg[i],
                                   *QUOTING_DELIMS_RIGHT, sep);
            while ((p3 = strpbrk(p3 + 1, quoting_delims)) && p3[2]) {
               memmove(p3+1, p3, strlen(p3) + 1);
               ++p3;
            }
         }
         else {
            sprintf(strchr(keytxt, '\0'), "%s%c", kw->arg[i], sep);
         }
      }
      sep = ' ';
   }

   return keytxt;
}
/*==========================end M0keytxt======================================*/

/*==============================Mcargnam======================================*/
/*
*$ Name:
*$      Mcargnam - Fetch all keyword names within parsed arg-fetching text.
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      int
*$      Mcargnam(int arg_handle, int maxkey, const char* keywords[maxkey])
*$
*$ Input:
*$      arg_handle  - Given arg-fetching handle, or 0 for McIDAS command.
*$      maxkey      - Given dimension size of keywords array.
*$                    If <= 0, then only returns the number of keywords.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      keywords    - Array of keyword names as they occur in text line.
*$
*$ Return values:
*$      >=    0     - Number of keyword names found.
*$         -905     - keyword dimension size exceeded
*$         -909     - no command line arg-fetching structure
*$      < -9999     - failure statuses
*$
*$ Remarks:
*$      The returned 'keywords' array, contains pointers to the arg-fetching
*$      structure's malloc()ed keyword names, WHICH MUST NOT BE FREED!
*$
*$      If the number of keywords exceeds 'maxkey', then the 'keywords' array
*$      contains the first 'maxkey' occurring keyword names.
*$
*$      If # of keywords < 'maxkey', then the 'keywords' array is NULL filled.
*$
*$      If 'maxkey' <= 0, then the number of existing keywords is returned.
*$
*$ Categories:
*$      USER_INTERFACE
*/

int                                        /* returned number of keywords     */
Mcargnam(                                  /* Retrieve Keyword Names          */
   int              arg_handle,            /* given arg-fetching handle       */
   int              maxkey,                /* dimension size of 'keywords'    */
   const char    *  keywords[])            /* string array of keyword names   */
{
   int              numkey = 0;            /* return # of keywords found      */
   const McArg   *  arg = M0argptr(arg_handle);      /* get arg structure ptr */
   const McKwArg *  kw;                    /* loop pointer to keyword cells   */
   const McKwArg *  endkw;                 /* pointer to end of arg keywords  */
   const char   **  endkey;                /* pointer to end of keyword array */
   int              i;                     /* loop counter                    */
/*----------------------------------------------------------------------------*/

   if (!keywords  &&  maxkey > 0) {        /* validate given parameter        */
      PROG_ERROR(parameter,                /* print standard diagnostic       */
         "missing char **keywords - can't retrieve keyword names", NULL);
      return PARM_RETERR;                  /* return invalid parm status      */
   }

   for (i=0;  i < maxkey;  i++)  keywords[i] = NULL;

   if (!arg) return -(ARG_KW + ARG_NONE);  /* arg-fetching data not there ?   */

   endkw  = arg->kw  + arg->nkw;
   endkey = keywords + maxkey;

   numkey = arg->nkw - 1;
/*----------------------------------------------------------------------------*/
   if (maxkey <= 0) {                      /* return # keywords if maxkey<=0  */

      for (kw = arg->kw + 1;  kw < endkw;  kw++)
         if (!kw->arg  ||  !strcmp(*kw->arg, QFLD_KEYWORD))  numkey--;

      return numkey;

   }
/*----------------------------------------------------------------------------*/

   for (kw = arg->kw + 1;  kw < endkw  &&  keywords < endkey;  kw++)
      if (!kw->arg  ||  !strcmp(*kw->arg, QFLD_KEYWORD)) numkey--;
      else                                               *keywords++ = *kw->arg;

   for (;  kw < endkw;  kw++)
      if (!kw->arg  ||  !strcmp(*kw->arg, QFLD_KEYWORD)) numkey--;

   if (numkey <= maxkey)  return numkey;                 /* return # keywords */
   else                   return -(ARG_KW + ARG_GT);     /* dim size exceeded */

}
/*==========================end Mcargnam======================================*/

/*==============================mcargnam_=====================================*/
/*
*$ Name:
*$      mcargnam - Fetch all keyword names within parsed arg-fetching text.
*$
*$ Interface:
*$      integer function
*$      mcargnam(integer arg_handle, integer maxkey,
*$               character*(*) keywords(maxkey))
*$
*$ Input:
*$      arg_handle  - Given arg-fetching handle, or 0 for McIDAS command.
*$      maxkey      - Given dimension size of keywords array.
*$                    If <= 0, then only returns the number of keywords.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      keywords    - Array of keyword names as they occur in text line.
*$
*$ Return values:
*$      >=    0     - Number of keyword names found.
*$      <     0     - # of keyword names that were truncated
*$         -905     - keyword dimension size exceeded
*$         -909     - no command line arg-fetching structure
*$      < -9999     - failure statuses
*$
*$ Remarks:
*$      If the number of keywords exceeds 'maxkey', then the 'keywords' array
*$      contains the first 'maxkey' occurring keyword names.
*$
*$      If # of keywords < 'maxkey', then the 'keywords' array is blank filled.
*$
*$      If 'maxkey' <= 0, then the number of existing keywords is returned.
*$
*$ Categories:
*$      USER_INTERFACE
*/

Fint                                      /* returned number of keywords      */
mcargnam_(                                /* fortran jacket for Mcargnam      */
   const Fint  *  arg_handle,             /* given arg-fetching handle or 0   */
   const Fint  *  maxkey,                 /* 'keywords' array dimension size  */
   char        *  keywords,               /* return array of keyword names    */
   FsLen          siz_keywords)           /* character*(*) size of 'keywords' */
{
   Fint           numkey;                 /* return # of keywords found       */
   int            numkey_;                /* declare C value                  */
   int            stat;                   /* status code                      */
   const char **  keywords_;              /* declare working C strings array  */

   MALLOC(keywords_, MAX(0,*maxkey) * sizeof(*keywords_));       

   numkey_ = Mcargnam(*arg_handle, *maxkey, keywords_);

                                          /* convert C array to fort char*(*) */
   stat = Mcarrtofs(keywords_, *maxkey, *maxkey, siz_keywords, keywords);
   free(keywords_);                       /* free the working C strings array */

   if (stat < 0)                               return stat;
   if ((stat = NUMC2F(&numkey, numkey_)) < 0)  return stat;

   return numkey;                         /* return the # of keywords         */
}
/*==========================end mcargnam_=====================================*/

/*==============================Mcargnum======================================*/
/*
*$ Name:
*$      Mcargnum - Return # args for given keyword in parsed arg-fetching text.
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      int
*$      Mcargnum(int arg_handle, const char* keyword)
*$
*$ Input:
*$      arg_handle  - Given arg-fetching handle, or 0 for McIDAS command.
*$      keyword     - Given name of the keyword, in key.word format,
*$                    where key=minimum keyword name, .word=maximum name,
*$                    or null/blanks to indicate a positional argument.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$         >= 0     - The number of arguments existing for given keyword.
*$      < -9999     - failure statuses
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      USER_INTERFACE
*/

int                                          /* returned status code          */
Mcargnum(                                    /* Get # Keyword Args            */
   int              arg_handle,              /* given arg-fetching handle     */
   const char    *  keyword)                 /* given key.word name or null   */
{
   const McKwArg *  kw = NULL;               /* ptr to given keyword's data   */
                                             /* get given keyword's data      */
   int              status = M0arggetkw(arg_handle, keyword, &kw);

   if (!kw)  return status;                  /* return 0 or error status code */

   return kw->narg - 1;                      /* return the # of arguments     */
}
/*==========================end Mcargnum======================================*/

/*==============================mcargnum_=====================================*/
/*
*$ Name:
*$      mcargnum - Return # args for given keyword in parsed arg-fetching text.
*$
*$ Interface:
*$      integer function
*$      mcargnum(integer arg_handle, character*(*) keyword)
*$
*$ Input:
*$      arg_handle  - Given arg-fetching handle, or 0 for McIDAS command.
*$      keyword     - Given name of the keyword, in key.word format,
*$                    where key=minimum keyword name, .word=maximum name,
*$                    or null/blanks to indicate a positional argument.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$         >= 0     - The number of arguments existing for given keyword.
*$      < -9999     - failure statuses
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      USER_INTERFACE
*/

Fint                                  /* returned status code                 */
mcargnum_(                            /* fortran jacket for Mcargnum          */
   const Fint *  arg_handle,          /* given arg-fetching handle or 0       */
   const char *  keyword,             /* given key.word name or blank         */
   FsLen         siz_keyword)         /* character*(*) size of 'keyword'      */
{
   int           num;                 /* # of arguments for given keyword     */
                                      /* convert fortran char*(*) to C string */
   char       *  keyword_ = fsalloc(keyword, siz_keyword);

   if (!keyword_)  return MALLOC_RETERR;

   num = Mcargnum(*arg_handle, keyword_);

   free(keyword_);                    /* free working C string                */

   return num;                        /* return # of arguments for keyword    */
}
/*==========================end mcargnum_=====================================*/

/*==========================end argutil.c=====================================*/
