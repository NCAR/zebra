/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 HANDLE.C 4-Apr-95,10:19:14,`JUDYK' McIDAS API Generic "Handle" Functions*/
/* 2 HANDLE.C 2-May-95,16:45:00,`JUDYK' chg M0handalloc() to M0handalloc(void*/
/* 3 HANDLE.C 12-May-95,9:33:44,`JUDYK' add const to M0handget               */
/* 4 HANDLE.C 6-Jun-95,15:02:40,`USER' Released                              */
/* 5 HANDLE.C 28-Jun-95,17:04:36,`JUDYK' change Fint4 to Fint                */
/* 6 HANDLE.C 15-Jan-96,14:49:16,`USER' Released                             */
/* 7 HANDLE.C 5-Apr-96,16:17:24,`DGLO' Handle uninitialized handle pool      */
/* 8 HANDLE.C 17-Apr-96,14:59:08,`USER' Released                             */
/* 9 HANDLE.C 12-Sep-96,13:30:24,`BILLL' Added programmer documentation      */
/*      (6993).                                                              */
/* 10 HANDLE.C 4-Oct-96,10:21:20,`USER' Released                             */
/**** McIDAS Revision History *** */

/*==============================handle.c======================================*/
/*
        handle.c   - McIDAS API Handle Functions
                     Assign and use integer handles in place of void* addresses.
                     Uses a global handle pool, known only to this file.
*/
/*----------------------------------------------------------------------------*/

#include <stddef.h>
#include <stdlib.h>

#include "mcidas.h"
#include "mcidasp.h"
#include "m0arg.h"

/*----------------------------------------------------------------------------*/
/* Define typedefs, structures and unions                                     */

   typedef struct HandPool {              /* define structure for handle pool */
      void *  ptr;                        /* pointer assigned to given handle */
      int     free;                       /* is handle assigned ? 1=yes, 0=no */
   } HandPool;

/*----------------------------------------------------------------------------*/
/* Define external global variables known only to following programs          */
                                 
static HandPool *  pool  = NULL;          /* pointer to available handle pool */
static int         npool = 0;             /* # of available handles           */

/*==============================M0handalloc===================================*/
/*
*| Name:
*|      M0handalloc - Allocates a new handle from the pool of
*|                    available handles.
*|
*| Interface:
*|      #include "mcidasp.h"
*|
*|      int
*|      M0handalloc(void)
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
*|      >= 0       - The new handle.
*|      <  0       - Failure.
*|
*| Remarks:
*|      The newly allocated handle is assigned to the NULL pointer.
*|      Expands the pool of available handles if necessary.
*|
*| Categories:
*|      utility
*/

int                                           /* returned handle              */
M0handalloc(void)                             /* Handle Allocate              */
{
   extern HandPool *  pool;                   /* ptr to available handle pool */
   extern int         npool;                  /* number of available handles  */
   HandPool           pool0 = { 0 };          /* used to init to zeros        */
   register           i;                      /* loop counter                 */

   for (i=0; i<npool && !pool[i].free; i++);  /* find free handle within pool */

   if (i >= npool) {                          /* not a free handle ?          */
      ++npool;                                /* expand the pool              */
      REALLOC(pool, npool * sizeof(*pool));
   }

   pool[i] = pool0;                           /* init new handle to zeros     */

   return i;                                  /* return new allocated handle  */
}
/*==========================end M0handalloc===================================*/

/*==============================m0handalloc_==================================*/
/*
*| Name:
*|      m0handalloc - Allocates a new handle from the pool of
*|                    available handles.
*|
*| Interface:
*|      integer function
*|      m0handalloc()
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
*|      >= 0       - The new handle.
*|      <  0       - Failure.
*|
*| Remarks:
*|      The newly allocated handle is assigned to the NULL pointer.
*|      Expands the pool of available handles if necessary.
*|
*| Categories:
*|      utility
*/

Fint                                          /* returned handle              */
m0handalloc_(void)                            /* M0handalloc's fortran jacket */
{
   int   handle = M0handalloc();
   Fint  ret    = handle;

   if (ret != handle)  return NUMC2F_RETERR;

   return ret;                                /* return new allocated handle  */
}
/*==========================end m0handalloc_==================================*/

/*==============================M0handfree====================================*/
/*
*| Name:
*|      M0handfree - Frees a handle from the pool of available handles.
*|
*| Interface:
*|      #include "mcidasp.h"
*|
*|      int
*|      M0handfree(int handle)
*|
*| Input:
*|      handle     - Handle to be freed.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|         0       - Success.
*|      <  0       - Failure.
*|
*| Remarks:
*|      Returns the given handle to the pool of available handles.
*|
*| Categories:
*|      utility
*/

int                                           /* returned status code         */
M0handfree(                                   /* Handle Free                  */
   int                handle)                 /* given handle to be freed     */
{               
   extern HandPool *  pool;                   /* ptr to available handle pool */
   extern int         npool;                  /* number of available handles  */
   register           i;                      /* loop counter                 */

   if (handle < 0)  return PARM_RETERR;

   if (handle >= npool) {                     /* validate given parameter     */
      PROG_ERROR(parameter,                   /* print standard diagnostic    */
         "invalid parm - can't free invalid handle=%d", handle);
      return PARM_RETERR;                     /* return invalid parm status   */
   }

   pool[handle].free = 1;                     /* set handle to "free"         */

/* if no assigned handles after newly freed handle, then reallocate the pool  */

   for (i = handle+1;  i < npool;  i++)
      if (!pool[i].free)  return 0;           /* assigned handle after ?      */

   npool = handle;                            /* reallocate the handle pool   */
   if (npool) {
      REALLOC(pool, npool * sizeof(*pool));
   }
   else {
      free(pool);
      pool = NULL;
   }

   return 0;                                  /* return successful status     */
}
/*==========================end M0handfree====================================*/

/*==============================m0handfree_===================================*/
/*
*| Name:
*|      m0handfree - Frees a handle from the pool of available handles.
*|
*| Interface:
*|      integer function
*|      m0handfree(integer handle)
*|
*| Input:
*|      handle     - Handle to be freed.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|         0       - Success.
*|      <  0       - Failure.
*|
*| Remarks:
*|      Returns the handle to the pool of available handles.
*|
*| Categories:
*|      utility
*/

Fint 
m0handfree_(const Fint* handle)              /* fortran jacket for M0handfree */
{
   return M0handfree(*handle);
}
/*==========================end m0handfree_===================================*/

/*==============================M0handget=====================================*/
/*
*| Name:
*|      M0handget - Returns the pointer assigned to a handle.
*|
*| Interface:
*|      #include "mcidasp.h"
*|
*|      int
*|      M0handget(int handle, void **ret_ptr)
*|
*| Input:
*|      handle     - Handle for the desired pointer.
*|
*| Input and Output:
*|      ret_ptr    - The pointer assigned to the handle.
*|
*| Output:
*|      none
*|
*| Return values:
*|         0       - success
*|      <  0       - failure
*|
*| Remarks:
*|      Copies the pointer assigned to the input handle, to the pointer
*|      whose location is given by ret_ptr.
*|
*| Categories:
*|      utility
*/

int                                           /* returned status code         */
M0handget(                                    /* Get Handle Pointer           */
   int                handle,                 /* given handle                 */
   void           **  ret_ptr)                /* returned pointer assignment  */
{
   extern HandPool *  pool;                   /* ptr to available handle pool */
   extern int         npool;                  /* number of available handles  */

   if (!ret_ptr) {                            /* validate given parameter     */
      PROG_ERROR(parameter,                   /* print standard diagnostic    */
         "missing void **ret_ptr - can't get pointer for handle=%d", handle);
      return PARM_RETERR;                     /* return invalid parm status   */
   }
   *ret_ptr = NULL;

   if (handle < 0 || npool <= 0)  return PARM_RETERR;

   if (handle >= npool) {                     /* validate given parameter     */
      PROG_ERROR(parameter,                   /* print standard diagnostic    */
         "invalid parm - can't get pointer for handle=%d", handle);
      return PARM_RETERR;                     /* return invalid parm status   */
   }

   *ret_ptr = pool[handle].ptr;               /* return the desired pointer   */

   return 0;                                  /* return successful status     */
}
/*==========================end M0handget=====================================*/

/*==============================M0handput=====================================*/
/*
*| Name:
*|      M0handput - Sets a pointer to a handle.
*|
*| Interface:
*|      #include "mcidasp.h"
*|
*|      int
*|      M0handput(int handle, const void* given_ptr)
*|
*| Input:
*|      handle     - Handle (within a pool) to update.
*|      given_ptr  - Pointer to which handle is set.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|         0       - Success.
*|      <  0       - Failure.
*|
*| Remarks:
*|      Sets the pointer associated with the given handle to given_ptr.
*|      The handle is one of a pool of handles.
*|
*| Categories:
*|      utility
*/

int                                           /* returned status code         */
M0handput(                                    /* Put Handle Pointer           */
   int                handle,                 /* given handle                 */
   const void      *  given_ptr)              /* given pointer                */
{
   extern HandPool *  pool;                   /* ptr to available handle pool */
   extern int         npool;                  /* number of available handles  */

   if (handle < 0)  return PARM_RETERR;

   if (handle >= npool) {                     /* validate given parameter     */
      PROG_ERROR(parameter,                   /* print standard diagnostic    */
         "invalid parm - can't put pointer for handle=%d", handle);
      return PARM_RETERR;                     /* return invalid parm status   */
   }

   pool[handle].ptr = (void*)given_ptr;       /* put the given pointer        */

   if (pool[handle].free)  return -1;         /* updating unassigned handle ? */

   return 0;                                  /* return successful status     */
}
/*==========================end M0handput=====================================*/

/*==========================end handle.c======================================*/
