/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 ARGCMD.C 3-Apr-95,15:42:10,`JUDYK' Arg-Fetching Command Handle Functions*/
/* 2 ARGCMD.C 2-May-95,16:28:52,`JUDYK' remove M0argptr, chg M0cmdget(void)  */
/* 3 ARGCMD.C 6-Jun-95,14:57:18,`USER' Released                              */
/* 4 ARGCMD.C 28-Jun-95,17:02:02,`JUDYK' change Fint4 to Fint                */
/* 5 ARGCMD.C 15-Jan-96,14:47:22,`USER' Released                             */
/* 6 ARGCMD.C 19-Feb-96,15:35:02,`DWS' reglue: modified file                 */
/* 7 ARGCMD.C 20-Feb-96,11:49:04,`USER' Released                             */
/**** McIDAS Revision History *** */

/*==============================argcmd.c======================================*/
/*
        argcmd.c - Argument-Fetching Command Line Functions
                   ARGAPI functions pertaining to the McIDAS command line.
*/
/*----------------------------------------------------------------------------*/

#include "mcidas.h"
#include "m0arg.h"

/*----------------------------------------------------------------------------*/
/* Define global variables known only to following programs                   */
                                 
static int  cmd_handle = 0;               /* command line arg-fetching handle */

/*==============================M0cmdput======================================*/
/*
*| Name:
*|      M0cmdput - Set McIDAS command argument-fetching to given arg handle,
*|                 so that command arguments can be fetched by Mccmdint, etc.
*|                 Sets global handle to command line's arg-fetching structure.
*|
*| Interface:
*|      #include "m0arg.h"
*|
*|      int    
*|      M0cmdput(int arg_handle)
*|
*| Input:
*|      arg_handle  - Given command line argument-fetching handle.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|        0         - success
*|      < 0         - illegal arg_handle
*|
*| Remarks:
*|      none
*|
*| Categories:
*|      USER_INTERFACE
*/

int                                       /* returned status code             */
M0cmdput(                                 /* Put Command Arg-Fetching Handle  */
   int         arg_handle)                /* given argument-fetching handle   */
{
   if (!arg_handle)  return 0;            /* return if given handle is 0      */

                                          /* validate given parameter         */
   if (Mcargnam(arg_handle, 0, NULL) == -(ARG_KW+ARG_NONE)) {
      PROG_ERROR(parameter,               /* print standard diagnostic        */
         "can't set arg-fetching to illegal arg_handle parm =%d", arg_handle);
      return PARM_RETERR;                 /* return invalid parm status       */
   }

   cmd_handle = arg_handle;               /* set cmd-fetching to given handle */
   return 0;                              /* return successful status         */
}
/*==========================end M0cmdput======================================*/

/*==============================m0cmdput_=====================================*/
/*
*| Name:
*|      m0cmdput - Set McIDAS command argument-fetching to given arg handle,
*|                 so that command arguments can be fetched by mccmdint, etc.
*|                 Sets global handle to command line's arg-fetching structure.
*|
*| Interface:
*|      integer function
*|      m0cmdput(integer arg_handle)
*|
*| Input:
*|      arg_handle  - Given command line argument-fetching handle.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|        0         - success
*|      < 0         - illegal arg_handle
*|
*| Remarks:
*|      none
*|
*| Categories:
*|      USER_INTERFACE
*/

Fint  
m0cmdput_(const Fint * arg_handle)        /* fortran jacket for M0cmdput      */
{
   return M0cmdput(*arg_handle);
}
/*==========================end m0cmdput_=====================================*/

/*==============================M0cmdget======================================*/
/*
*| Name:
*|      M0cmdget - Get the current McIDAS command's argument-fetching handle.
*|
*| Interface:
*|      #include "m0arg.h"
*|
*|      int
*|      M0cmdget(void)
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
*|      Current command's argument-fetching handle.
*|
*| Remarks:
*|      none
*|
*| Categories:
*|      USER_INTERFACE
*/

int                                       /* returned arg-fetching handle     */
M0cmdget(void)                            /* Get Command Arg-Fetching Handle  */
{
   return cmd_handle;                     /* return command line arg handle   */
}
/*==========================end M0cmdget======================================*/

/*==============================m0cmdget_=====================================*/
/*
*| Name:
*|      m0cmdget - Get the current McIDAS command's argument-fetching handle.
*|
*| Interface:
*|      integer function
*|      m0cmdget()
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
*|      Current command's argument-fetching handle.
*|
*| Remarks:
*|      none
*|
*| Categories:
*|      USER_INTERFACE
*/

Fint                                      /* returned arg-fetching handle     */
m0cmdget_(void)                           /* Get Command Arg-Fetching Handle  */
{
   return M0cmdget();                     /* return command line arg handle   */
}
/*==========================end m0cmdget_=====================================*/

/*==========================end argcmd.c======================================*/
