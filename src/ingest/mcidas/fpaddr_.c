/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 FPADDR.C 10-Feb-93,11:54:26,`SUEG' get shared memory address for redirec*/
/* 2 FPADDR.C 12-Mar-93,11:05:52,`USER' Released for McIDAS-X Only           */
/* 3 FPADDR.C 1-Mar-94,10:04:04,`RUSSD' Standardize function names           */
/* 4 FPADDR.C 31-Mar-94,22:17:06,`BARRYR' Add proprietary statement          */
/* 5 FPADDR.C 31-Mar-94,23:46:08,`BARRYR' Add proprietary statement          */
/* 6 FPADDR_.C 2-May-94,15:59:18,`USER' Released for McIDAS-X Only           */
/* 7 FPADDR_.C 3-Aug-94,16:28:46,`DWS' Modified for Solaris (4731)           */
/* 8 FPADDR_.C 22-Aug-94,6:40:44,`USER' Released for McIDAS-X Only           */
/* 9 FPADDR_.C 1-May-95,16:43:18,`DWS' LADDR removal (5402)                  */
/* 10 FPADDR_.C 18-May-95,13:52:12,`JMB' addressing fox for os/2 (5402)      */
/* 11 FPADDR_.C 6-Jun-95,12:08:28,`RICKK' Updated revision history cards -   */
/*      moving from aix to com.                                              */
/* 12 FPADDR_.C 6-Jun-95,15:02:02,`USER' Released                            */
/* 13 FPADDR_.C 19-Feb-96,15:47:32,`DWS' reglue: modified file               */
/* 14 FPADDR_.C 20-Feb-96,11:52:50,`USER' Released                           */
/**** McIDAS Revision History *** */

/*--------------------------------------------------------------------*/
/* [fpaddr_.c ] 
 *
 * purpose:  
 *        Control access to the shared memory segment containing
 *  REDIRECT entries.
 *
 */
/*--------------------------------------------------------------------*/


#include <stdlib.h>
#include <sys/types.h>
#include <string.h>

#include "m0glue.h"


#define ADDRESS ((char *) m0posuc+Mcluc(465)) /*  offset to start of VOLNAM memory */

static char *address=0;        /*  local pointer to VOLNAM memory   */



/*
*| Name:
*|      m0volrd_  copy data from VOLNAM block to application memory
*|
*| Interface:
*|      #include "mcidas.h"
*|
*|      void
*|      m0volrd_( int *bytes, int *offset, void* location)
*|
*| Input:
*|      bytes    - number of bytes to transfer
*|      offset   - relative to the start of the block as 0
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      location - target of the copy
*|
*| Return values:
*|       none              
*|
*| Remarks:
*|      Copies from VOLNAM memory to application memory
*|      if returns bytes of 0 if nothing has ever been written
*|
*| Categories: 
*|      SYSTEM
*/
void
m0volrd_( int *bytes, int *offset, void *location)
{
    /* try to set address if necessary */
    if(address== 0) 
        if(m0posuc !=(int *) 0)
            address=ADDRESS;

    /* source zeros or data */
    if(address==0) {
        (void) memset( location, 0, *bytes);
    }  else  {
        (void) memcpy( location , address + *offset, *bytes);
    }
    
}

/*
*| Name:
*|      m0volwr_  copy data to VOLNAM block from application memory
*|
*| Interface:
*|      #include "mcidas.h"
*|
*|      void
*|      m0volwr_( int *bytes, int *offset, void *location)
*|
*| Input:
*|      bytes    - number of bytes to transfer
*|      offset   - relative to the start of the block as 0
*|      location - source of the copy
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|       none              
*|
*| Remarks:
*|      Copies to VOLNAM memory from application memory
*|      if UC is not set up, creates local storage for table
*|
*| Categories: 
*|      SYSTEM
*/
void
m0volwr_( int *bytes, int *offset, void *location)
{
    /* try to set address if necessary and if possible */
    if(address== 0) 
        if(m0posuc !=(int *) 0)
            address=ADDRESS;

    /* if there is no address, create one ! */
    if(address==0) address=malloc(30000);

    (void) memcpy( address + *offset, location, *bytes);
    
}
