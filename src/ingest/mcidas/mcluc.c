/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 MCLUC.C 5-May-95,11:11:38,`JMB' re-packaged for convenience of os2 5401 */
/* 2 MCLUC.C 5-May-95,15:05:16,`RICKK' Removed prototypes for puc_ and luc_  */
/*      and added category information (5401)                                */
/* 3 MCLUC.C 6-Jun-95,15:08:52,`USER' Released                               */
/* 4 MCLUC.C 19-Feb-96,16:01:36,`DWS' reglue: modified file                  */
/* 5 MCLUC.C 20-Feb-96,11:59:44,`USER' Released                              */
/* 6 MCLUC.C 10-Jun-96,9:01:06,`BILLL' Added programmer documentation        */
/*      (6653).                                                              */
/* 7 MCLUC.C 6-Sep-96,10:18:22,`USER' Released                               */
/**** McIDAS Revision History *** */

#include "mcidasp.h"

/*
*$ Name:
*$      Mcluc  - Retrieves user common value designated by input index.
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      Fint 
*$      Mcluc(int index)
*$
*$ Input:
*$      index  - Index into user common array.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      Desired user common value.
*$
*$ Remarks:
*$      Returns the value currently stored at the designated position in 
*$      user common.
*$      For positive indexes, the value is from session based common;
*$      for negative indexes, it is from a process-based common.
*$      The index 0 is treated differently between UNIX and os2.
*$
*$ Categories:
*$      sys_config
*$      system
*/

Fint 
Mcluc(int index)
{
	Fint i = (Fint) index;

	return luc_(&i);
}


/*
*$ Name:
*$      Mcpuc - Modifies the contents of a designated user common location.
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      void
*$      Mcpuc(Fint value, int index)
*$
*$ Input:
*$      value  - Value to placed into user common array.
*$      index  - Index into user common array.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      None
*$
*$ Remarks:
*$      Changes the contents of a particular word in user common.
*$      For positive indexes, the value is in session based common;
*$      for negative indexes, it is in a process-based common.
*$      The index 0 is treated differently between UNIX and os2.
*$
*$ Categories:
*$      sys_config
*$      system
*/

void
Mcpuc(Fint value, int index)
{
	Fint i = (Fint) index;

	puc_(&value, &i);
}
