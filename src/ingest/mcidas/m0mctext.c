/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 M0MCTEXT.C 19-Feb-96,15:18:22,`DWS' reglue: new file                    */
/* 2 M0MCTEXT.C 20-Feb-96,12:11:14,`USER' Released                           */
/**** McIDAS Revision History *** */

#include <stdlib.h>

#include "mcidasp.h"
#include "m0glue.h"

/*
*|  Name:
*|        M0mctext - are we running under MCTEXT?
*|
*|  Interface:
*|        #include "mcidasp.h"
*|
*|        int
*|        M0mctext(void)
*|
*|  Input:
*|        none
*|
*|  Input and output:
*|        none
*|
*|  Output:
*|        none
*|
*|  Return values:
*|        0	- the process is NOT attached to a MCTEXT window
*|        <>0	- the process is attached to a MCTEXT window
*|
*|  Remarks:
*|        none
*/

int
M0mctext(void)
{
	static int ans;
	static int have_ans;

	if(!have_ans)
	{
		ans = (getenv(M0ENV_TEXT) ? 1 : 0);
		have_ans = 1;
	}
	
	return ans;
}

/*
*|  Name:
*|        m0mctext - are we running under MCTEXT?
*|
*|  Interface:
*|        integer function
*|        m0mctext(void)
*|
*|  Input:
*|        none
*|
*|  Input and output:
*|        none
*|
*|  Output:
*|        none
*|
*|  Return values:
*|        0	- the process is NOT attached to a MCTEXT window
*|        <>0	- the process is attached to a MCTEXT window
*|
*|  Remarks:
*|        none
*/

Fint
m0mctext_(void)
{
	return (Fint) M0mctext();
}
