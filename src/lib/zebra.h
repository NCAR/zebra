#ifndef _ZEBRA_H
#define _ZEBRA_H

/* 
 * glibc rpc.h (included in the auto-generated zebra_rpc.h below) defines
 * TRUE and FALSE in an obnoxious manner, without first checking to see if
 * they exist.  It conflicts with a lot of things, including zl_param.h and
 * X11/Intrinsic.h.  Clear any existing definitions of those macros when
 * we're in this situation, and just let it have its way... 
 */
# ifdef __GLIBC__
#	undef TRUE
#	undef FALSE
# endif

# include "zebra_rpc.h"
# include "defs.h"	/* so programs can include zebra.h first instead */

#endif /* ifndef _ZEBRA_H */
