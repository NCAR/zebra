#ifndef __zeb_copyright_h_
#define __zeb_copyright_h_

#if !defined(SABER) && !defined(lint) && !defined(LINT)

static char *_Copyright = 
"		Copyright (C) 1987-1996 by UCAR\n\
	University Corporation for Atmospheric Research\n\
		   All rights reserved\n\
\n\
No part of this work covered by the copyrights herein may be reproduced\n\
or used in any form or by any means -- graphic, electronic, or mechanical,\n\
including photocopying, recording, taping, or information storage and\n\
retrieval systems -- without permission of the copyright owner.\n\
\n\
This software and any accompanying written materials are provided \"as is\"\n\
without warranty of any kind.  UCAR expressly disclaims all warranties of\n\
any kind, either express or implied, including but not limited to the\n\
implied warranties of merchantibility and fitness for a particular purpose.\n\
UCAR does not indemnify any infringement of copyright, patent, or trademark\n\
through use or modification of this software.  UCAR does not provide \n\
maintenance or updates for its software.\n";

#if __GNUC__
static inline const char *Z_copyright()
{
	return (_Copyright);
}
#else
static char *Z_copyright()
{
	return (_Copyright);
}
#endif


#endif /* !lint */

#endif /* !__zeb_copyright_h_ */
