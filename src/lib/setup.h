/*
 * Set up configuration variables so that they are available through
 * library calls and at the UI level.
 */
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

# define DIRLENGTH 128

/*
 * Try to hide internal library variables with defines
 */
#define Basedir _zl_Basedir
#define Bindir _zl_Bindir
#define Libdir _zl_Libdir
#define Projdir _zl_Projdir
#define DataDir _zl_Datadir

/*
 * Global UI indirect variables in ConfigVars.c
 */
extern char Basedir[DIRLENGTH], Bindir[DIRLENGTH], Libdir[DIRLENGTH];
extern char Projdir[DIRLENGTH], Datadir[DIRLENGTH];

#define InitDirVariables _zl_InitDirVariables
void InitDirVariables FP ((void));

/*
 * Global UI indirect variables in dm_lib.c
 */
#define DisplayManager _zl_DisplayManager
#define WindowName _zl_WindowName
extern char DisplayManager[ CFG_MSGNAME_LEN ];
extern char WindowName[ CFG_MSGNAME_LEN ];

