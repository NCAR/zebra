/*
 * Axis-Control (include this file to use Axis Package)
 */
/*		Copyright (C) 1987,88,89,90,91 by UCAR
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

/*
 * General definitions
 */

# define MAX_AXIS	5	/* The maximum number of axes on a side */

/*
 * Function prototypes
 */
extern int ac_DisplayAxes FP(( ));
extern void ac_GetComponentAxes FP((plot_description, char*, int[4]));
extern int ac_QueryAxisState FP((plot_description,char*,int,char*));
extern void ac_UpdateAxisState FP((plot_description,char*,int, char*,int*));
extern int ac_AxisSide FP(( int ));
