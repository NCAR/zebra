/*
 * Definitions for plot primitives stuff
 * $Id: PlotPrim.h,v 2.1 1993-12-01 17:21:25 burghart Exp $
 */
/*
 *		Copyright (C) 1993 by UCAR
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
 * Line style
 */
typedef enum {L_solid, L_dashed, L_dotted} LineStyle;

/*
 * Prototypes
 */
void pp_Clip FP ((DataValPtr, DataValPtr, DataValPtr, DataValPtr, int));
void pp_UnClip FP ((void));
void pp_Pline FP ((DataValPtr, DataValPtr, int, LineStyle, Pixel));
void pp_WindVector FP ((DataValPtr, DataValPtr, DataValPtr, DataValPtr, 
			int, int, double, LineStyle, XColor*, int, double));
void pp_WindBarb FP ((DataValPtr, DataValPtr, DataValPtr, DataValPtr, int, 
		      int, int, LineStyle, XColor*, int, double, int));
void pp_Icons FP ((DataValPtr, DataValPtr, int, char*, Pixel, char*, char*));

