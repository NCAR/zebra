/*
 * $Id: twidget.h,v 2.2 1995-04-27 14:54:49 granger Exp $
 *
 * Separate include file for the time widget, since it is not required
 * by most programs which link with the Zebra library.
 */
/*		Copyright (C) 1987-1995 by UCAR
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

#ifndef _zebra_twidget_h_
#define _zebra_twidget_h_

/*
 * The time widget has associated with it a list of "hot times" which
 * can be selected from a menu button.  The application can set callbacks
 * for additions or deletions on the list.
 */
#define HT_LABEL_LEN 40
#define MAX_HOT_TIMES 20
typedef struct s_HotTime {
	ZebTime ht_zt;
	char ht_label[HT_LABEL_LEN];
} HotTime;

#define MAX_WINDOWS 20

void 	tw_DefTimeWidget FP ((int (*callback) (), char *title));
void	tw_SetWindowNames FP ((int nwin, char **names));
void	tw_AddHelpCallback FP ((void (*callback) ()));
void	tw_AddPopupCallback FP ((void (*callback) ()));
void	tw_SetTime FP ((ZebTime *init_or_null)); /* null ==> use system time */
void 	tw_DialAdjust FP ((int, int));
void	tw_DeleteHotTime FP ((ZebTime *zt));
void	tw_AddHotTime FP ((ZebTime *zt, char *label));
void	tw_AddHTDeleteCallback FP ((void (*func)(/* ZebTime *zt */)));
void	tw_AddHTAddCallback FP ((void (*func)(/* ZebTime *zt, char *desc */)));
const HotTime *tw_ListHotTimes FP ((int *ntime));


#endif /* ndef _zebra_twidget_h_ */

