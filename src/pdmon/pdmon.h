/*
 * Description of the PD monitor protocol.
 */
/* $Id: pdmon.h,v 1.1 1992-09-15 15:09:08 corbet Exp $ */
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

typedef enum
{
/*
 * Monitor messages.
 */
	pdm_HookIn,		/* Hook into a graphic process	*/
	pdm_NewPD,		/* Here comes a new PD		*/
	pdm_UnHook,		/* I'm going away		*/
/*
 * Gp messages.
 */
	pdm_MyPD,		/* Here is my plot description	*/
	pdm_Exit,		/* Graphics process exiting	*/
} PdmMsgType;


/*
 * The template used to split out messages.  Also used for very simple
 * messages.
 */
typedef struct _pdmTemplate
{
	PdmMsgType	pt_Type;	/* Type of this message	*/
} pdmTemplate;


/*
 * These are used to send plot descriptions around.
 */
typedef struct _pdmPD
{
	PdmMsgType	pt_Type;	/* Message type		*/
	int		pt_Len;		/* Length of the PD	*/
	char		pt_Pd[1];	/* The actual stuff	*/
} pdmPD;
