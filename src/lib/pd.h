/*
 * Plot description related stuff.
 *
 * $Id: pd.h,v 1.12 1995-04-15 00:35:18 granger Exp $
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

#ifndef __zeb_pd_h_
#define __zeb_pd_h_

/*
 * Deal with prototypes in both STDC and non-STDC worlds
 */
# ifndef FP
#    ifdef __STDC__
#	define FP(stuff) stuff
#    else
#	define FP(stuff) ()
#    endif
# endif

/*
 * The format of a raw plot description.  This stuff needs to be moved around
 * so the format is public.
 */
typedef struct rawpd
{
	int	rp_len;		/* Length of the PD data		*/
	char 	*rp_data;	/* The actual information		*/
} raw_plot_description;


/*
 * Internal plot descriptions are hidden.
 */
typedef void *plot_description;


/*
 * The PD routines.
 */
plot_description pd_Load FP((raw_plot_description *raw));
plot_description pd_Read FP((char *file));
raw_plot_description *pd_Unload FP((plot_description pd));
void pd_RPDRelease FP((raw_plot_description *raw));
void pd_Merge FP((plot_description dest, plot_description src));
void pd_MergeComp FP((plot_description dest, char *destname,
		      plot_description src, char *srcname));
bool pd_Retrieve FP((plot_description pd, char *comp, char *param,
	char *target, int type));
char **pd_CompList FP((plot_description pd));
void pd_Release FP((plot_description pd));
plot_description pda_GetPD FP((char *name));
void pda_StorePD FP((plot_description pd, char *name));
bool pda_Search FP((plot_description pd, char *comp, char *param,
	char *qual, char *dest, int type));
bool pda_ReqSearch FP((plot_description pd, char *comp, char *param,
	char *qual, char *dest, int type));
void pda_ReleaseAll FP ((void));
plot_description pd_CopyPD FP((plot_description pd));
void pd_Store FP((plot_description pd, char *comp, char *param,
	char *value, int type));
void pd_RemoveParam FP((plot_description pd, char *comp, char *param));
int pd_RemoveComp FP((plot_description pd, char *name));
plot_description pd_ReadComponent FP((plot_description, char *, char *));
void pd_AddComponent FP((plot_description, plot_description, int));
void pd_MoveComponent FP((plot_description, char *, int));
bool pd_CompExists FP((plot_description, char *));
void pd_TraverseParameters FP((plot_description pd, char *compname, 
			       int (*func)(), void *arg));

#endif /* !__zeb_pd_h_ */
