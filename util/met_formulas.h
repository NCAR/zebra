/*
 * Forward declarations
 */
/*
 *		Copyright (C) 1988-91 by UCAR
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
 *
 * $Id: met_formulas.h,v 1.7 1993-07-21 21:51:50 burghart Exp $
 */

# ifndef __STDC__
	double	w_sat (), e_sw (), t_mr (), theta_to_t (), theta_e ();
	double	t_sat (), square (), ten_to_the (), lcl_pres (), lcl_temp ();
	double	theta_dry (), dewpoint (), e_from_dp (), t_v (), t_wet ();
# else
	double	w_sat (double t, double p);
	double	e_sw (double t);
	double	t_mr (double p, double w);
	double	theta_to_t (double pt, double p);
	double	theta_e (double t, double dp, double p);
	double	t_sat (double ept, double p);
	double	square (double x);
	double	ten_to_the (double x);
	double	lcl_pres (double temp, double dp, double pres);
	double	lcl_temp (double temp, double dp);
	double	theta_dry (double t, double p);
	double	dewpoint (double e);
	double	e_from_dp (double dp);
	double	t_v (double t, double p, double e);
	double	t_wet (double t, double p, double rh);
# endif

