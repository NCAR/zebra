/*
 * Forward declarations
 *
 * $Revision: 2.1 $ $Date: 1991-09-12 20:27:54 $ $Author: corbet $
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
# ifndef __STDC__
	double	w_sat (), e_w (), t_mr (), theta_to_t (), theta_e (), t_sat ();
	double	square (), ten_to_the (), lcl_pres (), lcl_temp ();
	double	theta_dry (), dewpoint (), e_from_dp (), t_v ();
# else
	double	w_sat (double t, double p);
	double	e_w (double t);
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
# endif

