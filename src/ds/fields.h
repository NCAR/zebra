/*
 * Fields information
 *
 * $Id: fields.h,v 2.2 1993-08-27 10:05:10 granger Exp $
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

#ifndef _zeb_fields_h_
#define _zeb_fields_h_

typedef enum
{
/* 0 */		f_null, f_time, f_lat, f_lon, f_alt,
/* 5 */		f_pres, f_temp, f_dp, f_rh, f_wspd,
/* 10 */	f_wdir, f_u_wind, f_v_wind, f_theta, f_theta_e,
/* 15 */	f_mr, f_qpres, f_qtemp, f_qrh, f_qu,
/* 20 */	f_qv, f_qwind, f_rtype, f_range, f_azimuth,
/* 25 */	f_u_prime, f_v_prime, f_ascent, f_vt
} fldtype;

# define TOTAL_FLDS 29

/*
 * prototypes
 */
# ifdef __STDC__
	fldtype	fd_num (char *);
	char	*fd_name (fldtype);
	char	*fd_desc (fldtype);
	char	*fd_units (fldtype);
	float	fd_bot (fldtype);
	float	fd_top (fldtype);
	float	fd_center (fldtype);
	float	fd_step (fldtype);
# else
	fldtype fd_num ();
	char	*fd_name ();
	char	*fd_desc ();
	char	*fd_units ();
	float	fd_bot ();
	float	fd_top ();
	float	fd_center ();
	float	fd_step ();
# endif

#endif /* _zeb_fields_h_ */
