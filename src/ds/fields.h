/*
 * Fields information
 *
 * $Revision: 2.0 $ $Date: 1991-07-18 23:20:21 $ $Author: corbet $
 */

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
