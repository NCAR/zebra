/*
 * Forward declarations
 *
 * $Revision: 1.4 $ $Date: 1990-05-11 14:37:11 $ $Author: burghart $
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

