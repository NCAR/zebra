/*
 * Forward declarations
 *
 * $Log: not supported by cvs2svn $
 */
# ifndef VMS
	float	w_sat (), e_w (), t_mr (), theta_to_t (), theta_w (), t_sat ();
	float	square (), ten_to_the (), lcl_pres (), lcl_temp ();
	float	theta_dry (), dewpoint (), e_from_dp (), t_v ();
# else
/*
 * These routines actaully take float parameters, but we have to declare them
 * as double on the VAX...
 */
	float	w_sat (double t, double p);
	float	e_w (double t);
	float	t_mr (double p, double w);
	float	theta_to_t (double pt, double p);
	float	theta_w (double t, double p);
	float	t_sat (double ept, double p);
	float	square (double x);
	float	ten_to_the (double x);
	float	lcl_pres (double temp, double dp, double pres);
	float	lcl_temp (double temp, double dp);
	float	theta_dry (double t, double p);
	float	dewpoint (double e);
	float	e_from_dp (double dp);
	float	t_v (double t, double p, double e);
# endif

