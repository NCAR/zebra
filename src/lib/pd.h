/*
 * Plot description related stuff.
 *
 * $Id: pd.h,v 1.2 1990-07-08 13:00:51 corbet Exp $
 */

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
# ifdef __STDC__
	plot_description pd_Load (raw_plot_description *raw);
	raw_plot_description *pd_Unload (plot_description pd);
	void pd_RPDRelease (raw_plot_description *raw);
	void pd_Merge (plot_description dest, plot_description src);
	bool pd_Retrieve (plot_description pd, char *comp, char *param,
		char *target, int type);
	char **pd_CompList (plot_description pd);
	void pd_Release (plot_description pd);
	plot_description pda_GetPD (char *name);
	void pda_StorePD (plot_description pd, char *name);
	bool pda_Search (plot_description pd, char *comp, char *param,
		char *qual, char *dest, int type);
	plot_description pd_CopyPD (plot_description pd);
	void pd_Store (plot_description pd, char *comp, char *param,
		char *value, int type);
	int pd_RemoveComp (plot_description pd, char *name);
	plot_description pd_ReadComponent (plot_description pd, char *comp);
# else
	plot_description pd_Load ();
	raw_plot_description *pd_Unload ();
	void pd_RPDRelease ();
	void pd_Merge ();
	bool pd_Retrieve ();
	char **pd_CompList ();
	void pd_Release ();
	plot_description pda_GetPD ();
	void pda_StorePD ();
	bool pda_Search ();
	plot_description pd_CopyPD ();
	void pd_Store ();
	int pd_RemoveComp ();
	plot_description pd_ReadComponent ();
# endif
