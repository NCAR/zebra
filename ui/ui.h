/* 10/86 jc */
/* $Id: ui.h,v 1.11 2001-11-30 00:42:05 granger Exp $ */

# ifndef UI_H_SYMBOLS
# define UI_H_SYMBOLS
/*
 * Declarations for use outside (as well as inside) the user interface.
 */
# include <ui_symbol.h>
/*
 * This is the structure filled in by the command parser.  One of these
 * structures will appear for each non-ignored token in the input
 * stream.
 */
struct ui_command
{
	int	uc_ctype;	/* The token type -- see below.	*/
	int	uc_vptype;	/* Value parameter type		*/
	union usy_value uc_v;	/* Value parameter value.	*/
	int	uc_col;		/* Column number of the token	*/
	char 	*uc_text;	/* The actual token text	*/
};
/*
 * Token types.
 */
# define UTT_END	0	/* End of the token list	*/
# define UTT_VALUE	1	/* Value parameter		*/
# define UTT_OTHER	2	/* Something else.		*/
# define UTT_KW		3	/* Keyword token type.		*/
# define UTT_PARTIAL	4	/* End of a partial command	*/
# define UTT_SYM	5	/* Non-subst sym		*/

/*
 * Macros to ease dealing with cmds stuff.
 */
# define UINT(cmd) 	(cmd).uc_v.us_v_int
# define UKEY(cmd) 	(cmd).uc_v.us_v_int
# define UBOOL(cmd)	(cmd).uc_v.us_v_int
# define UPTR(cmd)	(cmd).uc_v.us_v_ptr
# define UFLOAT(cmd)	(cmd).uc_v.us_v_float
# define UDATE(cmd)	(cmd).uc_v.us_v_date
# define UCOL(cmd)	(cmd).uc_col

/*
 * User interface routine prototypes.
 */
# ifdef __STDC__
	void ui_init (char *, int, int);
	void ui_setup (char *, int *, char **, char *);
	void ui_get_command (char *, char *, int (*)(), long);
	void ui_subcommand (char *, char *, int (*)(), long);
	void ui_perform (char *);
	int ui_OutputRoutine (void (*)(), void (*)());
	int ui_ErrorOutputRoutine (void (*)());
	int ui_ErrHook (void (*)());
	int ui_printf (char *, ...);
	int ui_nf_printf (char *, ...);
	int ui_ErrOut (char *);
	int ui_WarnOut (char *);
	void uf_def_function (char *, int, int *, int (*)());
	int uii_set_handler (void (*)(), int);
	int uii_clear_handler (void (*)());
	int ui_int_prompt (char *, char *, int, int, int);
	double ui_float_prompt (char *, char *, double, double, double);
	void ui_string_prompt (char *, char *, char *, char *);
	void ui_date_prompt (char *, char *, date *, date *);
	int ui_kw_prompt (char *, char *, char *, char *);
	char *usy_string (const char *);
	void usy_rel_string (char *);
	char *usy_pstring (const char *);
	void tty_watch (int, void (*)());
	void tty_nowatch (int);
	struct ui_command *uip_clone_clist (struct ui_command *);
# ifdef _XtIntrinsic_h
	void uw_def_widget (char *, char *, Widget (*)(), void (*)(), char *);
	int uw_ForceWindowMode (char *, Widget *, XtAppContext *);
	void uw_IWRealize (char *, Widget);
	Widget uw_IWWidget (char *);
	void uw_IWPopup (char *);
	void uw_PositionWidget (Widget w);
	Widget uw_get_menubutton();
# endif
	void uw_mk_list (char *, char *, int, char **, void (*)(), char *);
	void uw_popup (char *);
	void uw_popdown (char *);
        char *uw_menu_title(char *menu);
	void ui_error (char *fmt, ...);
	void ui_cl_error (bool jump, int col, char *fmt, ...);
	void ui_ns_error (char *fmt, ...);
	void ui_bailout (char *fmt, ...);
	void ui_warning (char *fmt, ...);
# else
	void ui_init ();
	void ui_setup ();
	void ui_get_command ();
	void ui_subcommand ();
	void ui_perform ();
	int ui_OutputRoutine ();
	int ui_ErrorOutputRoutine ();
	int ui_ErrHook ();
		/* int ui_printf (char *, ...);	*/
		/* int ui_nf_printf (char *, ...); */
	int ui_ErrOut ();
	int ui_WarnOut ();
	void uf_def_function ();
	int uii_set_handler ();
	int uii_clear_handler ();
	int ui_int_prompt ();
	double ui_float_prompt ();
	void ui_string_prompt ();
	void ui_date_prompt ();
	int ui_kw_prompt ();
	char *usy_string ();
	void usy_rel_string ();
	char *usy_pstring ();
	void tty_watch ();
	void tty_nowatch ();
	struct ui_command *uip_clone_clist ();
# ifdef _XtIntrinsic_h
	void uw_def_widget ();
	int uw_ForceWindowMode ();
	void uw_IWRealize ();
	Widget uw_IWWidget ();
	void uw_IWPopup ();
	void uw_PositionWidget ();
	Widget uw_get_menubutton();
# endif
	void uw_mk_list ();
	void uw_popup ();
	void uw_popdown ();
	void ui_error ();
	void ui_cl_error ();
	void ui_ns_error ();
	void ui_bailout ();
	void ui_warning ();
# endif

/*
 * Let the application know if X support is present.
 */
# ifdef XSUPPORT
# define UI_X_SUPPORT
# endif

# endif
