/* 10/85 jc */
/* $Id: ui_error.h,v 1.8 2002-07-11 22:50:44 burghart Exp $ */

# ifndef UI_ERROR_SYMBOLS
# define UI_ERROR_SYMBOLS

# include <setjmp.h>
/* 
 * Macros for jmpbuf handling.
 */
# define ERRORCATCH	{\
				jmp_buf __JB__; \
				ui_epush (&__JB__); \
				if (setjmp (__JB__) == 0) {
# define ON_ERROR	} else {
# define ENDCATCH	} ui_epop (); }

# define RESIGNAL	ui_eresignal ();

/*
 * Function prototypes.  Some of these won't be right until the varargs
 * stuff is cleaned up.
 */
# ifdef __STDC__
	void ui_epush (jmp_buf*);
	void ui_epop (void);
	void ui_eresignal (void);
# endif
# endif
