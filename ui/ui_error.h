/* 10/85 jc */
/* $Id: ui_error.h,v 1.3 1990-09-19 08:50:19 corbet Exp $ */

# ifndef UI_ERROR_SYMBOLS
# define UI_ERROR_SYMBOLS

/*
 * On systems which have _setjmp and _longjmp, we want to use those,
 * since they will save vast amounts of time.
 */
# ifdef sun
# define setjmp _setjmp
# endif

# include <setjmp.h>
/* 
 * Macros for jmpbuf handling.
 */
# define ERRORCATCH	{\
				jmp_buf __JB__; \
				ui_epush (__JB__); \
				if (setjmp (__JB__) == 0) {
# define ON_ERROR	} else {
# define ENDCATCH	} ui_epop (); }

# define RESIGNAL	ui_eresignal ();

/*
 * Function prototypes.  Some of these won't be right until the varargs
 * stuff is cleaned up.
 */
# ifdef __STDC__
	int ui_epush (jmp_buf);
	int ui_epop (void);
	int ui_eresignal (void);
# endif
# endif
