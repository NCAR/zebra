/* 10/85 jc */
/* $Header: /code/cvs/rdss/rdsslibs/ui/ui_error.h,v 1.1 1989-02-08 13:28:15 corbet Exp $ */

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

# endif
