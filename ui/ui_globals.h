/* 10/86 jc */
/* $Id: ui_globals.h,v 1.6 1999-06-25 19:21:00 burghart Exp $ */
/*
 * Global variable definitions.
 */
# ifndef GLOBAL_SYMBOLS
# define GLOBAL_SYMBOLS

# include "ui_symbol.h"

# ifndef var
#   define var extern
# endif

/*
 * Ui_variable_table is the symbol table used to hold ui parameters of
 * interest.
 */
var stbl	Ui_variable_table;
var stbl	Arg_table;

/*
 * The command table.
 */
var stbl	Ui_command_table;

/*
 * Bol is TRUE iff the cursor is at the beginning of a screen line.
 */
var bool	Bol;

/*
 * The number of lines that have been output to the screen since the
 * last input or pager stall.
 */
var int Nlines;			/* Number of lines since last stall */

/*
 * True iff the keypad is in application mode.
 */
var bool Keypad_on;

/*
 * The central control stack.
 */
var struct cs_entry *Cs;

/*
 * TRUE iff we should bail out of command files on errors.
 */
var bool Bail;

/*
 * Application setup information.
 */
var char Appl_name[80];
var int *Argc;
var char **Argv;
var char *Resources;

# endif
