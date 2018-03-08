/* $Header: /code/cvs/rdss/rdsslibs/ui/ui_tty.h,v 1.2 1999-06-25 19:21:02 burghart Exp $ */
/*
 * Terminal handling info.
 */
# define UP_ARROW	0x81
# define DOWN_ARROW	0x82
# define RIGHT_ARROW	0x83
# define LEFT_ARROW	0x84

/*
 * Other special keys.
 */
# define MIN_FUNCTION_KEY 0x85
# define K_PF1		0x85
# define K_PF2		0x86
# define K_PF3		0x87
# define K_PF4		0x88

/*
 * Keypad numeric keys.
 */
# define K_0		0x89
# define K_1		0x8a
# define K_2		0x8b
# define K_3		0x8c
# define K_4		0x8d
# define K_5		0x8e
# define K_6		0x8f
# define K_7		0x90
# define K_8		0x91
# define K_9		0x92

/*
 * Other keypad keys.
 */
# define K_COMMA	0x93
# define K_ENTER	0x94
# define K_MINUS	0x95
# define K_PERIOD	0x96

/*
 * This is not really a tty key, but it is best defined with all the rest.
 */
# define K_NOINPUT	0xff

/*
 * Functions.
 */
char *tty_get_key_name ();
int tty_get_key_code ();
