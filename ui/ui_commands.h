/* $Id: ui_commands.h,v 1.10 1992-12-18 21:10:30 corbet Exp $ */
/*
 * This file defines all of the command keyword numbers.  It absolutely MUST
 * match the numbers given in the state transition information, or things
 * will not work well at all.
 */
# define UIC_DEFINE	-1
# define UIC_EXIT	-2
# define UIC_SAVE	-3
# define UIC_STATE	-4
# define UIC_ENDDEF	-5
# define UIC_INPUT	-6
# define UIC_DUMP	-7
# define UIC_READ	-8
# define UIC_EOFTEXT	-23
/*
 * Stuff for the INPUT command.
 */
# define UIC_STRING	-9
# define UIC_INTEGER	-10
# define UIC_REAL	-11
# define UIC_DATE	-12
# define UIC_BOOLEAN	-13
# define UIC_EOS	-14
# define UIC_OTHER	-15

/*
 * For way within the INPUT command.
 */
# define UIC_REJECT	-16
# define UIC_MESSAGE	-17
# define UIC_IGNORE	-18
# define UIC_NEXT	-19
# define UIC_PUSHBACK	-20	/* Not currently used */
# define UIC_ENDINPUT	-21
# define UIC_DONE	-22

# define UIC_TABLE	-24
# define UIC_SET	-25
# define UIC_COMMAND	-26
# define UIC_PARTIAL	-27	/* No longer used 	*/
# define UIC_CTABLE	-28
# define UIC_HELPFILE	-29
# define UIC_LOAD	-30
# define UIC_TEST	-31	/* For quickly testing things */
/* # define UIC_PARSE	-32 */
# define UIC_EVAL	-33
# define UIC_RECALL	-34
# define UIC_VSET	-36
# define UIC_CSAVE	-37
# define UIC_SSET	-38
# define UIC_KEY	-39
# define UIC_LOWERCASE	-40
# define UIC_EDT	-41

/*
 * Prompting stuff.
 */
# define UIC_PROMPT	-42
# define UIC_LOWER	-43
# define UIC_UPPER	-44
# define UIC_DEFAULT	-45
# define UIC_Y_OR_N	-46
# define UIC_YES	-47
# define UIC_NO		-48

# define UIC_TYPE	-49
/*
 * Menuing stuff.
 */
# define UIC_MENU	-50
# define UIC_TITLE	-51
# define UIC_CHOICE	-52
# define UIC_MRETURN	-53
# define UIC_ENDCHOICE	-54
# define UIC_PCALL	-55
# define UIC_RETURN	-56
# define UIC_MODE	-57
# define UIC_ENDMODE	-58
# define UIC_NEWDEFAULT	-59
# define UIC_KEYPAD	-60
# define UIC_REPLACE	-61
# define UIC_PUSH	-62
# define UIC_DELETE	-63
# define UIC_EXPRESSION -64

/*
 * Window system stuff.
 */
# define UIC_WIDGET	-65
# define UIC_LIST	-66
# define UIC_ITEMS	-67
# define UIC_POPUP	-68
# define UIC_POPDOWN	-69
# define UIC_ENTRY	-70
# define UIC_SELECTOR	-71
# define UIC_MAPPING	-72
# define UIC_ENDMAPPING -73
# define UIC_HORIZONTAL	-74
# define UIC_OVERRIDE	-75
# define UIC_NODECORATE	-76
# define UIC_ENDMENU	-77
# define UIC_LINE	-78

# define UIC_LOCAL	-79
# define UIC_BITMAP	-80
# define UIC_VERTICAL	-81
# define UIC_NOHEADER	-82
# define UIC_LOCATION	-83
# define UIC_SIZE	-84

# define UIC_FORCEEVAL	-85
# define UIC_SUBMENU	-86
# define UIC_LABEL	-87
# define UIC_VARIABLE	-88
# define UIC_WIDTH	-89
# define UIC_FONT	-90
# define UIC_COLOR	-91
# define UIC_NEWLINE	-92
# define UIC_ENDFORM	-93
# define UIC_BUTTON	-94
# define UIC_BLANK	-95
# define UIC_FORMTEXT	-96
# define UIC_PULLDOWN	-97
# define UIC_FORMMENU	-98

/*
 * Control structure commands have their own range.  The UI dispatcher is
 * counting on this, so don't change things.
 */
# define UIC_WHILE	-1000
# define UIC_ENDWHILE	-1001
# define UIC_FOREACH	-1002
# define UIC_ENDFOR	-1003
# define UIC_IF		-1004
# define UIC_THEN	-1005
# define UIC_ELSE	-1006
# define UIC_ELSEIF	-1007
# define UIC_ENDIF	-1008
# define UIC_PROCEDURE	-1009
# define UIC_ENDPROCEDURE -1010
