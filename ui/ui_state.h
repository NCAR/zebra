/* $Id: ui_state.h,v 1.3 1991-02-14 16:55:35 corbet Exp $ */
/*
 * Structures for the state table.
 */
# ifndef STATE_SYMBOLS
# define STATE_SYMBOLS

# include "ui_param.h"
# define STATE_NAME_LEN		32	/* Max len of a state name	*/
# define HELP_LEN		52	/* Help file length		*/
# define MSG_LEN		80	/* Max message length		*/
# define KW_LEN			32	/* Max keyword length		*/
# define ST_NO_VP		100	/* No value parameter here	*/


/*
 * This structure defines an action to be performed on successful match
 * of a token.
 */
struct state_action
{
	int	act_flags;	/* Flags defining this action.		*/
	char	act_next[STATE_NAME_LEN];	/* Next state		*/
	char	act_mtext[MSG_LEN];	/* Message text, if used	*/
};


/*
 * Now we have a structure that defines a possible keyword.
 */
struct state_keyword
{
	char	stk_keyword[KW_LEN];	/* The actual keyword.		*/
	int	stk_kwnum;		/* Associated keyword number	*/
	struct state_action stk_action;	/* Associated parser action	*/
};

/*
 * Finally, we have an actual state table entry, one for each state.
 */
struct state_table_entry
{
	char	sta_name[STATE_NAME_LEN];	/* Name of this state.	*/
	byte	sta_vptype;		/* Value parameter type		*/
	byte	sta_flags;		/* State flags -- see below	*/
	short	sta_nkw;		/* Number of keywords		*/
	struct state_action *sta_eosact; /* EOS action			*/
	struct state_action *sta_otheract; /* OTHER action		*/
	struct state_action sta_vpact;	/* Value parameter action	*/
	struct state_keyword *sta_kw;	/* Keyword array.		*/
	char	sta_eoftext[STATE_NAME_LEN]; /* EOF subst text		*/
	char	sta_ctable[STATE_NAME_LEN];	/* Command table name	*/
	char	sta_helpfile[HELP_LEN];	/* Helpfile, if any	*/
};


/*
 * State flags.
 */
# define STF_EOS	0x01	/* An EOS action exists.		*/
# define STF_OTHER	0x02	/* An OTHER action exists.		*/
# define STF_EOFTXT	0x04	/* EOF text exists			*/
# define STF_BOOT	0x08	/* Bootstrap entry (no relvm ())	*/
# define STF_CTABLE	0x10	/* A command table exists		*/
# define STF_HELP	0x20	/* A help file exists			*/
# define STF_INITIAL	0x40	/* State created in initial load	*/

/*
 * Action flags.
 */
# define STAF_MSG	0x01	/* Message text exists			*/
# define STAF_IGNORE	0x02	/* Ignore this token			*/
# define STAF_REJECT	0x04	/* Reject this token.			*/
# define STAF_DONE	0x08	/* This is a terminal token.		*/
# define STAF_PARTIAL	0x10	/* Partial cmd OK after this		*/
# define STAF_LOWERCASE 0x20	/* Fold to lower case			*/
# define STAF_EVAL	0x40	/* Always evaluate VParams		*/

/*
 * Routines.
 */
struct state_table_entry *ust_lookup_state ();

# endif
