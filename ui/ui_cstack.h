/* 7/88 jc */
/* $Id: ui_cstack.h,v 1.2 1989-06-05 16:04:16 corbet Exp $ */
/*
 * Control stack definitions.
 */

/*
 * This is the format of an entry on the control stack.  One of these exists
 * for every control structure, procedure call, or mode that is active.
 */
struct cs_entry
{
	byte	cs_type;	/* Entry type -- see below		*/
	byte	cs_mode;	/* Mode type for major modes		*/
	bool	cs_exec;	/* Are we currently executing commands? */
	bool	cs_done;	/* Is this structure done? (if-then-else)*/
	struct input_stack *cs_input;	/* The input stack		*/
	struct csave *cs_csave;	/* Saved command structure		*/
	struct cs_entry *cs_next;	/* The next entry in the stack	*/
	struct token_context *cs_tctx;	/* The tokenizer context	*/
	/*
	 * The rest of these are used for control structures.
	 */
	char *cs_test;		/* Loop test condition			*/
	char *cs_tstr;		/* Test string (foreach)		*/
	char *cs_term;		/* Expected termination keyword		*/
	char *cs_cvar;		/* Control variable (foreach)		*/
	stbl cs_arg_table;	/* Procedure argument table		*/
	int cs_text;		/* Where the loop starts in csave.	*/
};

/*
 * Control stack entry types.
 */
# define CST_IF		0	/* If/then/else/endif structure		*/
# define CST_WHILE	1	/* While loop				*/
# define CST_FOREACH	2	/* Foreach loop				*/
# define CST_PROC	3	/* Procedure invocation			*/
# define CST_DEFPROC	4	/* Procedure definition			*/
# define CST_MODE	5	/* Major mode				*/
# define CST_PUSHCMD	6	/* Pushed command			*/


/*
 * Csave structures look like this.
 */
struct csave
{
	struct cs_entry *c_backptr;	/* Pointer to orig. cs entry	*/
	char **c_save;		/* The actual saved commands		*/
	int c_where;		/* The current write spot in csave	*/
	int c_read;		/* The current read spot		*/
	bool c_release;		/* Deallocate this csave when done	*/
};



/*
 * The input stack is made up of these.
 */
struct input_stack
{
	int s_type;		/* Input type -- see below		*/
	LUN s_lun;		/* File logical unit			*/
	struct input_stack *s_next;	/* Next entry in this stack	*/
	struct cs_entry *s_backptr;	/* Backptr to orig. cs entry	*/
	struct csave *s_csave;	/* For csave sources -- structure ptr	*/
	struct pb *s_pb;	/* Pushback structure pointer		*/
	char s_xline[MAXLINE];	/* For certain weird kinds of pushback	*/
	char s_name[128];	/* A name for this source		*/
	char *s_xp;		/* Current pointer into s_xline		*/
	bool s_snarf;		/* Pushback should be grabbed silently	*/
};

/*
 * Input stack entry types.
 */
# define IST_TTY	0	/* Interactive terminal		*/
# define IST_FILE	1	/* Disk file			*/
# define IST_CSAVE	2	/* Csave list			*/

/*
 * A structure used for pushback info.
 */
struct pb
{
	char *pb_text;		/* Actual pushback text	*/
	char *pb_tptr;		/* Where we are in the text */
	struct pb *pb_next;	/* Next pushback entry.	*/
	int pb_flags;		/* Input flags		*/
};

/*
 * Cstack-related routines.
 */
extern struct cs_entry *ucs_new_entry ();
extern struct input_stack *ucs_input ();
char *ucs_g_csline ();
