/*
 * User interface state table bootstrap stuff.
 *
 * The user-definable state-transition scheme has one minor shortcoming, which
 * is that a certain number of commands have to be given to the system before
 * it is able to read commands.  To that end, the state entries are
 * hardcoded here.
 */
# include "ui_param.h"
# include "ui_state.h"
# include "ui_symbol.h"

# define var /* nothing */
# define forward extern

forward struct state_keyword boot_initial_kw[], define_what_kw[];
forward struct state_keyword input_initial_kw[], input_what_kw[];
forward struct state_keyword within_input_kw[];

/*
 * define state ust$boot_initial	! The initial state.
 * 
 * 	input 'define' 1	! Initial definition of the DEFINE command.
 * 		next ust$define_what
 * 	endinput
 * 
 * 	input 'exit' 2		! Gotta be able to quit.
 * 		next ust$no_more
 * 	endinput
 * 
 * 	input 'save' 3		! Want to be able to save these tables too.
 * 		next ust$save_file
 * 	endinput
 * enddef
 */
 
var struct state_table_entry st_boot_initial =
{
	"ust$boot_initial",	/* sta_name			*/
	ST_NO_VP,		/* No value params.		*/
	STF_EOFTXT | STF_BOOT,
	6,			/* Five keywords.		*/
	(struct state_action *) 0,	/* EOS action	*/
	(struct state_action *) 0,	/* OTHER action			*/
	{ ___, "", "" },		/* value action			*/
	 boot_initial_kw, "exit", ___ , ___
};



var struct state_keyword boot_initial_kw[] =
{
	{ "define",	-1,
		{ ___,		"ust$define_what",	"" }},
	{ "dump",	-7,
		{ ___,		"ust$final_string",	"" }},
	{ "exit",	-2,
		{ ___,		"ust$no_more",	"" }},
	{ "save",	-3,
		{ ___,		"ust$save_file",		"" }},
	{ "read",	-8,
		{ ___,		"ust$final_string",	"" }},
	{ "stdump",	-100,
		{ ___,		"ust$final_string",	"" }}
};



/*
 * define state ust$define_what	! Deal with the object of a define command.
 * 	input 'state' 1		! This define state command.
 * 		next ust$final_string	! Get the name of the state.
 * 	endinput
 * 	! No other DEFINE's for now.
 * enddef
 */
 
var struct state_table_entry st_define_what =
{
	"ust$define_what",
	ST_NO_VP,  STF_BOOT,  1,	/* One keyword.			*/
	0,
	0,
	{ ___, "", "" },
	define_what_kw, "", ___, ___
};


var struct state_keyword define_what_kw[] =
{
	{ "state",	-4,
		{ ___,		"ust$final_string",		"" }}
};


/*
 * define state ust$final_string	! Get the name of a defined state.
 * 	input string		! The actual name.
 * 		next ust$no_more	! all done.
 * 	endinput
 * enddef
 */
var struct state_table_entry st_state_name =
{
	"ust$final_string",
	SYMT_STRING,		/* Take a string parameter */
	STF_BOOT, 0,
	0,	/* No EOS action		*/
	0, 	/* No OTHER action		*/
	{ ___, "ust$no_more", "" },
	___, ___, ___, ___
};


/*
 * define_state ust$no_more		! Handle the end of a command.
 * 	input EOS
 * 		ignore
 * 		done
 * 	endinput
 * 
 * 	input other
 * 		message 'Unexpected stuff past the end of a good command'
 * 		reject
 * 		next ust$no_more
 * 	endinput
 * enddef
 */
var struct state_action st_nm_eos =
	{ STAF_IGNORE | STAF_DONE, "", "" };
var struct state_action st_nm_other = 
	{ STAF_REJECT | STAF_MSG, "ust$no_more",
			"Unexpected stuff past the end of a good command" };

var struct state_table_entry st_no_more =
{
	"ust$no_more",
	ST_NO_VP, STF_EOS | STF_OTHER | STF_BOOT,
	0,	/* no keywords */
	&st_nm_eos,
	&st_nm_other,
	{ ___, "", "" },
	___, ___, ___, ___
};


/*
 * define state ust$save_file	! Get the name of a save file.
 * 	input string		! The file name
 * 		next ust$no_more
 * 	endinput
 * 
 * 	input EOS		! Be friendly with premature eos
 * 		message 'Please give me a file name'
 * 		reject
 * 		next ust$save_file
 * 	endinput
 * enddef
 */
var struct state_action st_sf_eos =
	{ STAF_MSG, "ust$save_file", "Please give me a save file name" };

var struct state_table_entry st_save_file =
{
	"ust$save_file",
	SYMT_STRING, STF_EOS | STF_BOOT, 0,
	&st_sf_eos,
	0,
	{ ___, "ust$no_more", ""},
	___, ___, ___, ___
};



/*
 * !
 * ! These states handle input for the interior of a DEFINE STATE command.
 * !
 * 
 * define state ust$input_initial	! Initial for within def state command
 * 	input 'input' 1		! The "input" command
 * 		next ust$input_what
 * 	endinput
 * 
 * 	input 'enddef' 2	! End the definition of this state.
 * 		next ust$no_more
 * 	endinput
 * enddef
 */
var struct state_table_entry st_in_initial =
{
	"ust$input_initial",
	ST_NO_VP, STF_EOFTXT | STF_BOOT, 2,
	0,
	0,
	{ ___, "", "" }, 
	input_initial_kw, "enddef", ___, ___
};



var struct state_keyword input_initial_kw[] =
{
	{ "input",	-6,
		{ ___,		"ust$input_what", ""}},
	{ "enddef",	-5,
		{ ___,		"ust$no_more", "" }}
};




/*
 * define state ust$input_what	! Handle a possible input value.
 * 	input 'string' 1	! String value param.
 * 		next ust$no_more
 * 	endinput
 * 	input 'integer' 2	! Integer value param.
 * 		next ust$no_more
 * 	endinput
 * 	input 'real' 3		! Real value param.
 * 		next ust$no_more
 * 	endinput
 * 	input 'date' 4		! Date value param.
 * 		next ust$no_more
 * 	endinput
 * 	input 'boolean' 5	! Boolean value param.
 * 		next ust$no_more
 * 	endinput
 * 	input 'EOS' 6		! End of string case
 * 		next ust$no_more
 * 	endinput
 * 	input 'other' 7		! None of the above
 * 		next ust$no_more
 * 	endinput
 * 	!
 * 	! String input is used for the definition of keywords.
 * 	!
 * 	input string		! keyword input.
 * 		next ust$final_int
 * 	endinput
 * enddef
 */
var struct state_table_entry st_in_what =
{
	"ust$input_what",
	SYMT_STRING, STF_BOOT, 7,
	0,
	0,
	{ ___, "ust$final_int", "" },
	input_what_kw, ___, ___, ___
};


var struct state_keyword input_what_kw[] =
{
	{ "string",	-9, { ___, "ust$no_more", ___}},
	{ "integer",	-10, { ___, "ust$no_more", ___}},
	{ "real",	-11, { ___, "ust$no_more", ___}},
	{ "date",	-12, { ___, "ust$no_more", ___}},
	{ "boolean",	-13, { ___, "ust$no_more", ___}},
	{ "eos",	-14, { ___, "ust$no_more", ___}},
	{ "other",	-15, { ___, "ust$no_more", ___}}
};




/*
 * define state ust$final_int		! Accept an integer and terminate.
 * 	input integer
 * 		next ust$no_more
 * 	endinput
 * 	input other
 * 		message "Enter an integer here, please"
 * 		reject
 * 		next ust$final_int
 * 	endinput
 * enddef
 */
var struct state_table_entry st_final_int =
{
	"ust$final_int",
	SYMT_INT, STF_BOOT, 0,
	0,
	0,
	{ ___, "ust$no_more", "" },
	___, ___, ___, ___
};





/*
 * !
 * ! This is for the depths of the input command.
 * !
 * define state ust$within_input
 * 	input 'reject'		! Reject this token.
 * 		next ust$no_more
 * 	endinput
 * 	input 'message'		! Put out a message.
 * 		next ust$message_text
 * 	endinput
 * 	input 'ignore'		! Ignore this kw
 * 		next ust$no_more
 * 	endinput
 * 	input 'next'		! Next state definition.
 * 		next ust$state_name
 * 	endinput
 * 	input 'pushback'	! Push back this token.
 * 		next ust$no_more
 * 	endinput
 * 	input 'endinput'	! End of this INPUT
 * 		next ust$no_more
 * 	endinput
 *	input 'done'
 *		next ust$no_more
 *	endinput
 * enddef
 */
var struct state_table_entry st_within_input =
{
	"ust$within_input",
	ST_NO_VP, STF_EOFTXT | STF_BOOT, 7,
	0,
	0,
	{ ___, "", "" },
	within_input_kw, "endinput", ___, ___
};



var struct state_keyword within_input_kw[] =
{
	{ "reject",	-16,	{ ___,	"ust$no_more",	___ }},
	{ "message",	-17,	{ ___,	"ust$final_string",	___ }},
	{ "ignore",	-18,	{ ___,	"ust$no_more",	___ }},
	{ "next",	-19,	{ ___,	"ust$final_string",	___ }},
	{ "pushback",	-20,	{ ___,	"ust$no_more",	___ }},
	{ "endinput",	-21,	{ ___,	"ust$no_more",	___ }},
	{ "done",	-22,	{ ___,	"ust$no_more",	___ }}
};



/*
 * Whew!  Now that we have all of the states defined, let's create a table
 * of them all, to make the initialization easy.
 */
static struct state_table_entry *boot_array[] =
{
	&st_boot_initial,
	&st_define_what,
	&st_state_name,
	&st_no_more,
	&st_save_file,
	&st_in_initial,
	&st_in_what,
	&st_final_int,
	&st_within_input,
	___
};




void
ust_bootstrap (table)
stbl table;
/*
 * Bootstrap the initial state tables into the system.
 */
{
	union usy_value v;
	int i;
	
	/* for (sp = boot_array; *sp; sp++) */
	for (i = 0; boot_array[i]; i++)
	{
		v.us_v_ptr = (char *) boot_array[i];
		usy_s_symbol (table, boot_array[i]->sta_name, SYMT_POINTER, &v);
	}
}
