/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 ARGPARSE.C 4-Apr-95,10:12:58,`JUDYK' Arg-Fetching Parsing Functions     */
/* 2 ARGPARSE.C 20-Apr-95,13:03:00,`JUDYK' fix to malloc copy of QFLD_KEYWORD*/
/* 3 ARGPARSE.C 2-May-95,16:39:48,`JUDYK' change strlen to STRLEN, etc.      */
/* 4 ARGPARSE.C 8-May-95,22:53:40,`JUDYK' add const                          */
/* 5 ARGPARSE.C 25-May-95,17:56:30,`JUDYK' upd doc, chg to Fint, free(toksep)*/
/* 6 ARGPARSE.C 30-May-95,13:44:52,`JUDYK' fix interface doc                 */
/* 7 ARGPARSE.C 6-Jun-95,14:58:38,`USER' Released                            */
/* 8 ARGPARSE.C 19-Feb-96,15:36:42,`DWS' reglue: modified file               */
/* 9 ARGPARSE.C 20-Feb-96,11:49:54,`USER' Released                           */
/* 10 ARGPARSE.C 2-Aug-96,14:38:30,`DGLO' Deal with unbalanced quotes (#6742)*/
/* 11 ARGPARSE.C 9-Aug-96,15:47:50,`DGLO' Fix my portability fix (#6742)     */
/* 12 ARGPARSE.C 16-Aug-96,10:48:22,`DGLO' Make error msg prettier (#6742)   */
/* 13 ARGPARSE.C 21-Oct-96,16:22:04,`USER' Released                          */
/**** McIDAS Revision History *** */

/*==============================argparse.c====================================*/
/*
	argparse.c - Argument-Fetching Parsing and Updating Functions
		     ARGAPI functions pertaining to parsing and updating.
*/
/*----------------------------------------------------------------------------*/

#include <ctype.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "mcidas.h"
#include "m0arg.h"

/*==============================M0cmdparse====================================*/
/*
*| Name:
*|      M0cmdparse - Parse the given McIDAS command into arg-fetching structure.
*|
*| Interface:
*|      #include "m0arg.h"
*|
*|      int
*|      M0cmdparse(const char* cmdlin, int* parsed_len)
*|
*| Input:
*|      cmdlin      - Given command line string to parse.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      parsed_len  - Optional length of parsed command, including any
*|                    terminating ';' if present.  (Use NULL to ignore).
*|
*| Return values:
*|      >  0        - Argument-fetching handle.
*|      <  0        - failure statuses
*|
*| Remarks:
*|      Use Mcargfree() to free the arg-fetching structure.
*|
*|      After M0cmdput(M0cmdparse()) is called, arguments from the given command
*|      may be fetched by calling Mccmdstr, Mccmdint, Mccmdnum, Mccmdnam, etc.
*|
*|      The correct sequence of functions to setup command arg-fetching is:
*|         stat = Mcargfree(0)
*|         stat = M0cmdput(M0cmdparse(cmdlin, &parsed_len))
*|         stat = M0cmdglo()
*|
*| Categories:
*|      USER_INTERFACE, CONVERTER
*/

int					/* returned argument-fetching handle  */
M0cmdparse(				/* Parse McIDAS Command Line          */
  const char	*cmdlin,		/* command line string to parse       */
  int		*parsed_len)		/* return the parsed command length   */
{
  return  Mcargparse(cmdlin, NULL, parsed_len);
}
/*==========================end M0cmdparse====================================*/

/*==============================m0cmdparse_===================================*/
/*
*| Name:
*|      m0cmdparse - Parse the given McIDAS command into arg-fetching structure.
*|
*| Interface:
*|      integer function
*|      m0cmdparse(character*(*) cmdlin, integer parsed_len)
*|
*| Input:
*|      cmdlin      - Given command line string to parse.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      parsed_len  - Optional length of parsed command, including any
*|                    terminating ';' if present.  (Use NULL to ignore).
*|
*| Return values:
*|      >  0        - Argument-fetching handle.
*|      <  0        - failure statuses
*|
*| Remarks:
*|      Use mcargfree() to free the arg-fetching structure.
*|
*|      After m0cmdput(m0cmdparse()) is called, arguments from the given command
*|      may be fetched by calling mccmdstr, mccmdint, mccmdnum, mccmdnam, etc.
*|
*|      The correct sequence of functions to setup command arg-fetching is:
*|         stat = mcargfree(0)
*|         stat = m0cmdput(m0cmdparse(cmdlin, parsed_len))
*|         stat = m0cmdglo()
*|
*| Categories:
*|      USER_INTERFACE, CONVERTER
*/

extern Fint
mcargparse_(const char *txtstr, const char *given_syntax, Fint *parsed_len,
	FsLen siz_txtstr, FsLen siz_given_syntax);

Fint					/* returned argument-fetching handle  */
m0cmdparse_(				/* fortran jacket for M0cmdparse      */
  const char	*cmdlin,		/* command line to parse              */
  Fint		*parsed_len,		/* return the parsed command length   */
  FsLen	 siz_cmdlin)			/* character*(*) size of 'cmdlin'     */
{
  return  mcargparse_(cmdlin, NULL, parsed_len, siz_cmdlin, 0);
}
/*==========================end m0cmdparse_===================================*/

/*==============================Mcargparse====================================*/
/*
*$ Name:
*$      Mcargparse - Parse the given text into arg-fetching structure.
*$
*$ Interface:
*$      #include "m0arg.h"
*$
*$      int
*$      Mcargparse(const char* txtstr, const McArgSyntax* given_syntax,
*$                 int* parsed_len)
*$
*$ Input:
*$      txtstr        - Given text string to parse.
*$      given_syntax  - Given argument syntax, or NULL to use McIDAS default.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      parsed_len    - Optional length of parsed text, including any
*$                      terminating ';' if present.  (Use NULL to ignore).
*$
*$ Return values:
*$      >  0          - Argument-fetching handle.
*$      <  0          - failure statuses
*$
*$ Remarks:
*$      Use Mcargfree() to free the argument-fetching structure.
*$
*$      After Mcargparse is called, arguments from the given text may be
*$      fetched by calling Mcargstr, Mcargint, Mcargnum, Mcargnam, etc.
*$
*$      The correct sequence of functions for text arg-fetching is:
*$         hand = Mcargparse(txtstr, NULL, &parsed_len)
*$         stat = Mcargstr(hand, ...), Mcargint(hand, ...), etc.
*$              :
*$         stat = Mcargfree(hand)
*$
*$      The argument-fetching sybsystem allows for parsing lines of text with
*$      varying syntax, based on the McIDAS command line syntax, thru programmer
*$      given parsing definitions (use sscanf() to parse other syntax types).
*$      Parsing syntax is defined in a programmer given array of 10 strings.
*$      Each string may define one or more, single character, specific type of
*$      syntactic separators, or other type of syntax, as follows.
*$
*$      McArgSyntax syntax =
*$      {
*$              " ",           * syntax.white_space          *
*$              "=,",          * syntax.keyword_separators   *
*$              ";",           * syntax.command_separators   *
*$              "{\"",         * syntax.qfld_delims_left     *
*$              "}",           * syntax.qfld_delims_right    *
*$              "'",           * syntax.quoting_delims_left  *
*$              "'",           * syntax.quoting_delims_right *
*$              NULL,          * syntax.quoting_escape       *
*$              "X",           * syntax.missing_argument     *
*$              " "            * syntax.missing_arg_value    *
*$      };
*$
*$      If any syntax isn't defined, it defaults to the shown McIDAS syntax,
*$      The value NULL leaves the syntax undefined, indicating McIDAS default.
*$
*$       [1] White Space Characters                        McIDAS default: " "
*$           called syntax.white_space
*$
*$           Define one or more white space characters.  A white space
*$           character is a special, single character (usually a blank),
*$           used to determine how to separate tokens, and which is ignored.
*$
*$           Used to separate tokens (terminates non-quoted alphanumeric token).
*$           Ignores intermixed white space and keyword separators between
*$           tokens.  Ignores white space characters between a token and a
*$           keyword separator.
*$
*$       [2] Keyword Separators                            McIDAS default: "=,"
*$           called syntax.keyword_separators
*$
*$           Define one or more keyword separators.  A keyword separator is a
*$           special, single character, used to identify a keyword, and which
*$           occurs after the keyword name, possibly separated with one or more
*$           of the white space characters defined in [1] above.
*$
*$           Used to separate tokens (terminates non-quoted alphanumeric token).
*$           Ignores intermixed white space and keyword separators between
*$           tokens.
*$
*$       [3] Command Separators                            McIDAS default: ";"
*$           called syntax.command_separators
*$
*$           Define one or more command separators.  A command separator is a
*$           special, single character, used to separate commands.
*$
*$           Terminates the parsing of the text line.
*$
*$           Used to separate tokens (terminates non-quoted alphanumeric token).
*$
*$       [4] Quote Field Left Delimiters                   McIDAS default: "{\""
*$           called syntax.qfld_delims_left
*$
*$           The quote field is an argument enclosed within special delimiters,
*$           which may contain embedded spaces and other special characters,
*$           and which allows delimiter nesting, such as {prog {nested field}}.
*$           The special function Mcargquo() fetches the quote field.
*$
*$           Define one or more, single character, left delimiters for quote
*$           field arguments.
*$
*$           Begins a quote field argument.
*$
*$           Used to separate tokens (terminates non-quoted alphanumeric token).
*$
*$       [5] Quote Field Right Delimiters                  McIDAS default: "}"
*$           called syntax.qfld_delims_right
*$
*$           Define one or more, single character, right delimiters which
*$           correspond to the left delimiters defined in [4] above.
*$           If NULL, then these quote fields will extend to end of text line.
*$
*$           Terminates a quote field argument.
*$
*$       [6] Quoting Char Left Delimiters                  McIDAS default: "'"
*$           called syntax.quoting_delims_left
*$
*$           Quoting disables a characer's special meaning and allows it to be
*$           used literally, as itself.  A quoted argument is an argument
*$           enclosed within the quoting char delimiters, as defined in [6] and
*$           [7]. Everything between quoting_delims_left[i] and
*$           quoting_delims_right[i] is taken literally, except for escaping as
*$           described in [8] below.
*$
*$           Define one or more, single character, left delimiters for quoting
*$           arguments containing embedded spaces, special characters, and
*$           escape characters, such as:  'I am a quoted argument'.
*$
*$           Begins a quoted argument.
*$
*$       [7] Quoting Char Right Delimiters                 McIDAS default: "'"
*$           called syntax.quoting_delims_right
*$
*$           Define right delimiters which correspond to the left delimiters
*$           defined in [6] above.
*$
*$           Terminates a quoted argument.
*$
*$       [8] Quoting Char Escape Value                     McIDAS default: NULL
*$           called syntax.quoting_escape
*$
*$           Define a single escape character for quoted arguments as defined in
*$           [6] and [7] above, such as:  '\' is a single quote'
*$           The character following the quoting_escape is taken literally,
*$           and the quoting_escape character itself is removed.
*$
*$           If NULL, then two quoting_delims_right delimiters in a row will
*$           escape itself, such as: ''' is a single quote'
*$
*$       [9] Missing Argument                              McIDAS default: "X"
*$           called syntax.missing_argument
*$
*$           Define syntax to indicate that an argument is missing.
*$
*$           Used in conjunction with the missing argument replacement
*$           (defined in [10] below).
*$
*$      [10] Missing Argument Replacement                  McIDAS default: " "
*$           called syntax.missing_arg_value
*$
*$           Define replacement for the missing argument (defined in [9] above).
*$
*$           This value gives the internal representation for missing arguments.
*$
*$ Categories:
*$      CONVERTER, USER_INTERFACE
*/

int					/* returned arg-fetching handle      */
Mcargparse(				/* Parse Argument-Fetching Text      */
  const char *		 txtstr,	/* given text string to parse        */
  const McArgSyntax	*given_syntax,	/* given text syntax or NULL for def */
  int *		 parsed_len)	/* return length of parsed text      */
{
  int			 arg_handle;	/* returned arg-fetching handle      */
  int			 status;	/* M0argtok() status code            */
  int			 null_len;	/* used for optional parsed_len=NULL */
  int			 qfld = 0;	/* quote field keyword encountered ? */
  char			*kw = NULL;	/* init current parsing keyword name */
  char			*argstr;	/* ptr to current parsed arg string  */
/*----------------------------------------------------------------------------*/

  if (!parsed_len)  parsed_len = &null_len;
  *parsed_len = 0;			/* init returned parsed length  */

  if ((arg_handle = M0argalloc()) < 0) {	/* allocate new arg structure */
    return arg_handle;
  }

/*----------------------------------------------------------------------------*/
/* Build the desired arg-fetching structure from the text line tokens.        */

  while (1) {					/* repeat til no more args    */

    status = M0argtok(txtstr, given_syntax, &argstr);	/* get next arg       */

    switch (status) {

    case TOKKW:					/* add keyword to structure   */
      kw = argstr;				/* set new parsing keywd name */
      status = M0argaddkw(arg_handle, kw);
      break;

    case TOKQFLD:				/* add quote field to struct  */
      if (qfld <= 0) {				/* add quote field keywd name */
	MALLOC(kw, strlen(QFLD_KEYWORD) + 1);
	strcpy(kw, QFLD_KEYWORD);		/* " becomes current keyword  */
	qfld = M0argaddkw(arg_handle, kw);
      } else {
	kw = QFLD_KEYWORD;
      }
      status = M0argadd(arg_handle, kw, argstr);
      break;

    case TOKARG:				/* add arg to current keyword */
      status = M0argadd(arg_handle, kw, argstr);
      break;

    default:					/* no more args, or error     */
      if (status >= 0) {
	*parsed_len = status;			/* return parsed length       */
	return arg_handle;			/* return arg handle          */
      }
      break;
    }

    if (status < 0) {				/* malloc err/unmatched quote */
      Mcargfree(arg_handle);
      return status;
    }

    txtstr = NULL;				/* set to get next cmd arg    */

  }
}
/*==========================end Mcargparse====================================*/

/*==============================mcargparse_===================================*/
/*
*$ Name:
*$      mcargparse - Parse the given text into arg-fetching structure.
*$
*$ Interface:
*$      integer function
*$      mcargparse(character*(*) txtstr, character*(*) given_syntax(10),
*$                 integer parsed_len)
*$
*$ Input:
*$      txtstr        - Given text string to parse.
*$      given_syntax  - Given argument syntax, or 0 to use McIDAS default.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      parsed_len    - Optional length of parsed text, including any
*$                      terminating ';' if present.  (Use NULL to ignore).
*$
*$ Return values:
*$      >  0          - Argument-fetching handle.
*$      <  0          - failure statuses
*$
*$ Remarks:
*$      Use Mcargfree() to free the argument-fetching structure.
*$
*$      After mcargparse is called, arguments from the given text may be
*$      fetched by calling mcargstr, mcargint, mcargnum, mcargnam, etc.
*$
*$      The correct sequence of functions for text arg-fetching is:
*$         hand = mcargparse(txtstr, 0, parsed_len)
*$         stat = mcargstr(hand, ...), mcargint(hand, ...), etc.
*$              :
*$         stat = mcargfree(hand)
*$
*$      The argument-fetching sybsystem allows for parsing lines of text with
*$      varying syntax, based on the McIDAS command line syntax, thru programmer
*$      given parsing definitions (use read() to parse other syntax types).
*$      Parsing syntax is defined in a programmer given array of 10 strings.
*$      Each string may define one or more, single character, specific type of
*$      syntactic separators, or other type of syntax, as follows.
*$
*$      character*(*) syntax(10) /
*$     &        ' ',          ! white space
*$     &        '=,',         ! keyword separators
*$     &        ';',          ! command separators
*$     &        '{"',         ! qfld delims left
*$     &        '}',          ! qfld delims right
*$     &        '''',         ! quoting delims left
*$     &        '''',         ! quoting delims right
*$     &        0,            ! quoting escape
*$     &        'X',          ! missing argument
*$     &        ' ' /         ! missing arg value
*$
*$      If any syntax isn't defined, it defaults to the shown McIDAS syntax,
*$      The value 0 leaves the syntax undefined, indicating McIDAS default.
*$
*$       (1) White Space Characters                        McIDAS default: ' '
*$           Define one or more white space characters.  A white space
*$           character is a special, single character (usually a blank),
*$           used to determine how to separate tokens, and which is ignored.
*$
*$           Used to separate tokens (terminates non-quoted alphanumeric token).
*$           Ignores intermixed white space and keyword separators between
*$           tokens.  Ignores white space characters between a token and a
*$           keyword separator.
*$
*$       (2) Keyword Separators                            McIDAS default: '=,'
*$           Define one or more keyword separators.  A keyword separator is a
*$           special, single character, used to identify a keyword, and which
*$           occurs after the keyword name, possibly separated with one or more
*$           of the white space characters defined in (1) above.
*$
*$           Used to separate tokens (terminates non-quoted alphanumeric token).
*$           Ignores intermixed white space and keyword separators between
*$           tokens.
*$
*$       (3) Command Separators                            McIDAS default: ';'
*$           Define one or more command separators.  A command separator is a
*$           special, single character, used to separate commands.
*$
*$           Terminates the parsing of the text line.
*$
*$           Used to separate tokens (terminates non-quoted alphanumeric token).
*$
*$       (4) Quote Field Left Delimiters                   McIDAS default: '{"'
*$           The quote field is an argument enclosed within special delimiters,
*$           which may contain embedded spaces and other special characters,
*$           and which allows delimiter nesting, such as {prog {nested field}}.
*$           The special function Mcargquo() fetches the quote field.
*$
*$           Define one or more, single character, left delimiters for quote
*$           field arguments.
*$
*$           Begins a quote field argument.
*$
*$           Used to separate tokens (terminates non-quoted alphanumeric token).
*$
*$       (5) Quote Field Right Delimiters                  McIDAS default: '}'
*$           Define one or more, single character, right delimiters which
*$           correspond to the left delimiters defined in (4) above.
*$           If NULL, then these quote fields will extend to end of text line.
*$
*$           Terminates a quote field argument.
*$
*$       (6) Quoting Char Left Delimiters                  McIDAS default: ''''
*$           Quoting disables a characer's special meaning and allows it to be
*$           used literally, as itself.  A quoted argument is an argument
*$           enclosed within the quoting char delimiters, as defined in (6) and
*$           (7). Everything between quoting_delims_left(i) and
*$           quoting_delims_right(i) is taken literally, except for escaping as
*$           described in (8) below.
*$
*$           Define one or more, single character, left delimiters for quoting
*$           arguments containing embedded spaces, special characters, and
*$           escape characters, such as:  'I am a quoted argument'.
*$
*$           Begins a quoted argument.
*$
*$       (7) Quoting Char Right Delimiters                 McIDAS default: ''''
*$           Define right delimiters which correspond to the left delimiters
*$           defined in (6) above.
*$
*$           Terminates a quoted argument.
*$
*$       (8) Quoting Char Escape Value                     McIDAS default: 0
*$           Define a single escape character for quoted arguments as defined in
*$           (6) and (7) above, such as:  '\' is a single quote'
*$           The character following the quoting_escape is taken literally,
*$           and the quoting_escape character itself is removed.
*$
*$           If 0, then two quoting_delims_right delimiters in a row will
*$           escape itself, such as: ''' is a single quote'
*$
*$       (9) Missing Argument                              McIDAS default: 'X'
*$           Define syntax to indicate that an argument is missing.
*$
*$           Used in conjunction with the missing argument replacement
*$           (defined in (10) below).
*$
*$      (10) Missing Argument Replacement                  McIDAS default: ' '
*$           Define replacement for the missing argument (defined in (9) above).
*$
*$           This value gives the internal representation for missing arguments.
*$
*$ Categories:
*$      CONVERTER, USER_INTERFACE
*/

Fint					/* returned argument-fetching handle */
mcargparse_(				/* fortran jacket for Mcargparse     */
  const char	*txtstr,		/* given command text to parse       */
  const char	*given_syntax,		/* given text syntax, or 0 for def   */
  Fint		*parsed_len,		/* return length of parsed command   */
  FsLen	 siz_txtstr,		/* character*(*) size of 'txtstr'    */
  FsLen	 siz_given_syntax)	/* char*(*) size of 'given_syntax'   */
{
  int		 arg_handle;		/* returned argument-fetching handle */
  int		 parsed_len_;		/* declare working C int             */
  char		*txtstr_;		/* declare working C string          */
  char		**given_syntax_ = NULL;	/* declare working C strings array   */

  *parsed_len = 0;			/* init returned value               */

					/* convert character*(*) to C string */
  if (!(txtstr_ = fsalloc(txtstr, siz_txtstr)))  return MALLOC_RETERR;

/* If syntax is given, convert char*(*) to C strings array (else use NULL).   */
  if (siz_given_syntax > 0  &&  *given_syntax != 0) {
    int i;					/* loop counter               */
    given_syntax_ = Mcfstoarr(given_syntax, nMcArgSyntax, siz_given_syntax);
    for (i = 0;  i < nMcArgSyntax;  i++) {	/* check each given syntax    */
      if (*given_syntax_[i]) continue;		/* for all blanks, and if so, */
      free(given_syntax_[i]);			/* free its null string,      */
      given_syntax_[i] = NULL;			/* & reset to NULL to use def */
    }
  }

  arg_handle = Mcargparse(txtstr_, (McArgSyntax*)given_syntax_, &parsed_len_);

  free(txtstr_);				/* free working C string      */
  Mcfreearr(given_syntax_, nMcArgSyntax);	/* free working C str array   */
  NUMC2F(parsed_len, parsed_len_);		/* return length to fortran   */

  return arg_handle;				/* return arg-fetching handle */
}
/*==========================end mcargparse_===================================*/

/*==============================M0argtok======================================*/
/*
*| Name:
*|      M0argtok - Retrieve next argument token within the given text string.
*|
*| Interface:
*|      #include "m0arg.h"
*|
*|      int
*|      M0argtok(const char* txtstr, const McArgSyntax* given_syntax,
*|               char** argstr)
*|
*| Input:
*|      txtstr        - New given text string, or NULL to get next token.
*|      given_syntax  - Given argument syntax, or NULL to use McIDAS default.
*|                      See Mcargparse's remarks for detailed description.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      argstr        - Malloc()ed arg token string, or NULL if none found.
*|
*| Return values:
*|      Status indicating type of argument token as follows:
*|      >=  0         - end of command, returns parsed command's length,
*|                      including any terminating ';' (if present)
*|         -1         - "normal" argument token   (TOKARG)
*|         -2         - keyword argument token    (TOKKW)
*|         -3         - quote field string        (TOKQFLD)
*|      <= -4         - failure statuses
*|
*| Remarks:
*|      Calling program is responsible for free()ing the returned argstr tokens.
*|
*|      The McIDAS quote field counts as one keyword, with a keyword name of ".
*|
*| Categories:
*|      CONVERTER, USER_INTERFACE
*/

int
M0argtok(
  const char		 *txtstr,	/* new command, or NULL for next arg  */
  const McArgSyntax	 *given_syntax,	/* argument syntax, or NULL for dflt  */
  char			**argstr)	/* malloc'ed arg token string, or NULL*/
{
  static char		 *cmd = NULL;	/* malloc original given command text */
  static char		 *c;		/* ptr to current place within cmd    */

  int			  status = 0;	/* init returned status code          */
  size_t		  lenarg;	/* length of next found arg token     */
  char			 *arg = NULL;	/* pointer to next found arg token    */
  char			 *w = NULL;	/* loop ptr to work copy of cmd's arg */
  char			 *left;		/* ptr to found left delimiter        */
  char			  right;	/* right delimiter to match with      */
  int			  pair;		/* wrk cnt 4 nested left/right delims */

					/* init given syntax or use default   */
  McArgSyntax		  syntax = M0argsyntax(given_syntax);
  char			 *toksep;	/* concatenate all token separators   */
  const char		 *carg = NULL;	/* pointer to const version of arg    */
/*----------------------------------------------------------------------------*/
  if (!argstr) {			/* validate given parameter           */
    PROG_ERROR(parameter,		/* print standard diagnostic          */
	       "missing char **argstr - can't get token for txtstr = %s",
	       (txtstr ? txtstr : cmd));
    return PARM_RETERR;		/* return invalid parm status         */
  }

  *argstr = NULL;			/* init the returned arg token string */
/*----------------------------------------------------------------------------*/
/* if new command text given, save new txtstr into newly allocated memory     */

  if (txtstr) {
    if (cmd != NULL)
      free(cmd);

    /* cmd is now a newly malloced string with trailing blanks removed */
    c = cmd = fsalloc(txtstr, (FsLen)strlen(txtstr));
    if (cmd == NULL) {
      Mceprintf("OUT OF MEMORY\n");
      PROG_ERROR(fsalloc, "Failed fsalloc for \"%s\"", txtstr);
      return MALLOC_RETERR;
    }
  }

  if (!cmd)  return status;		/* check for no command text to parse */

/*----------------------------------------------------------------------------*/
/* Concatenate all of the argument separator delimiters together.             */

  MALLOC(toksep, 1 + STRLEN(syntax.white_space)
		   + STRLEN(syntax.keyword_separators)
		   + STRLEN(syntax.command_separators)
		   + STRLEN(syntax.qfld_delims_left)
		   + STRLEN(syntax.quoting_delims_left));

  strcat(strcpy(toksep, syntax.white_space), syntax.keyword_separators);

  c += strspn(c, toksep);		/* ignore white space & keyword syms  */

  strcat(toksep, syntax.command_separators),
  strcat(toksep, syntax.qfld_delims_left);
  strcat(toksep, syntax.quoting_delims_left);

/*----------------------------------------------------------------------------*/
/* skip parsing if end of command text - when command separator or \0         */

  if (strchr(syntax.command_separators, *c)) {
  }

/* otherwise, point "arg" to beginning of next argument, and determine lenarg */
/*----------------------------------------------------------------------------*/
/* quote field argument token - match up nested left & right delims           */

  else if (left = strchr(syntax.qfld_delims_left, *c)) {
    right = syntax.qfld_delims_right[left - syntax.qfld_delims_left];

    status = TOKQFLD;			/* set type to quote field string     */
    arg = ++c;				/* adjust ptr to begin of argument    */

    for (pair=1;  *c && pair>0;  c++)
      if      (*c == right)  pair--;	/* pair up all left/right delims      */
      else if (*c == *left)  pair++;

    lenarg = c - arg;			/* calc argument token length         */
    if (pair == 0) {
      --lenarg;			/* adjust length if right delimiter   */
      if (*left == right)  --c;	/* adjust current ptr if ending "     */
    }

    else if (*left==right && !strcspn(arg, " "))
      arg = NULL;			/* ignore trailing blanks             */

  }
/*----------------------------------------------------------------------------*/
/* quoting string arg - remove escapes and find ending right delimiter        */

  else if (left = strchr(syntax.quoting_delims_left, *c)) {
    right = syntax.quoting_delims_right[left - syntax.quoting_delims_left];

    status = TOKARG;			/* set type to "normal" arg token     */
    c++;				/* adjust ptr to begin of argument    */
    MALLOC(arg, strlen(c) + 1);		/* Point new arg to malloc()ed        */
    strcpy(arg, c);			/*  work copy of current place.       */

/*  when escape char defined, remove them, and find quoting string end */
    if (syntax.quoting_escape && *syntax.quoting_escape)
      for (w=arg;  *w && *w!=right;  w++) {
	if (*w == *syntax.quoting_escape) {
	  memmove(w, w+1, strlen(w));	/* shift left to erase escape char    */
	  ++c;				/* adjust cmd's current place ptr     */
	}
      }

/*  right delim escapes itself - remove them & find string end */
    else
      for (w=arg;  *w;  w++) {
	if (*w == right) {		/* if this is ending delim            */
	  if (*(w+1) != right) break;	/* stop if ending delim by itself     */
	  memmove(w, w+1, strlen(w));	/* shift left to erase escape char    */
	  ++c;				/* adjust cmd's current place ptr     */
	}
      }

/*  missing right quote? */
    if (*w != right) {
      free(toksep);
      free(arg);
      Mceprintf("Mismatched quotes: %s\n", cmd);
      return QUOTE_RETERR;		/* return mismatched quote status     */
    }

    lenarg = w - arg;			/* calculate arg length               */
    c += lenarg;			/* increment current ptr              */
    if (*c)  ++c;			/* adjust ptr to after right delim    */

/*  keyword argument token? */
    c += strspn(c, syntax.white_space);	/* skip over 1 or more blanks       */
    if (*c  &&  strchr(syntax.keyword_separators, *c)) {
      status = TOKKW;		/* set type to keyword token          */
    }
  }
/*----------------------------------------------------------------------------*/
/* non-quoted alphanumeric argument token                                     */

  else {

    status = TOKARG;			/* set type to "normal" arg           */
    arg = c;				/* point to new arg token             */
					/* calc arg len & incre ptr           */
    c += lenarg = strcspn(arg, toksep);

/*  keyword argument token? */
    c += strspn(c, syntax.white_space);	/* skip over 1 or more blanks       */
    if (*c  &&  strchr(syntax.keyword_separators, *c)) {
      status = TOKKW;			/* set type to keyword token          */
    }

/*  missing argument token?   Should be case insensitive. */
    else if (lenarg == STRLEN(syntax.missing_argument)) {
      if (Mcstrnicmp(arg, syntax.missing_argument, lenarg) == 0) {
	carg = syntax.missing_arg_value;	/* reset to missing arg value */
      }
      w = NULL;					/* reset w to NULL            */
    }
  }

  if (carg == NULL) {
    carg = arg;					/* set const arg ptr if unset */
  }

/*----------------------------------------------------------------------------*/
/* "carg" now points to beginning of next argument, and "lenarg" = its length */
/* allocate new memory for argument token, and copy it there as a string      */

  if (carg) {				/* Allocate new memory for arg token, */

    MALLOC(*argstr, lenarg + 1);
    memcpy(*argstr, carg, lenarg);	/*  and copy it there,                */
    (*argstr)[lenarg] = '\0';		/*   as a string.                     */

    if (status == TOKKW)		/* if keyword token,                  */
      Mcupcase(*argstr);		/* convert keyword to upper case      */

    if (w) free(arg);			/* check for & free copy of cmd       */

  }
/*----------------------------------------------------------------------------*/
/* end of current command - want to return length of scanned command          */

  else {				/* c points to either ';' or 0        */

    if (*c)  ++c;			/* adjust c to next token (after ';') */
    status = (int)(c - cmd);		/* compute length of parsed command   */

    if (!*c) {
      free(cmd);			/* free command text if done parsing  */
      cmd = NULL;
    }

  }
/*----------------------------------------------------------------------------*/
  free(toksep);

  return status;			/* return status code                 */
}
/*==========================end M0argtok======================================*/

/*==============================m0argtok_=====================================*/
/*
*| Name:
*|      m0argtok - Retrieve next argument token within the given text string.
*|
*| Interface:
*|      integer function
*|      m0argtok(character*(*) txtstr, character*(*) given_syntax,
*|               character*(*) arg)
*|
*| Input:
*|      txtstr        - New given text string, or 0 to get next token.
*|      given_syntax  - Given argument syntax, or 0 to use McIDAS default.
*|                      See mcargparse's remarks for detailed description.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      arg           - Next argument token string, or blanks if none found.
*|
*| Return values:
*|      Status indicating type of argument token as follows:
*|      >=  0         - end of command, returns parsed command's length,
*|                      including any terminating ';' (if present)
*|         -1         - "normal" argument token
*|         -2         - keyword argument token
*|         -3         - quote field string
*|      <= -4         - failure statuses
*|
*| Remarks:
*|      The McIDAS quote field counts as one keyword, with a keyword name of ".
*|
*| Categories:
*|      CONVERTER, USER_INTERFACE
*/

Fint					/* returned status code               */
m0argtok_(				/* fortran jacket for M0argtok        */
  const char	 *txtstr,		/* new text, or ' ' to get next arg   */
  const char	 *given_syntax,		/* given text syntax, or 0 for def    */
  char		 *arg,			/* next argument token string or ' '  */
  FsLen	  siz_txtstr,		/* character*(*) size of 'txtstr'     */
  FsLen	  siz_given_syntax,	/* char*(*) size of 'given_syntax'    */
  FsLen	  siz_arg)		/* character*(*) size of 'arg'        */
{
  int		  status;		/* returned status code               */
  char		 *arg_ = NULL;		/* declare working C string           */
					/* convert character*(*) to C string  */
  char		 *txtstr_ = fsalloc(txtstr, siz_txtstr);
  char		**given_syntax_ = NULL;	/* declare working C strings array    */

  memset(arg, ' ', siz_arg);		/* init returned arg to blanks        */

  if (!txtstr_)  return MALLOC_RETERR;

/* If syntax is given, convert char*(*) to C strings array (else use NULL).   */
  if (siz_given_syntax > 0  &&  *given_syntax != 0) {
    int i;					/* loop counter               */
    given_syntax_ = Mcfstoarr(given_syntax, nMcArgSyntax, siz_given_syntax);
    for (i = 0; i < nMcArgSyntax; i++) {	/* check each given syntax    */
      if (*given_syntax_[i]) continue;		/* for all blanks, and if so, */
      free(given_syntax_[i]);			/* free its null string,      */
      given_syntax_[i] = NULL;			/* and reset to NULL for def  */
    }
  }

  status = M0argtok(txtstr_, (McArgSyntax*)given_syntax_, &arg_);

  /* free allocated memory */
  free(txtstr_);
  Mcfreearr(given_syntax_, nMcArgSyntax);

  /* convert C string to fortran  */
  Mcstrtofs(arg, arg_, siz_arg);
  free(arg_);

  /* return type of token status  */
  return status;
}
/*==========================end m0argtok_=====================================*/

/*==============================M0argsyntax===================================*/
/*
*| Name:
*|      M0argsyntax - Update and return the default McIDAS syntax definition.
*|
*| Interface:
*|      #include "m0arg.h"
*|
*|      McArgSyntax
*|      M0argsyntax(const McArgSyntax* given_syntax)
*|
*| Input:
*|      given_syntax  - Given text parsing syntax or NULL to get McIDAS default.
*|                      Uses given_syntax[n] if it's not NULL, otherwise def.
*|                      See Mcargparse's remarks for detailed description.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      !NULL         - New text parsing syntax array, updated with McIDAS defs.
*|                      See Mcargparse's remarks for detailed description.
*|       NULL         - failure
*|
*| Remarks:
*|      none
*|
*| Categories:
*|      CONVERTER, USER_INTERFACE
*|
*/

McArgSyntax
M0argsyntax(
  const McArgSyntax *given_syntax)		/* given text syntax, or NULL */
{
  McArgSyntax syntax = {			/* initialize mcidas syntax  */
    WHITE_SPACE, KEYWORD_SEPARATORS, COMMAND_SEPARATORS,
    QFLD_DELIMS_LEFT, QFLD_DELIMS_RIGHT,
    QUOTING_DELIMS_LEFT, QUOTING_DELIMS_RIGHT, QUOTING_ESCAPE,
    MISSING_ARGUMENT, MISSING_ARG_VALUE
  };
/*----------------------------------------------------------------------------*/
  if (given_syntax) {

    int i;					/* loop counter               */

    for (i=0; i<nMcArgSyntax; i++)		/* Look for each given syntax */
      if (((char* const *)given_syntax)[i])	/*  if there, then use it.    */
	((char**)&syntax)[i] = ((char* const *)given_syntax)[i];

  }
/*----------------------------------------------------------------------------*/
  return syntax;
}
/*==========================end M0argsyntax===================================*/

#ifdef TEST_ARGPARSE

/*
 * To build some simple regression tests, do:
 *
 *	cc -O -DTEST_ARGPARSE -o test_argparse -I/home/mcidas/inc \
 *		argparse.c -L/home/mcidas/lib -lmcidas
 *
 * (You may need to add '-Ae' on HP-UX) then run:
 *
 *	./test_argparse
 *
 * which should print:
 *
 *	All tests succeeded!
 */

#include <stdlib.h>
#include "mcidas.h"
#include "m0arg.h"

const int verbose = 0;

const char *tokerr[] = {
  "Handle is zero",
  "Returned token is a \"normal\" argument",
  "Returned token is a keyword argument",
  "Returned token is a quote field argument",
};

static void
print_aperr(const char *str, int err)
{
  const char *errstr;

  if (err >= -4) {
    errstr = tokerr[-err];
  } else if (err == QUOTE_RETERR) {
    errstr = "QUOTE_RETERR";
  } else if (err == MALLOC_RETERR) {
    errstr = "MALLOC_RETERR";
  } else {
    errstr = "?? unknown ??";
  }
  fprintf(stderr, "Mcargparse(%s) returned %s(%d)\n", str, errstr, err);
}

static int
sq_check_one(const char *str)
{
  char *cmd;
  int handle, plen;
  int rtn;
  char *val;
  char ch;

  handle = Mcargparse(str, NULL, &plen);
  if (verbose) {
    if (handle > 0) {
      printf("\"%s\": plen %d", str, plen);
    } else {
      print_aperr(str, handle);
    }
  }

  (void )M0cmdput(handle);

  if (m0cmdglo_() != 0) {
    return -7;
  }

  if (verbose) {
    cmd = Mcargcmd(handle);
    printf(" -> \"%s\"", cmd);
    printf("\n");

    rtn = Mcargstr(handle, NULL, 0, NULL, &val);
    ch = rtn == 1000 ? ':' : '!';
    printf("\t0%c%s%c", ch, val ? val : "<NULL>", ch);

    rtn = Mcargstr(handle, NULL, 1, NULL, &val);
    ch = rtn == 1000 ? ':' : '!';
    printf("\t1%c%s%c", ch, val ? val : "<NULL>", ch);

    rtn = Mcargquo(handle, &val);
    ch = rtn == 1000 ? ':' : '!';
    printf("\tQ%c%s%c", ch, val ? val : "<NULL>", ch);

    free(cmd);
    printf("\n\n");
  }

  Mcargfree(handle);
  return (handle < 0 ? handle : 0);
}

struct sq_regress_struct {
  const char *str;
  int rtnval;
} sq_regress[] = {
  { "cmd1 '",				-12345 },
  { "cmd1 xxx'",			-12345 },

  { "cmd1 \"Judy's",			0 },
  { "cmd1 'Judy's'",			-12345 },

  { "cmd1 \"string\"",			0 },
  { "cmd1 \"string",			0 },

  { "cmd1 'string split'",		0 },
  { "cmd1 'string''s split'",		0 },
  { "cmd1 'string''s split",		-12345 },
  { "cmd1 'string split",		-12345 },

  { "cmd1 'can''t assume anything'",	0 },
  { "cmd1 'can't assume anything'",	-12345 },
};
const int sq_regress_len = sizeof(sq_regress) / sizeof(sq_regress[0]);

static int
regress_singlequote(void)
{
  int i, rtnval, throwback;

  throwback = 0;
  for (i = 0; i < sq_regress_len; i++) {
    rtnval = sq_check_one(sq_regress[i].str);
    if (rtnval != sq_regress[i].rtnval) {
      fprintf(stderr, "For |%s|, expected %d, got %d\n", sq_regress[i].str,
		sq_regress[i].rtnval, rtnval);
      throwback = 1;
    }
  }

  return throwback;
}

enum default_state {
  SMALL_X =   0x00,
  LARGE_X =   0x01,
  SHORT_ARG = 0x02,
  LONG_ARG =  0x03,

  DEFAULT_STATE_MASK = 0x03,
  DEFAULT_STATE_SHIFT = 2
};

static const char *
default_arg(int num)
{
  const char *arg;

  switch (num) {
  case SMALL_X:
    arg = "x";
    break;
  case LARGE_X:
    arg = "X";
    break;
  case SHORT_ARG:
    arg = "z";
    break;
  case LONG_ARG:
    arg = "syzygy";
    break;
  default:
    arg = 0;
    break;
  }

  return arg;
}

static int
validate_dflt(int what, const char *argin, const char *argout, int argtype)
{
  if ((what == SMALL_X || what == LARGE_X) && (argtype == 0 && argout == 0)) {
    return 1;
  }

  if ((what == SHORT_ARG || what == LONG_ARG) &&
     (argtype == 1000 || strcmp(argin, argout) == 0))
  {
    return 1;
  }

  return 0;
}

#define NUM_DEFAULT_ARGS	3
#define DFLTLINE_LEN		64

static int
regress_default(void)
{
  int i, j;
  int throwback = 0;
  int what[NUM_DEFAULT_ARGS];
  const char *arg[NUM_DEFAULT_ARGS];

  for (i = 0; i < (NUM_DEFAULT_ARGS << DEFAULT_STATE_SHIFT); i++) {
    int badarg = 0;

    for (j = 0; j < NUM_DEFAULT_ARGS; j++) {

      /* get args */
      what[j] = (i >> (DEFAULT_STATE_SHIFT * j)) & DEFAULT_STATE_MASK;
      arg[j] = default_arg(what[j]);
      if (arg[j] == 0) {
	badarg = 1;
      }
    }

    /* if both args are valid */
    if (!badarg) {
      char line[DFLTLINE_LEN];
      char *cmd;
      int handle, plen;
      int rtn;
      char *val;

      /* build command line */
      strcpy(line, "CMD");
      for (j = 0; j < NUM_DEFAULT_ARGS; j++) {
	strncat(line, " ", DFLTLINE_LEN);
	strncat(line, arg[j], DFLTLINE_LEN);
      }

      handle = Mcargparse(line, NULL, &plen);
      if (verbose) {
	if (handle > 0) {
	  printf("\"%s\": plen %d", line, plen);
	} else {
	  print_aperr(line, handle);
	  throwback = -1;
	}
      }

      cmd = Mcargcmd(handle);
      if (verbose ) {
	printf("\"%s\" -> \"%s\"", line, cmd);
      }

      for (j = 0; j < NUM_DEFAULT_ARGS; j++) {
	rtn = Mcargstr(handle, NULL, j+1, NULL, &val);
	if (!validate_dflt(what[j], arg[j], val, rtn)) {
	  fprintf(stderr, "arg %d of \"%s\"", j + 1, line);
	  fprintf(stderr, " returns %d \"%s\",", rtn, val ? val : "<NULL>");
	  fprintf(stderr, " instead of 0 \"<NULL>\"\n");
	  throwback = 1;
	} else if (verbose) {
	  printf("\t%d%s(%d)", j + 1, val ? val : "<NULL>", rtn);
	}
      }

      free(cmd);
      if (verbose) {
	printf("\n\n");
      }

      Mcargfree(handle);
    }
  }

  return throwback;
}

int
main(int argc, char *argv[])
{
  if (regress_singlequote() != 0) {
    return(1);
  }

  if (regress_default() != 0) {
    return(1);
  }

  printf("All tests succeeded!\n");
  return(0);
}
#endif /* TEST_ARGPARSE */

/*==========================end argparse.c====================================*/
