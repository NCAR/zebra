/* 
Copyright 1996 COMPUTER GENERATION, INC.,

The software is provided "as is", without warranty of any kind, express
or implied, including but not limited to the warranties of
merchantability, fitness for a particular purpose and noninfringement.
In no event shall Computer Generation, inc. nor the author be liable for
any claim, damages or other liability, whether in an action of contract,
tort or otherwise, arising from, out of or in connection with the
software or the use or other dealings in the software.

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation.

Author:
Gary Aviv 
Computer Generation, Inc.,
gary@compgen.com
*/
/*
;+
LiteClueTest	--	Test program for LiteClue widget

Author:		Gary Aviv (gary@compgen.com)

Functions:	Provide a test fixture for LiteClue widget
 
;-
*/

/*	Revision Information:

$Log: not supported by cvs2svn $

$log
Initial version.
$log

*/

#ifndef __VMS
#define 	VERSION "$Revision: 1.1 $"
#ident		"@(#)$Id: LiteClueTest.c,v 1.1 2001-11-30 00:42:07 granger Exp $ $Revision: 1.1 $"
#else
# define        VERSION "1.2"
# pragma module LiteClueTest "LiteClueTest 1.2"
#endif

#define	PGMNAME		"LiteClueTest"

/*----------- Include Files -------------*/

#include <stdio.h>
#include <stdlib.h>

#ifdef NeedFunctionPrototypes
#undef NeedFunctionPrototypes
#endif
#define NeedFunctionPrototypes 1

#ifdef NeedVarargsPrototypes
#undef NeedVarargsPrototypes
#endif
#define NeedVarargsPrototypes 1

#include <X11/Intrinsic.h>
#ifndef __VMS 
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <LiteClue.h>
#else
# include <Xaw_Directory/SimpleMenu.h>
# include <Xaw_Directory/Box.h>
# include <Xaw_Directory/Command.h>
# include "LiteClue.h"
#endif
#define SET_ARG(a,b) XtSetArg(args[arg_cnt], a, b),arg_cnt++
static void exit_cb(Widget  widg, XtPointer ctx ,XtPointer   cb_data);
static XFontSet check_font_set(Widget w, char * font_string );


static Widget toplevel;
static int debug = 0;
static 	XtAppContext AppContext;

main(int 	argc, char 	*argv[])
{
	Widget liteClue, menubar, popup, menub, megatb2, megatb, pulldown;
	int	in, out, numargs, len;	/* argument processing */
	char * font_str = NULL;
	int debug = 0;
	Arg args[10];
	Cardinal arg_cnt = 0;
  

	/* process and remove arguments specific to this program */
	for (numargs = argc, out = in = 1; in < numargs; in++, argc--)
	{
		if (strcmp (argv[in], "-debug" ) == 0)
		{
			debug = 1;
			continue;
		}
		if (strcmp (argv[in], "-fn") == 0)
		{
			argc--;
			in++;
			font_str = strdup(argv[in]);
			continue;
		}
		argv[out++] = argv[in];	/* close up - not a local arg */
	}
	argv[out] = NULL;		/* terminate */


  	toplevel = XtVaAppInitialize(&AppContext,
			"LiteClueTest",
			NULL, 0, /*appl cmd line options*/
			&argc, argv, 
			NULL, /* fallback resources */
			NULL);	/* top level resources, VA args */

	if (font_str)
		SET_ARG(XtNfontSet, check_font_set(toplevel, font_str) );
	liteClue = XtCreatePopupShell( 
		"popup_shell", 
		xcgLiteClueWidgetClass, toplevel,
		args,arg_cnt);

	menub = XtVaCreateManagedWidget("menub", boxWidgetClass, 
			toplevel, XtNorientation, XtEhorizontal, NULL);

	megatb = XtVaCreateManagedWidget("Button1", commandWidgetClass, 
		menub, XtNlabel, "Button1", NULL);
	XcgLiteClueAddWidget(liteClue, megatb,  "LiteClue1", 0, 0);

	megatb2 = XtVaCreateManagedWidget("Button2", commandWidgetClass, 
		menub, XtNlabel, "Button2", NULL);
	XcgLiteClueAddWidget(liteClue, megatb2,  "LiteClue2", 0, 0);

	megatb = XtVaCreateManagedWidget("Exit", commandWidgetClass, 
		menub, XtNlabel, "Exit", NULL);
	XcgLiteClueAddWidget(liteClue, megatb,  "Exit Now", 0, 0);
	XtAddCallback(megatb,   XtNcallback, 
		(XtCallbackProc) exit_cb, NULL );

#if 0
	{	/* Set Values test */
	int wait, cancel;
	XtVaSetValues(liteClue, XgcNwaitPeriod, 1000, NULL);
	XtVaGetValues(liteClue, XgcNwaitPeriod, &wait, NULL);
	fprintf(stderr,"waitPeriod=%d\n", wait);
	}
#endif

	XtRealizeWidget(toplevel);

#if 1
	/* clue on insensitive widget test */
	XtSetSensitive(megatb2, False);
	{
	    XEvent event;

	    for (;;) {
	        XtAppNextEvent(AppContext, &event);
		XcgLiteClueDispatchEvent(liteClue, &event) ;
	        XtDispatchEvent(&event);
	    }
	} 

#else
	XtAppMainLoop(AppContext);
#endif
}

static void exit_cb(Widget  widg, XtPointer ctx ,XtPointer   cb_data)
{
	exit(0);
}

static XFontSet check_font_set(Widget w, char * font_string )
{
	XFontSet fontset;
	char ** missing_charsets;
	int num_missing_charsets=0;
	char * default_string;
	int i;

	fontset = XCreateFontSet(XtDisplay(w), font_string ,
		&missing_charsets, &num_missing_charsets, &default_string);
	if (num_missing_charsets)
	{
		fprintf(stderr, "The following %d charsets are missing from %s:\n", 
				num_missing_charsets, font_string ) ;
		for (i=0; i < num_missing_charsets; i++)
		{
			fprintf(stderr, "%s\n", missing_charsets[i] );
		}
		if (!default_string) 
			fprintf(stderr, "default_string=%s\n", default_string);
		XFreeStringList(missing_charsets);
	}
	return fontset;
}

