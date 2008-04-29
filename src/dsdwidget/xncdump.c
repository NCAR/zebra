
/*
 * $Id: xncdump.c,v 1.4 2001-12-20 21:38:55 burghart Exp $
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/AsciiText.h>


/*
 * ---------------- Debugging macros --------------
 */

#ifndef _DEBUG_H_
#define _DEBUG_H_

#ifdef DEBUG
#define IFD( x ) x
#else
#define IFD( x ) 
#endif

#endif  /* _DEBUG_H_ */


#ifndef NCDUMP
#define NCDUMP "ncdump -h"
#endif

Widget text = NULL;
char *filename = NULL;

Boolean using_tmp = False;	/* Flag true when tmp file being used */
static char tmp[256];   	/* Use global tmp name to remove 
				 * before exiting */
static char rcsid[]="$Id: xncdump.c,v 1.4 2001-12-20 21:38:55 burghart Exp $";

static char *fallbacks[] = {
"Xncdump*text.height:		400",
"Xncdump*text.width:		700",
"Xncdump*text.scrollVertical:	always",
"Xncdump*text.scrollHorizontal:	always",
"Xncdump*foreground:		darkviolet",
"Xncdump*background:		lightyellow",
NULL };



void clean_up( )
{
   if (using_tmp)   		 /* Remove old file, no longer needed */
      unlink(tmp);
}   


void QuitCallbackHandler(w,client_data,call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
   XtDestroyApplicationContext(XtWidgetToApplicationContext(w));
   clean_up();
   exit(0);
} /* End CancelCallbackHandler */



void
ShowFile (w, showdata, calldata)
Widget w;
XtPointer showdata;
XtPointer calldata;
{
   Arg args[2];
   static char tmp2[256];
   char cmd[256];
   char fn[256];

   strcpy (fn, filename);
   IFD(fprintf(stderr,"File selected: %s\n",fn);)
   if (strstr(fn,".cdf") || strstr(fn,".nc"))  	/* Check for netCDF file */
   {
      IFD(fprintf(stderr,"CDF file detected.\n");)
      XtSetArg(args[0], XtNstring, "Reading netCDF file...\n");
      XtSetArg(args[1], XtNtype, XawAsciiString);
      XtSetValues(text, args, 2);
      strcpy(tmp2, "/tmp/ncdumpXXXXXX");
      mkstemp(tmp2);
      sprintf(cmd,"ncdump%s %s > %s",(showdata)?"":" -h",fn,tmp2);
      IFD(fprintf(stderr,"Using shell command: %s\n",cmd);)
      if (system(cmd))   /* Make sure no errors from shell */
      {
	 XtSetArg(args[0], XtNstring, "Dump of netCDF file failed!\n");
         XtSetArg(args[1], XtNtype, XawAsciiString);
         XtSetValues(text, args, 2);
	 return;
      }
      strcpy(fn,tmp2);   /* Otherwise use new tmp file */
      XtSetArg(args[0], XtNstring, fn);
      XtSetArg(args[1], XtNtype, XawAsciiFile);
      XtSetValues(text, args, 2);
      if (using_tmp)
	 unlink(tmp);
      strcpy(tmp,tmp2);
      using_tmp = True;
   }
   else
   {
      XtSetArg(args[0], XtNstring, "Reading text file...\n");
      XtSetArg(args[1], XtNtype, XawAsciiString);
      XtSetValues(text, args, 2);
      XtSetArg(args[0], XtNstring, fn);
      XtSetArg(args[1], XtNtype, XawAsciiFile);
      XtSetValues(text, args, 2);
      if (using_tmp)
	 unlink(tmp);
      using_tmp = False;
   };
   return;
}


/*-----------------------------------------------------------*

	main(argc,argv)

 *-----------------------------------------------------------*/

int
main(argc,argv)
int argc;
char **argv;
{
	Widget top,pane,commands,quit,data,header;
	XtAppContext app_context;

	top = XtAppInitialize(&app_context, "Xncdump",
			      NULL,0,
			      &argc,argv,
			      fallbacks, NULL,0);

	/* Expect a single file name on the command line */
	if (argc != 2)
	{
		fprintf (stderr, "usage: %s <file>\n", argv[0]);
		exit (9);
	}

	pane = XtCreateManagedWidget("pane", panedWidgetClass, top, NULL,0);

	commands = XtVaCreateManagedWidget("commands", boxWidgetClass, pane,
					   XtNorientation, XtorientHorizontal,
					   NULL);
	quit = XtVaCreateManagedWidget("quit", commandWidgetClass, commands, 
				       XtNlabel, "Quit",
				       NULL);
	data = XtVaCreateManagedWidget("data", commandWidgetClass, commands, 
				       XtNlabel, "Show Data",
				       NULL);
	header = XtVaCreateManagedWidget("header", commandWidgetClass, 
					 commands,
					 XtNlabel, "Show Header",
					 NULL);
        text = XtVaCreateManagedWidget("text", asciiTextWidgetClass,
					pane,
					XtNtype, XawAsciiString,
					XtNstring, "Reading file...",
					NULL);
	XawTextDisplayCaret(text, False);

	XtAddCallback(quit, XtNcallback, QuitCallbackHandler, NULL);
	XtAddCallback(data, XtNcallback, ShowFile, (XtPointer)True);
	XtAddCallback(header, XtNcallback, ShowFile, (XtPointer)False);
	XtRealizeWidget(top);
	filename = argv[1];
	ShowFile (header, (XtPointer)False, (XtPointer)NULL);

	XtAppMainLoop(app_context);
	return (0);

} /* End main */

