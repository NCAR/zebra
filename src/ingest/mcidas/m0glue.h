/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 M0GLUE.H 19-Feb-96,15:16:20,`DWS' reglue: new file                      */
/* 2 M0GLUE.H 20-Feb-96,12:10:36,`USER' Released                             */
/* 3 M0GLUE.H 22-Mar-96,11:52:22,`DWS' reglue: modified file                 */
/* 4 M0GLUE.H 25-Mar-96,13:55:46,`USER' Released                             */
/**** McIDAS Revision History *** */

#ifndef _M0GLUE_H
#define _M0GLUE_H

/*    M0glue.h
 *
 *    functions and declarations used to manage user common 
 *    and control processes
 */



#ifndef _MCIDAS_H
#include "mcidas.h"
#endif


/*  defins a structure in which frame creation requests are held */

struct M0Framereq  {
    int num_frm;
    int lines;
    int elements;
    struct M0Framereq *link;
} ;

/*
 * modifiers of this struct should call M0uclock 
 * on the byte offset of this address from the start of UC
 */
struct M0Frameflags {

    /*
     * processes which modify the contents of the frame object
     * and want to apprise the image application(s) of the fact
     * set 'dirty' to -1 (all one bits)
     *
     * image applications can use individual bits and reset them
     * at will
     */
    int dirty;

    /*
     *  Bounding rectangle.  Line and element start at 1
     * (1,1) is upper left corner
     *  0 means the rectangle is empty
     */
    int ul_line;     /* upper left */
    int ul_elem;
    int lr_line;     /* lower right*/
    int lr_elem;

    /*
     * stretch table always has 256 entries from 0 - (maxcolors-1)
     */
    int stretch_table[256];

    /*
     * color table has up to 256 entries
     */
    unsigned int color_table[256];

};



typedef struct M0Framereq   M0framereq;
typedef struct M0Frameflags M0frameflags;

/*
 * If a keyin finds KEYIN_ARGV0_CHAR in its argv[0], it should
 * treat argv[1] as the entire command string.
 *
 * If there is anything after the KEYIN_ARGV0_CHAR in argv[0],
 * it is a program name used in the Mciniterr() error messages.
 *
 * If there is anything before the KEYIN_ARGV0_CHAR in argv[0],
 * it can be ignored.
 */

#define KEYIN_ARGV0_CHAR SPACE

/* environment variable names */

/* holds key for rendezvous with positive uc */
#define M0ENV_POSUC	"MCENV_POSUC"
/* holds key for rendezvous with negative uc */
#define M0ENV_NEGUC	"MCENV_NEGUC"
/* if present, means that output will be read by the text window */
#define M0ENV_TEXT	"MCENV_TEXT"

/* name of file for REDIRECT */
#define LW_PATH "LWPATH.NAM"

/* name of file for mcclean */
#define MCCLEAN_FILE "mcclean.lock"

/* size of process user common (negative UC) */
#define NEGUC_SIZE 800

/* size of display common (positive UC) */
#define POSUC_SIZE 8192*(int)sizeof(Fint)

/* size of REDIRECT memory */
#define VOLNAM_SIZE 30004

/* rounds to a multiple of sizeof(Fint) */
#define ROUND_BYTES(x) (((x)%(int)sizeof(Fint))==0 ? (x) : (x) + (int)sizeof(Fint) - (x)%(int)sizeof(Fint))

/*
 * size of a frame object: pels, flag struct, and rounding
 *
 * We assume that an M0frameflags structure will be a multiple of
 * sizeof(Fint).
 */
#define FRAME_SIZE(lin,ele) ((int)sizeof(M0frameflags) + (int)ROUND_BYTES((lin)*(ele)))


/*  define certain UCwords which are used in video setup */

enum
{
	UC_SDEST_KEY                   = -31,
	UC_EDEST_KEY                   = -32,
	UC_DDEST_KEY                   = -33,

	UC_SDEST_FD                    = -35,
	UC_EDEST_FD                    = -36,
	UC_DDEST_FD                    = -37,

	UC_TWIN                        = -45,
	UC_TCOL                        = -46,


	UC_USER_INITIALS               = 2,
	UC_NAVFILE                     = 4,
	UC_MDFILE                      = 5,
	UC_GRIDFILE                    = 6,
	UC_FIXED_LINESIZE              = 11,
	UC_FIXED_ELESIZE               = 12,
	UC_NUMBER_OF_FRAMES            = 13,
	UC_NUMBER_OF_GRAPHICS          = 14,
	UC_VIDEO                       = 16,
	UC_G_KEY                       = 20,
	UC_Q_KEY                       = 21,
	UC_TERMINAL_TYPE               = 37,
	UC_GRA_WIDTH		       = 46,
	UC_GRA_DASH_LEN                = 47,
	UC_GRA_GAP_LEN                 = 48,
	UC_GRA_DASH_LEV                = 49,
	UC_LOOPING                     = 50,
	UC_CURRENT_FRAME               = 51,
	UC_LOOP_UPPER_BOUND            = 52,
	UC_LOOP_LOWER_BOUND            = 53,
	UC_FRAMES_MOVABLE              = 54,
	UC_FRAME_VISIBLE               = 55,
	UC_CURRENT_GRAPHIC             = 56,
	UC_GRAPHIC_VISIBLE             = 60,
	UC_CURSOR_VERT_SIZE            = 61,
	UC_CURSOR_HORIZ_SIZE           = 62,
	UC_CURSOR_LINE                 = 63,
	UC_CURSOR_ELEMENT              = 64,
	UC_CURSOR_TYPE                 = 65,
	UC_CURSOR_COLOR                = 66,

	UC_WINDCO_CURSOR               = 164,
	UC_WINDCO_SAMPLING             = 165,
	UC_WINDCO_ACTIVE               = 166,

	UC_COMM_CONNECT                = 177,
	UC_IGNORE_MOUSE_BUTTON_PRESS   = 178,
	UC_MOUSE_BUTTON_STATUS         = 185,

	UC_HOST_MODE                   = 189,

	UC_EXPORT_REQUEST_POINTER      = 192,
	UC_EXPORT_COMPLETE_POINTER     = 193,

	UC_IMPORT_COMPLETE_POINTER     = 197,
	UC_IMPORT_REQUEST_POINTER      = 199,

	UC_SHUTDOWN_SYSTEM             = 194,

	UC_PREVIOUS_TEXT_WINDOW        = 195,	/* used on EGA/VGA only */
	UC_COMM_PORT_NAME              = 198,
	UC_CURRENT_TEXT_WINDOW         = 200,
	UC_MOUSE_RAW_Y                 = 202,
	UC_MOUSE_RAW_X                 = 203,
	UC_LAST_FRAME_DISPLAYED        = 204,

	UC_ZOOM_TOGGLE                 = 300,

	UC_NUMBER_OF_TEXT_WINDOWS      = 400,
	UC_IGNORE_SIGNALS              = 455,
	UC_COMMAND_WINDOW_ID           = 456,
	UC_IMAGE_WINDOW_ID             = 457,

	UC_VOLNAM_OFFSET               = 465,

	UC_FONTFLAG                    = 470,

	UC_NUMBER_GRAPHICS             = 500,
	UC_INTERPOLATION_MODE          = 501,
	UC_IMAGE_WINDOW_RESIZE         = 502,
	UC_ZOOM_FACTOR                 = 503,
	UC_IMAGE_WINDOW_PROCESS        = 504,

	UC_NUMBER_IMAGE_COLORS         = 600,

	UC_TCP_LOADED                  = 700,
	UC_IMPORT_LOADED               = 701,

	/*
	 * We use UC_VIDEO_OFFSETS + a frame number, so there
	 * is no conflict with UC_IMAGE_WINDOW_RAISE.
	 */

	UC_IMAGE_WINDOW_RAISE          = 2000,
	UC_VIDEO_OFFSETS               = 2000,
	UC_FRAME_SIZES                 = 3000,
	UC_LOOP_CONTROL                = 4000,
	UC_DWELL_RATES                 = 5000,
	UC_OPPOSITE_SEQ                = 7000
};

/*
 * text colors
 *
 * The numbers actually come from OS/2, but now they are pervasive
 * throughout McIDAS:
 *
 * TCOLOR_FOREGROUND_WHITE		local text default
 * TCOLOR_FOREGROUND_BRIGHT_GREEN	echoed commands
 * TCOLOR_FOREGROUND_BRIGHT_MAGENTA	communications status (e.g. from IMPORT)
 * TCOLOR_FOREGROUND_BRIGHT_YELLOW	EDEST output
 * TCOLOR_FOREGROUND_BRIGHT_WHITE	mainframe text default
 */

enum
{
	TCOLOR_FOREGROUND_MASK		= 0x0F,

	TCOLOR_FOREGROUND_BLACK		= 0x00,
	TCOLOR_FOREGROUND_BLUE		= 0x01,
	TCOLOR_FOREGROUND_GREEN		= 0x02,
	TCOLOR_FOREGROUND_CYAN		= 0x03,
	TCOLOR_FOREGROUND_RED		= 0x04,
	TCOLOR_FOREGROUND_MAGENTA	= 0x05,
	TCOLOR_FOREGROUND_YELLOW	= 0x06,	/* so dim, it's olive */
	TCOLOR_FOREGROUND_WHITE		= 0x07,

	TCOLOR_FOREGROUND_BRIGHT_BLACK	= 0x08, /* grayish */
	TCOLOR_FOREGROUND_BRIGHT_BLUE	= 0x09,
	TCOLOR_FOREGROUND_BRIGHT_GREEN	= 0x0A,
	TCOLOR_FOREGROUND_BRIGHT_CYAN	= 0x0B,
	TCOLOR_FOREGROUND_BRIGHT_RED	= 0x0C,
	TCOLOR_FOREGROUND_BRIGHT_MAGENTA= 0x0D,
	TCOLOR_FOREGROUND_BRIGHT_YELLOW	= 0x0E,
	TCOLOR_FOREGROUND_BRIGHT_WHITE	= 0x0F,

	TCOLOR_BACKGROUND_MASK		= 0x70,

	TCOLOR_BACKGROUND_BLACK		= 0x00,
	TCOLOR_BACKGROUND_BLUE		= 0x10,
	TCOLOR_BACKGROUND_GREEN		= 0x20,
	TCOLOR_BACKGROUND_CYAN		= 0x30,
	TCOLOR_BACKGROUND_RED		= 0x40,
	TCOLOR_BACKGROUND_MAGENTA	= 0x50,
	TCOLOR_BACKGROUND_YELLOW	= 0x60,
	TCOLOR_BACKGROUND_WHITE		= 0x70
};

/* M0tcolor_to_ansi() - convert McIDAS text color value to
 * corresponding ANSI SGR attributes */
extern void
M0tcolor_to_ansi(int tcolor, int *in, int *fg, int *bg);

#define DEFAULT_LOOPRATE           6

#define DEFAULT_FRAME_LINES        480
#define DEFAULT_FRAME_ELEMS        640

#define MIN_ZOOM_FACTOR        2
#define MAX_ZOOM_FACTOR        6
#define DEFAULT_ZOOM_FACTOR    MIN_ZOOM_FACTOR


/*
 * CURSES is the number of color levels to reserve for cursor colors.
 */

#ifdef __EMX__
#define CURSES 6
#else
#define CURSES 1
#endif

/*
 * A frame object's data bytes always have 256 possible values.  The
 * first G of them, values [0..G-1] inclusive are used to indicate
 * non-imagery colors.  The rest are used for image grey levels.
 * Non-imagery colors are the background color, the cursor color,
 * and the user-definable graphics levels.
 *
 * The background color level is the 'color' for an erased frame, or
 * the color used when the frame has been moved partially off the
 * window.
 *
 * GRAPHICS is the default value of G, which is used for initalizing
 * UC, for the case where there is no mcimage.  The 1
 * is for the background color level, and the 8 is the default number
 * of user-definable graphics levels.  Note that 8 is also the
 * MAX_COLORS value for the XWIN TERMCHAR entry; it is hardcoded here
 * because it is needed too early in the initialization process to
 * be able to call ITRMCH().
 *
 * COLORS is the default number of entries in the color table.  The
 * color table has entries for the GRAPHICS colors, and also has one
 * entry for each grey level that mcimage uses.  By default, mcimage
 * uses 48 gray levels.  Note that 48 is also the GRAY_LEVELS value
 * for the XWIN TERMCHAR entry; it is hardcoded here because it is
 * needed too early in the initialization process to be able to call
 * ITRMCH().
 *
 * Note that mcimage may change MAX_COLORS and GRAY_LEVELS if it is
 * given appropriate options or if the limitations of the display
 * necessitate it.
 */

#define GRAPHICS (1+8+CURSES)
#define COLORS   (48+GRAPHICS)

/*  this accounts for the color levels reserved to the cursor */
#define GRAPHICS_LEVEL(x)  ( (x) <1 ? 0 : ( (x)+CURSES))


/* frame sizes macros */

#define FRAME_ELEMS(word)		((word)/65536)
#define FRAME_LINES(word)		((word)%65536)
#define FRAME_WORD(lines,elems)		((lines) + 65536*(elems))

#define LINE_SIZE(x) FRAME_LINES(Mcluc(UC_FRAME_SIZES+(x)))
#define ELEM_SIZE(x) FRAME_ELEMS(Mcluc(UC_FRAME_SIZES+(x)))

#ifndef PC_RESERVED
#define PC_RESERVED 0
#endif

/*
 * the purpose of these macros is so only one place has to know
 * the exqact way the color table is packed into an integer
 */
#define RGBF(f,r,g,b) (PC_RESERVED*(f)*16777216+(b)+256*(g)+65536*(r))
#define RGBB(x) ((x)%256)
#define RGBG(x) (((x)/256)%256)
#define RGBR(x) (((x)/65536)%256)

extern int m0neguc_fd;
extern Fint *m0posuc;
extern Fint *m0neguc;
extern int m0uc_offset;

/*-------------------------------------------------------------------*/
/*      McIDAS low-level memory and process control functions        */

extern int
M0varg(int argc, char **argv);  /* create McIDAS handle for argv */


extern char **
M0argv(int handle);         /* create argv from McIDAS command handle */

extern int
M0neguc(void);              /* create negative UC                     */

extern int
M0posuc_size(M0framereq *req, int extra); /* compute positive UC size */

extern int
M0posuc(M0framereq *req, int extra); /* create positive UC */

#ifndef __EMX__
/*
 * These are ifdef'd because there is no typedef for pid_t in EMX,
 * and the functions do not exist in OS2 anyway.
 */
extern void
M0zombie_add(pid_t kidpid);	/* add pid to list of child processes */

extern void
M0zombie_poll(void);		/* see if any children are dead */

extern pid_t
M0waitpid(pid_t pid, int *status, int options);
#endif

extern char **
M0keyin_argv(const char *command);	/* create argv for M0sync()/M0async() */

extern int
M0sync(const char *file, char **argv);   /* execute synchronous child       */

extern int
M0async(const char *file, char **argv, pid_t *pidval);  /* start asynchronous child        */

extern int
M0shmget(int size, int kind);      /* get key of shared memory or mmapped  */

extern char *
M0shmat(int token, int kind);      /* connect to memory or mmapped file */

extern int
M0shmctl(int token);               /* destroy shared memory  */


extern void
m0getred_(char *, FsLen);          /* fill redirection memory */

extern const char * const *
M0specfiles(void);		   /* list of per instance files */

extern int
M0makefiles(void);                 /* create per instance files */

extern void
M0delfiles(void);                  /* delete instance files */

typedef enum
{ M0MCINIT_INHERIT	/* inherit the parent UC if possible */
, M0MCINIT_RECURSE	/* ignore the parent UC and make a new one */
} M0mcinitkey;

/* low-level initializer, called by mcenv and Mcinit() */
extern int
M0mcinit(int argc, char **argv, M0framereq *f, int extra, M0mcinitkey key);

extern int
M0mctext(void);                    /* running under MCTEXT? */

/* is the buffer a command for the given keyin? */
extern int
M0iskeyin(const char *buf, const char *keyin);

/*------------------------------------------------------------*/
/*    FORTRAN calls needed to initialize data files           */
/*    and run commands                                        */

extern void
initrm_(char *, Fint *, FsLen);  /* create termchar file */

extern void
savsiz_(void);                   /* create alloc.www file */

extern void
getseg_(void);                   /* OS/2 memory initialization */

/* the following is NOT a const char *, since prescn_() modifies
 * its string argument */
extern void
prescn_(char *cmd, FsLen cmd_len);

extern void
ucrctx_(Fint *slot);		/* move saved uc context from file to uc*/

extern void
ucwctx_(Fint *slot);		/* save uc in file slot */

#endif  /* _M0GLUE_H  */
