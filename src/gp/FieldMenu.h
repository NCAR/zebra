/* $Id: FieldMenu.h,v 2.2 2001-11-30 21:29:28 granger Exp $ */
		
/* maximum number of extra entries */
# define FM_MAXEXTRA 20

/* maximum possible entries in any field menu or field widget */
# define FM_MAXENTRY (MAXFIELD+FM_MAXEXTRA+1)

/*
 * Stuff for storing the context when a field menu is activated.
 */
typedef struct struct_fm_Context
{
    FieldId fields[MAXFIELD];
    int nfield;
    char platform[PlatformListLen];	/* Platform of interest	*/
    char extras[512];  /* the extras string */
    char *pextras[FM_MAXEXTRA]; /* pointers to the parsed extra entries */
    int nextra;
    char icomp[256];   /* icon_component */
    char area_type[32];
    char icon_platform[PlatformListLen];
    char fsc[256];     /* field-select-command */
    char title[256];   /* optional more descriptive title for the menu */
    char fparam[256];  /* field parameter we're changing, if known */
    char fcurrent[256]; /* current field setting */
    void *fentry;     /* widget entry for current field */
} fm_Context;


struct fm_Callback
{
    FieldId fid;
    char *extra;	/* non-null if supercedes field id */
    void *refdata;
    int i;
    fm_Context *fm_context;
};


void fm_Init ();
int fm_GetContext (fm_Context *fmc, char *spec);
void fm_StoreContext (fm_Context *fmc);
/**
 * Check if @p spec specifies a FieldMenu.  If so, and if @p title
 * is non-null, copy the menu title into @p title and return the
 * menu name; otherwise return null.
 **/
char *fm_CheckFieldMenu (char *spec, char *title);
char *fm_SetupFieldMenu (char *spec);
int fm_NumEntries (fm_Context *fmc);
Widget fm_CreateFieldChooser (fm_Context *fmc, Widget parent, 
			      XtCallbackProc callback, void *refdata,
			      struct fm_Callback *callbacks);
void fm_SelectField (fm_Context *fmc, char *field);
void fm_SelectEntry (fm_Context *fmc, Widget w);
void fs_Create (struct ui_command *cmds);
void fs_CreateWithContext (fm_Context *fmc);
