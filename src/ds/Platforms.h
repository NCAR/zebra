
#include <config.h>	/* for CFG_FILEPATH_LEN */

/* Need "dsPrivate.h" for dsp_ClassStruct, PlatformClass, and so on */

/*
 * The routines for manipulating platform class and instance structures.
 */
void dt_InitDirectories FP((void));
void dt_InitClass FP((PlatformClass *pc, const char *name));
void dt_Subclass FP((PlatClassId superid, const PlatformClass *super, 
		     PlatformClass *sub, const char *name));
void dt_AddClassSubPlat FP((PlatformClass *pc, PlatClassId subplat, 
			    const char *name));
SubPlatform *dt_NewClassSubPlat FP((PlatformClass *pc));
void dt_CopyClassSubPlats FP((const PlatformClass *src, PlatformClass *dest));
void dt_EraseClassSubPlats FP((PlatformClass *pc));
void dt_FillClassDirs FP((PlatformClass *pc, const PlatformClass *super));
bool dt_ValidateClass FP((PlatformClass *pc));
void dt_CopyClass FP((PlatformClass *dest, const PlatformClass *src));
void dt_EraseClass FP((PlatformClass *pc));
void dt_SetComment FP((PlatformClass *pc, const char *comment));
int dt_ExtractClass FP((PlatformClass *pc, struct dsp_ClassStruct *dsp, 
			int len));
struct dsp_ClassStruct *dt_InjectClass FP((PlatformClass *pc, 
				   struct dsp_ClassStruct *am, int *len));
int dt_DecodeClass FP((FILE *fp, const PlatformClass *pc, 
		       const PlatformClass *spc));
int dt_FindSubplat FP((const PlatformClass *pc, SubPlatform *dps));

/* -- platform instance routines -- */
void dt_FillDirs FP((const PlatformClass *pc, const char *defname, char *dir,
		     char *rdir, const char *pdir, const char *prdir));
void dt_AddSubPlat FP((PlatformInstance *plat, PlatformId subid));

/* -- misc -- */
int dt_SetString FP((char *dest, const char *src, int maxlen, char *op));

/* -- data directories -- */
int dt_CreateDataDir FP ((const char *dir, const char *name, 
			  unsigned short *flags));
int dt_MakeDataDir FP ((const char *));

/* -- table searching -- */
/*
 * Used to pass search criteria and message structures to the
 * matching function during a platform table traverse.
 */
typedef struct dsp_PlatformSearch PlatformSearch;

struct SearchInfo {
	PlatformSearch *si_req;
	PlatformId *si_pids;
	int si_npids;
};

void ds_SearchPlatTable FP ((void *symbol_table, int (*function)(), 
			     PlatformSearch *req,
			     PlatformId *pids, int *npids));

/* ----------------
 * Global variables maintained and utilized in Platforms.c.  Global only
 * so that the Daemon can provide access to them for ui config files.
 */

/*
 * The default data directory.
 */
#define DDIR_LEN	CFG_FILEPATH_LEN

#define DefDataDir _ds_DefDataDir
#define RemDataDir _ds_RemDataDir
extern char DefDataDir[DDIR_LEN];
extern char RemDataDir[DDIR_LEN];

/*
 * If this flag is set, no remote directories will be accessed.
 */
#define DisableRemote _ds_DisableRemote
extern bool DisableRemote;

/*
 * The default keep period defined in Platforms.c
 */
#define DefaultKeep _ds_DefaultKeep
extern int DefaultKeep;

