/*
 * $Id: Platforms.h,v 3.8 2002-01-19 06:50:02 granger Exp $
 *
 * The interface to platform classes and instances shared by daemon
 * and client.  The application, be they daemon or client, can choose
 * either a local table or a client cache, and this interface does the
 * right thing transparently.
 */
#ifndef __zebra_Platforms_h_
#define __zebra_Platforms_h_

#include "DataStore.h"

# if __cplusplus
extern "C"
{
# endif

/*
 * How many platforms (both top-level and subplatforms) do we allow?  Even
 * though the daemon platform tables grow dynamically, we need an absolute
 * limit with which to dimension the client-side cache arrays.  This limit
 * is then enforced by the daemon.
 * 
 * Now that we have client-side caching of classes, this is now also the
 * limit on class definitions, though hopefully no project every comes
 * even remotely close to this.
 */
# define MAXPLAT	CFG_MAX_PLATFORMS

/*
 * Public access to table parameters, so they can be configured by the daemon.
 */
extern int PTableSize;		/* Platform table parameters 	*/
extern int PTableGrow;

extern int CTableSize;		/* Class table initial size	*/
extern int CTableGrow;		/* Amount to grow by		*/

/*
 * Structure for describing a subplatform 'template', where the class info
 * comes from the subplatform's class.  An instance name is also required
 * to create instances of classes which reference this subplatform.
 */
typedef struct ds_SubPlatform
{
	int	dps_class;		/* Class of this subplatform	*/
	char	dps_name[CFG_PLATNAME_LEN];	/* Name for instances	*/
} SubPlatform;

/*
 * Information that is common among platform instances and does not
 * change during run-time resides in the platform class structure.
 */
typedef struct ds_PlatformClass
{
	int	dpc_id;			/* Class id			*/
	char	dpc_name[CFG_PLATNAME_LEN];	/* name of this class	*/
	int	dpc_superclass;		/* Class hierarchy backpointer	*/
	DataOrganization dpc_org;	/* Native data organization	*/
	FileType dpc_ftype;		/* Default file type		*/
	unsigned int dpc_maxsamp;	/* Maximum file samples		*/
	unsigned int dpc_splitseconds;  /* Seconds to split files at    */
	unsigned short dpc_keep;	/* Minimum data keep		*/
	unsigned short dpc_flags;	/* Attribute flags -- see below	*/
	char 	*dpc_comment;		/* Comment about this class	*/
	SubPlatform *dpc_subplats;	/* Subplatform templates	*/
	int	dpc_nsubplats;		/* Number of subplats in array	*/
/*
 * Additional field definitions for the platform.
 */
        FieldId *dpc_fields;
	int     dpc_nfields;
/*
 * Platform class derivations.
 */
        char    *dpc_derivations;
/*
 * Info for directory suggestions
 */
	char	dpc_dir[CFG_FILEPATH_LEN];/* Source-relative or absolute*/
	InheritDirFlag dpc_inherit;	/* Directory inheritance flags	*/
	InstanceDirFlag dpc_instance;	/* Directory instance flags	*/
} PlatformClass;

/*
 * Default value for the dpc_keep member above
 */
extern int DefaultKeep;

/*
 * The platform instance structure.  Most of the static information is
 * retrieved by following the pointer to the class structure in the class
 * table.  The instance structure has its own members for stuff that is
 * likely to be different among instances of a class.  This structure is
 * shared between client and daemon, so daemon information is attached
 * in a "derived" structure.  This is the information which can be derived
 * by a class instantiation and thus is needed by both client and daemon.
 * For now, the subplats list is not sent between client and daemon and
 * is only valid where instantiated.
 */
typedef struct ds_PlatformInstance
{
	int	dp_id;			/* Platform instance id 	*/
	char	dp_name[CFG_PLATNAME_LEN];/* Full name of this platform	*/
	PlatClassId dp_class;		/* Class id of the platform	*/
	PlatformId dp_parent;		/* Hierarchy backpointer	*/
	unsigned short dp_flags;	/* Attribute flags		*/
	int	*dp_subplats;		/* Indices to subplat instances	*/
	int	dp_nsubplats;		/* Number of indices (not alloc)*/
} PlatformInstance;

typedef PlatformInstance Platform;

/*
 * These flags belong to the class
 */

# define DPF_MOBILE	0x0001		/* Does this platform move?	*/
# define DPF_COMPOSITE	0x0002		/* A grouping of other plats?	*/
# define DPF_DISCRETE	0x0004		/* "Continuous" data?		*/
# define DPF_REGULAR	0x0008		/* Regularly-spaced (time) samples? */
# define DPF_SUBPLATFORM 0x010		/* This is a sub platform	*/
# define DPF_REMOTE	0x0020		/* A remote dir has been given	*/
# define DPF_MODEL	0x0080		/* Model data, i.e., has	*/
					/* separate issue/valid times	*/
# define DPF_VIRTUAL	0x0100		/* Only a node in the hierarchy */
# define DPF_ABSTRACT	0x0200		/* Abstract platform class	*/

/* =====================================================================
 * This is the internal interface for looking up and defining classes and
 * structures on both the daemon and client side.  Defines prototypes
 * for routines shared between the p_ and Platforms modules.
 */

# define INLINE static inline

/*
 * A bunch of inline boolean tests for class flags, which happen to be
 * copied into the instances also and thus can ge tested there.
 * This may change.
 */
INLINE int pi_Subplatform (const PlatformInstance *pi)
{ return (pi->dp_flags & DPF_SUBPLATFORM); }

INLINE int pi_Mobile (const PlatformInstance *pi)
{ return (pi->dp_flags & DPF_MOBILE); }

INLINE int pi_Composite (const PlatformInstance *pi)
{ return (pi->dp_flags & DPF_COMPOSITE); }

INLINE int pi_Regular (const PlatformInstance *pi)
{ return (pi->dp_flags & DPF_REGULAR); }

INLINE int pi_Remote (const PlatformInstance *pi)
{ return (pi->dp_flags & DPF_REMOTE); }

int pi_Daysplit (const PlatformInstance *pi);

INLINE int pi_Model (const PlatformInstance *pi)
{ return (pi->dp_flags & DPF_MODEL); }

INLINE int pi_Virtual (const PlatformInstance *pi)
{ return (pi->dp_flags & DPF_VIRTUAL); }

/*
 * Access for other platform instance members
 */
INLINE const char *pi_Name (const PlatformInstance *pi)
{ return (pi->dp_name); }

INLINE PlatformId pi_Id (const PlatformInstance *pi)
{ return (pi->dp_id); }


INLINE PlatClassId pi_ClassId (const PlatformInstance *pi)
{ return (pi->dp_class); }

INLINE PlatformId pi_ParentId (const PlatformInstance *pi)
{ return (pi->dp_parent); }

/*
 * And a method to return the suggested subdirectory for this platform
 */
const char* pi_SuggestedDir (const PlatformInstance *pi);

/*
 * Return non-zero if the platform instance is a member of the given class
 * or a member of a subclass of the given class.
 */
int pi_IsSubclass (const PlatformInstance *pi, const PlatformClass *spc);

/*
 * Easy access to class members
 */
INLINE PlatClassId pc_Id (const PlatformClass *pc)
{ return (pc->dpc_id); }

INLINE const char *pc_Name (const PlatformClass *pc)
{ return (pc->dpc_name); }

INLINE DataOrganization pc_DataOrg (const PlatformClass *pc)
{ return (pc->dpc_org); }

INLINE FileType pc_FileType (const PlatformClass *pc)
{ return (pc->dpc_ftype); }

INLINE unsigned short pc_Keep (const PlatformClass *pc)
{ return (pc->dpc_keep); }

INLINE unsigned int pc_MaxSamp (const PlatformClass *pc)
{ return (pc->dpc_maxsamp); }

INLINE PlatClassId pc_SuperClassId (const PlatformClass *pc)
{ return (pc->dpc_superclass); }

INLINE const char* pc_SuggestedDir (const PlatformClass *pc)
{ return (pc->dpc_dir); }

INLINE InheritDirFlag pc_InheritDirFlag (const PlatformClass *pc)
{ return (pc->dpc_inherit); }

INLINE InstanceDirFlag pc_InstanceDirFlag (const PlatformClass *pc)
{ return (pc->dpc_instance); }


/* ---------------- p_Table.c ---------------- */
/*
 * This module maintains the internal table of known/cached/defined
 * platforms and structures.
 *
 * There are two mappings, one from id to structure,
 * the other from name to structure.  If you just want id to name and vice
 * versa you have to get them from the returned pointer.  These functions
 * return NULL if the platform or class could not be found, either in the
 * cache or through asking the daemon (if applicable).
 */

/*
 * Platform search lists and message typedefs
 */
typedef struct _PlatformList
{
	PlatformId *pl_pids;
	int pl_npids;
}
PlatformList;

/*
 * The structure actually gets defined in dsPrivate.h, since
 * it's part of the protocol.  We only need it as a forward
 * reference since the interface here only declares pointers to it.
 */
typedef struct dsp_PlatformSearch PlatformSearch;

/*
 * Define the set of methods which the table module can call when in 
 * client mode.
 */
typedef struct _PlatformMethods
{
	Platform *(*PlatStruct)(int id, const char *name);
	PlatformClass *(*ClassStruct)(int id, const char *name);
	PlatformId (*DefinePlatform)(int cid, const char *name, int parent);
	PlatClassId (*DefineClass)(PlatClassRef pc);
	void (*SendSearch)(PlatformSearch *, PlatformList *pl);
	int (*GetNPlat)();
} 
PlatformMethods;

/* -- initialize -- */
void dt_SetMethods (PlatformMethods *);

/* -- retrieval -- */
const PlatformClass *dt_FindClassName (const char *name);
const PlatformClass *dt_FindClass (PlatClassId id);
const Platform *dt_FindPlatformName (const char *name);
const Platform *dt_FindPlatform (PlatformId pid);

/* -- definition -- */
PlatClassId dt_DefineClass (PlatClassRef pc);
PlatformId dt_DefineSubPlatform (PlatClassId cid, const char *name,
				 PlatformId parent);

/* -- searching -- */

void dt_GetPlatformList (PlatformSearch *search, PlatformList *pl);
struct dsp_PlatformList *dt_AnswerSearch (PlatformSearch *req, 
					  int *npids, int *len);

/* -- other info -- */
int dt_NPlatform (void);
int dt_NClass (void);

/* ---------------- more inline ---------------- */
/*
 * More inline routines which translate members to pointers
 * using the table routines prototyped above.
 */

INLINE const PlatformClass *pi_Class (const PlatformInstance *pi)
{ return (dt_FindClass (pi_ClassId (pi))); }

INLINE const PlatformClass *pc_SuperClass (const PlatformClass *pc)
{ return (dt_FindClass (pc_SuperClassId (pc))); }

INLINE const PlatformInstance *pi_Parent (const PlatformInstance *pi)
{ return (dt_FindPlatform (pi_ParentId (pi))); }

/*
 * Access to class info through an instance structure.
 * Note these will fail miserably if the platform's class
 * structure cannot be found for some reason.
 */
INLINE DataOrganization pi_DataOrg (const PlatformInstance *pi)
{ return (pi_Class(pi)->dpc_org); }

INLINE FileType pi_FileType (const PlatformInstance *pi)
{ return (pi_Class(pi)->dpc_ftype); }

INLINE unsigned short pi_Keep (const PlatformInstance *pi)
{ return (pi_Class(pi)->dpc_keep); }

INLINE unsigned int pi_MaxSamp (const PlatformInstance *pi)
{ return (pi_Class(pi)->dpc_maxsamp); }

INLINE const char* pi_ClassDir (const PlatformInstance *pi)
{ return (pi_Class(pi)->dpc_dir); }

/* ---------------- p_Client.c ---------------- */
/*
 * This module contains the routines which communicate platform
 * information with the daemon.  There is only one public routine:
 * the one which turns on client behavior rather than local (standalone)
 * behavior.
 */
void dt_PlatformClientMode ();

/* ---------------- p_Appl.c ---------------- */
/*
 * The public application layer for defining and accessing platform class
 * and instance structures independent of underlying table implementation
 * or behavior.  This interface is built on top of the functionality of
 * p_Table.c and Platforms.c with no knowledge of p_Client.c.  All of its
 * prototypes are in DataStore.h, and none of those prototypes rely on
 * definitions in this header file.
 */


/* ---------------- Platforms.c ---------------- */
/*
 * The internal interface for manipulating platform classes and related
 * convenvience routines.
 */

void dt_ExtendPlatforms (int size, void (*init)(Platform *), 
			 void (*destroy)(Platform *));
PlatClassRef dt_Subclass (const PlatformClass *super, const char *name);
void dt_AddClassSubPlat (PlatformClass *pc, PlatClassId subplat,
			 const char *name);
void dt_CopyClassSubPlats (const PlatformClass *src, PlatformClass *dest);
void dt_EraseClassSubPlats (PlatformClass *pc);

void dt_AddClassField (PlatformClass *pc, FieldId fid);
void dt_CopyClassFields (const PlatformClass *src, PlatformClass *dest);
void dt_EraseClassFields (PlatformClass *pc);
void dt_AddClassDerivation (PlatformClass *pc, char *dtext);
void dt_SetDerivations (PlatformClass *pc, const char *dtext);

void dt_FillClassDir (PlatformClass *pc, const PlatformClass *super);
zbool dt_ValidateClass (PlatformClass *pc);
void dt_CopyClass (PlatformClass *dest, const PlatformClass *src);
void dt_EraseClass (PlatformClass *pc);
void dt_SetComment (PlatformClass *pc, const char *comment);
void dt_FreePlatform (PlatformInstance *pi);
void dt_DestroyClass (PlatClassRef pc);

struct dsp_ClassStruct;	/* actual struct defined in dsPrivate.h */
int dt_ExtractClass (PlatformClass *pc, struct dsp_ClassStruct *dsp, 
		     int len);
struct dsp_ClassStruct *dt_InjectClass (const PlatformClass *pc, 
					struct dsp_ClassStruct *am, int *len);
int dt_DecodeClass (FILE *fp, const PlatformClass *pc, 
		    const PlatformClass *spc);

/* -- platform instance routines -- */
PlatformInstance *dt_NewPlatform (const char *name);
void dt_AddSubPlat (PlatformInstance *plat, PlatformId subid);

/* -- misc -- */
int dt_SetString (char *dest, const char *src, int maxlen, char *op);

# if __cplusplus
}	// end extern "C"
# endif

# undef INLINE	/* other headers want to define this their own way... */
    
#endif /* ndef __zebra_Platforms_h_ */

