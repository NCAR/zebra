/* $Id: zl_symbol.h,v 2.2 1998-10-28 21:22:50 corbet Exp $ */
/*
 * Global declarations for the symbol table module.
 */
# ifndef UI_SYMBOL_TABLE_SYMBOLS
# ifndef ZL_SYMBOL_TABLE_SYMBOLS
# define ZL_SYMBOL_TABLE_SYMBOLS

# include "defs.h"
# include "zl_param.h"

/*
 * The symbol union definitions are so pervasive that they have been moved
 * to zl_param.h, which will be included by defs.h, so that applications
 * which do not use symbol tables can still use the union (e.g. dsDetails).
 * Note that including ui.h before defs.h will still circumvent all
 * definitions in both zl_symbol.h and zl_param.h, and thereby avoid
 * duplicate definitions of the symbol union.
 */

/*
 * Symbol operations.
 */
# define SOP_READ	0x1	/* Read operation		*/
# define SOP_WRITE	0x2	/* Write operation		*/
# define SOP_DESTROY	0x4	/* Destroy			*/

/*
 * The (external) type for a symbol table.
 */
typedef char *stbl;

/*
 * Zebra library symbol table routines 
 */
# ifdef __cplusplus
int zl_traverse (const stbl, 
   int (*)(const char *, int, const SValue *, long), long, int);
int zl_search (const stbl, 
   int (*)(const char *, int, const SValue *, long), long, int, char *);
# else
int zl_traverse FP ((const stbl, int (*)(), long, int));
int zl_search FP ((const stbl, int (*)(), long, int, char *));
# endif /* __cplusplus */

void zl_usy_init FP ((void));
stbl zl_c_stbl FP ((const char *));
void zl_z_stbl FP ((stbl));
void zl_z_symbol FP ((stbl, const char *));
int zl_g_symbol FP ((const stbl, const char *, int *, SValue *));
zbool zl_usy_defined FP ((const stbl, const char *));
void zl_s_symbol FP ((stbl, const char *, int, const SValue *));
stbl zl_g_stbl FP ((const char *));
void zl_c_indirect FP ((stbl, const char *, const void *, int, int));
#ifdef notdef
void zl_dump_table FP ((const stbl));
int zl_usy_daemon FP ((stbl, const char *, int, int (*)(), char *));
int zl_z_daemon FP ((stbl, const char *, int, int (*)(), char *));
#endif

/* defines to distinguish zebra library routines from UI library */
#define usy_init zl_usy_init
#define usy_c_stbl(s) zl_c_stbl(s)
#define usy_z_stbl(stbl) zl_z_stbl(stbl)
#define usy_z_symbol(stbl,s) zl_z_symbol(stbl,s)
#define usy_g_symbol(stbl,s,i,sv) zl_g_symbol(stbl,s,i,sv)
#define usy_defined(stbl,s) zl_usy_defined(stbl,s)
#define usy_s_symbol(stbl,s,i,sv) zl_s_symbol(stbl,s,i,sv)
#define usy_dump_table(stbl) zl_dump_table(stbl)
#define usy_g_stbl(stbl) zl_g_stbl(stbl)
#define usy_c_indirect(stbl,s,a,i,j) zl_c_indirect(stbl,s,a,i,j)
#define usy_daemon(stbl,s,i,fn,a) zl_usy_daemon(stbl,s,i,fn,a)
#define usy_z_daemon(stbl,s,i,fn,a) zl_z_daemon(stbl,s,i,fn,a)
#define usy_traverse(stbl,fn,l,i) zl_traverse(stbl,fn,l,i)
#define usy_search(stbl,fn,l,i,a) zl_search(stbl,fn,l,i,a)

/*
 * zl_string routines
 */
char *zl_pstring FP((const char *s));
char *zl_string FP((const char *s));
void zl_rel_string FP((const char *s));
char *zl_zapcase FP((char *s));

# define usy_pstring(s) zl_pstring(s)
# define usy_string(s) zl_string(s)
# define usy_rel_string(s) zl_rel_string(s)

# endif /* ZL_SYMBOL_TABLE_SYMBOLS */
# endif /* UI_SYMBOL_TABLE_SYMBOLS */

