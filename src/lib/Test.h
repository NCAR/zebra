/*
 * $Id: Test.h,v 2.1 1996-11-19 07:50:39 granger Exp $
 * 
 * Testing interface declarations
 */

#ifndef _zebra_Test_h_
#define _zebra_Test_h_

#include <defs.h>

/*
 * The Expect interface.
 */
void TX_Init FP ((void));
void TX_Closure FP ((void));
/* -- register expected messages -- */
int TX_Expect FP ((int mask, int suppress, char *re));
int TX_ExpectMany FP ((int mask, int suppress, int count, char *re));
int TX_Catch FP ((char *re));
int TX_Problem FP ((int count));
/* -- check that expected messages have cleared -- */
int TX_ClearAll FP ((int mask));
int TX_Clear FP ((int id, int mask));
int TX_Seen FP ((void));
int TX_Pending FP ((void));
#define TX_Caught() (TX_Seen())
#define EXPECT(n)	TX_Problem(n)
#define SEEN		TX_Seen()

/*
 * The Profile interface
 */
int TP_Push FP ((const char *name));	/* returns profile id */
int TP_Pop FP ((void));			/* pops and reports last profile */
int TP_Begin FP ((const char *name));	/* returns id */
int TP_End FP ((int id));		/* end and report on this id */
int TP_PopAll FP ((void));		/* pop all profiles off stack */
float TP_Elapsed FP ((int id));		/* return msecs since id began */ 
int TP_Count FP ((int id, int count));	/* add count repeats to this id */
int TP_Profile FP ((int enable));	/* nonzero to enable profiles */
int TP_ReportProfiles FP ((int mask));
int TP_ReportElapsed FP ((int id, const char *name));
	/* report elapsed stats for this id at the named intermediate point */

/*
 * Macros to allow conditional compilation of profiling routines
 */
#ifdef TP_PROFILE_NOCOMPILE
#define TP_Push(n) (0)
#define TP_Pop() (0)
#define TP_Begin(n) (0)
#define TP_End(n) (0)
#define TP_Elapsed(n) ((float)-1)
#define TP_Count(n) (-1)
#define TP_Profile(n) (0)
#define TP_ReportProfiles(n) (0)
#define TP_ReportElapsed(i,n) (0)
#endif

/*
 * #include <prof.h>
 */
#ifndef MARK
#define MARK(L) {}
#else
#undef MARK
#define MARK(L) {\
                asm("   .reserve        ."#L"., 4, \"data\", 4");\
                asm("M."#L":");\
                asm("   sethi   %hi(."#L".), %o0");\
                asm("   call    mcount");\
                asm("   or      %o0, %lo(."#L".), %o0");\
                }
#endif

#endif /* _zebra_Test_h_ */
