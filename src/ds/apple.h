
#include <defs.h>
#include <Test.h>

/*
 * Global variables to adjust the test output and behavior
 */
extern int DiffDataChunks;
/* Avoid a conflict with the same symbol in the ingest library. */
#define DumpDataChunks apple_DumpDataChunks
extern int DumpDataChunks;
extern int Debug;
extern int Verbose;
extern int Errors;
extern int NoBuffer;
extern float test_data[];
extern int NUM_PLATFORMS;
extern int NUM_TESTFIELDS;
extern int EF_STATS;
#define EF_TEST EF_DEBUG
#define NoPlatform (BadPlatform - 1)

#define T_DumpDC(dc) 	{ if (DumpDataChunks) dc_DumpDC (dc); }
#define T_DiffDC(dc1,dc2) { if (DiffDataChunks) dc_Diff ((dc1),(dc2)); }

typedef struct _TestField
{
	char *name;
	char *desc;
	char *units;
} TestField;

extern TestField TestFields[];

typedef struct TestPlatform {
	char *name;
	FileType ftype;
	DataOrganization org;
	int maxsamples;
	zbool mobile;
	PlatformId platid;
} TestPlatform;

extern struct TestPlatform TestPlatforms[];

/*
 * The testing interface.  The structure holds a pointer to the test routine,
 * which returns the number of errors encountered, the type of parameter
 * list to be passed, and the arguments themselves.  Each testing module
 * adds a TestRoutine structure to the symbol table under the name of the
 * test.
 */

typedef enum
{
	TR_BEGIN = 0,	/* (const ZebTime *zt) */
	TR_PLAT = 1	/* (const ZebTime *zt, const char *platform) */
} TestParms;

typedef struct _TestRoutine
{
	char *		tr_name;	/* name of test */
	FileType	tr_ftype;	/* filetype that gets tested */
	DataClass	tr_class;	/* DataChunk class that gets tested */
	int		tr_flags;	/* 2 low bits are parameter type */
	int (*tr_test)();		/* the test function itself */
	char *		tr_desc;	/* description of test */
	char *		tr_arg;		/* optional argument */
} TestRoutine;

#define TF_NeedsData  (1<<2)		/* local data prerequisite */
#define TF_NeedsDaemon (1<<3)		/* cannot run standalone */


DataChunk *T_ScalarNSpaceChunk FP((ZebTime *start, int nsample, int nfield,
				   int is_mobile, int addatts));
DataChunk *T_SimpleScalarChunk FP((ZebTime *start, int interval,
				   int nsample, int nfield, int is_mobile,
				   int addatts));
int T_MetUniform FP ((DataChunk *dc));
void T_MetDataStats FP ((void));
void T_ApplStats FP ((void));
void T_DumpData FP((float *retrieve, int n, int len, char *fname));
int T_CompareData FP((float *src1, float *src2, int size));
int T_CompareDataPrec(float *src1, float *src2, int size, float epsilon);

int T_VerifyObs FP((PlatformId pid, ZebTime *begin, ZebTime *end, int nsamp));
void T_ReceiveNotify FP((PlatformId pid, int param, ZebTime *when,
			 int nsample, UpdCode ucode));
int T_ExpectNotify FP((PlatformId pid, ZebTime *when, int nsamp, UpdCode));
int T_CatchNotify FP(());
void Announce FP ((const char *header));
PlatformId NewPlatform FP ((struct TestPlatform *tp));
PlatformId NeedPlatform FP ((const char *name));
PlatformId MakePlatform FP ((struct TestPlatform *tp));
PlatformId DefinePlatform FP ((struct TestPlatform *tp));
void CleanPlatform FP ((PlatformId pid));
int T_TrHints FP ((DataChunk *dc, int *ns, int *nsa, int *hns,
		   int *hss, int *no));
zbool T_Store FP ((DataChunk *dc, int newfile, dsDetail *details,
		  int ndetail));
DataChunk *T_FetchObs FP ((PlatformId pid, DataClass c, ZebTime *when,
			   FieldId *fields, int nfield, dsDetail *details,
			   int ndetail));
DataChunk *T_Fetch FP ((PlatformId pid, DataClass c, ZebTime *begin,
			ZebTime *when, FieldId *fields, int nfield, 
			dsDetail *details, int ndetail));
#define ds_Store(dc,nf,det,ndet) T_Store((dc),(nf),(det),(ndet))
#define ds_StoreBlocks(dc,nf,det,ndet) T_Store((dc),(nf),(det),(ndet))
#ifdef notdef
#define ds_Fetch(pid,c,begin,when,fields,nfield,det,ndet) \
	T_Fetch(pid,c,begin,when,fields,nfield,det,ndet)
#define ds_FetchObs(pid,c,when,fields,nfield,det,ndet) \
	T_FetchObs(pid,c,when,fields,nfield,det,ndet)
#endif
zbool TP_Store FP ((DataChunk *dc, int newfile, dsDetail *details,
		   int ndetail));
DataChunk *TP_FetchObs FP ((PlatformId pid, DataClass c, ZebTime *when,
			    FieldId *fields, int nfield, dsDetail *details,
			    int ndetail));
DataChunk *TP_Fetch FP ((PlatformId pid, DataClass c, ZebTime *begin,
			 ZebTime *when, FieldId *fields, int nfield, 
			 dsDetail *details, int ndetail));

/*
 * Test routine arrays of individual modules 
 */
#define END_TESTS { "" }

