/** COMMON BLOCKS TO BE USED WITH THE HEADER AND DATA FILES FOR THE DAP **
   <861105.1604>
     Revised by RLR to add integer declarations   <880225.1133> **
     Revised by RLR to add SAVE declarations      <880328.1300> **
     Revised by MDD to include SIGNAL flags       <891211.1700> **
*/ 
#define DAPDATA "/users/dap/data/"
#define NVARSZ 300
#define DNAMLEN 6
struct Daphead1{
	int bytelength;		/* length of record for f77 */
	char nmuser[16];	/* name of the user */
	char idatef[6];		/* date of last file access */
	char itimef[6];		/* time of last file access  */
	char notes[80];    	/* user note space  */
	int idated[3];		/* date data was taken  */
	int iproj;		/* project number */
	char iarcft[4];		/* aircraft number */
	int iflght;		/* flight number  */
	int nmrecs;		/* number of records in data file */
	int nwords;		/* number of words in a data file record  */
	int ideltt;		/* time between samples in the data file(msec)*/
	int ntmseg;		/* number of time segments in the data file */
	int nvar;		/* total number of variables  */
	int nvolt;		/* number of mode 1 (voltages) variables in the file  */
	char names[NVARSZ][DNAMLEN];	/* variable names  */
	char units[NVARSZ][4]; 	/* units of each variable */
	char qcx[DNAMLEN];	/* name of qc for computing tas,winds */
	char psx[DNAMLEN];	/* name of static press. for tas,winds  */
	char ttx[DNAMLEN];	/* name of temperature for tas,winds  */
	char avane[DNAMLEN];	/* name of attack vane for winds  */
	char bvane[DNAMLEN];	/* name of sideslip vane for winds  */
	char dpx[DNAMLEN];	/* name of dew point used for humidity  */
	int bytecheck;		/* length of record for f77 */
	};
  
struct Daphead2 {
/*  start time of segment #n in isegtm[n,1],[n,2],[n,3] 
    end time of segment #n in isegtm[n,4],[n,5],[n,6] 
*/
	int bytelength;		/* length of record for f77 */
	int itmseg[50][6];
	int bytecheck;		/* length of record for f77 */
	};

struct Dapdata {
	int ihr;		/* time of record hours */
	int imin;		/* time of record minutes */
	int isec;		/* time of record seconds */
	int imsec;		/* time of record millisecons */
	float values[NVARSZ];	/*values of each variable IEEE hp floating point */
	};

struct Dapmisc{
	char nmhead[DNAMLEN];	/* name of the header file Hxxxxx */
	char nmdata[DNAMLEN];	/* name of the data file Dxxxxx */
	short int iopnfi;	/* file is open, if TRUE */
	short int brkflg;	/* TRUE when ^c pressed */
	short int fpeflg;	/* TRUE when floating point error occurs */
	};
