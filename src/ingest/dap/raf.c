/* c----------------------------------------------------------------------- */
# include "dap_file.h"
# include <time.h>
# include <sys/time.h>

typedef struct {
    int next;
    double dap_time;
    float alat;
    float alon;
    float palt;
    float hgme;
    float gsf;
    float pitch;
    float roll;
    float thdg;
    float vew;
    float vns;
    float wp3;
    float ui;
    float vi;
    float wi;
} SAVED_DAP_INFO;

typedef float *FLOAT_PTR;

# define BLOCK_SIZE 65536
# define MATCH 0
# define ONE_DAY 86400
# define MAX_VARS 555
# define NAME_LEN DNAMLEN
# define DAP_NAMES_OFFSET 158
# define DAP_NUM_VAR_OFFSET 156
# define END_DAP -1
# define MOVE_UF -2
# define MAX_UF_GATES 2048
# define NULL 0
# define YES 1
# define NO 0

char *DAP_header_name=NULL;
char *DAP_data_name=NULL;
SAVED_DAP_INFO *sdi=NULL;

int num_dap_vars;
int Last_sdi=0, Next_sdi=0;
int fid_dap, fid_out;
int dap_rec_len;
short Year=-1, Month, Day;
float ray_time_adjust=0;

FLOAT_PTR hr,min,sec,alat,alon,palt,hgme,gsf,pitch,roll,thdg,vew,vns,wp3;
FLOAT_PTR ui, vi, wi;

struct Daphead1 *DAP_hed;
struct Dapdata *DAP_struct;

/* c----------------------------------------------------------------------- */

main( argc, argv )
  int argc;
  char *argv[];
{
    long int i, count=0;
    char str[88], *ctime();
    double fmod(), atan2(), sqrt(), d, ang, spd;


    if( init_it() == NO )
	  exit(1);;

    for(;;) {
	if( next_dap_rec() == END_DAP )	
	      return(NO);
	if(!(count % 300)) {
	    ang = atan2((double)sdi->vi, (double)sdi->ui);
	    ang = fmod((double)(ang+360.), (double)360.);
	    spd = sqrt((double)(sdi->ui*sdi->ui +sdi->vi*sdi->vi));
	    i = sdi->dap_time;
	    printf("%6.3f %7.3f %5.0f %3.0f %3.0f %4.1f  %s"
		   , sdi->alat
		   , sdi->alon
		   , sdi->palt
		   , sdi->thdg
		   , ang
		   , spd
		   , ctime(&i)
		   );
	}
	count++;
    }

    close( fid_dap);
}
/* c----------------------------------------------------------------------- */

float *dap_var_search( s )
      char *s;
{
    int i, j=strlen(s);
    char a[NAME_LEN];

    for(i=0; i < NAME_LEN; i++ )
	  a[i] = ' ';
    for(i=0; i < j; i++ )
	  a[i] = *s++;

    for(i=0; i < num_dap_vars; i++ )
	if( strncmp( a, DAP_hed->names[i], NAME_LEN ) == 0 )
	      return( &DAP_struct->values[i] );
    return( NULL );
}
/* c----------------------------------------------------------------------- */

grab_dap_info(ndx)
  int ndx;
{
    time_t i=0;
    double d;
    struct tm tm;

    tm.tm_wday = 0;
    tm.tm_yday = 0;
    tm.tm_isdst = 0;
    tm.tm_zone = "GMT";
    tm.tm_gmtoff = 0;

    tm.tm_sec = DAP_struct->isec;
    tm.tm_min = DAP_struct->imin;
    tm.tm_hour = DAP_struct->ihr;
    tm.tm_mday = DAP_hed->idated[2];
    tm.tm_mon = DAP_hed->idated[1]-1;
    tm.tm_year = DAP_hed->idated[0];
    i = timegm(&tm);

    sdi->dap_time = (double)(.001*DAP_struct->imsec +i);

    sdi->alat = *alat;    
    sdi->alon = *alon;    
    sdi->palt = *palt;    
    sdi->hgme = *hgme;    
    sdi->gsf = *gsf;    
    sdi->pitch = *pitch;
    sdi->roll= *roll;    
    sdi->thdg = *thdg;    
    sdi->vew = *vew;    
    sdi->vns = *vns;    
    sdi->wp3 = *wp3;      
    sdi->ui = *ui;      
    sdi->vi = *vi;      
    sdi->wi = *wi;      
# ifdef obsolete
# endif
}
/* c----------------------------------------------------------------------- */

init_it()
{
    char *getenv();
    int i, j, n, in, all_ok=YES, all_found=YES;
    char *a, str[256];
    float *f, *dap_var_search();
    short yy, mon, dd, hh, mm, ss, ms;


    DAP_hed = (struct Daphead1 *)malloc(BLOCK_SIZE);
    DAP_struct = (struct Dapdata *)malloc(BLOCK_SIZE);
    /*
     * set up a que for DAP info to aid synchronization
     */
    sdi = (SAVED_DAP_INFO *)malloc(sizeof(SAVED_DAP_INFO));

    DAP_header_name = "/raf/H817R1";
    if( a = getenv( "DAP_HEADER" )) {
	DAP_header_name = a;
    }

    if( DAP_header_name ) {
	if((in = open( DAP_header_name, 0)) < 0 ) {
	    printf( "Unable to open %s\n", DAP_header_name );
	    return(NO);
	}
    }
    else {
	printf("Unable to establish DAP header name!\n");
	return(NO);
    }
    if((n = read( in, (char *)DAP_hed, BLOCK_SIZE )) <= 0 ) {
	printf("Bad read on DAP header...status=%d\n", n );
	return(NO);
    }
    close( in );
    
    num_dap_vars = DAP_hed->nvar;
    dap_rec_len = DAP_hed->nwords*sizeof(int);

    for(str[6]='\0',i=0,j=0; i < num_dap_vars; i+=10) {
	for(j=i; j < i+10 && j < num_dap_vars; j++ ) {
	    strncpy(str, DAP_hed->names[j], 6);
	      printf("%s ", str);
	}
	printf("\n");
    }


    all_found = (alat = dap_var_search( "ALAT" )) != NULL ? all_found : NO;
    all_found = (alon = dap_var_search( "ALON" )) != NULL ? all_found : NO;
    all_found = (palt = dap_var_search( "PALT" )) != NULL ? all_found : NO;
    all_found = (hgme = dap_var_search( "HGME" )) != NULL ? all_found : NO;
    all_found = (gsf = dap_var_search( "GSF" )) != NULL ? all_found : NO;
    all_found = (pitch = dap_var_search( "PITCH" )) != NULL ? all_found : NO;
    all_found = (roll = dap_var_search( "ROLL" )) != NULL ? all_found : NO;
    all_found = (thdg = dap_var_search( "THDG" )) != NULL ? all_found : NO;
    all_found = (vew = dap_var_search( "VEW" )) != NULL ? all_found : NO;
    all_found = (vns = dap_var_search( "VNS" )) != NULL ? all_found : NO;
    all_found = (wp3 = dap_var_search( "WP3" )) != NULL ? all_found : NO;
    all_found = (ui = dap_var_search( "UI" )) != NULL ? all_found : NO;
    all_found = (vi = dap_var_search( "VI" )) != NULL ? all_found : NO;
    all_found = (wi = dap_var_search( "WI" )) != NULL ? all_found : NO;
# ifdef obsolete
# endif
    if( !all_found ) {
	printf("Not all required DAP varibles could be found\n");
	return(NO);
    }
    
    DAP_data_name = "/raf/D817R1";

    if( a = getenv( "DAP_DATA" )) {
	DAP_data_name = a;
    }

    if( DAP_data_name ) {
	if((fid_dap = open( DAP_data_name, 0)) < 0 ) {
	    printf( "Unable to open %s\n", DAP_data_name );
	    return(NO);
	}
    }
    else {
	printf("Unable to establish DAP data name!\n");
	return(NO);
    }
}
/* c----------------------------------------------------------------------- */

next_dap_rec()
{
    int i;
    static int rec_count=0;

    /* read the next record */
    if((i=read( fid_dap, DAP_struct, dap_rec_len )) < dap_rec_len ) {
	printf("End of DAP data after record %d  %d\n", rec_count, i );
	return( END_DAP );
    }
    rec_count++;
    grab_dap_info(i);
    return(YES);
}
/* c----------------------------------------------------------------------- */

/* c----------------------------------------------------------------------- */

/* c----------------------------------------------------------------------- */



