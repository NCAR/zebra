/******************************************************************************
 * TEST_SERVER.C Test program for the data servers 
 */

#include <stdio.h>
#include <signal.h>
#include "cdata_server.h"

#define WRITE_BUF_SIZE 1024
#define READ_BUF_SIZE 262144
#define DIVISOR 8192

/************** GLOBAL DATA *****************************/

char host_name[64];
int port,server_id;
int protofd,sockfd;
int verbose_mode;

cd_command_t com;
cd_reply_t reply;
cd_grid_info_t info;

unsigned char * get_cidd_data();

typedef struct {
  long year,month,day,hour,min,sec;
  long unix_time;
} date_time_t;

/******************************************************************************
 * MAIN : Open file and send the output to STDOUT
 *
 */

main(argc,argv)

     int argc;
     char *argv[];

{

  int i,j;
  int type,seqno,nbytes;
  int num_fields;
  unsigned char *buf,*ptr; 
  char string[128];
  double field[6];
  FILE *infile;
  
  process_args(argc,argv); /* process command line arguments */
  
  init_sockets(); /* Set up the TCP/IP socket connections */
  
  while(1) {
    
    printf("Enter Field #, Low Height, High Height (km): ");
    
    fgets(string,128,stdin);
    
    numfields = freeform(string,128,field);
    
    if(num_fields != 3) exit(0);
    
    /* Set up the command request */

    com.primary_com = GET_INFO | GET_MOST_RECENT | GET_PLANE_HEIGHTS;
    com.second_com = GET_V_PLANE;
    
    com.min_x = -1000.0;
    com.max_x = 1000.0;
    com.min_y = -1000.0;
    com.max_y = 1000.0;
    
    com.min_z = 0.0;
    com.max_z = 20.0;
    
/*    com.min_z = field[1];
    com.max_z = field[2]; */
    
    com.data_field = field[0];
    com.data_type = CHAR;
    
    com.time_min = time(0);
    com.time_max = time(0);
    
    buf = get_cidd_data(&com,&reply,&info,host_name,port);
    
    print_reply(&reply,stderr);
    
    print_info(&info,stderr);
    
    if(buf != NULL ) {

      if(verbose_mode == 1) {
	
	ptr = buf;

	for(i=0; i < reply.nx; i++ ) {
	  printf("\n");
	  for(j=0;j < reply.ny; j++) {
	    printf("%3u ",(unsigned int) *ptr++);
	  }
	}
	printf("\n");
      }
      free(buf);

    } else {

      printf("\n*** No data Returned! ***\n");

    }

  }

}

/*****************************************************************
 * PRINT_INFO: Produce diagnostic output on the info 
 */

print_info(info,file)
     cd_grid_info_t *info;
     FILE *file;
{
  
  fprintf(file,
	  "\n******************** GRID INFO **********************\n");

  fprintf(file,"data_field: %d\t",info->data_field);
  fprintf(file,"projection: %d\n",info->projection);
  fprintf(file,"source_x: %g\t",info->source_x);
  fprintf(file,"source_y: %g\t",info->source_y);
  fprintf(file,"source_z: %g\n",info->source_z);
  fprintf(file,"nx: %d\t",info->nx);
  fprintf(file,"ny: %d\t",info->ny);
  fprintf(file,"nz: %d\n",info->nz);
  fprintf(file,"num_gates: %d\t",info->num_gates);
  fprintf(file,"max_gates: %d\t",info->max_gates);
  fprintf(file,"min_gates: %d\n",info->min_gates);
  fprintf(file,"num_tilts: %d\t",info->num_tilts);
  fprintf(file,"data_length: %d\n",info->data_length);
  
  fprintf(file,"\nlat_origin: %g\t",(info->lat_origin));
  fprintf(file,"lon_origin: %g\t",(info->lon_origin));
  fprintf(file,"ht_origin: %g\n",(info->ht_origin));
  fprintf(file,"dx: %g\t",(info->dx));
  fprintf(file,"min_x: %g\t",(info->min_x));
  fprintf(file,"max_x: %g\n",(info->max_x));
  fprintf(file,"dy: %g\t",(info->dy));
  fprintf(file,"min_y: %g\t",(info->min_y));
  fprintf(file,"max_y: %g\n",(info->max_y));
  fprintf(file,"dz: %g\t",(info->dz));
  fprintf(file,"min_z: %g\t",(info->min_z));
  fprintf(file,"max_z: %g\n",(info->max_z));
  fprintf(file,"north_angle: %g\t",(info->north_angle));
  fprintf(file,"gate_spacing: %g\t",(info->gate_spacing));
  fprintf(file,"wavelength: %g\t",(info->wavelength));
  fprintf(file,"frequency: %g\n",(info->frequency));
  fprintf(file,"min_range: %g\t",(info->min_range));
  fprintf(file,"max_range: %g\n",(info->max_range));
  fprintf(file,"min_elev: %g\t",(info->min_elev));
  fprintf(file,"max_elev: %g\n",(info->max_elev));
  fprintf(file,"radar_const: %g\t",(info->radar_const));
  fprintf(file,"nyquist_vel: %g\n",(info->nyquist_vel));
  fprintf(file,"delta_azmith: %g\t",(info->delta_azmith));
  fprintf(file,"start_azmith: %g\t",(info->start_azmith));
  fprintf(file,"beam_width: %g\t",(info->beam_width));
  fprintf(file,"pulse_width: %g\n",(info->pulse_width));
  fprintf(file,"noise_thresh: %g\t",(info->noise_thresh));
  fprintf(file,"nfields: %ld\n",(info->nfields));
  
  fprintf(file,"\nunits_label_x: %s\n",info->units_label_x);
  fprintf(file,"units_label_y: %s\n",info->units_label_y);
  fprintf(file,"units_label_z: %s\n",info->units_label_z);
  fprintf(file,"field_units: %s\n",info->field_units);
  fprintf(file,"field_name: %s\n",info->field_name);
  fprintf(file,"source_name: %s\n",info->source_name);

}

/*****************************************************************
 * PRINT_REPLY: Produce diagnostic output on the reply 
 */

print_reply(rep,file)
     cd_reply_t *rep;
     FILE *file;
{
  
  fprintf(file,
	  "\n********************* REPLY **********************\n");
  fprintf(file,"status: %d\t",rep->status);
  fprintf(file,"orient: %d\n",rep->orient);
  fprintf(file,"nx: %d\t",rep->nx);
  fprintf(file,"ny: %d\t",rep->ny);
  fprintf(file,"nz: %d\n",rep->nz);
  fprintf(file,"dx: %g\t",(rep->dx));
  fprintf(file,"dy: %g\t",(rep->dy));
  fprintf(file,"dz: %g\n",(rep->dz));
  fprintf(file,"x1: %d\t",rep->x1);
  fprintf(file,"x2: %d\t",rep->x2);
  fprintf(file,"y1: %d\t",rep->y1);
  fprintf(file,"y2: %d\t",rep->y2);
  fprintf(file,"z1: %d\t",rep->z1);
  fprintf(file,"z2: %d\n",rep->z2);
  fprintf(file,"scale: %g\t",(rep->scale));
  fprintf(file,"bias: %g\n",(rep->bias));
  
  fprintf(file,"time_begin: %d = %s\t",
	  rep->time_begin,
	  utimestr(udattim(rep->time_begin)));

  fprintf(file,"time_cent: %d = %s\t",
	  rep->time_cent,
	  utimestr(udattim(rep->time_cent)));

  fprintf(file,"time_end: %d = %s\t",
	  rep->time_end,
	  utimestr(udattim(rep->time_end)));

  fprintf(file,"bad_data_val: %d\t",rep->bad_data_val);
  fprintf(file,"data_type: %d\t",rep->data_type);
  fprintf(file,"data_field: %d\t",rep->data_field);
  fprintf(file,"n_points: %d\t",rep->n_points);
  fprintf(file,"data_length: %d\n",rep->data_length);

}

#define ARG_OPTION_STRING "p:h:v"

/*****************************************************************
 * PROCESS_ARGS: Progess command line arguments. Set option flags
 * And print usage info if necessary
 */

process_args(argc,argv)

     int argc;
     char *argv[];

{
  int err_flag =0;
  int c,i;
  extern int optind; /* index to remaining arguments */
  extern char *optarg; /* option argument string */
  
  if(argc < 3) err_flag++;
  
  while ((c = getopt(argc, argv,ARG_OPTION_STRING)) != EOF) {
    switch(c) {
    case 'v' : /* verbose mode */
      verbose_mode = 1;
      break;
      
    case 'p' : /* alternate port */
      port = atoi(optarg);
      break;
      
    case 'h' : /* alternate host name */
      strncpy(host_name,optarg,32);
      break;
      
    case '?': /* error in options */
    default:
      err_flag++;
      break;
    }
    
  };
  
  if(err_flag) {
    fprintf(stderr,"Usage:test_server -h host_name -p port [-v]\n");
    fprintf(stderr,"-v: Verbose mode - Prints data plane\n");
    exit(-1);
  }
  
}

/*****************************************************************
 * INIT_SOCKETS : Init all Socket Connections
 */

init_sockets()

{

  int pid;

  void signal_trap();
  void sig_io_trap();
  
  signal(SIGINT,signal_trap);
  signal(SIGTERM,signal_trap);

  pid = getpid();
  
}

/*****************************************************************
 * SIGNAL_TRAP : Traps Signals so as to die gracefully
 */

void sig_io_trap()
     
{
  
  fprintf(stderr,"\n############## SIG_IO ##################\n");
  fflush(stderr);
  
}

/*****************************************************************
 * SIGNAL_TRAP : Traps Signals so as to die gracefully
 */

void signal_trap(sig)

{
   
  exit(sig);

}



/*******************************************************************************
 * FREEFORM : A Routine that returns floating point fields contained in
 * a given character array seperated by any non-digit chars
 *
 * int freeform(inpstr,maxchr,outfields);
 * char inpstr[]; * input string *
 * int maxchr; * maximum characters to examine in string *
 * double outfields[]; * array of found numeric fields (output) *
 *
 * Routine returns the number of numeric fields found
 *
 * Written by Frank Hage -UNC 7/86
 * Last Update : 3/15/87
 *
 */

freeform(inpstr,maxchr,outfields)

     char inpstr[];
     int maxchr;
     double outfields[];

{

  double atof();
  char tmpbuf[256]; /* temporary buffer for conversion */
  char blanks[256];
  register int ii = 0; /* input string counter */
  register int jj = 0; /* temp buffer counter */
  register int kk = 0; /* numeric field counter */
  int fstat = 0; /* field status: 0=empty, 1=in progress */
  
  for(ii = 0; ii < 256; ii++) {
    blanks[ii] = '\0';
  }
  ii = 0;
  
  while(ii < maxchr && inpstr[ii] != '\000'){ /* not the end */
    /* not a digit, decimal point, plus or minus */
    if(inpstr[ii] <= '\052' || inpstr[ii] == '\057' ||
       inpstr[ii] == '\054' || inpstr[ii] >= '\072' ){ 
      if(inpstr[ii]=='\105' || inpstr[ii]=='\145' && jj!= 0) {
	/* is an 'e' - part of exponential number */
	tmpbuf[jj++] = inpstr[ii++];
      } else {
	tmpbuf[jj] = '\000'; /* terminate conv buffer */
	jj = 0; /* start a new conversion */
	ii++; /* look at next input character */
      }
    } else { /* is part of a numerical field */
      tmpbuf[jj++] = inpstr[ii++];
      fstat = 1; /* conversion buffer is not empty */
      if(inpstr[ii] == '\055') { /* next char/num is - */
	tmpbuf[jj] = '\000'; /* terminate */
	jj = 0; /* signal end of current conv */
      }
    }
    
    if( fstat == 1 && jj == 0 ) { /* field is complete */
      outfields[kk++] = atof(tmpbuf);
      fstat = 0;
      strncpy(tmpbuf,blanks,256); /* clear out buffer */
    }
  }
  
  if(fstat) { /* something still in conversion buffer */
    tmpbuf[ii] = '\000'; /* add terminator */
    outfields[kk++] = atof(tmpbuf); /* do conversion */
  }
  return(kk);
}

