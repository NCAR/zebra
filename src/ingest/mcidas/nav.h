/* mcidas.h - Include file describing the structures for the McIDAS data.
 *
 * Written by:  Nancy Rehak
 *              30 June 1992
 *
 * RAP, NCAR, Boulder, CO, USA
 */

extern int nvxini_c();
extern int nvxsae_c();
extern int nvxeas_c();
extern int nvxopt_c();


/* Define the structure for the navigation codicil (NX_NAV) for the GOES
 * (not using the Chebyshev polynomial) satellite data.
 */

typedef struct {
  long     nav_type;        /* navigation type, 4-byte EBCDIC             */
                            /*   can be one of these:                     */
                            /*      GOES for GOES satellites              */
                            /*      GVAR for GVAR satellites              */
                            /*      NOAA for NOAA polar orbiters          */
                            /*      PS for polar stereographic projection */
                            /*      LAMB for Lambert conformal projection */
                            /*      MERC for Mercator projection          */
                            /*      MET for METEOSAT                      */
                            /*      '    ' (or binary 0) if there is no   */
                            /*            navigation for this image       */
                            /*      RADR for radar projection             */
                            /*      RECT for rectilinear projection       */
                            /*            (pseudo-Mercator)               */
  long     sensor_source;   /* sensor source, year and Julian day, SSSYYDDD  */
  long     start_time;      /* nominal image start time, HHMMSS              */
  long     orbital_type;    /* always 1                                      */
  long     epoch_date;      /* epoch date (ETIMY), YYMMDD                    */
  long     epoch_time;      /* epoch time (ETIMH), HHMMSS                    */
  long     semimajor_axis;  /* semimajor axis (SEMIMA), km * 100             */
  long     orbital_ecc;     /* orbital eccentricity (ECCEN), * 1000000       */
  long     orbital_incl;    /* orbital inclination (ORBINC), degrees * 1000  */
  long     mean_anomaly;    /* mean_anomaly (MEANA), degrees * 1000          */
  long     arg_of_perigee;  /* argument of perigee (PERIGEE), degrees * 1000 */
  long     right_asc_asc;   /* right ascension of ascending node (ASNODE),   */
                            /*      degrees * 1000                           */
  long     decl_axis;       /* declination of satellite axis (DECLIN),       */
                            /*      DDDMMSS                                  */
  long     right_asc_axis;  /* right ascension of satellite axis (RASCEN),   */
                            /*      DDDMMSS                                  */
  long     pict_center;     /* picture center line number (PICLIN)           */
  long     spin_period;     /* spin period (SPINP); either the period of a   */
                            /*      satellite on a given day in microseconds,*/
                            /*      or the spin rate in rev/min              */
  long     sweep_ang_line;  /* total sweep angle, line direction (DEGLIN),   */
                            /*      DDDMMSS                                  */
  long     scan_lines;      /* number of scan lines (LINTOT), NNLLLLL; NN is */
                            /*      the number of sensors; LLLLL is the      */
                            /*      number of scans; the total number of     */
                            /*      actual lines is NN * LLLLL               */
  long     sweep_ang_elem;  /* total sweep angle, element direction (DEGELE),*/
                            /*      DDDMMSS                                  */
  long     num_elem;        /* number of elements in a scan line (ELETOT)    */
  long     pitch;           /* forward-leaning (PITCH), DDDMMSS              */
  long     yaw;             /* sideways-leaning (YAW), DDDMMSS               */
  long     rotation;        /* rotation (ROLL), DDDMMSS                      */
  long     reserved1;       /* RESERVED                                      */
  long     ew_adjust;       /* east-west adjustment value (IAJUST), in       */
                            /*      visible elements (+ or -)                */
  long     adjust_time;     /* time that IAJUST computed from the first valid*/
                            /*      landmark of the day (IAJTIM), HHMMSS     */
  long     reserved2;       /* RESERVED                                      */
  long     ise_angle;       /* angle between the VISSR and sun sensor        */
                            /*      (ISEANG), DDDMMSS                        */
  long     skew;            /* skew                                          */
  long     reserved4;       /* RESERVED                                      */
  long     scan_1;          /* scan line 1                                   */
  long     scan_1_time;     /* time of scan line 1 (beginning), HHMMSS       */
  long     scan_1_time_ms;  /* time of scan line 1 (continued),              */
                            /*      milliseconds * 10                        */
  long     beta_count_1;    /* beta count 1                                  */
  long     scan_2;          /* scan line 2                                   */
  long     scan_2_time;     /* time of scan line 2 (beginning), HHMMSS       */
  long     scan_2_time_ms;  /* time of scan line 2 (continued),              */
                            /*      milliseconds * 10                        */
  long     beta_count_2;    /* beta count 2                                  */
  long     gamma;           /* gamma, element offset * 100; this is the      */
                            /*      nominal offset at time 0 of this day     */
  long     gamma_dot;       /* gamma dot, element drift per hour * 100       */
  long     reserved5[80];   /* RESERVED                                      */
  char     memo[32];        /* memo entry; up to 32 characters of comments   */
} nx_nav_goes;




/* Define the structure for the area directory (DATDIR).
 */

typedef struct {
  long     area_status;     /* area staus; if < 0, the area does not exist;  */
                            /*      if = 0, the area does exist              */
  long     type;            /* type = 4, area version number                 */
  long     satellite_id;    /* satellite ID number, SSS                      */
  long     image_date;      /* year and Julian date of the image, YYDDD      */
  long     image_time;      /* time of the image, HHMMSS                     */
  long     y_coor;          /* Y-COOR, upper-left line in the satellite      */
                            /*      coordinates                              */
  long     x_coor;          /* X-COOR, upper_left element in the satellite   */
                            /*      coordinates                              */
  long     z_coor;          /* Z-COOR, upper-left z coordinate; not currently*/
                            /*      used                                     */
  long     y_size;          /* Y-SIZE, number of lines in the image          */
  long     x_size;          /* X-SIZE, number of elements in the image       */
  long     z_size;          /* Z-SIZE, number of bytes per data element      */
  long     y_res;           /* Y-RES, line resolution                        */
  long     x_res;           /* X-RES, element resolution                     */
  long     z_res;           /* Z-RES, z resolution; number of bands or       */
                            /*      channels                                 */
  long     line_prefix;     /* line prefix, number of bytes; must be         */
                            /*      multiple of 4                            */
  long     proj_num;        /* project number that created the area          */
  long     create_date;     /* creation date, YYDDD                          */
  long     create_time;     /* creation time, HHMMSS                         */
  long     filter_map;      /* filter map for soundings; a 32-bit vector;    */
                            /*      if bit = 1, data for that band exists in */
                            /*      the file; the right-most bit is for      */
                            /*      band 1                                   */
  long     image_id;        /* image ID number (optional)                    */
  long     ident[4];        /* identification (reserved); for RADAR 21,22 =  */
                            /*      call letters, 23 = range, 24 = number of */
                            /*      bad lines; for METEOSTAT 21-24 =         */
                            /*      calibration coefficients; for GMS 21 =   */
                            /*      IR calibration ID                        */
  char     memo[32];        /* up to 32 characters for comments              */
  long     prim_calib_key;  /* primary key of the associated calibration     */
                            /*      codicil                                  */
  long     prim_nav_key;    /* primary key of the associated navigation      */
                            /*      codicil                                  */
  long     sec_nav_key;     /* secondary key of the associated navigation    */
                            /*      codicil; if the primary key < 0, use     */
                            /*      SSSYYDD, HHMMSS to fetch the system      */
                            /*      navigation                               */
  long     validity_code;   /* validity code; must match the line prefix     */
  long     pdl[8];          /* PDL; in packed-byte format if Mode AA image   */
  long     band_8_source;   /* where band 8 came from if Mode AA-DS image    */
  long     start_date;      /* actual image start day, YYDDD                 */
  long     start_time;      /* actual image start time, HHMMSS               */
  long     start_scan;      /* actual starting scan                          */
  long     prefix_doc_len;  /* length of the prefix documentation (DOC)      */
                            /*      section, multiple of 4 bytes             */
  long     prefix_cal_len;  /* length of the prefix calibration (CAL)        */
                            /*      section, multiple of 4 bytes             */
  long     prefix_lev_len;  /* length of the prefix level (LEV) section,     */
                            /*      multiple of 4 bytes                      */
  char     source_type_c[4];/* source type: VISR, VAS, AAA, TIRO, etc.       */
  char     calib_type[4];   /* calibration type: BRIT, RAW, TEMP, RAD, etc.  */
  long     data_avg;        /* data was averaged (0) or sampled (1)          */
  char     poes_type[4];    /* POES signal type: LAC, GAC, HRPT              */
  long     poes_direction;  /* POES ascending/descending 1=A, 2=D, 3=AD, 4=DA*/
  long     source_type;     /* original source type of data                  */
  long     reserved[7];     /* RESERVED                                      */
} datdir;
