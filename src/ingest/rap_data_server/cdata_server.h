/******************************************************************
 * CDATA_SERVER.H
 *
 * Structures and defines for cartesian data servers and clients.
 *
 * The command is sent to the server via the struct cdata_comm_t.
 * The primary command is coded as bits in the long primary_com.
 * The secondary command is set as a long in secondary_com.
 *
 * The reply is received via a number of structs and arrays in a
 * specified order, as follows:
 *
 * 1. cdata_reply_t - always
 *
 * 2. cdata_info_t - if (reply.status & INFO_FOLLOWS) is true
 *
 * 3. plane_heights as a long array if (reply.status & PLANE_HEIGHTS_FOLLOW)
 *    is true. If plane heights are sent, the info struct is always sent,
 *    and the info.divisor value applies to the plane heights. The number
 *    height values is (info.nz * 3), i.e. there are 3 height values for
 *    each plane. These are lower, mid and upper limits for each plane, and
 *    are sent consecutively, with the plane number changhing for each 3
 *    values sent.
 *   
 * 4. the byte data for the requested field and plane.
 *
 * The 'cdata_' class of data structs referred to above contain only
 * long and char data. Floating point values are sent as longs, and 
 * are to be scaled by the 'divisor' value if marked ## in the
 * comment. These structs are used for data transmission, and the 
 * longs are transmitted in network-byte-order.
 *
 * The corresponding 'cd_' class of structs contain the data in the
 * native floating point format, and mayu be used by the server or the
 * client for convenience, but are not used for data transmission.
 *
 * Frank Hage, RAP, NCAR, Boulder, CO, USA, 1991
 *
 * Updated by Mike Dixon, December 1991, May 1992
 *
 ***********************************************************************/

#define CDATA_COMMAND 46464       /* ID cookie for Cart DATA command */
#define CDATA_REPLY 46465         /* ID cookie for Cart DATA reply */
#define CDATA_GRID_INFO 46466     /* ID cookie for Cart DATA info */
#define CDATA_GRID_DATA 46467     /* ID cookie for Cart DATA */
#define CDATA_DIVISOR 10000.0     /* Scaler to convert Longs to Doubles */

/*
 * PRIMARY COMMANDS - BIT MASK
 */

#define GET_INFO          0x00000001 /* Return Info on the data, grid, etc */
#define GET_DATA          0x00000002 /* Return some data */
#define GET_MOST_RECENT   0x00000004 /* Return the most recent data possible */

#define GET_NEW           0x00000008 /* Return the most recent data possible.
				      * In this mode, the request time
				      * centroid contains the time of
				      * the last data received. If there
				      * is new data (later that the 
				      * previous request), then the 
				      * new data is sent. Otherwise,
				      * no data is sent */

#define GET_PLANE_HEIGHTS 0x00000010 /* Return array of plane heights - 
				      * there are 3 heights for each
				      * plane: lower, mid, upper. The heights
				      * for each plane are sent
				      * consecutively */

/*
 *  SECONDARY COMMANDS
 */

#define GET_XY_PLANE 1       /* Retrieve a Horizontal plane of data */
#define GET_XZ_PLANE 2       /* Retrieve a Horizontal plane of data */
#define GET_YZ_PLANE 3       /* Retrieve a Horizontal plane of data */
#define GET_V_PLANE 4        /* Retrieve a Vertical plane of data */
#define GET_VOLUME 5         /* Retrieve a volume of data */
#define GET_MAX_XY_PLANE 6   /* Retrieve The Maxvalue in H orient */ 
#define GET_MAX_XZ_PLANE 7   /* Retrieve The Maxvalue in H orient */
#define GET_MAX_YZ_PLANE 8   /* Retrieve The Maxvalue in H orient */

/*
 * DATA SERVER DATA TYPES
 */

#define CHAR 1
#define SHORT 2
#define LONG 3
#define FLOAT 4
#define DOUBLE 5

/*
 * DATA ORIENTATIONS
 */

#define XY_PLANE 1  /* Top View */
#define XZ_PLANE 2  /* Longitude along X Height along Y */
#define YZ_PLANE 3  /* Latitude along X, Height along Y */
#define V_PLANE 4   /* Arbitrary Vertical plane   */

/*
 * DATA PROJECTIONS
 */

#define CARTESIAN 1
#define POLAR_STEREO 2

/*
 * SERVER RETURNED  STATUS MESSAGES - BITS
 */

#define REQUEST_SATISFIED    0x00000001 /* Indicates success */
#define INFO_FOLLOWS         0x00000002 /* Information Message follows reply */
#define DATA_FOLLOWS         0x00000004 /* Data Message(s) follows reply */
#define MOST_RECENT          0x00000008 /* Data is the most recent available */
#define NEW_DATA             0x00000010 /* Data changed since last request */
#define NO_NEW_DATA          0x00000020 /* Data changed since last request */
#define NO_DATA              0x00000040 /* No data available */
#define NO_INFO              0x00000080 /* Data info is not availible */
#define VOLUME_LIMITS        0x00000100 /* No data in desired volume */
#define TIME_LIMITS          0x00000200 /* No data within the time frame */
#define PLANE_HEIGHTS_FOLLOW 0x00000400

#define LAB_LEN 32  /* Character label lengths */

/*
 * The following structures are passed across the network in
 * network-byte-order
 */

/*
 * The client command request
 */

typedef struct { /* ## = APPLY SCALING QUANTITY */

  long primary_com; /* The primary command */
  long second_com;  /* The secondary command */
  long divisor;     /* Scaling quanity to convert longs to double */

  long lat_origin;  /* ## Degrees latitude of the origin of
		     * the coord system */
  long lon_origin;  /* ## Degrees longitude of the origin of
		     * the coord system */
  long ht_origin;   /* ## Km */

                    /* times are secs since 1/1/1970 0:00:00) */

  long time_min;    /* start time */
  long time_cent;   /* time centroid */
  long time_max;    /* end time */

  long min_x,max_x; /* ## coord limits (Km) */
  long min_y,max_y; /* ## coord limits (Km) */
  long min_z,max_z; /* ## coord limits (Km) */

  long data_field;  /* Which data field to return data/info on */
  long data_type;   /* Type of data to return. i.e.Float, int, char */
  long add_data_len;/* Additional data bytes to read for more request info */

} cdata_comm_t;

/*
 * The server reply
 */

typedef struct { /* ## = APPLY SCALING QUANTITY */

  long status;       /* Status message bit mask */
  long divisor;      /* Scaling quanity to convert longs to doubles */
  long orient;       /* The data's orientation */
  long nx,ny,nz;     /* number of grid points returned in each direction */

  long dx,dy,dz;     /* ## Size of grid points in each direction - (Km)
		      * If Plane heights are returned, dz is not
		      * applicable. Therefore, it is set to 0 
		      * in this struct */

  long x1,x2;        /* Grid limits in returned data */
  long y1,y2;        /* Grid limits in returned data */
  long z1,z2;        /* Grid limits in returned data */
  long scale;        /* ## Multiplier to reconstruct orig value */
  long bias;         /* ## Bias value to reconstruct orig value */
  long time_begin;   /* Time when data began to accumulate */
  long time_end;     /* Time when data finished accumulating */
  long time_cent;    /* Time Centroid (Weighted) */
  long bad_data_val; /* Value that represents bad or missing data */
  long data_type;    /* Type of data in data plane */
  long data_field;   /* Which field info is about */
  long expire_time;  /* Unix time at which data becomes invalid */
  long n_points;     /* number of points in data plane */
  long data_length;  /* bytes of remaining data to be read  - info,
		      * plane_heights, data plane */

} cdata_reply_t;

/*
 * the grid info
 */

typedef struct { /* ## = APPLY SCALING QUANTITY */

  long divisor;      /* Scaling quanity to convert longs to doubles */
  long order;        /* 0 = right handed coord system, 1 = left */
  long data_field;   /* Which field info is about */
  long projection;   /* Type of coordinate projection used for data grid */
  long lat_origin;   /* ## Degrees lat of the origin of the coord system */
  long lon_origin;   /* ## Degrees long of the origin of the coord system */
  long source_x;     /* Data source X location in coordinate system -KM */
  long source_y;     /* Data source Y location in coordinate system -KM */
  long source_z;     /* Data source Z location in coordinate system -KM */
  long ht_origin;    /* ## Km */
  long nx,ny,nz;     /* number of grid points in each direction */
  long dx,dy,dz;     /* ## Size of grid points in each direction - (Km) */
  long min_x,max_x;  /* ## limits of grid in x direction */
  long min_y,max_y;  /* ## limits of grid in y direction */
  long min_z,max_z;  /* ## limits of grid in z direction */
  long north_angle;  /* ## Angle of Y axis relitive to true north */
  long gate_spacing; /* ## meters */
  long wavelength;   /* ## micro-meters */
  long frequency;    /* ## kHz */
  long min_range;    /* ## km */
  long max_range;    /* ## km */
  long num_gates;    /*  */
  long min_gates;    /*  */
  long max_gates;    /*  */
  long num_tilts;    /* The number of unique radar tilts */
  long min_elev;     /* ## Minimum  elevation of scans - Deg */
  long max_elev;     /* ## Maximum elevation */
  long radar_const;  /* ## */
  long nyquist_vel;  /* ##  */
  long delta_azmith; /* ## degrees between each beam */
  long start_azmith; /* ## degrees */
  long beam_width;   /* ## degrees */
  long pulse_width;  /* ## nano-seconds */
  long data_length;  /* bytes of remaining data to read */
  long noise_thresh; /* ## Signal/Noise threshold for data rejection */
  long nfields;      /* number of data fields */
  char units_label_x[LAB_LEN];  /* Units km (meters) etc */
  char units_label_y[LAB_LEN];  /* Units km (meters) etc */
  char units_label_z[LAB_LEN];  /* Units mbar (meters) etc */
  char field_units[LAB_LEN];    /* Units dbz, (m/sec) etc */
  char field_name[LAB_LEN]; 
  char source_name[LAB_LEN];    /* Radar, Algorithm name etc */

} cdata_info_t;

#define NUM_INFO_LONGS 43  /* the number of longd in the info struct */

/*
 * The following are convenience structs in native floating point
 * format where applicable - they are not used for data transmission
 */

/*
 * The Client command request
 */

typedef struct { /* Native Floating point format */

  long primary_com;   /* The primary command */
  long second_com;    /* The secondary command */

                      /* times are secs since 1/1/1970 0:00:00) */

  long time_min;      /* start time */
  long time_cent;     /* time centroid */
  long time_max;      /* end time */

  long data_field;    /* Which data field to return data/info on */
  long data_type;     /* Type of data to return. i.e.Float, int, char */
  long add_data_len;  /* Additional data bytes to read for more request info */

  double lat_origin;  /* Degrees lat of the origin of the coord system */
  double lon_origin;  /* Degrees long of the origin of the coord system */
  double ht_origin;   /* Km */

  double min_x,max_x; /* coord limits (Km) */
  double min_y,max_y; /* coord limits (Km) */
  double min_z,max_z; /* coord limits (Km) */

} cd_command_t;

/*
 * the server reply
 */

typedef struct { /* Native Floating Point format */

  long status;        /* Status message bit mask */
  long orient;        /* The data's orientation */
  long nx,ny,nz;      /* number of grid points returned in each direction */

  double dx,dy,dz;    /* Size of grid points in each direction - (Km)
		       * If dz is 0, then plane heights are included */

  long x1,x2;         /* Grid limits in returned data (grid units) */
  long y1,y2;         /* Grid limits in returned data (grid_units) */
  long z1,z2;         /* Grid limits in returned data (grid_units) */
  double scale;       /* Multiplier to reconstruct orig value */
  double bias;        /* Bias value to reconstruct orig value */
  long time_begin;    /* Time when data began to accumulate */
  long time_end;      /* Time when data finished accumulating */
  long time_cent;     /* Time Centroid (Weighted) */
  long bad_data_val;  /* Value that represents bad or missing data */
  long data_type;     /* Type of data in data plane */
  long data_field;    /* Which field info is about */
  long expire_time;   /* Unix time at which data becomes invalid */
  long n_points;      /* number of points in data plane */
  long data_length;   /* bytes of remaining data to be read */

} cd_reply_t;

/*
 * the grid info
 */

typedef struct { /* Native Floating Point format */

  long order;          /* 0 = left handed coord system, 1 = right */
  long data_field;     /* Which field info is about */
  long projection;     /* Type of coordinate projection used for data grid */
  double lat_origin;   /* Degrees lat of the origin of the coord system */
  double lon_origin;   /* Degrees long of the origin of the coord system */
  double source_x;     /* Data source X location in coordinate system -KM */
  double source_y;     /* Data source Y location in coordinate system -KM */
  double source_z;     /* Data source Z location in coordinate system -KM */
  double ht_origin;    /* Km */
  long nx,ny,nz;       /* number of grid points in each direction */
  double dx,dy,dz;     /* Size of grid points in each direction - (Km) */
  double min_x,max_x;  /* limits of grid in x direction */
  double min_y,max_y;  /* limits of grid in x direction */
  double min_z,max_z;  /* limits of grid in x direction */
  double north_angle;  /* Angle of Y axis relitive to true north */
  double gate_spacing; /* meters */
  double wavelength;   /* micro-meters */
  double frequency;    /* kHz */
  double min_range;    /* km */
  double max_range;    /* km */
  long num_gates;
  long min_gates;
  long max_gates;
  long num_tilts;      /* The number of unique radar tilts */
  double min_elev;     /* Minimum  elevation of scans - Deg */
  double max_elev;     /* Maximum elevation */
  double radar_const;
  double nyquist_vel;
  double delta_azmith; /* degrees between each beam */
  double start_azmith; /* degrees */
  double beam_width;   /* degrees */
  double pulse_width;  /* nano-seconds */
  long data_length;    /* bytes of remaining data to read */
  double noise_thresh; /* Signal/Noise threshold for data rejection */
  long nfields;
  long spare;
  char units_label_x[LAB_LEN]; /* Units km (meters) etc */
  char units_label_y[LAB_LEN]; /* Units km (meters) etc */
  char units_label_z[LAB_LEN]; /* Units mbar (meters) etc */
  char field_units[LAB_LEN];   /* Units dbz, (m/sec) etc */
  char field_name[LAB_LEN]; 
  char source_name[LAB_LEN];   /* Radar, Algorithm name etc */

} cd_grid_info_t;

