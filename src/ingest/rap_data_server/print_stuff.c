/*
 * Print out stuff that get returned from a data server.
 */

#include <stdio.h>
#include "cdata_server.h"

typedef struct {
        long    year, month, day, hour, min, sec;
        long    unix_time;
} date_time_t;

print_info (info, file)
cd_grid_info_t	*info;
FILE		*file;
{
	 
	fprintf(file,"\n***************************** GRID INFO **********************\n");
	fprintf(file,"order: %d\t",info->order);
	fprintf(file,"data_field: %d\t",info->data_field);
	fprintf(file,"projection: %d\n",info->projection);
	fprintf(file,"source_x: %d\t",info->source_x);
	fprintf(file,"source_z: %d\t",info->source_y);
	fprintf(file,"source_y: %d\n",info->source_z);
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

	fprintf(file,"\nunits_label_x: %s\n",info->units_label_x);
	fprintf(file,"units_label_y: %s\n",info->units_label_y);
	fprintf(file,"units_label_z: %s\n",info->units_label_z);
	fprintf(file,"field_units: %s\n",info->field_units);
	fprintf(file,"field_name: %s\n",info->field_name);
	fprintf(file,"source_name: %s\n",info->source_name);
}


print_reply (rep, file)
cd_reply_t	*rep;
FILE		*file;
{
	date_time_t dt;

	fprintf(file,"\n***************************** REPLY **********************\n");
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

	fprintf(file,"time_begin: %d\t",rep->time_begin);
	convert_from_unix_time (rep->time_begin, &dt);
	fprintf(file," %d/%d/%d %d:%02d:%02d\n", dt.month, dt.day, dt.year,
			dt.hour, dt.min, dt.sec);

	fprintf(file,"time_end: %d\t",rep->time_end);
	convert_from_unix_time(rep->time_end,&dt);
	fprintf(file," %d/%d/%d %d:%02d:%02d\n", dt.month, dt.day, dt.year,
			dt.hour, dt.min, dt.sec);

	fprintf(file,"time_cent: %d\t",rep->time_cent);
	convert_from_unix_time(rep->time_cent,&dt);
	fprintf(file," %d/%d/%d %d:%02d:%02d\n", dt.month, dt.day, dt.year,
			dt.hour, dt.min, dt.sec);

	fprintf(file,"bad_data_val: %d\t",rep->bad_data_val);
	fprintf(file,"data_type: %d\t",rep->data_type);
	fprintf(file,"data_field: %d\t",rep->data_field);
	fprintf(file,"n_points: %d\t",rep->n_points);
	fprintf(file,"data_length: %d\n",rep->data_length);
}


