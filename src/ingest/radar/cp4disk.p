!
! Parameters to read in sur scans from CP4 disk files.
!
set x_resolution 900
set y_resolution 900
set x_radar 450
set y_radar 450
set gmt_offset 0
set platform "cp4"
set minimum_sweep 50
set pixels_per_km 3
set ui$pager_mode false
source file /bitter/dt/wisp
field 'reflectivity' offset 4
field 'velocity' offset 2
threshold offset 0 threshold 50
! set niceness -19
consumer ds_consumer mhr
mode window status
