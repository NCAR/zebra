!
! Parameters to read in sur scans (LOWRES) from MHR
!
set x_resolution 900
set y_resolution 900
set x_radar 450
set y_radar 450
set gmt_offset 0
set platform "mhr"
set minimum_sweep 50
set pixels_per_km 2
set ui$pager_mode false
source file /bitter/dt/wisp
field 'reflectivity' offset 0
field 'velocity' offset 1
threshold offset 3 threshold 10
! set niceness -19
mode window status
