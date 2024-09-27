! Unit test for the merge routine.

 program ftst_merge

 implicit none

 integer, parameter   :: lon = 1
 integer, parameter   :: lat = 1

 integer              :: binary_lake

 real                 :: lat2d(lon,lat)
 real                 :: ocn_frac(lon,lat)
 real                 :: lake_frac(lon,lat)
 real                 :: lake_depth(lon,lat)
 real                 :: land_frac(lon,lat)
 real                 :: slmsk(lon,lat)

 print*,"Begin test of merge routine"

! test point 1

 binary_lake = 0
 lat2d(1,1) = 30.0
 ocn_frac(1,1) = .75
 lake_frac(1,1) = 0.0
 lake_depth(1,1) = 0.0
 land_frac(1,1) = -99.
 slmsk(1,1) = -99.

 call merge(lon, lat, binary_lake, lat2d, ocn_frac, &
            lake_frac, lake_depth, land_frac, slmsk)

 print*,'test point 1 '
 print*,'ocn_frac ',ocn_frac(1,1)
 print*,'lake_frac ',lake_frac(1,1)
 print*,'lake_depth ',lake_depth(1,1)
 print*,'land_frac ',land_frac(1,1)
 print*,'slmsk ', slmsk(1,1)

! test point 2

 binary_lake = 0
 lat2d(1,1) = 30.0
 ocn_frac(1,1) = 0.0
 lake_frac(1,1) = .75
 lake_depth(1,1) = 100.0
 land_frac(1,1) = -99.
 slmsk(1,1) = -99.

 call merge(lon, lat, binary_lake, lat2d, ocn_frac, &
            lake_frac, lake_depth, land_frac, slmsk)

 print*,'test point 2 '
 print*,'ocn_frac ',ocn_frac(1,1)
 print*,'lake_frac ',lake_frac(1,1)
 print*,'lake_depth ',lake_depth(1,1)
 print*,'land_frac ',land_frac(1,1)
 print*,'slmsk ', slmsk(1,1)
 print*, "OK"

 print*, "SUCCESS!"

 end program ftst_merge
