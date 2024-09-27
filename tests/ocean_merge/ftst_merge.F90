! Unit test for the merge routine.

 program ftst_merge

 implicit none

 integer, parameter   :: lon = 1
 integer, parameter   :: lat = 1

 integer              :: binary_lake

 real, parameter      :: epsilon=0.00001
 real                 :: lat2d(lon,lat)
 real                 :: ocn_frac(lon,lat)
 real                 :: lake_frac(lon,lat)
 real                 :: lake_depth(lon,lat)
 real                 :: land_frac(lon,lat)
 real                 :: slmsk(lon,lat)

 print*,"Begin test of merge routine"

! Test point 1

! Some ocean. No lake. 

 print*,'Test point 1.'

 binary_lake = 0         ! keep fractional lake.
 lat2d(1,1) = 30.0       ! point at 30N.
 ocn_frac(1,1) = .75     ! .75 ocean/.25 land
 lake_frac(1,1) = 0.0    ! no lake.
 lake_depth(1,1) = 0.0   ! no lake.
 land_frac(1,1) = -99.   ! based on ocn_frac, should be .25.
 slmsk(1,1) = -99.       ! should be zero (nint of land_frac)

 call merge(lon, lat, binary_lake, lat2d, ocn_frac, &
            lake_frac, lake_depth, land_frac, slmsk)

 if ( abs(lake_frac(1,1) - 0.) > epsilon) stop 2
 if ( abs(lake_depth(1,1) - 0.) > epsilon) stop 2
 if ( abs(land_frac(1,1) - .25) > epsilon) stop 2
 if ( abs(slmsk(1,1) - 0.) > epsilon) stop 2

! test point 2

! No ocean. Some lake.

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

! test point 3

! Some lake and some ocean. Ocean should dominate.

 binary_lake = 0
 lat2d(1,1) = 30.0
 ocn_frac(1,1) = .45
 lake_frac(1,1) = .75
 lake_depth(1,1) = 100.0
 land_frac(1,1) = -99.
 slmsk(1,1) = -99.

 call merge(lon, lat, binary_lake, lat2d, ocn_frac, &
            lake_frac, lake_depth, land_frac, slmsk)

 print*,'test point 3 '
 print*,'ocn_frac ',ocn_frac(1,1)
 print*,'lake_frac ',lake_frac(1,1)
 print*,'lake_depth ',lake_depth(1,1)
 print*,'land_frac ',land_frac(1,1)
 print*,'slmsk ', slmsk(1,1)

! test point 4

! No ocean. Some lake. Lake has a missing depth that should
! be given a default value.

 binary_lake = 0
 lat2d(1,1) = 30.0
 ocn_frac(1,1) = 0.0
 lake_frac(1,1) = .75
 lake_depth(1,1) = -9.
 land_frac(1,1) = -99.
 slmsk(1,1) = -99.

 call merge(lon, lat, binary_lake, lat2d, ocn_frac, &
            lake_frac, lake_depth, land_frac, slmsk)

 print*,'test point 4 '
 print*,'ocn_frac ',ocn_frac(1,1)
 print*,'lake_frac ',lake_frac(1,1)
 print*,'lake_depth ',lake_depth(1,1)
 print*,'land_frac ',land_frac(1,1)
 print*,'slmsk ', slmsk(1,1)

! Test point 5
! Some ocean (but very small percentage). No lake. 
! The ocean is removed and point becomes all land.

 binary_lake = 0
 lat2d(1,1) = 30.0
 ocn_frac(1,1) = 1.e-6
 lake_frac(1,1) = 0.0
 lake_depth(1,1) = 0.0
 land_frac(1,1) = -99.
 slmsk(1,1) = -99.

 call merge(lon, lat, binary_lake, lat2d, ocn_frac, &
            lake_frac, lake_depth, land_frac, slmsk)

 print*,'test point 5 '
 print*,'ocn_frac ',ocn_frac(1,1)
 print*,'lake_frac ',lake_frac(1,1)
 print*,'lake_depth ',lake_depth(1,1)
 print*,'land_frac ',land_frac(1,1)
 print*,'slmsk ', slmsk(1,1)

! Test point 6
! Some ocean (almost all ocean). Some lake.
! The ocean is set to 1.0 and there is no land.

 binary_lake = 0
 lat2d(1,1) = 30.0
 ocn_frac(1,1) = 0.99999
 lake_frac(1,1) = 0.24
 lake_depth(1,1) = 50.0
 land_frac(1,1) = -99.
 slmsk(1,1) = -99.

 call merge(lon, lat, binary_lake, lat2d, ocn_frac, &
            lake_frac, lake_depth, land_frac, slmsk)

 print*,'test point 6 '
 print*,'ocn_frac ',ocn_frac(1,1)
 print*,'lake_frac ',lake_frac(1,1)
 print*,'lake_depth ',lake_depth(1,1)
 print*,'land_frac ',land_frac(1,1)
 print*,'slmsk ', slmsk(1,1)

! test point 7

! No ocean. Some lake, but near Antarctica.
! Lake to be removed.

 binary_lake = 0
 lat2d(1,1) = -70.0
 ocn_frac(1,1) = 0.0
 lake_frac(1,1) = .75
 lake_depth(1,1) = 100.0
 land_frac(1,1) = -99.
 slmsk(1,1) = -99.

 call merge(lon, lat, binary_lake, lat2d, ocn_frac, &
            lake_frac, lake_depth, land_frac, slmsk)

 print*,'test point 7 '
 print*,'ocn_frac ',ocn_frac(1,1)
 print*,'lake_frac ',lake_frac(1,1)
 print*,'lake_depth ',lake_depth(1,1)
 print*,'land_frac ',land_frac(1,1)
 print*,'slmsk ', slmsk(1,1)

! test point 8

! No ocean. Some lake. Assume binary yes/no lake.

 binary_lake = 1
 lat2d(1,1) = 30.0
 ocn_frac(1,1) = 0.0
 lake_frac(1,1) = .15
 lake_depth(1,1) = 100.0
 land_frac(1,1) = -99.
 slmsk(1,1) = -99.

 call merge(lon, lat, binary_lake, lat2d, ocn_frac, &
            lake_frac, lake_depth, land_frac, slmsk)

 print*,'test point 8 '
 print*,'ocn_frac ',ocn_frac(1,1)
 print*,'lake_frac ',lake_frac(1,1)
 print*,'lake_depth ',lake_depth(1,1)
 print*,'land_frac ',land_frac(1,1)
 print*,'slmsk ', slmsk(1,1)



 print*, "OK"

 print*, "SUCCESS!"

 end program ftst_merge
