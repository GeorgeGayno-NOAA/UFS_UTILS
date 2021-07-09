
 module atm_utils

 public

 contains

 subroutine mid_pt_sphere(p1, p2, pm)
      real, intent(IN)  :: p1(2), p2(2)
      real, intent(OUT) :: pm(2)
!------------------------------------------
      real e1(3), e2(3), e3(3)

      call latlon2xyz(p1, e1)
      call latlon2xyz(p2, e2)
      call mid_pt3_cart(e1, e2, e3)
      call cart_to_latlon(1, e3, pm(1), pm(2))

 end subroutine mid_pt_sphere

 subroutine latlon2xyz(p, e, id)
!
! Routine to map (lon, lat) to (x,y,z)
!
 real, intent(in) :: p(2)
 real, intent(out):: e(3)
 integer, optional, intent(in):: id   ! id=0 do nothing; id=1,
                                      ! right_hand

 integer n
 real :: q(2)
 real :: e1, e2, e3

    do n=1,2
       q(n) = p(n)
    enddo

    e1 = cos(q(2)) * cos(q(1))
    e2 = cos(q(2)) * sin(q(1))
    e3 = sin(q(2))
!-----------------------------------
! Truncate to the desired precision:
!-----------------------------------
    e(1) = e1
    e(2) = e2
    e(3) = e3

 end subroutine latlon2xyz

 subroutine mid_pt3_cart(p1, p2, e)
       real, intent(IN)  :: p1(3), p2(3)
       real, intent(OUT) :: e(3)
!
       real :: q1(3), q2(3)
       real :: dd, e1, e2, e3
       integer k

       do k=1,3
          q1(k) = p1(k)
          q2(k) = p2(k)
       enddo

       e1 = q1(1) + q2(1)
       e2 = q1(2) + q2(2)
       e3 = q1(3) + q2(3)

       dd = sqrt( e1**2 + e2**2 + e3**2 )
       e1 = e1 / dd
       e2 = e2 / dd
       e3 = e3 / dd

       e(1) = e1
       e(2) = e2
       e(3) = e3

 end subroutine mid_pt3_cart

 subroutine cart_to_latlon(np, q, xs, ys)
! vector version of cart_to_latlon1
  integer, intent(in):: np
  real, intent(inout):: q(3,np)
  real, intent(inout):: xs(np), ys(np)
! local
  real, parameter:: esl=1.d-10
  real:: p(3)
  real:: dist, lat, lon
  integer i,k

  do i=1,np
     do k=1,3
        p(k) = q(k,i)
     enddo
     dist = sqrt(p(1)**2 + p(2)**2 + p(3)**2)
     do k=1,3
        p(k) = p(k) / dist
     enddo

     if ( (abs(p(1))+abs(p(2)))  < esl ) then
          lon = 0.
     else
          lon = atan2( p(2), p(1) )   ! range [-pi,pi]
     endif

     if ( lon < 0.) lon = 2.*pi + lon
! RIGHT_HAND system:
     lat = asin(p(3))

     xs(i) = lon
     ys(i) = lat
! q Normalized:
     do k=1,3
        q(k,i) = p(k)
     enddo
  enddo

 end  subroutine cart_to_latlon

 subroutine get_unit_vect2( e1, e2, uc )
   real, intent(in) :: e1(2), e2(2)
   real, intent(out):: uc(3) ! unit vector e1--->e2
! Local:
   real, dimension(3):: pc, p1, p2, p3

! RIGHT_HAND system:
   call latlon2xyz(e1, p1)
   call latlon2xyz(e2, p2)

   call mid_pt3_cart(p1, p2,  pc)
   call vect_cross(p3, p2, p1)
   call vect_cross(uc, pc, p3)
   call normalize_vect( uc )

 end subroutine get_unit_vect2

 subroutine vect_cross(e, p1, p2)
 real, intent(in) :: p1(3), p2(3)
 real, intent(out):: e(3)
!
! Perform cross products of 3D vectors: e = P1 X P2
!
      e(1) = p1(2)*p2(3) - p1(3)*p2(2)
      e(2) = p1(3)*p2(1) - p1(1)*p2(3)
      e(3) = p1(1)*p2(2) - p1(2)*p2(1)

 end subroutine vect_cross

 subroutine normalize_vect(e)
!                              Make e an unit vector
 real, intent(inout):: e(3)
 real:: pdot
 integer k

    pdot = e(1)**2 + e(2)**2 + e(3)**2
    pdot = sqrt( pdot )

    do k=1,3
       e(k) = e(k) / pdot
    enddo

 end subroutine normalize_vect

 subroutine get_latlon_vector(pp, elon, elat)
 real, intent(IN)  :: pp(2)
 real, intent(OUT) :: elon(3), elat(3)

         elon(1) = -SIN(pp(1))
         elon(2) =  COS(pp(1))
         elon(3) =  0.0
         elat(1) = -SIN(pp(2))*COS(pp(1))
         elat(2) = -SIN(pp(2))*SIN(pp(1))
!!! RIGHT_HAND
         elat(3) =  COS(pp(2))
! Left-hand system needed to be consistent with rest of the codes
!        elat(3) = -COS(pp(2))

 end subroutine get_latlon_vector

 real function inner_prod(v1, v2)
       real,intent(in):: v1(3), v2(3)
       real :: vp1(3), vp2(3), prod16
       integer k

         do k=1,3
            vp1(k) = v1(k)
            vp2(k) = v2(k)
         enddo
         prod16 = vp1(1)*vp2(1) + vp1(2)*vp2(2) + vp1(3)*vp2(3)
         inner_prod = prod16

 end function inner_prod
 
 end module atm_utils
