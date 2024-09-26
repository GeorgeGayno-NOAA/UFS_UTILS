!> @file
!! @brief Determines the water mask by merging the lake mask with
!! the mapped ocean mask from MOM6.
!! @author Shan Sun
!! @author Rahul Mahajan
!! @author Sanath Kumar

!> Determine the water mask by merging the lake mask with the mapped ocean
!! mask from MOM6, both are on the FV3 grid. During merge, the ocean mask
!! dominates the lake mask if there is a conflict. After the merge, the remaining
!! non-water fraction is the land fraction.
!! 
!! @return 0 for success, error code otherwise.
!! @author Shan Sun
!! @author Rahul Mahajan
!! @author Sanath Kumar
program merge_lake_ocnmsk
  use netcdf

  implicit none

  character(len=120) :: pth1
  character(len=120) :: pth2,pth3
  character(len=10)  :: atmres,ocnres
  real, parameter    :: min_land=1.e-4, def_lakedp=10.
 ! this variable is now renamed as binary_lake and is passed in from the name
 ! list
 ! logical, parameter :: int_lake=.true.  
 ! all instances of int_lake was changed to binary_lake  
  integer :: binary_lake

  character(len=250) :: flnm
  integer :: ncid,ndims,nvars,natts,lat,lon,v1id,v2id,v3id,v4id,start(2),count(2),i,j,latid,lonid,ncid4, dims(2),tile,nodp_pt
  integer :: lake_pt,vlat
  real, allocatable :: lake_frac(:,:),lake_depth(:,:),land_frac(:,:),ocn_frac(:,:),slmsk(:,:),lat2d(:,:)

  call read_nml(pth1, pth2, atmres, ocnres, pth3,binary_lake)
  
  print *, pth1
  nodp_pt=0
  lake_pt=0  
  do tile=1,6
    write(flnm,'(5a,i1,a)') trim(pth1),trim(atmres),'.',trim(ocnres),'.tile',tile,'.nc'
    call handle_err (nf90_open (flnm, NF90_NOWRITE, ncid))
    call handle_err (nf90_inquire (ncid, ndimensions=ndims, nvariables=nvars, nattributes=natts))
    write(6,*) 'flnm_ocn=',flnm,' ncid=',ncid, ' ndims=',ndims, 'nvars=',nvars,' natts=',natts
    call handle_err (nf90_inq_dimid (ncid, 'grid_xt', latid))  ! RM: lon is no longer in this file; try grid_xt
    call handle_err (nf90_inq_dimid (ncid, 'grid_yt', lonid))  ! RM: lat is no longer in this file; try grid_tyt
    call handle_err (nf90_inquire_dimension (ncid, latid, len=lat))
    call handle_err (nf90_inquire_dimension (ncid, lonid, len=lon))
    if (tile==1) then
      write(6,*) 'lat=',lat,'lon=',lon
      allocate (lake_frac(lon,lat),lake_depth(lon,lat),land_frac(lon,lat),ocn_frac(lon,lat),slmsk(lon,lat),lat2d(lon,lat))
      start(1:2) = (/1,1/)
      count(1:2) = (/lon,lat/)
    end if
    call handle_err (nf90_inq_varid(ncid, 'land_frac', v1id))
    call handle_err (nf90_get_var (ncid, v1id, ocn_frac, start=start, count=count))

    write(flnm,'(4a,i1,a)') trim(pth2),'oro.',trim(atmres),'.tile',tile,'.nc'
    print *,' flnm2=',trim(flnm)
    call handle_err (nf90_open (flnm, NF90_NOWRITE, ncid))
    call handle_err (nf90_inquire (ncid, ndimensions=ndims, nvariables=nvars, nattributes=natts))
    write(6,*) 'flnm_lake=',flnm,' ncid=',ncid, ' ndims=',ndims, 'nvars=',nvars,' natts=',natts
    call handle_err (nf90_inq_varid(ncid, 'lake_frac', v2id))
    call handle_err (nf90_inq_varid(ncid, 'lake_depth',v3id))
    call handle_err (nf90_inq_varid(ncid, 'geolat'    ,vlat))
    call handle_err (nf90_get_var (ncid, v2id, lake_frac, start=start, count=count))
    call handle_err (nf90_get_var (ncid, v3id, lake_depth,start=start, count=count))
    call handle_err (nf90_get_var (ncid, vlat, lat2d,     start=start, count=count))

    do i=1,lon
    do j=1,lat
      if (binary_lake.eq.1) lake_frac(i,j)=nint(lake_frac(i,j))     ! using integer lake_frac
      if (lat2d(i,j).le.-60.) lake_frac(i,j)=0.             ! ignore lakes on Antarctica
      land_frac(i,j)=1.-ocn_frac(i,j)
      if (land_frac(i,j) <    min_land) land_frac(i,j)=0.   ! ignore land  < min_land
      if (land_frac(i,j) > 1.-min_land) land_frac(i,j)=1.   ! ignore water < min_land
      if (1.-land_frac(i,j) > 0.) lake_frac(i,j)=0.         ! ocn dominates

      if (lake_frac(i,j) > 0.) then
        lake_pt=lake_pt+1            ! calculating total lake points
        if (binary_lake.eq.1) then
          land_frac(i,j)=0.
        else
          land_frac(i,j)=1.-lake_frac(i,j)
        end if
        if (lake_depth(i,j) <= 0.) then
          lake_depth(i,j)=def_lakedp ! set missing lake depth to default value
          nodp_pt=nodp_pt+1          ! calculating total lake points without depth
        end if
      else
        lake_depth(i,j)=0.
      end if
       slmsk(i,j) = nint(land_frac(i,j)) ! nint got the land pts correct
    end do
    end do

    write(flnm,'(4a,i1,a)') trim(atmres),'.',trim(ocnres),'.tile',tile,'.nc'
    print *,'output=',trim(flnm)
    call handle_err (nf90_create (path=trim(pth3)//trim(flnm), &
    cmode=or(NF90_CLOBBER, NF90_64BIT_OFFSET), ncid=ncid4))   ! netcdf3

    call handle_err (nf90_def_dim (ncid4,'lon', lon, dims(1)))
    call handle_err (nf90_def_dim (ncid4,'lat', lat, dims(2)))
    call handle_err (nf90_def_var (ncid4,'land_frac', nf90_float, dims(1:2), v1id))
    call handle_err (nf90_def_var (ncid4,'lake_frac', nf90_float, dims(1:2), v2id))
    call handle_err (nf90_def_var (ncid4,'lake_depth',nf90_float, dims(1:2), v3id))
    call handle_err (nf90_def_var (ncid4,'slmsk',     nf90_float, dims(1:2), v4id))

    call handle_err (nf90_enddef  (ncid4))

    call handle_err (nf90_put_var (ncid4, v1id,land_frac))
    call handle_err (nf90_put_var (ncid4, v2id,lake_frac))
    call handle_err (nf90_put_var (ncid4, v3id,lake_depth))
    call handle_err (nf90_put_var (ncid4, v4id,slmsk))
    call handle_err (nf90_close(ncid4))

  end do ! tile
  write(*,'(a,i8,a,i8,a)') 'total lake point ',lake_pt,' where ',nodp_pt,' has no depth'

end program merge_lake_ocnmsk
