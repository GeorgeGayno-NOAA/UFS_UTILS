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
 ! this variable is now renamed as binary_lake and is passed in from the name
 ! list
 ! logical, parameter :: int_lake=.true.  
 ! all instances of int_lake was changed to binary_lake  
  integer :: binary_lake

  character(len=250) :: flnm
  integer :: ncid,ndims,nvars,natts,lat,lon,v1id,v2id,v3id,start(2),count(2),latid,lonid,tile
  integer :: vlat
  real, allocatable :: lake_frac(:,:),lake_depth(:,:),land_frac(:,:),ocn_frac(:,:),slmsk(:,:),lat2d(:,:)

  call read_nml(pth1, pth2, atmres, ocnres, pth3,binary_lake)
  
  print *, pth1

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

    call merge(lon, lat, binary_lake, lat2d, ocn_frac, lake_frac, lake_depth, land_frac, slmsk)

    call write_data(atmres,ocnres,pth3,tile,lon,lat,land_frac, &
                    lake_frac,lake_depth,slmsk)

  end do ! tile

end program merge_lake_ocnmsk
