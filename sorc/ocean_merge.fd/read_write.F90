!> Write the merged data to a NetCDF file. Each tile is in
!! its own file.
!!
!! @param[in] atmres Atmospheric resolution.
!! @param[in] ocnres Ocean resolution.
!! @param[in] pth3 Diretory path to output file.
!! @param[in] tile Tile number.
!! @param[in] lon E/W dimension of tile.
!! @param[in] lat N/S dimension of tile.
!! @param[in] land_frac Land fraction.
!! @param[in] lake_frac Lake fraction.
!! @param[in] lake_depth Lake depth.
!! @param[in] slmsk Land/sea mask.
!!
!! @author Shan Sun
!! @author Rahul Mahajan
!! @author Sanath Kumar
 subroutine write_data(atmres,ocnres,pth3,tile,lon,lat,land_frac, &
                       lake_frac,lake_depth,slmsk)

 use netcdf

 implicit none

 character(len=*), intent(in)       :: atmres, ocnres, pth3
 
 integer, intent(in)                :: tile, lon, lat

 real, intent(in)                   :: land_frac(lon,lat), lake_frac(lon,lat)
 real, intent(in)                   :: lake_depth(lon,lat), slmsk(lon,lat)

 character(len=250) :: flnm

 integer            :: ncid4, dims(2), v1id, v2id, v3id, v4id
 
 write(flnm,'(4a,i1,a)') trim(atmres),'.',trim(ocnres),'.tile',tile,'.nc'
 print *,'output data to file= ',trim(flnm)
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

 end subroutine write_data
