MODULE ncmod
  !-------------------------------------------------------------------
  !  Created     : Mateo Duque Villegas
  !  Last updated: 2-Feb-2018
  !-------------------------------------------------------------------  
  IMPLICIT NONE
  
CONTAINS
  
  SUBROUTINE ncgen(ncfile,nyr,nmon,nlat,mlon,vtime,ylat,xlon,ncdata)

    USE netcdf
    
    IMPLICIT NONE

    ! Global namespace. Var description in main program
    CHARACTER(LEN=*), INTENT(IN) :: ncfile
    INTEGER(KIND=4),  INTENT(IN) :: nyr
    INTEGER(KIND=4),  INTENT(IN) :: nmon
    INTEGER(KIND=4),  INTENT(IN) :: nlat
    INTEGER(KIND=4),  INTENT(IN) :: mlon
    INTEGER(KIND=4), DIMENSION(nyr*nmon),           INTENT(IN) :: vtime
    REAL(KIND=8),    DIMENSION(nlat),               INTENT(IN) :: ylat
    REAL(KIND=8),    DIMENSION(mlon),               INTENT(IN) :: xlon
    REAL(KIND=8),    DIMENSION(mlon,nlat,nmon*nyr), INTENT(IN) :: ncdata

    ! Local namespace
    CHARACTER(LEN=50) :: lname
    CHARACTER(LEN=50) :: timestr
    CHARACTER(LEN=15) :: units
    CHARACTER(LEN=10) :: time
    CHARACTER(LEN=8)  :: date  
    CHARACTER(LEN=4)  :: sname
        
    ! NetCDF stuff
    INTEGER(KIND=4) :: ncid     ! Unit ID for netCDF
    INTEGER(KIND=4) :: t_dimid  ! ID for time dimension
    INTEGER(KIND=4) :: x_dimid  ! ID for X dimension
    INTEGER(KIND=4) :: y_dimid  ! ID for Y dimension
    INTEGER(KIND=4) :: varid    ! ID for data variable
    INTEGER(KIND=4) :: t_varid  ! ID for time variable
    INTEGER(KIND=4) :: x_varid  ! ID for X variable
    INTEGER(KIND=4) :: y_varid  ! ID for Y variable
    INTEGER(KIND=4), DIMENSION(3) :: dimids  ! ID for all dimensions
    
    ! Create the netCDF file and assign unit.
    CALL check(nf90_create(ncfile, NF90_CLOBBER, ncid))

    ! Define the dimensions numbers.
    CALL check(nf90_def_dim(ncid, "time", 0, t_dimid))
    CALL check(nf90_def_dim(ncid, "lon", mlon, x_dimid))
    CALL check(nf90_def_dim(ncid, "lat", nlat, y_dimid))

    ! Define coordinate variable LON and attributes
    CALL check(nf90_def_var(ncid, "time", NF90_INT, t_dimid,&
         & t_varid))
    CALL check(nf90_put_att(ncid, t_varid, "standard_name","time"))
    CALL check(nf90_put_att(ncid, t_varid, "units", "days since 1850&
         &-01-01 12:00:00"))
    CALL check(nf90_put_att(ncid, t_varid, "calendar","standard"))
    CALL check(nf90_put_att(ncid, t_varid, "axis","T"))

    ! Define coordinate variable LON and attributes
    CALL check(nf90_def_var(ncid, "lon", NF90_REAL, x_dimid, x_varid))
    CALL check(nf90_put_att(ncid, x_varid, "standard_name", "longitude"))
    CALL check(nf90_put_att(ncid, x_varid, "long_name", "longitude"))
    CALL check(nf90_put_att(ncid, x_varid, "units", "degrees_east"))
    CALL check(nf90_put_att(ncid, x_varid, "axis", "X"))
    
    ! Define coordinate variable LAT and attributes
    CALL check(nf90_def_var(ncid, "lat", NF90_REAL, y_dimid, y_varid))
    CALL check(nf90_put_att(ncid, y_varid, "standard_name", "latitude"))
    CALL check(nf90_put_att(ncid, y_varid, "long_name", "latitude"))
    CALL check(nf90_put_att(ncid, y_varid, "units", "degrees_north"))
    CALL check(nf90_put_att(ncid, y_varid, "axis", "Y"))

    ! Dims for data array
    dimids = (/ x_dimid, y_dimid, t_dimid /)
    
    ! Define variable
    CALL check(nf90_def_var(ncid, "slp", NF90_FLOAT, dimids&
         &, varid))
    CALL check(nf90_put_att(ncid, varid, "standard_name", "Sea level p&
         &ressure")) 
    CALL check(nf90_put_att(ncid, varid, "long_name", "Hadley centre&
         & sea level pressure dataset (HadSLP2)"))
    CALL check(nf90_put_att(ncid, varid, "units", "hPa"))
    CALL check(nf90_put_att(ncid, varid, "_FillValue", -999.9))
    CALL check(nf90_put_att(ncid, varid, "missing_value", -999.9))

    ! Global attributes
    CALL DATE_AND_TIME(date,time)
    
    timestr = "Created by Mateo on " //date(1:4) //"-"//date(5:6)//&
         &"-"//date(7:8)//" "//time(1:2)//":"//time(3:4)//":"//&
         &time(5:6)
    
    CALL check(nf90_put_att(ncid,NF90_GLOBAL, "history", timestr))
    CALL check(nf90_put_att(ncid,NF90_GLOBAL, "source", "Met Office Ha&
         &dley Centre Observation Datasets"))
    CALL check(nf90_put_att(ncid,NF90_GLOBAL, "institution", "Universi&
         &dad de Antioquia"))
    CALL check(nf90_put_att(ncid,NF90_GLOBAL, "Conventions", "CF&
         &-1.0"))
    
    ! End definitions
    CALL check(nf90_enddef(ncid))
    
    ! Write Data
    CALL check(nf90_put_var(ncid, t_varid, vtime))
    CALL check(nf90_put_var(ncid, x_varid, xlon))
    CALL check(nf90_put_var(ncid, y_varid, ylat))
    CALL check(nf90_put_var(ncid, varid, ncdata))
    CALL check(nf90_close(ncid))

    RETURN    
  END SUBROUTINE ncgen
  
  SUBROUTINE check(istatus)
    
    USE netcdf
    
    IMPLICIT NONE
    
    INTEGER(KIND=4), INTENT (IN) :: istatus

    ! Check for errors everytime run netcdf library stuff
    IF (istatus /= nf90_noerr) THEN   
       WRITE(*,*) TRIM(ADJUSTL(nf90_strerror(istatus)))
       CALL EXIT(0)
    END IF

    RETURN
  END SUBROUTINE check

END MODULE ncmod
