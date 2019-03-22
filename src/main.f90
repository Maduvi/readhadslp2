PROGRAM main
  !-------------------------------------------------------------------
  !  Created     : Mateo Duque Villegas
  !  Last updated: 2-Feb-2018
  !-------------------------------------------------------------------
  USE readermod
  USE gridmod
  USE calmod
  USE ncmod

  IMPLICIT NONE
  
  CHARACTER(LEN=100) :: arg1
  CHARACTER(LEN=100) :: dataf
  CHARACTER(LEN=100) :: ncfile
  INTEGER(KIND=4)    :: cindex
  INTEGER(KIND=4)    :: iyear
  INTEGER(KIND=4)    :: imon
  INTEGER(KIND=4)    :: nyr
  INTEGER(KIND=4)    :: nmon
  INTEGER(KIND=4)    :: nlat
  INTEGER(KIND=4)    :: mlon
  REAL(KIND=4)       :: ydegr
  REAL(KIND=4)       :: xdegr
  REAL(KIND=4)       :: blat
  REAL(KIND=4)       :: blon
  REAL(KIND=8),    DIMENSION(:),       ALLOCATABLE :: ylat
  REAL(KIND=8),    DIMENSION(:),       ALLOCATABLE :: xlon
  INTEGER(KIND=4), DIMENSION(:),       ALLOCATABLE :: dates
  REAL(KIND=8),    DIMENSION(:,:,:,:), ALLOCATABLE :: gd
  REAL(KIND=8),    DIMENSION(:,:,:),   ALLOCATABLE :: ncdata

  !! Specific HadSLP2 stuff
  nyr = 155
  nmon = 12
  nlat = 37
  mlon = 72
  ydegr = 5.0
  xdegr = 5.0
  blat = 95.0
  blon = 180.0
  
  !! Allocate fields
  ALLOCATE(ylat(nlat))
  ALLOCATE(xlon(mlon))
  ALLOCATE(dates(nyr*nmon))
  ALLOCATE(gd(mlon,nlat,nmon,nyr))
  ALLOCATE(ncdata(mlon,nlat,nmon*nyr))
  
  !! File name from command line
  CALL GET_COMMAND_ARGUMENT(1, arg1)
  dataf = TRIM(arg1)

  !! Read formatted asc file
  CALL readerhad(dataf,nyr,nmon,nlat,mlon,dates,gd)

  !! Get lat lon
  CALL latlon(blat,blon,ydegr,xdegr,nlat,mlon,ylat,xlon)

  !! Convert array
  cindex = 1
  DO iyear = 1,nyr
     DO imon = 1,nmon
        ncdata(:,:,cindex) = gd(:,:,imon,iyear)
        cindex = cindex + 1
     END DO
  END DO

  !! Output netCDF filename
  ncfile = TRIM(ADJUSTL(dataf))//".nc"

  !! Output netCDF
  CALL ncgen(ncfile,nyr,nmon,nlat,mlon,dates,ylat,xlon,ncdata)
  
     
END PROGRAM main
