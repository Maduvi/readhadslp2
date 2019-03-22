MODULE gridmod
  !-------------------------------------------------------------------
  !  Created     : Mateo Duque Villegas
  !  Last updated: 2-Feb-2018
  !-------------------------------------------------------------------  
  IMPLICIT NONE
  
CONTAINS
  
    SUBROUTINE latlon(blat,blon,ydegr,xdegr,nlat,mlon,ylat,xlon)

    IMPLICIT NONE

    !! Global namespace
    INTEGER(KIND=4), INTENT(in) :: nlat
    INTEGER(KIND=4), INTENT(in) :: mlon
    REAL(KIND=4),    INTENT(in) :: ydegr
    REAL(KIND=4),    INTENT(in) :: xdegr
    REAL(KIND=4),    INTENT(in) :: blat
    REAL(KIND=4),    INTENT(in) :: blon
    REAL(KIND=8),    DIMENSION(nlat), INTENT(out)  :: ylat
    REAL(KIND=8),    DIMENSION(mlon), INTENT(out)  :: xlon

    !! Local namespace
    INTEGER(KIND=4)                                :: ilon
    INTEGER(KIND=4)                                :: ilat

    !! Longitudes going from -180 to 180
    DO ilon = 1,mlon
       xlon(ilon) = -blon + (ilon-1)*xdegr
    END DO
  
    !! Latitudes going from 90 to -90
    DO ilat = 1,nlat
       ylat(ilat) = blat - ilat*ydegr
    END DO

    RETURN
    
  END SUBROUTINE latlon
  
END MODULE gridmod
