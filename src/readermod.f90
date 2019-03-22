MODULE readermod
  !-------------------------------------------------------------------
  !  Created     : Mateo Duque Villegas
  !  Last updated: 2-Feb-2018
  !-------------------------------------------------------------------
  USE calmod
  
  IMPLICIT NONE
  
CONTAINS
  
  SUBROUTINE readerhad(dataf,nyr,nmon,nlat,mlon,dates,gd)

    IMPLICIT NONE

    !! Global namespace
    CHARACTER(LEN=*), INTENT(in) :: dataf
    INTEGER(KIND=4),  INTENT(in) :: nyr
    INTEGER(KIND=4),  INTENT(in) :: nmon
    INTEGER(KIND=4),  INTENT(in) :: nlat
    INTEGER(KIND=4),  INTENT(in) :: mlon
    INTEGER(KIND=4),  DIMENSION(nyr*nmon), INTENT(out) :: dates
    REAL(KIND=8),     DIMENSION(mlon,nlat,nmon,nyr), INTENT(out) :: gd

    !! Local namespace
    INTEGER(KIND=4) :: unit = 11  ! Reading unit
    INTEGER(KIND=4) :: cindex
    INTEGER(KIND=4) :: iyear
    INTEGER(KIND=4) :: imon
    INTEGER(KIND=4) :: ilon
    INTEGER(KIND=4) :: ilat
    INTEGER(KIND=4) :: days
    INTEGER(KIND=4) :: operr      ! Error variable
    INTEGER(KIND=4) :: rderr      ! Error variable
    INTEGER(KIND=4), DIMENSION(2)         :: header
    INTEGER(KIND=4), DIMENSION(mlon,nlat) :: idata

    !! Open file and check for errors
    OPEN(unit,file=dataf,form='FORMATTED',status='old',action='read'&
         &,iostat=operr)
    IF(operr>0) THEN
       WRITE(*,'(A)') "reader: error 1: could not open file."
       CALL EXIT(0)
    END IF

    !! Loop through data and store in gd
    cindex = 1
    DO iyear = 1,nyr
       DO imon = 1,nmon
          !! Read header
          READ(unit,'(2I7)',iostat=rderr) header
          IF(rderr>0) THEN
             WRITE(*,'(A)') "reader: error 2: could not read file."
             CALL EXIT(0)
          END IF
          
          !! Calculate number of days as date and store
          CALL getdays(header(1),header(2),days)
          dates(cindex) = days
          
          !! Store data current data
          READ(unit,'(72i8)',iostat=rderr) idata
          IF(rderr>0) THEN
             WRITE(*,'(A)') "reader: error 2: could not read file."
             CALL EXIT(0)
          END IF
          
          !! Save as a grid array
          DO ilat = 1,nlat
             DO ilon = 1,mlon
                gd(ilon,ilat,imon,iyear) = (idata(ilon,ilat)*0.01)
             END DO
          END DO
          cindex = cindex + 1
       END DO
    END DO

    CLOSE(unit)
    
    RETURN
    
  END SUBROUTINE readerhad
         
END MODULE readermod
