MODULE calmod
  !-------------------------------------------------------------------
  !  Created     : Mateo Duque Villegas
  !  Last updated: 2-Feb-2018
  !-------------------------------------------------------------------
  IMPLICIT NONE
  
CONTAINS
  
  SUBROUTINE getdays(yr,mon,days)

    !! Global namespace
    INTEGER(KIND=4), INTENT(in)  :: yr
    INTEGER(KIND=4), INTENT(in)  :: mon
    INTEGER(KIND=4), INTENT(out) :: days

    !! Local namespace
    INTEGER(KIND=4) :: i
    INTEGER(KIND=4) :: cyear
    INTEGER(KIND=4) :: nextlp
    INTEGER(KIND=4) :: yrdiff
    INTEGER(KIND=4), PARAMETER    :: baseyr = 1850
    INTEGER(KIND=4), PARAMETER    :: frstlp = 1852
    INTEGER(KIND=4), DIMENSION(4) :: mon30 = (/4,6,9,11/)
    INTEGER(KIND=4), DIMENSION(7) :: mon31 = (/1,3,5,7,8,10,12/)

    !! Make sure some things are cool
    IF (yr < 1850) THEN
       STOP "calmod: error: date must be at least 1850."
    END IF

    IF (mon < 1 .or. mon > 12) THEN
       STOP "calmod: error: number of month must be between 1-12."
    END IF
    
    days = 0
    yrdiff = yr - baseyr
    nextlp = frstlp
    
    !! First get the number of yearly days
    DO i = 1,yrdiff
       IF (cyear == nextlp) THEN
          days = days + 366
          nextlp = nextlp + 4
       ELSE
          days = days + 365
       END IF
       cyear = baseyr + i
    END DO

    !! Now get the number of days from months (not the last)
    DO i = 1,mon-1
       IF (ANY(mon30 == i)) THEN
          days = days + 30
       ELSE IF (ANY(mon31 == i)) THEN
          days = days + 31
       ELSE IF (i == 2) THEN
          IF (cyear == nextlp) THEN
             days = days + 29
          ELSE
             days = days + 28
          END IF
       END IF
    END DO
             
    RETURN
    
  END SUBROUTINE getdays
  
END MODULE calmod
