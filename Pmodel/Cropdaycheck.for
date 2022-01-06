      SUBROUTINE CROPDAYCHECK(ID,IM,IYYY)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The Subroutine checks if on a particularday there is standing crop on the 
C     field or not
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  12/08/2016       D SADHUKHAN          ORIGNIAL CODE
C
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C     i                  INTEGER            -                 Loop Counter 
C
C FUNCTIONS/SUBROUTINE CALLED ::
C
C       NAME                    DESCRIPTION
C    ----------              ------------------           
C 1. 
C
C===============================================================================
C                         END OF SPECIFICATION
C===============================================================================      
       USE VARIABLE
       IMPLICIT NONE
       INTEGER :: i
       INTEGER,INTENT(IN) :: ID,IM,IYYY
       INTEGER :: Day,Month,Year
       DAY = ID
       Month = IM
       Year = IYYY
       DO i=1, Ncrop
          IF(Day==Cpday(i).AND.Month==Cpmon(i).AND.Year==Cpyear(i))THEN
          Cropday = .TRUE.
          Slnc = i
          END IF
         IF(Day==Chday(i).AND.Month==Chmon(i).AND.Year==Chyear(i)) THEN
          Cropday = .FALSE. 
          Pstress(Slnc) = 1.0
         END IF 
       END DO
       
       END SUBROUTINE CROPDAYCHECK