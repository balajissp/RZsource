      SUBROUTINE HEATUNIT(ID,IM,IYYY)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The Subroutine CALCULATES THE HEAT UNIT OF A PARTICULAR DAY 
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
C 
C
C FUNCTIONS/SUBROUTINE CALLED ::
C
C       NAME                    DESCRIPTION
C    ----------              ------------------           
C 
C
C===============================================================================
C                         END OF SPECIFICATION
C===============================================================================   
      USE VARIABLE
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: ID,IM,IYYY
      INTEGER :: Day,Month,Year
       DAY = ID
       Month = IM
       Year = IYYY
      
        IF(Tempt>Basetemp(Slnc)) THEN
          Hu(Slnc) = Hu(Slnc) + (Tempt- Basetemp(Slnc)) 
        END IF
      
      
      END SUBROUTINE HEATUNIT