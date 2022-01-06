      SUBROUTINE KgHaToKg (Kgha,Kg,Areap)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The Subroutine Converts Variables Kg/ha to Kg
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  22/07/2016       D SADHUKHAN          ORIGNIAL CODE
C
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS          t     DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C 1.  Kgha                REAL           Kg/ha               Any Variable in Kg/ha
C 2.  Kg                  REAL           Kg                  Converted variable to Kg
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
      
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN) :: Kgha,Areap
      DOUBLE PRECISION,INTENT(OUT) :: Kg
      Kg = Kgha * Areap
      RETURN
      END SUBROUTINE KghaToKg