      SUBROUTINE KgToKgHa (Kg,Kgha,Areap)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The Subroutine Converts Variables Kg to Kg/Ha
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
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C 1.  Kg               REAL                Kg                     Any Variable in Kg
C 2.  Kgha                  REAL           Kg/ha                  Converted variable to Kg/ha
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
      DOUBLE PRECISION, INTENT(OUT) :: Kgha
      DOUBLE PRECISION,INTENT(IN) :: Kg,Areap
      Kgha = Kg / Areap
      RETURN
      END SUBROUTINE KgToKgHa