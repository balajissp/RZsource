      SUBROUTINE WEIGHT(N,AVG,BEGIN,END1,WT,WDT)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE CALCULATES THE WEIGHTED AVERAGE OF VARIABLES AT
C     THE BEGINNING AND END OF THE TIME STEP
C
C        WT = WEIGHTING FOR VALUES AT THE BEGINNING OF THE TIME STEP
C        WDT = WEIGHTING FOR VALUES AT THE END OF THE TIME STEP
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       I          L    canopy, snow, residue or soil layer
C       N          I    node num starting from surface material
C       WDT        I    weighting factor for end of time step values
C       WT         I    weightin factor for beginning of time step values
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                       NONE
C
C       CALLED FROM:
C
C       PROGRAMMER:
C
C       VERSION:
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION BEGIN(N), END1(N), AVG(N)
C
      DO 10 I=1,N
         AVG(I) = WT*BEGIN(I) + WDT*END1(I)
   10 CONTINUE
      RETURN
      END
