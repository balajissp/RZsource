      SUBROUTINE STMMLT(SPMELT,STRMLT,DELT,DAYTIM,COR)
C
C======================================================================
C
C       PURPOSE:  THIS ROUTINE DIRECTS THE DIRECTION SNOW MELT TAKES.
C                 IT IS ACCUMULATED FOR THE INFILTRATION MODEL AT
C                 THE END OF THE DAY.
C
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       DELT       I    TIME STEP (HR)
C       STRMLT     O    STORM MATRIX FOR SNOW MELT
C       SPMELT     I    SNOWPACK MELT (CM)
C
C
C       EXTERNAL REFERENCES:
C                       NONE
C
C       CALLED FROM:  PHYSCL
C
C       PROGRAMMER:   KAREN JOHNSEN
C
C       VERSION:      1.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION STRMLT(2)
      DATA SUMTM/0.0D0/, SUMSP/0.0D0/
      save sumtm, sumsp
C
C     ..ACCUMULATE SNOW MELT
      IF (SPMELT .GT. 0.0D0) THEN
        SUMTM = SUMTM + DELT
        SUMSP = SUMSP + SPMELT
      ENDIF
C
      IF (SUMSP .GT. 0.0D0) THEN
C
C       ..SEND TO INFIL AT END OF DAY OR WHEN SUFFICIENTLY ACCUMULATED
c        IF (SUMSP .GT. 2.0D0 .OR. abs((DAYTIM+DELT)-24.0D0).lt.COR) THEN
         IF (SUMSP .GT. 2.0D0 .OR.(DAYTIM+DELT).GE. (24.0D0-COR)) then
          STRMLT(1) = SUMTM
          STRMLT(2) = SUMSP
          SUMTM = 0.0D0
          SUMSP = 0.0D0
        ENDIF
      ENDIF
C
C
      RETURN
      END
