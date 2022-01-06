C
      SUBROUTINE ENHANC(I,EN,VLC,CLAY,SAT)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE CALCULATES THE ENHANCEMENT COEFFICIENT FOR VAPOR
C     TRANSPORT DUE TO THERMAL GRADIENTS FOR NODE I
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       EN        I/O   enhancement coeff for vapor flux due to thermal grad
C       EN1        P    coeff for calc vapor flux enhancement factor
C       EN2        P    coeff for calc vapor flux enhancement factor
C       EN3        I    exponent for calc vapor flux enhancement factor
C       EN4        P    coeff for calc vapor flux enhancement factor
C       EN5        P    coeff for calc vapor flux enhancement factor
C       I          I    canopy, snow, residue or soil layer
C       SAT        I    sat WC
C       VLC        I    vol liquid content
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
      PARAMETER (MXNOD=300)
C
      DOUBLE PRECISION VLC(MXNOD),CLAY(MXNOD),SAT(MXNOD)
C
      PARAMETER (EN1=9.5D0, EN2=3.0D0, EN4=1.0D0, EN5=4.0D0)
C
      IF (CLAY(I) .LE. 0.02) THEN
C        EN3 BECOMES INFINITELY LARGE WHEN CLAY IS ZERO
C        (SMALLEST CLAY CONTENT IN DATA FROM CASS ET. AL. WAS 2%)
         EN3=SAT(I)*(1.0D0+2.6D0/SQRT(0.02D0))
        ELSE
         EN3=SAT(I)*(1.0D0+2.6D0/SQRT(CLAY(I)))
      END IF
      EXPON=-(EN3*VLC(I)/SAT(I))**EN5
C
C     EXP(-50.) APPROXIMATELY = 0.0D0, BUT ERRORS MAY RESULT IF YOU TRY
C     TO USE VERY SMALL NUMBERS  --->  CHECK IF  < -50.
      IF (EXPON .LE. -50.0D0) THEN
         EXPON=0.0D0
       ELSE
         EXPON=EXP(EXPON)
      END IF
C
C**** CALCULATE ENHANCEMENT FACTOR
      EN = EN1 + EN2*VLC(I)/SAT(I) - (EN1-EN4)*EXPON
      CONTINUE
      RETURN
      END
C
