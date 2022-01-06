C
      SUBROUTINE VSLOPE(S,SATV,T)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE CALCULATES THE SATURATED VAPOR DENSITY AND THE
C     SLOPE OF THE VAPOR DENSITY CURVE  (SATV IS IN KG/M**3)
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       S         I/O   slope of sat vapor density curve  (kg/m^3-K)
C       SATV      I/O   sat vapor density
C       T          I    temp
C       UGAS       P    universal gas constant
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
      PARAMETER (UGAS=8.3143D0)
      logical tmp1
C
      TMP=T+273.16D0
      
c      tmp1=isnan(tmp)
c       if (tmp1.or.tmp.lt.0.0d0) then
c	return
c	endif
c	if(t.gt.150.or.t.lt.-50.)t=t0
      SATV=EXP(52.57633D0-6790.4985D0/TMP-5.02808D0*LOG(TMP))
     +     *1.0D3/(UGAS*TMP/0.018D0)
      S = 1.65D-5 + 4944.43D0*SATV/(TMP**2.0D0)
c	t0=t
      RETURN
      END
