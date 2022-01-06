C***********************************************************************
C
      SUBROUTINE QVRES(NR,QVR,CONV,VAPR,VAPRDT,WT,WDT)
C
C     THIS SUBOUTINE CALCULATES VAPOR FLUX BETWEEN RESIDUE NODES.
C
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (NODRES=10)
C
      DOUBLE PRECISION QVR(NODRES),CONV(NODRES),VAPR(NODRES),
     +  VAPRDT(NODRES)
      common /shawp/ pplastic,emitc,emitr(10),emitsp,emits,emitf
     &      ,palbedo,ptransm
C
      DO 10 I=1,NR
         QVR(I) = CONV(I)*(WT*(VAPR(I)-VAPR(I+1))
     >                 + WDT*(VAPRDT(I)-VAPRDT(I+1)))
c      IF (PPLASTIC.GT.0.0D0) QVR(I)=QVR(I)*(1.0D0-PPLASTIC)
   10 CONTINUE
      RETURN
      END
