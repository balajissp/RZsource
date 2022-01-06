C
      SUBROUTINE LWRRES(LWRES,IN,NSTART,NSTOP,NCOUNT,TR,TRDT,WDT,
     +                   TDIFFU)
C
C     THIS SUBROUTINE SUMS THE NET LONG-WAVE RADIATION EXCHANGE
C     FOR A GIVEN SIDE OF EACH RESIDUE LAYER (ABOVE OR BELOW).  THE
C     VALUE OF "IN" COMING INTO THE SUBROUTINE IS LONG-WAVE FLUX
C     ENTERING A GIVEN SIDE OF THE RESIDUE, AND THE VALUE OF "IN"
C     LEAVING THE SUBROUTINE IS THE LONG-WAVE FLUX LEAVING THE RESIDUE
C     ON THE OPPOSITE SIDE.
C
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (NODRES=10)
C
      DOUBLE PRECISION IN,LWRES(NODRES),TR(NODRES),TRDT(NODRES),
     +  TDIFFU(NODRES)
C
      PARAMETER (STEFAN=5.6697D-08)
      common /shawp/ pplastic,emitc,emitr(10),emitsp,emits,emitf
     &      ,palbedo,ptransm
C
      DO 10 I=NSTART,NSTOP,NCOUNT
         TRK=TR(I)+273.16d0
         OUT=(1-TDIFFU(I))*EMITR(I)*STEFAN
     >        *(TRK**4.d0+4.d0*WDT*(TRK**3.d0)*(TRDT(I)-TR(I)))
         LWRES(I)=LWRES(I) + (1.d0-TDIFFU(I))*EMITR(I)*IN - OUT
         IN = TDIFFU(I)*IN + OUT
   10 CONTINUE
C
      RETURN
      END
C***********************************************************************
