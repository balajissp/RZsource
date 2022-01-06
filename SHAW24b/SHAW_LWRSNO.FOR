C
      SUBROUTINE LWRSNO(LWRNET,IN,TSP,TSPDT,WDT)
C
C     THIS SUBROUTINE SUMS THE NET LONG-WAVE RADIATION EXCHANGE
C     FOR A GIVEN SIDE OF THE SNOWPACK (TOP OR BOTTOM).  THE
C     VALUE OF "IN" COMING INTO THE SUBROUTINE IS LONG-WAVE FLUX
C     ENTERING A GIVEN SIDE OF THE SNOWPACK, AND THE VALUE OF "IN"
C     LEAVING THE SUBROUTINE IS THE LONG-WAVE FLUX EMITTED BY THE
C     THE SIDE OF THE SNOWPACK DESIGNATED BY IBOTM.
C     (IBOTM = 1 --> TOP OF SNOW;  IBOTM = NSP --> BOTTOM OF SNOWPACK)
C     Note: since calling routine (LWBSNO) assumes no lw exchange with
C     soil, ibotm set to 1, to compute outgoing radiation, returning
C     only net longwave radiation for the top of the snow surface
C     R.Aiken 4/6/94
C
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (NODSNO=100)
C
      DOUBLE PRECISION LWRNET,LWRTOP,IN,TSP(NODSNO),TSPDT(NODSNO)
C
      PARAMETER (STEFAN=5.6697D-08, IBOTM=1)
      common /shawp/ pplastic,emitc,emitr(10),emitsp,emits,emitf
     &      ,palbedo,ptransm
C
      TSPK = TSP(IBOTM)+273.16d0
      LWRTOP = EMITSP*IN
      OUT = EMITSP*STEFAN
     >    *(TSPK**4.d0+4.d0*WDT*(TSPK**3.d0)*(TSPDT(IBOTM)-TSP(IBOTM)))
      LWRNET = LWRTOP - OUT
      IN = OUT
C
      RETURN
      END
C
C***********************************************************************
