C
      SUBROUTINE TRANSR(NR,TDIREC,TDIFFUswr,TDIFFU,ANGLE,RCOVER)
C
C     THIS SUBROUTINE CALCULATES THE DIRECT AND DIFFUSE TRANSMISSION
C     COEFFICIENTS FOR THE RESIDUE LAYERS, GIVEN THE ANGLE BETWEEN THE
C     SURFACE AND THE LINE OF DIRECT APPROACH (SUN-SLOPE ANGLE)
C
C     TRANSMISSION COEFFICIENTS BASED ON TONNES/HA OF RESIDUE PRESENT
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (NODRES=10)
C
      DOUBLE PRECISION TDIREC(NODRES),TDIFFUswr(NODRES),TDIFFU(NODRES)
C
      PARAMETER (DIFRES=0.667D0)
      common /shawp/ pplastic,emitc,emitr(10),emitsp,emits,emitf
     &      ,palbedo,ptransm
C
C**** DETERMINE THE TRANSMISSIVITY TO DIRECT AND DIFFUSE RADIATION
C     BETWEEN RESIDUE NODES
      DO 10 I=1,NR
         IF (ANGLE .LE. 0.0d0) THEN
C           SUN IS ON OR BELOW HORIZON OF LOCAL SLOPE - 
C           SET DIRECT TRANSMISSIVITY TO ZERO.
            TDIREC(I)=0.0
           ELSE
            TDIREC(I)=(1.D0-RCOVER)*SIN(ANGLE)
         END IF
            TDIFFUswr(I)=DIFRES*(1.D0-RCOVER)
            TDIFFU(I)=DIFRES*(1.D0-RCOVER)
   10 CONTINUE
      if (pplastic.gt.0.0d0) then
         TDIREC(1)=(1.D0-pplastic)+pplastic*ptransm    !Liwang 2017
         TDIFFU(1)=1.D0-pplastic
         TDIFFUswr(1)=(1.D0-pplastic)+pplastic*ptransm
      end if
C
      RETURN
      END
C
C***********************************************************************
