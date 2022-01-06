C
      SUBROUTINE RESHT(NR,CRES,GMC,RHOR,PPLASTIC)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBOUTINE CALCULATES THE VOLUMETRIC SPECIFIC HEAT FOR EACH
C     RESIDUE NODE.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       CL         P    spec heat cap of liquid water  (J/kg-K)
C       CR         P    spec heat cap of residue  (J/kg-K)
C       CP         P    spec heat cap of plastic  (J/kg-K)
C       CRES      I/O   vol spec heat of residue plus absorbed water
C       GMC        I    grav water content of i-th residue node at time t
C       I          L    canopy, snow, residue or soil layer
C       NR         I    num of matted residue layers
C       RHOR       I    density of residue in matted layer  (kg/m^3)
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
      PARAMETER (NODRES=10)
C
      DOUBLE PRECISION CRES(NODRES),GMC(NODRES),RHOR(NODRES)
      PARAMETER (CL=4.2D3, CR=1.9D3, CP=1.0D3)
C
      DO 10 I=1,NR
         CRES(I)=RHOR(I)*(CR + GMC(I)*CL)
   10 CONTINUE
         IF (PPLASTIC.GT.0.0D0) CRES(1)=RHOR(1)*CP
      RETURN
      END
C
