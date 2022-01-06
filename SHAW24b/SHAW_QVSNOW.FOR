C
      SUBROUTINE QVSNOW(NSP,QVSP,CONV,SLOPE,TSP,ZSP,VAPSP,PRESUR)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE CALCULATES THE VAPOR DIFFUSION IN THE SNOWPACK
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       CONV       I    vapor cond between node i and i+1
C       I          L    canopy, snow, residue or soil layer
C       LF         P    latent heat of fusion
C       NSP        I    num of layers in snowpack
C       NSP1       L    NSP+1
C       P0         P    stand atm pressure at sea level  (101,300 Pa)
C       PRESUR     I    ave atm pressure at site  (N/m^2)
C       QVSP      I/O   vapor flux between snow nodes i and i+1
C       SATV       L    sat vapor density
C       SLOPE     I/O   slope of simulation site
C       TSP        I    temp of canapy layer at time t
C       UGAS       P    universal gas constant
C       VAPSNO     L    vap diffusivity through snow
C       VAPSP      I    vap den of lower bound of snowpack at t (kg/m^3)
C       VAPSPX     P    temp exp for vapor diffusivity through snowpack
C       VDIFSP     P    vap diffusivity of snowpack at stand conditions
C       ZSP        I    depth of node i in the snow from the snowpack top
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                       CONDUC
C                       VSLOPE
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
      PARAMETER (NODSNO=100)
C
      DOUBLE PRECISION QVSP(NODSNO),TSP(NODSNO),ZSP(NODSNO),
     +  VAPSNO(NODSNO),VAPICE(NODSNO),SLOPE(NODSNO),CONV(NODSNO)
      DOUBLE PRECISION LF

      PARAMETER(LF=3.35D5,UGAS=8.3143D0,P0=1.013D5,VDIFSP=9.0D-5,
     +  VAPSPX=14.0D0)
C
C**** DETERMINE THE VAPOR DIFFUSIVITY, DENSITY AND SLOPE OF EACH NODE
      DO 10 I=1,NSP
         VAPSNO(I) = VDIFSP*(P0/PRESUR)*(1.0D0+TSP(I)/273.16D0)**VAPSPX
         CALL VSLOPE (SLOPE(I),SATV,TSP(I))
C        CALCULATE THE SATURATED VAPOR DENSITY OVER ICE
         TMP = TSP(I) + 273.16D0
         HUMID = EXP(0.018D0/(UGAS*TMP)*LF*TSP(I)/TMP)
         VAPICE(I) = HUMID*SATV
         SLOPE(I) = HUMID*SLOPE(I)
   10 CONTINUE
      VAPICE(NSP+1) = VAPSP
      VAPSNO(NSP+1) = VAPSNO(NSP)
      CALL VSLOPE (SLOPE(NSP+1),SATV,TSP(NSP+1))
      SLOPE(NSP+1) = SLOPE(NSP+1)*VAPSP/SATV
C
C**** DETERMINE THE CONDUCTANCE TERM BETWEEN NODES
      NSP1 = NSP+1
      CALL CONDUC (NSP1,ZSP,VAPSNO,CONV)
C
C**** CALCULATE THE VAPOR FLUX BETWEEN EACH NODE
      DO 20 I=1,NSP
         QVSP(I) = CONV(I)*(VAPICE(I)-VAPICE(I+1))
   20 CONTINUE
      RETURN
      END
