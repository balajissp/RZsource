C
      SUBROUTINE QVSOIL(NS,QSV,TS,MAT,VLC,VIC,CONC,PRESUR,CLAY,RHOB,
     +                   SAT,ZS,NSALT)

C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE CALCULATES THE VAPOR FLUX BETWEEN SOIL NODES DUE
C     TO GRADIENTS IN POTENTIAL AND TEMPERATURE GRADIENTS. (FLUX DUE TO
C     TEMPERATURE GRADIENTS ARE MULTIPLIED BY AN ENHANCEMENT FACTOR.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       V.ARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       CONC       I    conc of j-th solute in soil soln at time t (eq/kq)
C       CONVP      L    vapor cond for pressure grad between node i and i+1
C       CONVT      L    vapor cond for temp grad between node i and i+1
C       DVP        L    vapor diffusion coeff for pressure grad in soil
C       DVT        L    vapor diffusion coeff for temp grad in layer i
C       EN         L    enhancement coeff for vapor flux due to thermal grad
C       G          P    acceleration of gravity
C       I          L    canopy, snow, residue or soil layer
C       J          L    specifies solute species in soil or plant species
C       MAT        I    matric potential  (m)
C       NS         I    num of soil layers
C       NSALT      I    num of different types of solutes to be simulated
C       P0         P    stand atm pressure at sea level  (101,300 Pa)
C       PRESUR     I    ave atm pressure at site  (N/m^2)
C       QSV       I/O   vapor flux between soil node i and i+1
C       RHOB       I    bulk density of soil  (kg/m^3)
C       RHOM       P    density of the soil (particle density) kg/m^3
C       S          L    slope of sat vapor density curve  (kg/m^3-K)
C       SAT        I    sat WC
C       SATVAP     L    sat vapor density
C       TOTPOT     L    total water potential
C       TS         I    temp of soil layer at time t
C       UGAS       P    universal gas constant
C       VAPCOF     L    coeff for calc vapor diffusivity in soil layer
C       VAPEXP     L    exponent for calc vapor diffusivity in soil layer
C       VDIFF      P    vap diffusivity in still air
C       VIC        I    vol ice content
C       VLC        I    vol liquid content
C       ZS         I    depth of node i in the soil from the soil surface
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                       CONDUC
C                       ENHANC
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
      PARAMETER (MXNOD=300, MXCHEM=15)
C
      DOUBLE PRECISION QSV(MXNOD),TS(MXNOD),MAT(MXNOD),VLC(MXNOD),
     +  VIC(MXNOD),CONC(MXCHEM,MXNOD),HUMID(MXNOD),DVT(MXNOD),
     +  DVP(MXNOD),CONVT(MXNOD),CONVP(MXNOD),VAPCOF(MXNOD),
     +  VAPEXP(MXNOD),RHOB(MXNOD),CLAY(MXNOD),SAT(MXNOD),ZS(MXNOD)
      PARAMETER(G=9.81D0,UGAS=8.3143D0,RHOM=2.65D3,VDIFF=2.12D-5,
     +  P0=1.013D5,RHOL=1.0D3,RHOI=9.2D2)
      SAVE VAPCOF,VAPEXP
C
      DATA  VAPCOF/MXNOD*0.66D0/ VAPEXP/MXNOD*1.0D0/
C
      DO 20 I=1,NS
ckej
         VAC=sat(i)-VIC(I)*rhoi/rhol-VLC(I)
c         VAC=1.0D0 - RHOB(I)/RHOM - VLC(I) - VIC(I)
         IF (VAC .LE. 0.0D0) THEN
C           NO AIR POROSITY --> NO VAPOR DIFFUSION
            DVT(I)=0.0D0
            DVP(I)=0.0D0
            HUMID(I)=1.0D0
            GO TO 20
         END IF
         DV=VDIFF*(((TS(I)+273.16D0)/273.16D0)**2.0D0)*(P0/PRESUR)
         DV=DV*VAPCOF(I)*(VAC**VAPEXP(I))
         CALL ENHANC (I,EN,VLC,CLAY,SAT)
         CALL VSLOPE (S,SATVAP,TS(I))
C
C****    DETERMINE THE HUMIDITY FROM THE TOTAL WATER POTENTIAL
         TLCONC=0.0D0
         DO 10 J=1,NSALT
            TLCONC= TLCONC+CONC(J,I)
   10    CONTINUE
         TOTPOT=MAT(I) - TLCONC*UGAS*(TS(I)+273.16D0)/G
         HUMID(I)=EXP(0.018D0*G/UGAS/(TS(I)+273.16D0)*TOTPOT)
         DVT(I)=DV*EN*HUMID(I)*S
         DVP(I)=DV*SATVAP
   20 CONTINUE
      CALL CONDUC (NS,ZS,DVT,CONVT)
      CALL CONDUC (NS,ZS,DVP,CONVP)
      DO 30 I=1,NS-1
         QSV(I)=CONVT(I)*(TS(I)-TS(I+1))+ CONVP(I)*(HUMID(I)-HUMID(I+1))
   30 CONTINUE
      RETURN
      END
