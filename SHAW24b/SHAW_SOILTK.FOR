C
c rma\gf  substituted G. Flerchinger routine
      SUBROUTINE SOILTK(NS,TK,VLC,VIC,RHOB,sand,silt,clay,om)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE CALCULATES THE THERMAL CONDUCTIVITY OF THE SOIL
C     LAYERS USING DE VRIES'S METHOD.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       CLAY       I    clay fraction in soil layer
c rma
C       GAair      P    DeVries' ellipsoid shape factor (ga) for air
C       GAC        P    DeVries' ellipsoid shape factor (ga) for clay
C       GAI        P    DeVries' ellipsoid shape factor (ga) for ice
C       GASI       P    DeVries' ellipsoid shape factor (ga) for silt
C       GAOM       P    DeVries' ellipsoid shape factor (ga) for org mtr
C       GASA       P    DeVries' ellipsoid shape factor (ga) for sand
C       I          L    canopy, snow, residue or soil layer
C       NS         I    num of soil layers
C       OM         I    organic matter fraction in soil layer
C       RHOB       I    bulk density of soil  (kg/m^3)
C       RHOM       P    density of the soil (particle density) kg/m^3
C       SAND       I    sand fraction in soil layer
C       SILT       I    silt fraction in soil layer
C       TK        I/O   ave thermal cond over the time step
C       TKA        P    thermal cond of air
C       TKI        P    thermal cond of ice
C       TKL        P    thermal cond of liquid
C       TKCL       P    thermal cond of clay fraction
C       TKSA       P    thermal cond of sand fraction
C       TKSI       P    thermal cond of silt fraction
C       TKOM       P    thermal cond of organic matter
C       VIC        I    vol ice content
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
      DOUBLE PRECISION TK(MXNOD),VLC(MXNOD),VIC(MXNOD),RHOB(MXNOD),
     +  SAND(MXNOD),SILT(MXNOD),CLAY(MXNOD),OM(MXNOD)
C
      PARAMETER (RHOM=2650.0D0,RHOA=1.25D0,RHOOM=1300.D0,
     +           TKL=0.57D0,TKI=2.2D0,TKA=0.025D0,
     +           TKSA=8.8d0,TKSI=2.92d0,TKCL=2.92d0,TKOM=0.25d0)
c rma
C     WEIGHTING FACTOR FOR ICE IS NOW SET TO 1.0 TO CORRECT PROBLEMS 
C     POINTED OUT BY KENNEDY & SHARRATT, SOIL SCI. 163(8):636-645.
c     PARAMETER (GAI=0.33D0,GAOM=0.5D0,GASA=0.144D0,GASI=0.144D0,
      PARAMETER (GAOM=0.5D0,GASA=0.144D0,GASI=0.144D0,
     +           GAC=0.125D0,gaair_lim=0.035d0)
C
C     DEFINE FUNCTION FOR CALCULATING DEVRIES WEIGHTING FACTORS
      DeVWF(TK0,TK1,GA) = 2.0/3.0/(1.0+(TK1/TK0-1.0)*GA)
     >                   + 1.0/3.0/(1.0+(TK1/TK0-1.0)*(1.0-2.0*GA))
C
      DATA IFIRST/ 0/
C
      IF (IFIRST .EQ. 0) THEN
C        FIRST TIME INTO SUBROUTINE -- CALCULATE WEIGHTING FACTORS FOR
C        SOIL COMPONENTS WHEN SOIL IS COMPLETELY DRY AND WHEN WET
         WFAIRD = 1.0
         WFSAD = DeVWF(TKA,TKSA,GASA)
         WFSID = DeVWF(TKA,TKSI,GASI)
         WFCLD = DeVWF(TKA,TKCL,GAC)
         WFOMD = DeVWF(TKA,TKOM,GAOM)
         WFICED = 1.0d0
C
C        SET THERMAL CONDUCTIVITY OF MOIST AIR EQUAL TO THAT OF DRY AIR
C        (VAPOR TRANSFER IS CALCULATED SEPARATELY AND INCLUDED IN HEAT
C        TRANSFER THROUGH SOIL; THEREFORE IT SHOULD NOT BE COMPENSATED
C        FOR IN COMPUTATION FOR THERMAL CONDUCTIVITY AS IS USUALLY DONE
C        WHEN USING DEVRIES METHOD.)
         TKMA = TKA
         WFL = 1.0
         WFSA = DeVWF(TKL,TKSA,GASA)
         WFSI = DeVWF(TKL,TKSI,GASI)
         WFCL = DeVWF(TKL,TKCL,GAC)
         WFOM = DeVWF(TKL,TKOM,GAOM)
         WFICE = 1.0d0
      END IF
C
      DO 10 I=1,NS
C       CORRECTION OF TEXTURE FOR ORGANIC MATTER
        XSM = 1.0 - OM(I)
        XSAND=XSM*SAND(I)
        XSILT=XSM*SILT(I)
        XCLAY=XSM*CLAY(I)
C
C       CONVERT WEIGHT FRACTION TO VOLUMETRIC FRACTION
        VSAND = XSAND*RHOB(I)/RHOM
        VSILT = XSILT*RHOB(I)/RHOM
        VCLAY = XCLAY*RHOB(I)/RHOM
        VOM = OM(I)*RHOB(I)/RHOOM
        PORO = 1.-VSAND-VSILT-VCLAY-VOM
        VAC = PORO-VIC(I)-VLC(I)
        IF (VAC .LT. 0.0) VAC=0.0
C
C       DETERMINE LIMIT OF DEVRIES METHOD FROM TEXTURE
C       SAND ==> 0.05;  LOAM ==> 0.10;  CLAY ==> 0.15
        VLMT = 0.10 + 0.2*CLAY(I) - 0.1*SAND(I)
        IF (VLMT .LT. 0.05) VLMT = 0.05
        IF (VLMT .GT. 0.15) VLMT = 0.15
C
        IF (VLC(I) .GT. VLMT) THEN
C         MOIST SOIL -- USE DEVRIES METHOD DIRECTLY
          GAAIR = 0.035+0.298*(VLC(I)-VLMT)/(PORO-VLMT)
          WFAIR= DeVWF(TKL,TKMA,GAAIR)
          TK(I)=(WFSA*VSAND*TKSA + WFL*VLC(I)*TKL + WFICE*VIC(I)*TKI
     >    + WFAIR*VAC*TKMA + WFSI*VSILT*TKSI + WFCL*VCLAY*TKCL
     >    + WFOM*VOM*TKOM)  /  (WFSA*VSAND + WFL*VLC(I) + WFICE*VIC(I)
     >    + WFAIR*VAC + WFSI*VSILT + WFCL*VCLAY + WFOM*VOM)
        ELSE
C         INTERPOLATE THERMAL CONDUCTIVITY BETWEEN WATER CONTENT AT 0
C         AND LIMIT OF DEVRIES METHOD
          TK0=1.25*(WFSAD*VSAND*TKSA + WFICED*VIC(I)*TKI 
     >    + WFAIRD*(VAC+VLC(I))*TKA
     >    + WFSID*VSILT*TKSI + WFCLD*VCLAY*TKCL + WFOMD*VOM*TKOM)
     >    /(WFSAD*VSAND + WFICED*VIC(I) + WFAIRD*(VAC+VLC(I))
     >    + WFSID*VSILT + WFCLD*VCLAY + WFOMD*VOM)
C
          WFAIR= DeVWF(TKL,TKMA,gaair_lim)
          TKLMT =(WFSA*VSAND*TKSA + WFL*VLMT*TKL + WFICE*VIC(I)*TKI
     >    + WFAIR*VAC*TKMA + WFSI*VSILT*TKSI + WFCL*VCLAY*TKCL
     >    + WFOM*VOM*TKOM) / (WFSA*VSAND + WFL*VLMT + WFICE*VIC(I)
     >    + WFAIR*VAC + WFSI*VSILT + WFCL*VCLAY + WFOM*VOM)
C
          TK(I) = TK0 + (TKLMT-TK0)*VLC(I)/VLMT
        ENDIF
   10 CONTINUE
      RETURN
      END
c
C original code, as modified by K  Johnsen
c      DOUBLE PRECISION TK(MXNOD),VLC(MXNOD),VIC(MXNOD),RHOB(MXNOD),
c     +       SAT(MXNOD)
c      PARAMETER (RHOM=2650.0D0,TKL=0.57D0,TKI=2.2D0,TKA=0.025D0,
c     +          RHOL=1.0D3,RHOI=9.2D2)
C
c      DO 10 I=1,NS
c       VMC=RHOB(I)/RHOM
cckej
c       VAC=sat(i)-VIC(I)*rhoi/rhol-VLC(I)
cc       VAC=1.0D0-VMC-VIC(I)-VLC(I)
c       IF (VAC .LT. 0.0D0) VAC=0.0D0
cC
cC      IF WATER CONTENT IS TOO LOW, DEVRIES METHOD IS NO LONGER VALID
c       IF (VLC(I) .LT. VLCLMT  .AND.  VIC(I) .LE. 0.0D0) THEN
cC         INTERPOLATE BETWEEN THE VALUE FOR TK AT THE WATER CONTENT
cC         LIMIT AND TK CALCULATED FOR DRY SOIL BASED ON CAMPBELL, 1985
c          TKLMT=(TKWM*VMC*TKM+TKWL*VLCLMT*TKL+TKWA*
c     +          (1.0D0-VMC-VLCLMT)*TKA)
c     +         /(TKWM*VMC+TKWL*VLCLMT+TKWA*(1.0D0-VMC-VLCLMT))
c          TKDRY=0.03D0 + 0.7D0*VMC*VMC
c          TK(I)=TKLMT - (VLCLMT-VLC(I))*(TKLMT-TKDRY)/VLCLMT
c       ELSE
cC      MOISTURE CONTENT IS HIGH ENOUGH SUCH THAT WATER CAN BE CONSIDERED
cC      TO BE THE CONTINUOUS MEDIUM
c       TK(I)=(TKWM*VMC*TKM+TKWL*VLC(I)*TKL+TKWI*VIC(I)*TKI+TKWA*VAC*TKA)
c     +      /(TKWM*VMC+TKWL*VLC(I)+TKWI*VIC(I)+TKWA*VAC)
c       END IF
c   10 CONTINUE
c      RETURN
c      END
C
