C
      SUBROUTINE STAB(ITER1,HEIGHT,HFLUX,PSIH,RH,TMPSFC,TMPAIR,ZH,ZERO,
     +                ZMLOG,ZHLOG,WIND,USTAR,STABLE,nc,zcan)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE CALCULATES THE ATMOSPHERIC STABILITY, WHEN SNOW
C     IS PRESENT.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       CA         P    spec heat of air  (J/kg-K)
C       CI         P    spec heat cap of ice  (J/kg-K)
C       CL         P    spec heat cap of liquid water  (J/kg-K)
C       CM         P    spec heat cap of mineral fract of soil (J/kg-K)
C       CV         P    spec heat of water vapor  (J/kg-K)
C       DUMMY      L    dummy variable passed but not needed
C       G          P    acceleration of gravity
C       HEIGHT     I    height of windspeed and temp measurements
C       HFLUX     I/O   turbulent heat flux at surface node
C       HTOLER     I    desired tolerance for heat flux calc at surface
C       HUM        I    humidity at beginning of time step
C       HUMDT      I    humidity at end of time step
C       ITER       I    current num of iter for energy and water balance
C       LF         P    latent heat of fusion
C       LS         P    latent heat of sublimation
C       LV         P    latent heat of vaporization
C       N          L    node num starting from surface material
C       NR         I    num of matted residue layers
C       NSP        I    num of layers in snowpack
C       P0         P    stand atm pressure at sea level  (101,300 Pa)
C       RHOA       P    density of air  (kg/m^3)
C       RHOI       P    density of ice   (kg/m^3)
C       RHOL       P    density of liquid water  (kg/m^3)
C       RHOM       P    density of the soil (particle density) kg/m^3
C       S          L    slope of sat vapor density curve  (kg/m^3-K)
C       SATV       L    sat vapor density
C       SATVDT     L    sat vapor density at end of time step
C       STABLE     L    atm stability coeff
C       T          I    temp
C       TA         I    air temp at beginning of time step
C       TADT       I    air temp at end of time step
C       TDT        I    temp at end of time step
C       TKA        P    thermal cond of air
C       UGAS       P    universal gas constant
C       USTAR      L    friction velocity (m/s)
C       VAP        I    vapor density of the surface node at t (kg/m^3)
C       VAPDT      I    vapor density of surface node at t+dt (kg/m^3)
C       VONKRM     P    von Karman constant
C       WDT        I    weighting factor for end of time step values
C       WIND       I    ave wind speed for the time step (m/s)
C       WT         I    weightin factor for beginning of time step values
C       ZERO       I    plane of zero displacement in wind profile calc.
C       ZH         I    surface roughness para for heat transfer
C       ZM         I    surface roughness para for momentum transfer
C
C
C       COMMENTS:   ORIGINALLY THIS ROUTINE WAS A SECTION IN SHAW'S ATSTAB
C                   ROUTINE, BUT WAS LATER MADE A SEPERATE ROUTINE TO
C                   ACCOMMODATE INTEGRATION INTO RZWQM.
C
C       EXTERNAL REFERENCES:
C                       VSLOPE
C
C       CALLED FROM: ATSTAB, POTEVP
C
C       PROGRAMMER:  GERALD FLERCHINGER, KEN ROJAS
C
C       VERSION: 1.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(G=9.81D0, RHOA=1.25D0, CA=1.006D3,VONKRM=0.4D0,
     +  TKA=0.025D0,HTOLER=0.1D0)
C
      HFLUX1 = HFLUX
C
C**** START ITERATIVE PROCESS TO OBTAIN HEAT FLUX
   10 ITER1=ITER1+1
      STABLE=VONKRM*(HEIGHT-ZERO)*G*HFLUX1/(RHOA*CA*(TMPAIR+273.16D0)
     +  *USTAR**3.0D0)
      IF (STABLE .GE. 0.0D0) THEN
C****    ATMOSPHERE IS STABLE
C        IF STABILITY IS GREATER THAN ZMLOG/9.4, COMPUTED HFLUX WILL
C        ACTUALLY DECREASE WITH INCREASING TEMPERATURE DIFFERENTIAL,
C        CAUSING NUMERICAL INSTABILITY AND CONVERGENCE PROBLEMS -- AVOID
C        THIS SITUATION BY LIMITING STABLE TO ZMLOG/9.4D0
         IF (STABLE .GT. ZMLOG/9.4D0) STABLE=ZMLOG/9.4D0
         PSIH=4.7D0*STABLE
         PSIM=PSIH
       ELSE
C****    ATMOSPHERE IS UNSTABLE
         PSIH=-2.0D0*LOG((1.0D0 + (1.0D0-16.0D0*STABLE)**0.5D0)/2.0D0)
         PSIM=0.6D0*PSIH
C        IF ATMOSPHERE IS SUFFICIENTLY UNSTABLE, EQUATIONS MAY RESULT
C        IN FRICTION VELOCITY LESS THEN ZERO !!??!!?? (LIMIT PSIM)
         IF (PSIM/ZMLOG .LT. -0.50d0) THEN
            PSIM=-0.50d0*ZMLOG
            PSIH=PSIM/0.6d0
            STABLE = -((2.d0*EXP(-PSIH/2.d0) - 1.d0)**2.d0 -1.d0)/16.d0
         END IF
      END IF
      USTAR=WIND*VONKRM/(ZMLOG + PSIM)
      RH=(ZHLOG + PSIH)/(USTAR*VONKRM)
      if (nc .gt. 0) then
C        RH IS TO ZERO PLANE DISPLACEMENT - ADJUST TO TOP OF CANOPY
         RH=RH*(1.d0-((LOG((ZCAN-ZERO+ZH)/ZH)+PSIH)/(ZHLOG + PSIH)))
      end if
      HFLUX2=RHOA*CA*(TMPAIR-TMPSFC)/RH
      ERROR=ABS(HFLUX2-HFLUX1)
      HFLUX1=HFLUX2
      ZBASE=ZERO
      IF (NC .GT. 0) ZBASE=ZCAN
      IF (ABS(HFLUX1).LT.ABS(TKA*(TMPAIR-TMPSFC)/(HEIGHT-ZBASE))) 
     +   GO TO 15
C        WIND IS SUFFICIENTLY SMALL, OR ATMOSPHERE IS SUFFICIENTLY
C        STABLE THAT TURBULENCE DOES NOT INCREASE FLUX -- NO NEED TO
C        ITERATE FURTHER
C
C**** CHECK IF TOLERANCE HAS BEEN MET
      IF (ITER1 .GT. 40) THEN
C        CONVERGENCE NOT MET, BUT PROBABLY CLOSE ENOUGH
         GO TO 15
      END IF
      IF (ERROR .GT. HTOLER) GO TO 10
C
C**** HEAT FLUX IS WITHIN TOLERABLE ERROR -- NOW CALCULATE VAPOR FLUX
   15 HFLUX=HFLUX1
C
      RETURN
      END
C
