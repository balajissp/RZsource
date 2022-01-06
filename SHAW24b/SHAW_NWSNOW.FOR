C
      SUBROUTINE NWSNOW(NSP,ICESPT,TSPDT,DLWDT,TWBULB,SNOW,
     +                   RHOSP,DZSP,ZSP,TRDT,TSDT,NR)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINES ADDS SNOW LAYERS FOR NEWLY FALLEN SNOW
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       CI         P    spec heat cap of ice  (J/kg-K)
C       DLWDT     I/O   liquid water depth for i-th snow pack node at
C                       end of time step
C       DZSP      I/O   thickness of i-th snowpack node
C       ICESPT    I/O   1 if snow layer contains both ice and water at t+dt
C       LF         P    latent heat of fusion
C       NSP        I    num of layers in snowpack
C       RHOL       P    density of liquid water  (kg/m^3)
C       RHOSP     I/O   density of ice fract of snowpack  (kg/m^3)
C       SNOW       I    water-equivalent of new snowfall (precip) [M]
C       THICK      P    desired thickness of snowpack layers
C       TRDT       I    temp of residue layer at time t + dt
C       TSDT       I    temp of soil layer at time t + dt
C       TSPDT     I/O   temp of snowpack layer at time t + dt
C       TWBULB     I    wet-bulb tempertue
C       ZSP       I/O   depth of node i in the snow from the snowpack top
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
      PARAMETER (MXNOD=300, NODRES=10, NODSNO=100)
C
      DOUBLE PRECISION TSPDT(NODSNO),DLWDT(NODSNO),RHOSP(NODSNO),
     +  DZSP(NODSNO),ZSP(NODSNO),TRDT(NODRES),TSDT(MXNOD)
      DOUBLE PRECISION LF
C
      INTEGER ICESPT(NODSNO)
C
      PARAMETER(LF=3.35D5,RHOL=1.0D3,CI=2.1D3,THICK=2.5D-2)
      COMMON/SNOWDEN/DEN_MAX,DEN_INIT
C
C     PRECIPITATION IS SNOW
C     COMPUTE DENSITY OF THE NEW SNOW BASED ON WETBULB TEMPERATURE
      IF (TWBULB .LE. -15.0D0) THEN
         DENSTY = DEN_INIT*1.D3   !50.0D0
       ELSE
C         DENSTY = 50.0D0 + 1.7D0*((TWBULB + 15.0D0)**1.5D0)
         DENSTY = DEN_INIT*1.0D3 + 1.7D0*((TWBULB + 15.0D0)**1.5D0)
      END IF
      DEPTH = SNOW*RHOL/DENSTY
C SNOW DOES NOT FALL ON CANOPY AND RESIDUE AS IN SHAW, 2-12-2011
      IF (NSP .NE. 0) THEN
C        NEW SNOW IS FALLING ON OLD SNOW - CHECK IF NEW SNOW CAUSES THE
C        THE SURFACE LAYER TO EXCEED THE SPECIFIED DEPTH LIMIT.
         IF((DZSP(1) + DEPTH) .LE. (1.55D0*THICK)) THEN
C           INCORPORATE ALL OF NEW SNOW INTO OLD SURFACE LAYER.
            DNS = DEPTH
            DEPTH = 0.0D0
            NWLAYR = 0
         ELSE
C           DETERMINE HOW MUCH SHOULD BE COMBINED WITH OLD SURFACE LAYER
            IF (DZSP(1) .GT. THICK) THEN
C              LAYER IS ALREADY GREATER THAN DESIRED THICKNESS - DO NOT
C              ADD NEW SNOW TO OLD SURFACE LAYER
               DNS = 0.0D0
C              GO SET UP NEW LAYERS FOR THE NEW SNOW
               GO TO 5
            END IF
            DNS = THICK - DZSP(1)
            DEPTH = DEPTH - DNS
         END IF
C
C        INCORPORATE NEW SNOW INTO OLD SURFACE LAYER - DEFINE PROPERTIES
         WEI=RHOSP(1)*DZSP(1)/RHOL
         WENS=DENSTY*DNS/RHOL
         WEL=WEI+WENS
         DZSP(1) = DZSP(1) + DNS
         RHOSP(1)=RHOL*WEL/DZSP(1)
         TSPDT(1)=(WENS*TWBULB + WEI*TSPDT(1))/WEL
         IF (ICESPT(1) .EQ. 1)  THEN
C           LIQUID-WATER AND SUBFREEZING TEMP MAY EXIST IN NEW LAYER
            FREEZE=-(TSPDT(1)*CI*WEL)/(LF*RHOL)
            IF (DLWDT(1) .LE. FREEZE) THEN
               TSPDT(1)=TSPDT(1)+(DLWDT(1)*LF*RHOL)/(CI*WEL)
               WEL=WEL+DLWDT(1)
               RHOSP(1)=RHOL*WEL/DZSP(1)
               DLWDT(1)=0.0D0
               ICESPT(1)=0
            ELSE
               DLWDT(1)=DLWDT(1)-FREEZE
               WEL=WEL+FREEZE
               RHOSP(1)=RHOL*WEL/DZSP(1)
               TSPDT(1)= 0.0D0
            END IF
         END IF
      END IF
C
C
C     COMPUTE THE NUMBER OF LAYERS WITH ENTIRELY NEW SNOW
    5 NWLAYR = int(DEPTH/THICK)
      EXTRA = DEPTH - NWLAYR*THICK
      IF (EXTRA .GT. 0.55D0*THICK) NWLAYR = NWLAYR + 1
      IF (EXTRA .GT. 0.0D0  .AND.  NWLAYR .EQ. 0) NWLAYR = 1
C
      IF (NSP .GT. 0) THEN
         DOWN = DEPTH + DNS
         IF (NWLAYR .EQ. 0) THEN
C           ADJUST DEPTHS OF THE LAYERS AND RETURN
            ZSP(1) = 0.0D0
            DO 10 I=2,NSP+1
               ZSP(I) = ZSP(I) + DOWN
   10       CONTINUE
            RETURN
         ELSE
C           MOVE LAYERS DOWN TO MAKE ROOM FOR NEW SNOW LAYERS
            ZSP(NSP+NWLAYR+1) = ZSP(NSP+1) + DOWN
            TSPDT(NSP+NWLAYR+1) = TSPDT(NSP+1)
            DO 15 I=NSP,1,-1
               RHOSP(NWLAYR + I)= RHOSP(I)
               TSPDT(NWLAYR + I)= TSPDT(I)
               DLWDT(NWLAYR + I)= DLWDT(I)
               ICESPT(NWLAYR + I)= ICESPT(I)
               ZSP(NWLAYR + I) = ZSP(I) + DOWN
               DZSP(NWLAYR + I) = DZSP(I)
   15       CONTINUE
         END IF
      END IF
C
C     LEFT-OVER SNOW GOES IN TOP LAYER
      EXTRA=DEPTH - (NWLAYR-1)*THICK
      ZSP(1)=0.0D0
      DZSP(1)=EXTRA
      RHOSP(1)=DENSTY
      TSPDT(1)=TWBULB
      DLWDT(1)=0.0D0
      ICESPT(1)= 0
      TDEPTH=DZSP(1)
C
C     FULL SIZE NEW SNOW LAYERS.
      DO 20 J=2,NWLAYR
         DZSP(J)=THICK
         ZSP(J)= TDEPTH + DZSP(J)/2.0D0
         TDEPTH=TDEPTH + DZSP(J)
         RHOSP(J)=DENSTY
         TSPDT(J)=TWBULB
         DLWDT(J)=0.0D0
         ICESPT(J)= 0
   20 CONTINUE
C
      IF (NSP .EQ. 0) THEN
C        DEFINE DEPTH OF BOTTOM BOUNDARY OF SNOWPACK
         ZSP(NWLAYR+1) = TDEPTH
C        NEW SNOW COVER - DEFINE TEMP OF BOTTOM BOUNDARY OF SNOWPACK
C        THIS NODE IS SHARED BY THE MATERIAL BELOW
         IF (NR .GT. 0) THEN
C           UNDERLYING MATERIAL IS RESIDUE
            TSPDT(NWLAYR+1) = TRDT(1)
         ELSE
C           UNDERLYING MATERIAL IS SOIL
            TSPDT(NWLAYR+1) = TSDT(1)
         END IF
      END IF
C
      NSP = NSP + NWLAYR
      RETURN
      END
