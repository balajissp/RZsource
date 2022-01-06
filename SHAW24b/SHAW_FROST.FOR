C***********************************************************************
C
      SUBROUTINE FROST (NSP,NS,LVLOUT,JULIAN,HOUR,YEAR,INITAL,
     >           ZSP,RHOSP,DZSP,DLWDT,ZS,VLCDT,VICDT,ICESDT,
     >           fdepth,tdepth)
C
C     THIS SUBROUTINE INTERPOLATES BETWEEN NODES TO DETERMINE THE FROST
C     DEPTH, THEN PRINT THE FROST DEPTH AND SNOW DEPTH AT THE DESIRED
C     OUTPUT INTERVAL
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
c      COMMON /CONSTN/ LF,LV,LS,G,UGAS,RHOL,RHOI,RHOM,RHOOM,RHOA,CL,CI,
c     >                CM,COM,CA,CV,CR,VONKRM,VDIFF,PRESUR,P0,
c     >                TKL,TKI,TKA,TKR,TKSA,TKSI,TKCL,TKOM
      PARAMETER (NODSNO=100,MXNOD=300)
C
      Dimension ZSP(NODSNO),RHOSP(NODSNO),DZSP(NODSNO),
     >     DLWDT(NODSNO),WLAG(11),
     >     ZS(MXNOD),VLCDT(MXNOD),VICDT(MXNOD)
      INTEGER HOUR,YEAR,ICESDT(MXNOD)
C
      SAVE NPRINT,LAST,LDAY,LHOUR,LYR,ZERO  !,FDEPTH,TDEPTH
C
      IF (INITAL .EQ. 0) THEN
C        LIMIT PRINT OUT OF ICE CONTENT TO TOP 15 SOIL NODES -- ANY MORE
C        COULD NOT FIT ON ONE LINE.
         NPRINT=NS
         IF (NPRINT .GT. 15) NPRINT=15
C
c         WRITE (27,100) (ZS(I), I=1,NPRINT)
C        LAST => FLAG STATING WHETHER THERE WAS FROST LAST TIME STEP
C        LDAY,LHOUR,LYR => LAST TIME THAT FROST CHECKED
         LAST = 0
         LDAY = 0
         LHOUR = 0
         LYR = 0
         ZERO = 0.0d0
         FDEPTH = 0.0d0
         TDEPTH = 0.0d0
         GO TO 5
      END IF
C
c      IF (MOD(HOUR,LVLOUT) .NE. 0) RETURN
C
C     FIND LAYERS OF MAXIMUM THAW AND FROST
    5 NTHAW = 0
      NFROST = 0
      DO 10 I=NS,1,-1
         IF (ICESDT(I) .EQ. 1) THEN
            IF (NFROST .EQ. 0) NFROST = I
          ELSE
            IF (NFROST .GT. 0 .AND. NTHAW .EQ. 0) THEN
               NTHAW = I+1
               GO TO 15
            END IF
         END IF
   10 CONTINUE
C
   15 IF (NFROST .EQ. 0  .AND.  LAST .EQ. 0  .AND.  NSP .LE. 0) THEN
C        SAVE TIME ON WHICH FROST WAS LAST CHECKED AND RETURN
         fdepth=0.0d0
         tdepth=0.0d0
         LDAY=JULIAN
         LHOUR=HOUR
         LYR=YEAR
         RETURN
      END IF
C
C     IF LAST = 0, SNOW OR FROST WAS NOT PRESENT LAST TIME STEP BUT IS
C     PRESENT THIS TIME STEP --> PRINT OUT ZEROES FOR LAST TIME STEP
c      IF (LAST .EQ. 0  .AND.  LDAY .NE. 0)
c     >    WRITE (27,110) LDAY,LHOUR,LYR,(ZERO,I=1,NPRINT+4)
c      LDAY=JULIAN
      LHOUR=HOUR
      LYR=YEAR
C
      IF (NFROST .GT. 0) THEN
C        CALCULATE DEPTH OF FROST
         LAST = 1
         NF = NFROST
         FRACTN = VICDT(NF)/(VLCDT(NF) + VICDT(NF))
         IF (NF .EQ. 1  .OR.  NF .EQ. NS) THEN
            IF (NF .EQ. 1) THEN
              FDEPTH=  FRACTN*(ZS(2)-ZS(1))/2.0d0
             ELSE
           FDEPTH= (ZS(NS)+ZS(NS-1))/2.d0+FRACTN*(ZS(NS)-ZS(NS-1))/2.d0
            END IF
          ELSE
          FDEPTH= (ZS(NF)+ZS(NF-1))/2.d0+FRACTN*(ZS(NF+1)-ZS(NF-1))/2.d0
          END IF
C
         TDEPTH = 0.0d0
         IF (NTHAW .GT. 0) THEN
C           CALCULATE DEPTH OF THAW
            NT = NTHAW
            FRACTN = VLCDT(NT)/(VLCDT(NT) + VICDT(NT))
            IF (NT .EQ. 1) THEN
              TDEPTH =  FRACTN*(ZS(2)-ZS(1))/2.d0
             ELSE
              IF (NT .EQ. NFROST) THEN
               TDEPTH=(ZS(NT)+ZS(NT-1))/2.d0 +
     >                FRACTN*(FDEPTH-(ZS(NT)+ZS(NT-1))/2.d0)
                ELSE
           TDEPTH=(ZS(NT)+ZS(NT-1))/2.d0+FRACTN*(ZS(NT+1)-ZS(NT-1))/2.d0
              END IF
            END IF
         END IF
C
       ELSE
C        NO FROST EXISTS IN THE PROFILE - IF FROST EXISTED AT THE LAST
C        OUTPUT, INDICATE THAT FROST HAS LEFT
         IF (LAST .GT. 0) THEN
C           FROST EXISTED IN PROFILE AT LAST OUTPUT -- ENSURE THAT
C           FROST DEPTH AND THAW DEPTH LINES COME TOGETHER
            IF (TDEPTH .EQ. 0.0d0) THEN
C              GROUND WAS THAWING FROM THE BOTTOM UP
               FDEPTH = 0.0d0
              ELSE
C              THAW AND FROST DEPTH COME TOGETHER SOMEWHERE BETWEEN THE
C              DEPTHS AT LAST OUTPUT - USUALLY 2/3 BETWEEN
               FDEPTH = (2.d0*FDEPTH + TDEPTH)/3.d0
               TDEPTH = FDEPTH
            END IF
          ELSE
C           NO FROST IN PROFILE AT LAST OUTPUT
            FDEPTH = 0.0d0
            TDEPTH = 0.0d0
         END IF
         LAST = 0
      END IF
C
c      SWE = 0.0
cc      IF (NSP .LE. 0) THEN   !commented out based on GNF 
c         SNOW = 0.0
cc        ELSE
C        SAVE SNOW DEPTH AND CALCULATE SNOW WATER EQUIVALENT
c         SNOW = ZSP(NSP+1)
c         DO 20 I=1,NSP
c            SWE = SWE + RHOSP(I)*DZSP(I)/RHOL + DLWDT(I)
c   20    CONTINUE
C        WATER IN PROCESS OF BEING LAGGED THROUGH THE SNOWPACK
c         SWE = SWE + STORE
c         DO 25 I=1,11
c            SWE = SWE + WLAG(I)
c   25    CONTINUE
cc         LAST = 1
cc      END IF
C
      tdepth=TDEPTH*100.d0
      fdepth=FDEPTH*100.d0
C     PRINT FROST, THAW AND SNOW DEPTH IN CENTIMETERS AND SWE IN MM
c      WRITE (27,110) JULIAN,HOUR,YEAR,TDEPTH*100,FDEPTH*100,SNOW*100,
c     >               SWE*1000.,(VICDT(I), I=1,NPRINT)
c      IF (LAST .EQ. 0) WRITE (27,*)
      RETURN
C
  100 FORMAT (/4X,'FROST AND SNOW DEPTH',///,' DAY HR  YR',
     >5X,'THAW   FROST    SNOW     SWE      ICE CONTENT OF EACH NODE ',
     >'(M3/M3)',/,17X,'CM      CM      CM       MM',6X,20F6.2)
  110 FORMAT (I4,I3,I5,4F8.1,6X,15F6.3)
C
      END
