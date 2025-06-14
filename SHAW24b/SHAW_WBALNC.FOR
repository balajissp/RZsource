C***********************************************************************
C
      SUBROUTINE WBALNC (NPLANT,NC,NSP,NR,NS,LVLOUT,JULIAN,HOUR,YEAR,
     >  ITYPE,INITAL,ZC,WCAN,WCANDT,PCAN,PCANDT,VAPC,VAPCDT,RHOSP,DZSP,
     >  DLWDT,WLAG,STORE,ZR,GMC,GMCDT,VAPR,VAPRDT,RHOR,ZS,VLC,VLCDT,
     >  VIC,VICDT,TOTFLO,PRECIP,RUNOFF,POND,EVAP1,ETSUM,EVAPS,SROOTXT)
C
C     THIS SUBROUTINE SUMS THE EVAPORATION AND DEEP PERCOLATION AT THE
C     END OF EACH HOUR, THEN PRINTS THE SUMMARY AT THE DESIRED OUTPUT
C     INTERVAL
C
C***********************************************************************
      COMMON /CONSTN/ LF,LV,LS,G,UGAS,RHOL,RHOI,RHOM,RHOOM,RHOA,CL,CI,
     >                CM,COM,CA,CV,CR,VONKRM,VDIFF,PRESUR,P0,
     >                TKL,TKI,TKA,TKR,TKSA,TKSI,TKCL,TKOM
      COMMON /CLAYRS/ DRYCAN(8,10),CANLAI(8,10),TOTLAI(8),IEVAP(8),
     >                RLEAF(8,10),RROOT(8,50),ROOTDN(8,50),TOTROT(8)
      DOUBLE PRECISION LF,LS,LV
C
      REAL ZC(11),WCAN(10),WCANDT(10),PCAN(8),PCANDT(8),VAPC(11),
     >     VAPCDT(11),RHOSP(100),DZSP(100),DLWDT(100),WLAG(11),ZR(10),
     >     GMC(10),GMCDT(10),VAPR(10),VAPRDT(10),RHOR(10),ZS(50),
     >     VLC(50),VLCDT(50),VIC(50),VICDT(50),TOTFLO(50)
      INTEGER HOUR,YEAR,ITYPE(8)
      SAVE RAIN,DPCAN,DCAN,DSNOW,DRES,DSOIL,TRUNOF,POND2,TPERC,TETSUM,
     >     TEVAP,CUMVAP,SWE,TEVAPS,TROOTXT
CXXXX>     TEVAP,CUMVAP,SWE
      DATA RAIN,DPCAN,DCAN,DSNOW,DRES,DSOIL,TRUNOF,POND2,TPERC,TETSUM,
     >     TEVAP,TEVAPS,TROOTXT,CUMVAP/14*0.0/
CXXXX>     TEVAP,CUMVAP/12*0.0/
C
      IF (INITAL .EQ. 0) THEN
         WRITE (26,100)
C        CALCULATE THE SNOW WATER EQUIVALENT FOR THE INITIAL CONDITIONS
         SWE=0.0
         DO 5 I=1,NSP
            SWE=SWE + RHOSP(I)*DZSP(I)/RHOL + DLWDT(I)
    5    CONTINUE
         RETURN
      END IF
C
C
C     END OF THE HOUR -- DETERMINE THE CHANGE IN STORAGE
C
C     CHANGE IN STORAGE OF CANOPY
      DO 15 I=1,NC
         DO 10 J=1,NPLANT
C           CALCULATE CHANGE IN WATER CONTENT OF DEAD PLANT MATERIAL
            IF (ITYPE(J) .EQ. 0) DCAN = DCAN +
     >      (WCANDT(I)-WCAN(I))*DRYCAN(J,I)*1000./RHOL
   10    CONTINUE
C        INCLUDE CHANGE IN VAPOR DENSITY OF AIR SPACE
         IF (I .EQ. 1)  THEN
           DZ = (ZC(2) - ZC(1))/2.
          ELSE
           DZ = (ZC(I+1) - ZC(I-1))/2.
         END IF
         DCAN = DCAN + DZ*(VAPCDT(I)-VAPC(I))*1000./RHOL
   15 CONTINUE
C
C     CHANGE IN SNOWPACK
      SWEDT = 0.0
      IF (NSP .GT. 0) THEN
         DO 20 I=1,NSP
            SWEDT = SWEDT + RHOSP(I)*DZSP(I)/RHOL + DLWDT(I)
   20    CONTINUE
C        WATER IN PROCESS OF BEING LAGGED THROUGH THE SNOWPACK
         SWEDT = SWEDT + STORE
         DO 25 I=1,11
            SWEDT = SWEDT + WLAG(I)
   25    CONTINUE
      END IF
C     CALCULATE CHANGE IN WATER CONTENT OF SNOWPACK IN MILLIMETERS
      DSNOW = DSNOW + (SWEDT - SWE)*1000.
      SWE = SWEDT
C
C     CHANGE IN STORAGE OF RESIDUE
      DO 30 I=1,NR
         IF (I .EQ. 1 .OR.  I .EQ. NR) THEN
            IF (NR .EQ.1) THEN
               DZ = ZR(NR+1)
              ELSE
               IF (I .EQ. 1)  DZ = (ZR(2) - ZR(1))/2.
               IF (I .EQ. NR) DZ = ZR(NR+1)-ZR(NR)+(ZR(NR)-ZR(NR-1))/2.
            END IF
          ELSE
           DZ = (ZR(I+1) - ZR(I-1))/2.
         END IF
         DRES = DRES + DZ*((GMCDT(I)-GMC(I))*RHOR(I)
     >                    +(VAPRDT(I)-VAPR(I)))*1000./RHOL
   30 CONTINUE
C
C     CHANGE IN STORAGE FOR THE SOIL
      DO 40 I=1,NS-1
         IF (I .EQ. 1) THEN
           DZ = (ZS(2) - ZS(1))/2.
          ELSE
           DZ = (ZS(I+1) - ZS(I-1))/2.
         END IF
         DSOIL= DSOIL + DZ*1000*(VLCDT(I) - VLC(I)
     >                        + (VICDT(I) - VIC(I))*RHOI/RHOL)
   40 CONTINUE
C
C     COMPUTE THE CHANGE IN PRECIP INTERCEPTED ON PLANT LEAVES
      TPCAN=0.0
      DO 50 J=1,NPLANT
         IF (ITYPE(J).NE.0) TPCAN=TPCAN + PCANDT(J)
         IF (ITYPE(J).NE.0) DPCAN=DPCAN + PCANDT(J)-PCAN(J)
   50 CONTINUE
C
      RAIN = RAIN + PRECIP
      TRUNOF = TRUNOF + RUNOFF
      TETSUM = TETSUM + ETSUM
      TEVAP = TEVAP + EVAP1
      TPERC = TPERC + TOTFLO(NS-1)
CXXXX
      TEVAPS = TEVAPS + EVAPS
      TROOTXT = TROOTXT + SROOTXT
C
C     RETURN IF HOURLY OUTPUT IS NOT REQUIRED AND IT IS NOT END OF DAY
      IF (MOD(HOUR,LVLOUT) .NE. 0) RETURN
C
C     CONVERT TO MILLIMETERS
      TPCAN=TPCAN*1000.
      DPCAN=DPCAN*1000.
      DPOND = POND*1000. - POND2
      POND2 = POND*1000.
      RAIN = RAIN*1000.
      TRUNOF = TRUNOF*1000.
      TETSUM = TETSUM*1000.
      TEVAP = TEVAP*1000.
CXXXX
      TROOTXT = TROOTXT*1000.d0
      TEVAPS = TEVAPS*1000.d0
      TPERC = TPERC*1000.d0
      CUMVAP = CUMVAP + TEVAP
      ERROR = RAIN+TEVAP-TPERC-TRUNOF-DPOND-DPCAN-DCAN-DSNOW-DRES-DSOIL
C
CXXXX WRITE (26,110)JULIAN,HOUR,YEAR,RAIN,TPCAN,-TEVAP,TETSUM,DCAN,
CXXXX>              DSNOW,DRES,DSOIL,TPERC,TRUNOF,POND2,-CUMVAP,ERROR
      WRITE (26,110)JULIAN,HOUR,YEAR,RAIN,TPCAN,-TEVAP,TETSUM,TROOTXT,
     >     TEVAPS,DCAN,DSNOW,DRES,DSOIL,TPERC,TRUNOF,POND2,-CUMVAP,ERROR
C
      RAIN=0.0
      DPCAN=0.0
      TRUNOF=0.0
      TETSUM=0.0
      TEVAPS=0.0
      TPERC=0.0
      DCAN=0.0
      DSNOW=0.0
      DRES=0.0
      DSOIL=0.0
CXXXX
      TEVAP=0.0
      TROOTXT=0.0
      RETURN
C
  100 FORMAT (/35X,'SUMMARY OF WATER BALANCE',//,70X,'CHANGE IN STORAGE'
     >,/,21X,'  PRECIP',8X,'  PLANT    ROOT    SOIL   ',30('-'),
     >2X,'DEEP',20X,' CUM.',/,
     >' DAY HR  YR   PRECIP   INTRCP    ET    TRANSP EXTRACT   EVAP   CA
     >NOPY   SNOW  RESIDUE   SOIL   PERC   RUNOFF   PONDED    ET    ERRO
     >R',/,11X,13(6X,'MM'))
  110 FORMAT (1X,I3,I3,I5,13(1X,F7.2),1X,F7.1,1X,F7.2)
      END
C***********************************************************************
