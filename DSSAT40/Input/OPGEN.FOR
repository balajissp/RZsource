C=======================================================================
C  OPGEN, Subroutine
C
C  Generates output for simulated data
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 GH  Written
C  05/28/1999 PWW Header revision and minor changes 
C  04/02/1996 GH  Added new output files for flooding and chemical apps
C  12/11/2000 GH  Modified write statemento fit 80 character limit
C  03/05/2002 GH  Modified for CSM modeling system
C  03/11/2005 GH  Modified format for P2R for millet and sorghum
C-----------------------------------------------------------------------
C  INPUT  : CUMDEP,TPESW,VRNAME,AINO3,AINH4,TLL,TDUL,TSAT,TSWINI,WTHADJ,
C           CO2,ECONAM,RUN,MODEL,CROP,CROPD,TITLER,ECONO,VARTY,ESW,
C           SWINIT,INO3,INH4,TSOC
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : INPUT
C
C  Calls  : CLEAR OPHEAD OPSOIL
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPGEN (CUMDEP,TPESW,VRNAME,AINO3,AINH4,TLL,TDUL,TSAT,
     &     TSWINI,RUN,MODEL,CROP,CROPD,TITLET,ECONO,VARTY,
     &     ESW,SWINIT,INO3,INH4,TSOC,WTHSTR,NYRS,
     &     EXPER,CG, ENAME,YRPLT,                !LIWANG MA, RZWQM-DSSAT
     &     PEDON,SLTX,SLDESC,WSTA,ICRES,TOTAPW,TOTNAP)               ! LIWANG MA, RZWQM-DSSAT

      USE ModuleDefs
      IMPLICIT NONE

C      INCLUDE 'COMSOI.BLK'
C      INCLUDE 'COMSWI.BLK'

      CHARACTER*  1 ANS
      CHARACTER*  2 CROP,CG
      CHARACTER*  6 VARTY,ECONO
      CHARACTER*  8 EXPER
      CHARACTER* 10 CROPD
      CHARACTER* 12 MODEL
	CHARACTER* 12 OUTPG,OUTPN,OUTPW,OUTPS,OUTPC,OUTPP,OUTSW
      CHARACTER* 14 OUTSWB
      CHARACTER* 12 OUTST,OUTSN,OUTSNB,OUTSC,OUTSCB,OUTS
      CHARACTER* 16 VRNAME
      CHARACTER* 80 TITLET
      CHARACTER*120 WTHSTR
      CHARACTER*60 ENAME
C MODIFIED BY LIWANG MA, RZWQM-DSSAT
      CHARACTER*5  SLTX
      CHARACTER*4  WSTA
      CHARACTER*10 PEDON
      CHARACTER*50 SLDESC

      INTEGER NYRS,RUN
      INTEGER LUNOV,LUNOUT,YRPLT

      REAL    AINO3,AINH4
      REAL    SWINIT(NL),TSWINI,INO3(NL),INH4(NL)
      REAL    CUMDEP,TPESW,ESW(NL)
      REAL    TLL,TDUL,TSAT,TSOC,ICRES
      PARAMETER (LUNOUT = 130, LUNOV = 180)
C MODIFIED BY LIWANG MA, RZWQM-DSSAT
      REAL TOTAPW,TOTNAP
      CHARACTER* 12 OUTO,OUTG,OUTSR,OUTF,OUTH,OUTR
      CHARACTER* 12 OUTT,OUTD,OUTWAT,OUTSP,OUTM,OUTWTH,OUTSPAM
      CHARACTER* 12 OUTP,OUTSOMC,OUTRSTG,SOUTE,SOUTR,SEVAL,PNBAL
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
      CALL GETPUT_CONTROL('GET',CONTROL)
      CALL GETPUT_ISWITCH('GET',ISWITCH)
      CALL GETPUT_SOILPROP('GET',SOILPROP)
      OUTO   = 'OVERVIEW.OUT'

C END OF MODIFICATION
     
      OUTS   = 'SUMMARY.OUT'
      OUTPG  = 'PLANTGRO.OUT'
	OUTPN  = 'PLANTN.OUT'
	OUTPC  = 'PLANTC.OUT'
      OUTPW  = 'WEATHER.OUT'
	OUTPS  = 'ET.OUT'
	OUTPP  = 'ETPHOT.OUT'
      OUTSW  = 'SOILWAT.OUT'
      OUTSWB = 'SOILWatBAL.OUT'
	OUTST  = 'SOILTEMP.OUT'
      OUTSN  = 'SOILN.OUT'
      OUTSNB = 'SOILNBAL.OUT'
      OUTSC  = 'SOILC.OUT'
	OUTSCB = 'CBAL.OUT'

      OUTD   = 'PEST.OUT'
      OUTP   = 'PHOSPHOR.OUT'
      OUTF   = 'FLOOD.OUT'
      OUTH   = 'CHEMICAL.OUT'
      OUTR   = 'OPERAT.OUT'

C-----------------------------------------------------------------------
C     Generate header file to be used by model output routines.
C-----------------------------------------------------------------------
      OPEN (UNIT=LUNOV, FILE='HEADER.OUT', STATUS = 'REPLACE')
      CALL OPHEAD(LUNOV, CUMDEP,TPESW,VRNAME,AINO3,AINH4,
     &     ECONO,RUN,MODEL,TITLET,WTHSTR,
     &     CROPD,EXPER,CG, ENAME,YRPLT,             ! LIWANG MA, RZWQM-DSSAT
     &     PEDON,SLTX,SLDESC,WSTA,ICRES,TOTAPW,TOTNAP)            ! LIWANG MA, RZWQM-DSSAT
      
C-----------------------------------------------------------------------
C     Generate a summary output for the screen
C-----------------------------------------------------------------------

       IF (CONTROL % RNMODE .EQ. 'I' .AND. CONTROL % NYRS .LE. 1) THEN
          CALL CLEAR
!         write header to console
          WRITE (6,50)                                    
          CALL OPHEAD (6,CUMDEP,TPESW,VRNAME,AINO3,AINH4, 
     &     ECONO,RUN,MODEL,TITLET,WTHSTR,
     &     CROPD,EXPER,CG, ENAME,YRPLT,             ! LIWANG MA, RZWQM-DSSAT
     &     PEDON,SLTX,SLDESC,WSTA,ICRES,TOTAPW,TOTNAP)            ! LIWANG MA, RZWQM-DSSAT
       ENDIF

C-----------------------------------------------------------------------
C     Changed conditional if statements to delete previous output files
C     if output option is N .. request from PKT
C-----------------------------------------------------------------------
      IF (RUN .EQ. 1) THEN
C MODIFIED BY LIWANG MA, RZWQM-DSSAT TO DELETE ALL THE FILES AT RUNINIT
C        IF (ISWITCH % IDETO .EQ. 'N') THEN
          OPEN  (UNIT=LUNOUT,FILE='LIST.OUT',STATUS='UNKNOWN')
          CLOSE (UNIT=LUNOUT,STATUS='DELETE')
          OPEN  (UNIT=LUNOUT,FILE='WARNING.OUT',STATUS='UNKNOWN')
          CLOSE (UNIT=LUNOUT,STATUS='DELETE')
          OPEN  (UNIT=LUNOUT,FILE='EVALUATE.OUT',STATUS='UNKNOWN')
          CLOSE (UNIT=LUNOUT,STATUS='DELETE')

          OPEN  (UNIT=LUNOUT,FILE=OUTO,STATUS='UNKNOWN')
          CLOSE (UNIT=LUNOUT,STATUS='DELETE')
C        ENDIF
C	  IF (ISWITCH % IDETS .EQ. 'N') THEN
          OPEN  (UNIT=LUNOUT,FILE=OUTS,STATUS='UNKNOWN')
          CLOSE (UNIT=LUNOUT,STATUS='DELETE')
C        ENDIF
C        IF (ISWITCH % IDETG .EQ. 'N') THEN
          OPEN  (UNIT=LUNOUT,FILE=OUTPG,STATUS='UNKNOWN')
          CLOSE (UNIT=LUNOUT,STATUS='DELETE')
	    OPEN  (UNIT=LUNOUT,FILE=OUTPN,STATUS='UNKNOWN')
          CLOSE (UNIT=LUNOUT,STATUS='DELETE')
	    OPEN  (UNIT=LUNOUT,FILE=OUTPW,STATUS='UNKNOWN')
          CLOSE (UNIT=LUNOUT,STATUS='DELETE')
	    OPEN  (UNIT=LUNOUT,FILE=OUTPS,STATUS='UNKNOWN')
          CLOSE (UNIT=LUNOUT,STATUS='DELETE')
	    OPEN  (UNIT=LUNOUT,FILE=OUTPC,STATUS='UNKNOWN')
          CLOSE (UNIT=LUNOUT,STATUS='DELETE')        
	    OPEN  (UNIT=LUNOUT,FILE=OUTPP,STATUS='UNKNOWN')
          CLOSE (UNIT=LUNOUT,STATUS='DELETE')        
	    OPEN  (UNIT=LUNOUT,FILE='plantNbal.out',STATUS='UNKNOWN')
          CLOSE (UNIT=LUNOUT,STATUS='DELETE')        
C        ENDIF
C        IF (ISWITCH % IDETW .EQ. 'N') THEN
          OPEN  (UNIT=LUNOUT,FILE=OUTSW,STATUS='UNKNOWN')
          CLOSE (UNIT=LUNOUT,STATUS='DELETE')
          OPEN  (UNIT=LUNOUT,FILE=OUTSWB,STATUS='UNKNOWN')
          CLOSE (UNIT=LUNOUT,STATUS='DELETE')
	    OPEN  (UNIT=LUNOUT,FILE=OUTST,STATUS='UNKNOWN')
          CLOSE (UNIT=LUNOUT,STATUS='DELETE')
C        ENDIF
C        IF (ISWITCH % IDETN .EQ. 'N') THEN
          OPEN   (UNIT=LUNOUT,FILE=OUTSN,STATUS='UNKNOWN')
	    CLOSE  (UNIT=LUNOUT,STATUS='DELETE')
	    OPEN   (UNIT=LUNOUT,FILE=OUTSNB,STATUS='UNKNOWN')
	    CLOSE  (UNIT=LUNOUT,STATUS='DELETE')
C	  ENDIF
C	  IF (ISWITCH % IDETC .EQ. 'N') THEN
          OPEN   (UNIT=LUNOUT,FILE=OUTSC,STATUS='UNKNOWN')
	    CLOSE  (UNIT=LUNOUT,STATUS='DELETE')
	    OPEN   (UNIT=LUNOUT,FILE=OUTSCB,STATUS='UNKNOWN')
	    CLOSE  (UNIT=LUNOUT,STATUS='DELETE')
C	  ENDIF
	ENDIF

      CALL OPSOIL (LUNOV,SOILPROP%LL,SOILPROP%DUL,SOILPROP%SAT,
     &     SOILPROP%DLAYR,SWINIT,SOILPROP%DS,SOILPROP%NLAYR,ESW,
     &     SOILPROP%WR,SOILPROP%BD,SOILPROP%PH,INO3,INH4,SOILPROP%OC,
     &     TLL,TDUL,TSAT,TPESW,TSWINI,AINO3,AINH4,TSOC,
     &     SOILPROP%SWCON,SOILPROP%U,SOILPROP%SALB,SOILPROP%CN, !CN2=CN?
     &     CROPD,VRNAME,VARTY,SOILPROP%SLPF,
     &     ECONO,SOILPROP%SLNF,CONTROL%CROP,ISWITCH%ISWWAT,
     &     CONTROL%RNMODE,RUN)

      IF (CONTROL%RNMODE .EQ. 'I' .AND.  NYRS .LE. 1) THEN
      ! RM - replaced $ At the end of the 2900 format with advance='no'
        WRITE (*,2900,advance='no')
        READ (5,'(1A1)') ANS
      ENDIF

      WRITE(LUNOV,'(//)')
      CLOSE (LUNOV)
      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

  50  FORMAT ('*SIMULATION OVERVIEW')
 2900 FORMAT (1X,'Please press < ENTER > key to continue ')

      END SUBROUTINE OPGEN

C=======================================================================
C  OPSOIL, Subroutine
C
C  Generates output for soil data
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 GH  Written
C  05/28/1999 PWW Header revision and minor changes 
C  03/11/2005 GH  Remove ANS, RNMODE and NYRS
C
C-----------------------------------------------------------------------
C  INPUT  : IDETO,NOUTDO,NYRS,LL,DUL,SAT,DLAYR,SWINIT,DS,NLAYR,ESW
C           SHF,BD,PH,INO3,INH4,OC,TLL,TDUL,TSAT,TPESW,TSWINI,AINO3,AINH4
C           TSOC,SWCON,U,SALB,CN2,CROPD,VRNAME,VARTY,SLPF,ECONO
C           SLNF,LUNOV,CROP
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPIBS3
C
C  Calls  : CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPSOIL (LUNOV,LL,DUL,SAT,
     &   DLAYR,SWINIT,DS,NLAYR,ESW,SHF,BD,PH,INO3,INH4,OC,
     &   TLL,TDUL,TSAT,TPESW,TSWINI,AINO3,AINH4,TSOC,
     &   SWCON,U,SALB,CN2,CROPD,VRNAME,VARTY,SLPF,
     &   ECONO,SLNF,CROP,ISWWAT, RNMODE, RUN)

      USE ModuleDefs
      IMPLICIT NONE

      INCLUDE 'COMGEN.BLK'

      CHARACTER*1  ISWWAT, RNMODE
      CHARACTER*2  CROP
      CHARACTER*6  VARTY,ECONO
      CHARACTER*10 CROPD
      CHARACTER*16 VRNAME

      INTEGER      NLAYR,I,L,LUNOV, RUN

      REAL         LL(NL),DUL(NL),SAT(NL),DLAYR(NL),DS(NL),SWINIT(NL)
      REAL         ESW(NL),SHF(NL),BD(NL),PH(NL),INO3(NL),INH4(NL)
      REAL         OC(NL),TLL,TDUL,TSAT,TPESW,TSWINI,AINO3,AINH4,TSOC
      REAL         SWCON,U,SALB,CN2,SLPF,SLNF

C MODIFIED BY LIWANG MA, RZWQM-DSSAT
      TYPE (PLANTVARTYPE)   PLANTVAR
!-----------------------------------------------------------------------
!      CHP 08/12/2005 Don't report initial conditions for
!           sequenced runs.
      IF (INDEX('FQ',RNMODE) .LE. 0 .OR. RUN == 1) THEN

        WRITE (LUNOV,300)
  300   FORMAT ('*SUMMARY OF SOIL AND GENETIC INPUT PARAMETERS',/)
!-----------------------------------------------------------------------
!       Write soils info
        IF (ISWWAT .NE. 'N') THEN
          WRITE(LUNOV,360)
          DO I = 1, NLAYR
            WRITE (LUNOV,410) NINT(DS(I)-DLAYR(I)),NINT(DS(I)),
     &        LL(I),DUL(I),SAT(I),ESW(I),SWINIT(I),SHF(I),BD(I),
     &        PH(I),INO3(I),INH4(I),OC(I)
          ENDDO
          WRITE (LUNOV,610) NINT(DS(NLAYR)),TLL,TDUL,TSAT,TPESW,
     &                        TSWINI,AINO3,AINH4,NINT(TSOC)
          WRITE (LUNOV,710) SALB,U,SLNF,CN2,SWCON,SLPF
        ENDIF

      ELSE
        WRITE(LUNOV,310)
  310   FORMAT ('*SUMMARY OF GENETIC INPUT PARAMETERS',/)
      ENDIF

C-----------------------------------------------------------------------
!     Write genetic coefficients
      WRITE (LUNOV,800) CROPD,VARTY,VRNAME,ECONO
      IF (INDEX ('BNPNSBFAPECHPPVBCPBRFBCASU',CROP) .GT. 0) THEN
C MODIFIED BY LIWANG MA, RZWQM-DSSAT
            CALL GETPUT_PLANTVAR('GET',PLANTVAR)
            CSDVAR = PLANTVAR% CSDVAR
            PPSEN = PLANTVAR% PPSEN
            PH2T5 = PLANTVAR% PH2T5
            PHTHRS(8) = PLANTVAR% PHTHRS(8)
            PHTHRS(10) = PLANTVAR% PHTHRS(10)
            WTPSD = PLANTVAR% WTPSD
            SDPDVR = PLANTVAR% SDPDVR
            SFDUR = PLANTVAR% SFDUR
            PODUR = PLANTVAR% PODUR
            XFRUIT = PLANTVAR% XFRUIT

         WRITE (LUNOV, 900) CSDVAR,PPSEN,PH2T5,
     &                      PHTHRS(8),PHTHRS(10)
         WRITE (LUNOV,1000) WTPSD,SDPDVR,SFDUR,PODUR,XFRUIT
      ELSEIF (INDEX ('TMPRCBCOCT',CROP) .GT. 0) THEN
C MODIFIED BY LIWANG MA, RZWQM-DSSAT
            CALL GETPUT_PLANTVAR('GET',PLANTVAR)
            CSDVAR = PLANTVAR% CSDVAR
            PPSEN = PLANTVAR% PPSEN
            PH2T5 = PLANTVAR% PH2T5
            PHTHRS(8) = PLANTVAR% PHTHRS(8)
            PHTHRS(10) = PLANTVAR% PHTHRS(10)
            WTPSD = PLANTVAR% WTPSD
            SDPDVR = PLANTVAR% SDPDVR
            SFDUR = PLANTVAR% SFDUR
            PODUR = PLANTVAR% PODUR
            XFRUIT = PLANTVAR% XFRUIT

         WRITE (LUNOV, 900) CSDVAR,PPSEN,PH2T5,
     &                      PHTHRS(8),PHTHRS(10)
         WRITE (LUNOV,1010) WTPSD,SDPDVR,SFDUR,PODUR,XFRUIT
      ELSEIF (INDEX ('MZWHSGBAMLMOMXTRBS',CROP) .GT. 0) THEN
         IF (CROP .EQ. 'MZ'.or.CROP.EQ.'BS') THEN
C MODIFIED BY LIWANG MA, RZWQM-DSSAT
            CALL GETPUT_PLANTVAR('GET',PLANTVAR)
            P1 = PLANTVAR% P1
            P2 = PLANTVAR% P2
            P5 = PLANTVAR% P5
            G2 = PLANTVAR% G2
            G3 = PLANTVAR% G3
            PHINT = PLANTVAR% PHINT

            WRITE (LUNOV, 901) P1,P2,P5
            WRITE (LUNOV,1001) G2,G3,PHINT
          ELSE IF (CROP .EQ. 'SF') THEN
C MODIFIED BY LIWANG MA, RZWQM-DSSAT
            CALL GETPUT_PLANTVAR('GET',PLANTVAR)
            P1 = PLANTVAR% P1
            P2 = PLANTVAR% P2
            P5 = PLANTVAR% P5
            G2 = PLANTVAR% G2
            G3 = PLANTVAR% G3
            O1 = PLANTVAR% O1

            WRITE (LUNOV, 901) P1,P2,P5
            WRITE (LUNOV,1001) G2,G3,O1
         ELSE IF ((CROP .EQ. 'SG').or.(crop.eq.'MO')
     &          .or.(crop.eq.'MX')) THEN
            CALL GETPUT_PLANTVAR('GET',PLANTVAR)
            P1 = PLANTVAR% P1
            P2O = PLANTVAR% P2O
            P2R = PLANTVAR% P2R
            P5 = PLANTVAR% P5
            G1 = PLANTVAR% G1
            G2 = PLANTVAR% G2
            PHINT = PLANTVAR% PHINT
            WRITE (LUNOV, 902) P1,P2O,P2R,P5
            WRITE (LUNOV,1002) G1,G2,PHINT
         ELSE IF (CROP .EQ. 'ML') THEN
c CHANGES MADE BY LIWANG MA, RZWQM-DSSAT
            CALL GETPUT_PLANTVAR('GET',PLANTVAR)
            P1 = PLANTVAR% P1
            P2O = PLANTVAR% P2O
            P2R = PLANTVAR% P2R
            P5 = PLANTVAR% P5
            G1 = PLANTVAR% G1
            G4 = PLANTVAR% G4
            PHINT = PLANTVAR% PHINT

            WRITE (LUNOV, 903) P1,P2O,P2R,P5
            WRITE (LUNOV,1003) G1,G4,PHINT
         ELSE IF (CROP .EQ. 'BA') THEN
C-GH	   ELSE IF (CROP .EQ. 'BA' .OR. CROP .EQ. 'WH') THEN
c CHANGES MADE BY LIWANG MA, RZWQM-DSSAT
            CALL GETPUT_PLANTVAR('GET',PLANTVAR)
            P1V = PLANTVAR% P1V
            P1D = PLANTVAR% P1D
            P5 = PLANTVAR% P5
            G1 = PLANTVAR% G1
            G2 = PLANTVAR% G2
            G3 = PLANTVAR% G3
            PHINT = PLANTVAR% PHINT

            WRITE (LUNOV, 904) P1V,P1D,P5
            WRITE (LUNOV,1004) G1,G2,G3,PHINT
	   ELSE IF ((CROP .EQ. 'WH').or.(crop.eq.'TR')) THEN
c CHANGES MADE BY LIWANG MA, RZWQM-DSSAT
            CALL GETPUT_PLANTVAR('GET',PLANTVAR)
            P1V = PLANTVAR% P1V
            P1D = PLANTVAR% P1D
            P5 = PLANTVAR% P5
            G1 = PLANTVAR% G1
            G2 = PLANTVAR% G2
            G3 = PLANTVAR% G3
            PHINT = PLANTVAR% PHINT

            WRITE (LUNOV, 954) P1V,P1D,P5  !,LT50H
            WRITE (LUNOV,1054) G1,G2,G3,PHINT
         ENDIF
      ELSEIF (INDEX ('CS',CROP) .GT. 0) THEN
         WRITE (LUNOV,2000) (GCOEFF(L),L=1,15)
      ELSEIF (INDEX ('PT',CROP) .GT. 0) THEN
c CHANGES MADE BY LIWANG MA, RZWQM-DSSAT
            CALL GETPUT_PLANTVAR('GET',PLANTVAR)
            G2 = PLANTVAR% G2
            G3 = PLANTVAR% G3
            G4 = PLANTVAR% G4
            PD = PLANTVAR% PD
            P2 = PLANTVAR% P2
            TC = PLANTVAR% TC

         WRITE (LUNOV, 905) G2,G3,G4
         WRITE (LUNOV,1005) PD,P2,TC
      ELSEIF (INDEX ('RI',CROP) .GT. 0) THEN
         WRITE (LUNOV, 906) P1,P2R,P5,P2O
         WRITE (LUNOV,1006) G1,G2,G3,G4
      ELSEIF (INDEX ('SC',CROP) .GT. 0) THEN
         WRITE (LUNOV, 907) P1,RATPOT,LFMAX
         WRITE (LUNOV,1007) G1,PI1,PI2,DTTPI
      ELSEIF (INDEX ('SF',CROP) .GT. 0) THEN
C MODIFIED BY LIWANG MA, RZWQM-DSSAT
            CALL GETPUT_PLANTVAR('GET',PLANTVAR)
            P1 = PLANTVAR% P1
            P2 = PLANTVAR% P2
            P5 = PLANTVAR% P5
            G2 = PLANTVAR% G2
            G3 = PLANTVAR% G3
            O1 = PLANTVAR% O1
         WRITE (LUNOV, 908) P1,P2,P5
         WRITE (LUNOV,1008) G2,G3,O1
      ELSEIF (INDEX ('PI',CROP) .GT. 0) THEN
         WRITE (LUNOV, 909) P2,P3,P4
         WRITE (LUNOV,1009) G2,G3,PHINT
      ELSEIF (INDEX ('TNTR',CROP) .GT. 0) THEN
         WRITE (LUNOV, 911) P1,P3,P4,P5
         WRITE (LUNOV,1011) G3,G4,PHINT,PCINT,PCGRD
      ELSEIF (INDEX ('CO',CROP) .GT. 0) THEN
         WRITE (LUNOV, 912) SCPB,RESPC,SQCON
         WRITE (LUNOV,1012) FCUT,FLAI,DDISQ
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------
      ! RM - removed the $ at the end of 100 FORMAT, this is not used anyway.
  100 FORMAT (1X,'Please press < ENTER > key to continue ')
  350 FORMAT (
     &    4X,'SOIL LOWER UPPER   SAT  EXTR  INIT   ROOT   BULK',
     &    5X,'pH    NO3    NH4    ORG',/,
     &    3X,'DEPTH LIMIT LIMIT    SW    SW    SW   DIST   DENS',26X,
     &    'C',/,
     &    4X,'cm',3X,3('cm3/cm3',4X),5X,'g/cm3',9X,'ugN/g  ugN/g',
     &    5X,'%',/,80('-'))
  360 FORMAT (
     &    3X,'SOIL LOWER UPPER   SAT  EXTR  INIT   ROOT   BULK',
     &    5X,'pH    NO3    NH4    ORG',/,
     &    2X,'DEPTH LIMIT LIMIT    SW    SW    SW   DIST   DENS',26X,
     &    'C',/,
     &    3X,'cm',3X,3('cm3/cm3',4X),5X,'g/cm3',9X,'ugN/g  ugN/g',
     &    5X,'%',/,79('-'))
  400 FORMAT (1X,I3,'-',I3,5(1X,F5.3),6(1X,F6.2))
  410 FORMAT (I3,'-',I3,5(1X,F5.3),6(1X,F6.2))
  600 FORMAT (/,1X,'TOT-',I3,5F6.1,2X,'<--cm   -','  kg/ha-->',2F7.1,I7)
  610 FORMAT (/,'TOT-',I3,5F6.1,2X,'<--cm   -','  kg/ha-->',2F7.1,I7)
  700 FORMAT (1X,'SOIL ALBEDO    :',F5.2,6X,'EVAPORATION LIMIT :',F5.2,
     &        8X,'MIN. FACTOR  :',F5.2,/,1X,'RUNOFF CURVE # :',F5.2,
     &        6X,'DRAINAGE RATE     :',F5.2,8X,'FERT. FACTOR :',F5.2,/)
  710 FORMAT ('SOIL ALBEDO    :',F5.2,6X,'EVAPORATION LIMIT :',F5.2,
     &        9X,'MIN. FACTOR  :',F5.2,/,'RUNOFF CURVE # :',F5.2,
     &        6X,'DRAINAGE RATE     :',F5.2,9X,'FERT. FACTOR :',F5.2,/)
  800 FORMAT (1X,A10,1X,'CULTIVAR :',A6,'-',A16,3X,'ECOTYPE :',
     &        A6)
  900 FORMAT (1X,'CSDVAR :',F5.2,'  PPSEN  :',F5.2,
     &         '  EMG-FLW:',F5.2,'  FLW-FSD:',F5.2,'  FSD-PHM :',F6.2)
 1000 FORMAT (1X,'WTPSD  :',F5.3,'  SDPDVR :',F5.2,
     &         '  SDFDUR :',F5.2,'  PODDUR :',F5.2,'  XFRUIT  :',F6.2,/)
 1010 FORMAT (1X,'WTPSD  :',F5.3,'  SDPDVR :',F5.1,
     &         '  SDFDUR :',F5.2,'  PODDUR :',F5.2,'  XFRUIT  :',F6.2,/)
  901 FORMAT (1X,'P1     :',F7.2,'  P2     :',F7.4,
     &         '  P5     :',F7.2)
  902 FORMAT (1X,'P1     :',F5.1,'  P2O    :',F5.2,
     &         '  P2R    :',F6.1,'  P5     :',F6.2)
  903 FORMAT (1X,'P1     :',F6.2,'  P2O    :',F6.3,
     &         '  P2R    :',F6.1,'  P5     :',F6.2)
!chp  904 FORMAT (1X,'P1V    :',F8.6,'  P1D    :',F8.6,
  904 FORMAT (1X,'P1V    :',F8.3,'  P1D    :',F8.3,
     &         '  P5     :',F8.2)
  954 FORMAT (1X,'P1V    :',F8.3,'  P1D    :',F8.4,
     &         '  P5     :',F8.2)      !,'  LT50H  :',F8.2)
  905 FORMAT (1X,'G2     :',F7.2,'  G3     :',F7.4,'  G4     :',F7.2)
  906 FORMAT (1X,'P1     :',F6.1,'  P2R    :',F6.1,
     &         '  P5     :',F6.1,'  P2O    :',F6.1)
  907 FORMAT (1X,'P1     :',F6.1,'  RATPOT :',F6.1,
     &         '  LFMAX  :',F6.1)
  908 FORMAT (1X,'P1     :',F7.2,'  P2     :',F7.4,
     &         '  P5     :',F7.2)
  909 FORMAT (1X,'P2     :',F6.1,'  P3     :',F6.1,
     &         '  P4     :',F6.0)
  911 FORMAT (1X,'P1     :',F7.1,' P3     :',F7.2,
     &          ' P4     :',F7.1,' P5     :',F7.2)
  912 FORMAT (1X,'SCPB   :',F7.1,' RESPC  :',F7.3,
     &          ' SQCON  :',F7.3)
 1001 FORMAT (1X,'G2     :',F7.2,'  G3     :',F7.3,'  PHINT  :',F7.3)
 1002 FORMAT (1X,'G1     :',F5.1,'  G2     :',F5.2,'  PHINT  :',F6.2)
 1003 FORMAT (1X,'G1     :',F6.2,'  G4     :',F6.2,'  PHINT  :',F6.2)
 1004 FORMAT (1X,'G1     :',F8.3,'  G2     :',F8.3,'  G3     :',F8.3,
     &         '  PHINT  :',F8.3)
 1054 FORMAT (1X,'G1     :',F8.3,'  G2     :',F8.3,'  G3     :',F8.3,
     &         '  PHINT  :',F8.3)
 1005 FORMAT (1X,'PD     :',F7.2,'  P2     :',F7.3,'  TC     :',F7.3)
 1006 FORMAT (1X,'G1     :',F6.1,'  G2     :',F6.4,
     &         '  G3     :',F6.2,'  G4     :',F6.2)
 1007 FORMAT (1X,'G1     :',F6.1,'  PI1    :',F6.1,
     &         '  PI2    :',F6.1,'  DTTPI  :',F6.1)
 1008 FORMAT (1X,'G2     :',F7.2,'  G3     :',F7.3,'  O1     :',F4.0)
 1009 FORMAT (1X,'G2     :',F6.1,'  G3     :',F6.2,'  PHINT  :',F6.1)
 1011 FORMAT (1X,'G3     :',F7.1,' G4     :',F7.1,
     &          ' PHINT  :',F7.1,' PCINT  :',F7.1,' PCGRD  :',F7.1)
 1012 FORMAT (1X,'FCUT   :',F7.3,' FLAI   :',F7.2,
     &          ' DDISQ  :',F7.1)
 2000 FORMAT (1X,'DUB1   :',F6.1,'  DUBR   :',F6.1,'  DESP   :',F6.2,
     &         '  PHCX   :',F6.2,'  S#PE   :',F6.1,/,
     &        1X,'S#FX   :',F6.1,'  S#PX   :',F6.1,'  SWNX   :',F6.1,
     &         '  L#IS   :',F6.2,'  L#IP   :',F6.2,/,
     &        1X,'LALX   :',F6.0,'  LAXA   :',F6.2,'  LAL3   :',F6.0,
     &         '  LAWS   :',F6.0,'  LFLI   :',F6.0)

 2002 FORMAT (1X,'PBAS_TL:',F6.2,'  PSAT_TH:',F6.2)

      END SUBROUTINE OPSOIL
