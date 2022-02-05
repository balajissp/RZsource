ChuddC $Id: Rzman.for,v 1.1 2002/08/27 23:59:42 rojas Exp $
C
      SUBROUTINE CDATE(JDAY,ID,IM,IYYY)
C
C======================================================================
C
C       PURPOSE: DATE RETURNS THE DAY-OF-THE-MONTH AND THE MONTH
C              GIVEN THE JULIAN DATE AND YEAR.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       I      L  INDEX VARIABLE
C       ID        I/O --DAY OF MONTH [EX 12).
C       IDAY   L  JULIAN DATE
C       IM        I/O --MONTH OF YEAR.
C       IYYY   I  --YEAR
C       JDAY   I  JULIAN DAY    [1..366]
C       KDA        L  --ARRAY OF JULIAN DATES REPRESENTING THE LAST DAY
C                 OF THE PREVIOUS MONTH.
C
C       EXTERNAL REFERENCES:
C                 NONE
C
C       PROGRAMMER:   KEN ROJAS
C
C       VERSION:  97.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION KDA(13)
      SAVE KDA
      DATA KDA /0,31,59,90,120,151,181,212,243,273,304,334,365/
C
      IDAY=JDAY
      IF(IYYY.EQ.(IYYY/4)*4) THEN
C
C       LEAP YEAR
        IF(IDAY.EQ.60) THEN
          IM=2
          ID=29
          GOTO 20
        ENDIF
C
        IF(IDAY.GT.60) IDAY=IDAY-1
      ENDIF
      DO 10 I=2,13
        IF(IDAY.LE.KDA(I)) THEN
          IM=I-1
          ID=IDAY-KDA(IM)
          GOTO 20
        ENDIF
C
   10 CONTINUE
   20 RETURN
      END
C
      SUBROUTINE CJULDATE(JULDAY,ID,IM,IYYY)
C
C======================================================================
C
C       PURPOSE: DATE RETURNS THE DAY-OF-THE-MONTH AND THE MONTH
C              GIVEN THE JULIAN DATE AND YEAR.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       I      L  INDEX VARIABLE
C       ID        I/O --DAY OF MONTH [EX 12).
C       IDAY   L  JULIAN DATE
C       IM        I/O --MONTH OF YEAR.
C       IYYY   I  --YEAR
C       JDAY   I  JULIAN DAY    [1..366]
C       KDA        L  --ARRAY OF JULIAN DATES REPRESENTING THE LAST DAY
C                 OF THE PREVIOUS MONTH.
C
C       EXTERNAL REFERENCES:
C                 NONE
C
C       PROGRAMMER:   KEN ROJAS
C
C       VERSION:  97.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      L=JULDAY+68569
	N=INT((4*L)/146097)
	L=L-INT((146097*N+3)/4)
	I=INT((4000*(L+1))/1461001)
	L=L-INT((1461*I)/4)+31
	J=INT((80*L)/2447)
	ID=L-INT((2447*J)/80)
	L=INT(J/11)
	IM=J+2-(12*L)
	IYYY=100*(N-49)+I+L
      RETURN
      END
C
      SUBROUTINE MAIRR(JDAY,NDXN2H,RDF,AIRR,JGROW,IYYY,IRPL,ADIW,IRLOC,
     +pori,ADIWMONTH,iyb,isumpet1,irrtyp,airrtype,tadiw,niw,curradiw,
     +ipl,TLCSHAW,TLEAFU,TLEAFL,T14)
C
C======================================================================
C
C       PURPOSE:  THIS ROUTINE HANDLES THE IRRIGATION MANAGEMENT. THERE
C             ARE 3 TYPES OF IRRIGATION ALLOWED: FLOOD/FURROW,
C             SPRINKLER, AND DRIP.  THERE ARE ALSO 3 TYPES OF APP
C             METHODS AVAILABLE: SPECIFIED DATE, INTERVAL DATE, AND
C             ROOT ZONE DRYING CRITERIA.  TO MATCH THESE ARE 3
C             METHODS FOR DETERMINING AMOUNT TO IRRIGATE: FIXED AMT,
C             VARYING AMT WITH DATE, REFILL ROOT ZONE.
C
C       REF:  K. E. SAXTON USDA-ARS -- SPAW MODEL
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       ADIW   L  ACC. DEPTH OF IRRIGATION WATER.[CM]
C       AEF        I  FIELD SATURATION FRACTION [0..1]
C       AIRR  I/O AMOUNT OF IRRIGATION WATER TO APPLY
C       DOIMAX     I  MAXIMUM DEPTH OF IRRIGATION TO BE APPLIED DURING THE
C                 SEASON.  IF DOIMAS = 0.0, THEN THE SEASON'S AMT OF
C                 WATER FOR IRRIGATION IS ASSUMED TO BE UNLIMITED.[CM]
C       FC        I/O VOL FIELD CAPCITY [CM^3-W/CM^3-S]
C       FDOI   I  FIXED DEPTH OF IRRIGATION.  A CONSTANT DEPTH OF
C                 WATER TO BE APPLIED WHEN IRRIGATION IS NECESSARY.
C                 CORRESPONDS TO IDOI = 1.[CM]
C       I      L  INDEX VARIABLE
C       ID         L  DAY OF MONTH [EX 12].
C       IDOI   I  DETERMINES METHOD OF IRRIGATION.  FOR IDOI =
C                   1.    FIXED AMT PER IRRIGATION
C                   2.    VARIABLE AMT PER IRRIGATION
C                   3.    REFILL ROOTZONE TO LIMIT OF AVAILABLE WATER
C       IEVNT I/O LOGICAL CONTAINING TRUE WHEN AN IRRIGATION EVENT
C                 HAS TAKEN PLACE IN A DAY
C       IM         L  MONTH [12, FOR DECEMBER]
C       IRLOC  I  ARRAY LOCATION OF THE IRRIGATION MANAGEMENT VALID
C                 FOR THE GIVEN JULDAY
C       ISPT  I/O
C       ITOI   I  DETERMINES WHEN TO IRRIGATE. ITOI =
C                   1.    FIXED INTERVAL
C                   2.    SPECIFIED DATES FOR THE SEASON
C                   3.    SOIL WATER DEPLETION USING ONLY THE ROOT ZONE
C       IYYY   I  YEAR ALL FOUR DIGITS
C       JDAY   I  JULIAN DAY    [1..366]
C       JDIRRB     I  JULIAN DATE ON WHICH IRRIGATION CAN BEGIN
C       JDIRRE     I  JULIAN DATE ON WHICH IRRIGATION MUST END
C       JDOI  I/O ARRAY CONTAINING THE JULIAN DATES OF IRRIGATION
C       JDOPI I/O JULIAN DATE OF PREVIOUS IRRIGATION (FOR IDOI=3 ONLY)
C       JGROW  I  NUMBER OF DAYS SINCE PLANTING
C                 AFTER HARVEST JGROW= -999.
C       JH         L  HORIZON INDEX
C       JKU        I  ARRAY OF DAYS SINCE PLANTING CORRESPONDING TO XKU.
C       JMAD   I  ARRAY OF DAYS SINCE PLANTING CORRESPONDING TO XMAD.
C       JULDAY     L  ACTUAL JULIAN DAY [MAY 23,1968 = 2,440,000]
C                 (REFERENCES TO JULIAN DAY IN MAIRR ARE OF THIS TYPE
C                  EXCEPT FOR JDAY)
C       L      L  INDEX VARIABLE
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODT     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       MXSPEC     P
C       NCOLK  L  INDICATES APPROPRIATE PAIR OF JKU AND XKU.
C       NCOLM  L  INDICATES APPROPRIATE PAIR OF JMAD AND XMAD.
C       NDAIRR     I  MINIMUM NUMBER OF DAYS BETWEEN IRRIGATIONS.
C                 (NOTE:  NOT APPLICABLE FOR ITOI = 1 OR 2.
C                       FOR ITOI = 1 OR 2, NDAIRR = 0)
C       NDRZ   L  INDEX ASSOCIATED WITH BOTTOM-MOST LAYER IN
C                 THE ROOT ZONE.
C       NDXN2H     I  INDEX FOR NUMERICAL LAYERS TO HORIZONS,
C                   IE. WHICH HORIZON IS NUMERICAL LAYER IN.
C       NKU        I  NUMBER OF PAIRS OF JKU AND XKU.
C       NMAD   I  NUMBER OF PAIRS OF JMAD AND XMAD.
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       RDF        I  ROOT DISTRIBUTION TABLE [0..1]
C       SEVNT  I  LOGICAL CONTAINING TRUE WHEN AN SURFACE
C                 APPLICATION EVENT HAS TAKEN PLACE IN A DAY
C       SMDEP  L  DEC.%--SOIL MOISTURE DEPLETION IN EITHER
C                 THE ROOT ZONE OR IN THE TOTAL SOIL PROFILE
C                 (DEPENDING ON WHETHER ITOI = 3 OR 4).  COMPARED
C                 TO THE APPROPRIATE VALUE OF XMAD TO DETERMINE
C                 IF IRRIGATION IS NECESSARY.
C       SOILHP     I  MODIFIED BROOKS-COREY PARAMETERS
C                   (1):   HB    - BUBBLING PRESSURE O(H) [CM]
C                   (2):   LAMDA - PORE SIZE DISTRIBUTION INDEX
C                   (3):   EPS   - EXPONENT FOR K(H) CURVE
C                   (4):   KSAT  - SAT HYDRAULIC CONDUCT [CM/HR]
C                   (5):   WR    - RESIDUAL WATER CONTENT
C                   (6):   WS    - SATURATION WATER CONTENT
C                   (7):   WFC   - FIELD CAPACITY (1/3 BAR) WC
C                   (8):   WFC   - FIELD CAPACITY (1/10 BAR) WC
C                   (9):   WWP   - WILTING POINT (15 BAR) WC
C                   (10):  HB    - BUBBLING PRESSURE K(H) CURVE [CM]
C                   (11):  C2    - SECOND INTRCEPT ON K(H) CURVE
C                   (12):  N1    - FIRST EXPONENT FOR K(H) CURVE
C                   (13):  A1    - CONSTANT FOR O(H) CURVE
C       SUMM   L  ACCUMS IRRIGATION DEPTHS TO PREVENT EXCEEDING
C                 MAXIMUM DEPTH PROFILE WILL HOLD FOR DEPTH
C                 OPTIONS 5 AND 6.[CM]
C       TAWIRZ     L  TOTAL AVAILABLE WATER IN THE ROOT ZONE.[CM]
C       TEVNT  I  LOGICAL CONTAINING TRUE WHEN AN EVENT
C                 INVOLVING TILLAGE HAS TAKEN PLACE IN A DAY
C       THETA  I  SOIL MOISTURE IN INCHES FOR EACH LAYER.[CM]
C       TL         I  THICKNESS OF EACH LAYER [CM]
C       TPAWRZ     L  THE TOTAL POSSIBLE AVAILABLE WATER WITHIN THE
C                 ROOT ZONE.[CM]
C       ULOI   L  %--UPPER LIMIT OF IRRIGATION.  MOISTURE LEVEL
C                 TO WHICH EACH SOIL LAYER WILL BE RECHARGED AT
C                 IRRIGATION TIME. PRODUCT OF XKU AND FC.
C       VDOI   I  VARYING DEPTH OF IRRIGATION (INCHES).  ENABLES
C                 THE USER TO INPUT VARYING DEPTHS OF IRRIGATION
C                 OVER THE COURSE OF THE SEASON.  SEE JDOI.[CM]
C       XKU        I  MODIFYING FACTOR USED TO INCREASE OR DECREASE THE
C                 UPPER LIMIT TO WHICH THE SOIL CAN BE RECHARGED.
C                 IF = 1 THEN THE SOIL IS RECHARGED TO FIELD CAPACITY.
C                 IF < 1 THEN THE SOIL IS RECHARGED TO BELOW FIELD
C                      CAPACITY (DEFICIT IRRIGATION)
C                 IF > 1 THEN THE SOIL IS RECHARGED TO ABOVE FIELD
C                      CAPACITY (LEACHING).
C                 XKU MAY BE VARIED OVER THE COURSE OF THE SEASON.
C       XMAD   I  DEC.%--LIMITING VALUE OF MAXIMUM ALLOWABLE SOIL
C                 WATER DEF BEYOND WHICH IRRIGATION IS NECESSARY
C       XWP       I/O
C
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                 CDATE
C                 JULDATE
C                 MANOUT
C
C       CALLED FROM:  --MAQUE
C
C       PROGRAMMER:  KEN ROJAS  (MODIFIED FROM SAXTON VERSION)
C                AMY GARRISON
C
C       VERSION:  3.2
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,MAXHOR=12,MXAPP=200)
C
      COMMON /IRRIG/ DOIMAX(MXAPP),FDOI(MXAPP),VDOI(MXAPP,MXAPP),
     +    XKU(MXAPP,20),XMAD(MXAPP,20),RRATE(MXAPP),amxirr,
     +    totaliw,DOIMONTH(12,200),TOTALMONTH,airrdepth,amirr,
     +    RRDUR(MXAPP),IIPL(MXAPP),ISPT(MXAPP),ITOI(MXAPP),
     +    JDIRRB(MXAPP),JDIRRE(MXAPP),NAPPI,NDAIRR(MXAPP),NKU(MXAPP),
     +    NMAD(MXAPP),JDOI(MXAPP,MXAPP),JKU(MXAPP,20),JMAD(MXAPP,20),
     +    IDOI(MXAPP),id_depth,MONTHIRR
c
      COMMON /HYDROL/ AEF,BOTFLX,DRDEP,DRSPAC,DRRAD,CLAT(MAXHOR),
     +    H(MXNOD),HKBAR(MXNOD),QF(MXNOD),SOILHP(13,MAXHOR),HFC,HWP,
     +    THETA(MXNOD),thetai(mxnod),RICH_EPS,IREBOT,ITBL,IDRAIN,
     +    MAXITER_RICH,MAXCYCLE_RICH
      COMMON /GEOMTR/XLONG,ASPECT,DELZ(MXNOD),ELEV,HORTHK(MAXHOR),SLOPE,
     +   AREA,TL(MXNOD),TLT(MXNOD),XLAT,ZN(MXNOD),NHOR,NN,NNT
      common/sumet/sumAET,sumCropET,sumPET,sumPT,sumAT,sumrain,deltheta
C
      DIMENSION RDF(MXNOD),NDXN2H(MXNOD),ADIW(MXAPP),totadiw(20)
      INTEGER JDOPI,irrtyp(mxapp)
      DIMENSION FC(MXNOD),pori(mxnod),adiwmonth(12,200),XWP(MXNOD)
      character*30 textmethod(5)
      character*10 airrtype(5)
C
      SAVE JDOPI,sumAET1,sumPET1,sumCropET1,NDRZ,sumAT1,sumPT1,sumrain1
      DIMENSION sumAET2(30),sumPET2(30),sumCropET2(30),sumAT2(30),
     &          sumPT2(30),sumrain2(30)
      DATA FC /MXNOD*0.0D0/,JDOPI /0/,totalavail/1.0d5/
      data textmethod/'FIXED INTERVAL','SPECIFIC DATES','RZ WATER 
     &DEPLETION','ET BASED', 'Canopy T'/
C      save adiwmonth
C
      AIRR=0.0D0
      if (isumpet1.eq.1) then
                  isumpet1=0
                  DO JJ=1,30
                  sumPET2(JJ)=0.0d0
                  sumAET2(JJ)=0.0d0
                  sumPT2(JJ)=0.0d0
                  sumAT2(JJ)=0.0d0
                  sumCropET2(JJ)=0.0d0
                  SUMRAIN2(JJ)=0.0D0
                  ENDDO
      endif
      CALL CDATE(JDAY,ID,IM,IYYY)
      JULDAY=JULDATE(ID,IM,IYYY)
      iyi=iyyy-iyb+1
c
      if((totaliw.gt.0.0d0).and.(niw.gt.1)) then
      if((tadiw*min(niw-1,iyyy-iyb)+curradiw).lt.totaliw) then
        totalavail=totaliw-(tadiw*min(niw-1,iyyy-iyb)+curradiw)
      else
        print*,'maximum irrigation water',nint(totaliw),  !round up, int is round down
     &'cm reached in',niw,'years'
c        write (70,*)'maximum irrigation water',nint(totaliw),
c     &'cm reached in',niw,'years'
          return   !skip irrigation event
      endif
      else
        totalavail=1.0d5  !make it very high so that will not be a limitation
      endif
c
      DO 110 II=1,NAPPI
      IF (((IDOI(II).EQ.3. OR. IDOI(II).EQ.4)).AND.IPL.EQ.0) GOTO 120
c
        IF(IIPL(II).EQ.IRPL) THEN
          IF(JULDAY.GE.JDIRRB(II).AND.JULDAY.LE.JDIRRE(II)) THEN
c          IF(JULDAY.LE.Julday+NDAIRR(II)) then
                 JDOPI=max(jdopi,JDIRRB(II))
                 sumPET1=SUMMOV(SUMPET2,NDAIRR(II),SUMPET,JDAY)  !sumPET1+sumPET
                 sumAET1=SUMMOV(SUMAET2,NDAIRR(II),SUMAET,JDAY)  !sumAET1+sumAET
                 sumPT1=SUMMOV(SUMPT2,NDAIRR(II),SUMPT,JDAY)   !sumPT1+sumPT
                 sumAT1=SUMMOV(SUMAT2,NDAIRR(II),SUMAT,JDAY)   !sumAT1+sumAT
                 sumCropET1=SUMMOV(SUMCROPET2,NDAIRR(II),SUMCROPET,JDAY) !sumCropET1+sumCropET
                 sumrain1=SUMMOV(SUMRAIN2,NDAIRR(II),SUMRAIN,JDAY)   !sumrain1+sumrain
c          else
c                  sumAET1=0.0d0
c                  sumCropET1=0.0d0
c                  sumPET1=0.0d0
c          endif
         IF ((TOTALMONTH.EQ.0.0D0).OR.(MONTHIRR.EQ.0)) THEN
C  LIMIT IRRIGATION BY TOTAL SEASONAL WATER AMOUNT
            IF(DOIMAX(II).EQ.0..OR.ADIW(II).LT.DOIMAX(II)) THEN
C
C=========================================================
C             *** 1) FIXED INTERVAL OR 2) SPECIFIED DATES FOR SEASON
C=========================================================
C
              IF(ITOI(II).EQ.1.OR.ITOI(II).EQ.2) THEN
                DO 10 K=1,MXAPP
                  IF(JDOI(II,K).EQ.JULDAY) THEN
                    PRINT *, "RZMAN CALLING MANOUT"
                    CALL MANOUT(19,0,JDAY,0,IYYY)
                    WRITE(70,1000) IM,ID,IYYY,textmethod(ITOI(II))
C=========================================================
C                   *** 1) FIXED AMOUNT PER IRRIGATION
C=========================================================
C
                    IF(IDOI(II).EQ.1) THEN
                      AIRR=FDOI(II)
                      IF(DOIMAX(II).NE.0.0D0) THEN
                        SUMM=ADIW(II)+AIRR
                        IF(SUMM.GE.DOIMAX(II)) THEN
                          AIRR=DOIMAX(II)-ADIW(II)
                        ENDIF
                      ENDIF
C=========================================================
C                   *** 2) VARYING AMOUNT PER IRRIGATION
C=========================================================
C
                    ELSEIF(IDOI(II).EQ.2) THEN
                      ISPT(II)=ISPT(II)+1
                      AIRR=VDOI(II,ISPT(II))
                      IF(DOIMAX(II).NE.0.0D0) THEN
                        SUMM=ADIW(II)+AIRR
                        IF(SUMM.GE.DOIMAX(II)) THEN
                          AIRR=DOIMAX(II)-ADIW(II)
                        ENDIF
                      ENDIF
                    ELSE
                      STOP'INPUT INCORRECT FOR IRRIGATION (TIMING)'
                    ENDIF
                    airr=min(airr,totalavail)
                    WRITE(70,1200) AIRR,airrtype(irrtyp(II))
                    WRITE(70,*) ' '
                  ENDIF
   10           CONTINUE
C=========================================================
C             *** 3) SOIL WATER DEPLETION USING ONLY THE ROOT ZONE
C=========================================================
C
              ELSEIF(ITOI(II).EQ.3) THEN
                  DO 61 L=1,NN
                    IF ((id_depth.eq.0).and.
     &                 (RDF(L).EQ.0.0D0)) GOTO 71
                    IF ((id_depth.gt.0).and.
     &                 (tlt(l).gt.dble(id_depth))) GOTO 71
                    NDRZ=L
   61             CONTINUE
71                DO 20 L=1,NN
                  JH=NDXN2H(L)
c                  FC(L)=SOILHP(6,JH)*AEF
C                  FC(L)=SOILHP(7,JH)
C                  XWP(II,L)=SOILHP(9,JH)
                  FC(L)=WC(HFC,SOILHP(1,JH),JH,0)
                  XWP(L)=WC(HWP,SOILHP(1,JH),JH,0)
   20           CONTINUE
                TAWIRZ=0.0D0
                TPAWRZ=0.0D0
                DO 30 L=1,NDRZ
                    TPAWRZ=TPAWRZ+(FC(L)-XWP(L))*TL(L)
                    TAWIRZ=TAWIRZ+(THETA(L)-XWP(L))*TL(L)
   30           CONTINUE
               if(id_depth.gt.0) then
                TPAWRZ=TPAWRZ+(FC(NDRZ+1)-XWP(NDRZ+1))
     &                *max(0.0d0,(dble(id_depth)-TLT(NDRZ)))
                TAWIRZ=TAWIRZ+(THETA(NDRZ+1)-XWP(NDRZ+1))
     &                *max(0.0d0,(dble(id_depth)-TLT(NDRZ)))
               endif
                IF(TPAWRZ.EQ.0.0D0) GOTO 120
c                TPAWRZ=TPAWRZ+(FC(1)-XWP(II,1))*TL(1)
c                TAWIRZ=TAWIRZ+(THETA(1)-XWP(II,1))*TL(1)
                SMDEP=TAWIRZ/TPAWRZ
                DO 40 NCOLM=1,NMAD(II)
                  IF(JGROW.LE.JMAD(II,NCOLM+1)) GOTO 50
   40           CONTINUE
                NCOLM=NMAD(II)
   50           IF(nint(SMDEP*100.d0).GT.nint(XMAD(II,NCOLM)*100.d0))
     &              GOTO 120
                IF(JULDAY.LT.JDOPI+NDAIRR(II)) GOTO 120
                PRINT *, "RZMAN CALLING MANOUT"
                CALL MANOUT(19,0,JDAY,0,IYYY)
                WRITE(70,1000) IM,ID,IYYY,textmethod(ITOI(II))
                WRITE(70,1100) SMDEP*100.0D0,XMAD(II,NCOLM)*100.0D0
C=========================================================
C               *** 3) REFILL ROOTZONE TO UPPER LIMIT OF AVAILABLE WATER
C=========================================================
C
                IF(IDOI(II).EQ.3) THEN
                  DO 80 NCOLK=1,NKU(II)
                    IF(JGROW.LE.JKU(II,NCOLK+1)) GOTO 90
   80             CONTINUE
                  NCOLK=NKU(II)
   90             DO 100 L=1,NDRZ
                    ULOI=XKU(II,NCOLK)*(FC(L)-XWP(L))*TL(L)
c                    IF (ULOI.LT.1.0) GOTO 120                        !IRRIGATE 1.0 CM MINIMUM
                    AIRR=AIRR+ULOI-(THETA(L)-XWP(L))*TL(L)   !to allow negative demand as a storage
  100             CONTINUE
                  if (id_depth.gt.0) then
                  ULOI=XKU(II,NCOLK)*(FC(NDRZ+1)-XWP(NDRZ+1))
     &                   *max((dble(id_depth)-TLT(NDRZ)),0.0d0)            !TPAW*%refill
c                    IF (ULOI.LT.1.0) GOTO 120                        !IRRIGATE 1.0 CM MINIMUM
                  AIRR=AIRR+ULOI-(THETA(NDRZ+1)-XWP(NDRZ+1))      !TPAW*%refill-PAW, to allow negative demand as a storage
     &                   *max((dble(id_depth)-TLT(NDRZ)),0.0d0)            !TPAW*%refill
                  endif
c                  if (sumAT.eq.0.0d0) AIRR=0.0d0    !no auto irrigation when no plant 
                  AIRR=MAX(AIRR,0.0D0)
                  AIRR=Min(AIRR,amxirr)
                    airr=min(airr,totalavail)
                  if (airr.lt.amirr) then  !.or.sumAT.eq.0.0d0) then
                    print*,'skip irrigation because amount is too small'
                    airr=0.0d0
                    goto 120
                  endif
                  IF(AIRR.GT.0.0D0) JDOPI=JULDAY
                  IF(DOIMAX(II).NE.0.0D0) THEN
                    SUMM=ADIW(II)+AIRR
                    IF(SUMM.GE.DOIMAX(II)) THEN
                      AIRR=DOIMAX(II)-ADIW(II)
                    ENDIF
                  ENDIF
C                 WRITE (70,*) 'ROOTS EXTEND TO LAYER :',NDRZ
C                 WRITE (70,710) IDOI(II)
                  WRITE(70,1300) AIRR,XKU(II,NCOLK)*100.0D0
                ELSE
                  STOP'INPUT INCORRECT FOR IRRIGATION (AMT)'
                ENDIF
C=========================================================
C             *** 4) ET based irrigation
C=========================================================
C
              ELSEIF(ITOI(II).EQ.4) THEN
                IF(sumPET1.EQ.0.0D0.or.sumAET1.eq.0.0d0) GOTO 120
                 SMDEP=min(sumAET1/sumPET1,1.0d0)
c                IF(sumPT1.EQ.0.0D0.or.sumAT1.eq.0.0d0) GOTO 120
c                 SMDEP=sumAT1/sumPT1
                DO 140 NCOLM=1,NMAD(II)
                  IF(JGROW.LE.JMAD(II,NCOLM+1)) GOTO 150
  140           CONTINUE
                NCOLM=NMAD(II)
  150           continue
                IF(nint(SMDEP*100.0d0).GT.nint(XMAD(II,NCOLM)*100.d0))
     &              GOTO 120
                IF(JULDAY.LT.JDOPI+NDAIRR(II)) GOTO 120
                PRINT *, "RZMAN CALLING MANOUT"
                CALL MANOUT(19,0,JDAY,0,IYYY)
                WRITE(70,1000) IM,ID,IYYY,textmethod(ITOI(II))
                WRITE(70,1400) SMDEP*100.d0,XMAD(II,NCOLM)*100.d0
C=========================================================
C               *** 4) ET based refill
C=========================================================
C
                IF(IDOI(II).EQ.4) THEN
                DO 220 L=1,NN
                  JH=NDXN2H(L)
C                  FC(L)=SOILHP(7,JH)
C                  XWP(II,L)=SOILHP(9,JH)
                  FC(L)=WC(HFC,SOILHP(1,JH),JH,0)
                  XWP(L)=WC(HWP,SOILHP(1,JH),JH,0)
220              continue
                  DO 160 L=1,NN
                    IF (RDF(L).EQ.0.0D0) GOTO 170
                    NDRZ=L
  160             CONTINUE
  170             DO 180 NCOLK=1,NKU(II)
                    IF(JGROW.LE.JKU(II,NCOLK+1)) GOTO 190
  180             CONTINUE
                  NCOLK=NKU(II)
  190             ULOI=0.0d0
                  DO 200 L=1,NDRZ                  !to only the root zone?
                    ULOI=ULOI+(FC(L)-theta(L))*TL(L)    !maximum irrigation amount
c                    AIRR=AIRR+MAX(0.0D0,ULOI-THETA(L)*TL(L))
  200             CONTINUE
                    ULOIET=max(0.0d0,XKU(II,NCOLK)*sumPET1-sumrain1)
c                    IF (ULOIET.LT.1.0) GOTO 120                       !IRRIGATE 1.0 CM MINIMUM
                    AIRR=AIRR+MAX(0.0D0,min(ULOIET,ULOI))
                  AIRR=Min(AIRR,amxirr)
                    airr=min(airr,totalavail)
                  if (airr.lt.amirr) then  !.or.sumAT.eq.0.0d0) then
                    print*,'skip irrigation because amount is too small'
                    airr=0.0d0
                    goto 120
                  endif
c                  sumRain1=0.0d0
C                  sumRain1=max(0.0d0,sumRain1-XKU(II,NCOLK)*sumPET1)  !excess rain is used in the next irrigation
                  DO JJ=1,30
                  sumPET2(JJ)=0.0d0
                  sumAET2(JJ)=0.0d0
                  sumPT2(JJ)=0.0d0
                  sumAT2(JJ)=0.0d0
                  sumCropET2(JJ)=0.0d0
                  SUMRAIN2(JJ)=0.0D0
                  ENDDO
c                    AIRR=MAX(AIRR,0.0D0)
c                  IF(AIRR.GT.0.0D0) JDOPI=JULDAY
                  JDOPI=JULDAY
                  IF(DOIMAX(II).NE.0.0D0) THEN
                    SUMM=ADIW(II)+AIRR
                    IF(SUMM.GE.DOIMAX(II)) THEN
                      AIRR=DOIMAX(II)-ADIW(II)
                    ENDIF
                  ENDIF
C                 WRITE (70,*) 'ROOTS EXTEND TO LAYER :',NDRZ
C                 WRITE (70,710) IDOI(II)
                  WRITE(70,1500) AIRR,XKU(II,NCOLK)*100.d0
                ELSE
                  STOP'INPUT INCORRECT FOR IRRIGATION (AMT)'
                ENDIF
c              ELSE
c                STOP'INPUT INCORRECT FOR IRRIGATION (DATES)'
c              ENDIF
c
C=========================================================
C             *** 5) Canopy T based irrigation:  need to be modified by Fang
C=========================================================
C
              ELSEIF(ITOI(II).EQ.5) THEN
                IF(sumPET1.EQ.0.0D0.or.sumAET1.eq.0.0d0) GOTO 120
                IF ((TLEAFU-T14)-(TLEAFL-T14).GT.0) THEN
                 CWSI=min(((TLCSHAW-T14)-(TLEAFL-T14))/
     &                    ((TLEAFU-T14)-(TLEAFL-T14)),1.0d0)
                 CWSI = MAX(0.0D0,CWSI)
                 ELSE
                 CWSI = -1.0D0
                 ENDIF
                DO 144 NCOLM=1,NMAD(II)
                  IF(JGROW.LE.JMAD(II,NCOLM+1)) GOTO 154
  144           CONTINUE
                NCOLM=NMAD(II)
  154           continue
                IF(nint(CWSI).LT.nint(XMAD(II,NCOLM))) GOTO 120
                IF(JULDAY.LT.JDOPI+NDAIRR(II)) GOTO 120
                PRINT *, "RZMAN CALLING MANOUT"
                CALL MANOUT(19,0,JDAY,0,IYYY)
                WRITE(70,1000) IM,ID,IYYY,textmethod(ITOI(II))
                WRITE(70,1400) CWSI,XMAD(II,NCOLM)
C=========================================================
C               *** 5) ET based refill (no changes needed)
C=========================================================
C
                IF(IDOI(II).EQ.4) THEN
                DO 224 L=1,NN
                  JH=NDXN2H(L)
C                  FC(L)=SOILHP(7,JH)
C                  XWP(II,L)=SOILHP(9,JH)
                  FC(L)=WC(HFC,SOILHP(1,JH),JH,0)
                  XWP(L)=WC(HWP,SOILHP(1,JH),JH,0)
224              continue
                  DO 164 L=1,NN
                    IF (RDF(L).EQ.0.0D0) GOTO 174
                    NDRZ=L
  164             CONTINUE
  174             DO 184 NCOLK=1,NKU(II)
                    IF(JGROW.LE.JKU(II,NCOLK+1)) GOTO 194
  184             CONTINUE
                  NCOLK=NKU(II)
  194             ULOI=0.0d0
                  DO 204 L=1,NDRZ                  !to only the root zone?
                    ULOI=ULOI+(FC(L)-theta(L))*TL(L)    !maximum irrigation amount
c                    AIRR=AIRR+MAX(0.0D0,ULOI-THETA(L)*TL(L))
  204             CONTINUE
                    ULOIET=max(0.0d0,XKU(II,NCOLK)*sumPET1-sumrain1)
c                    IF (ULOIET.LT.1.0) GOTO 120                       !IRRIGATE 1.0 CM MINIMUM
                    AIRR=AIRR+MAX(0.0D0,min(ULOIET,ULOI))
                  AIRR=Min(AIRR,amxirr)
                    airr=min(airr,totalavail)
                  if (airr.lt.amirr) then  !.or.sumAT.eq.0.0d0) then
                    print*,'skip irrigation because amount is too small'
                    airr=0.0d0
                    goto 120
                  endif
c                  sumRain1=0.0d0
C                  sumRain1=max(0.0d0,sumRain1-XKU(II,NCOLK)*sumPET1)  !excess rain is used in the next irrigation
                  DO JJ=1,30
                  sumPET2(JJ)=0.0d0
                  sumAET2(JJ)=0.0d0
                  sumPT2(JJ)=0.0d0
                  sumAT2(JJ)=0.0d0
                  sumCropET2(JJ)=0.0d0
                  SUMRAIN2(JJ)=0.0D0
                  ENDDO
c                    AIRR=MAX(AIRR,0.0D0)
c                  IF(AIRR.GT.0.0D0) JDOPI=JULDAY
                  JDOPI=JULDAY
                  IF(DOIMAX(II).NE.0.0D0) THEN
                    SUMM=ADIW(II)+AIRR
                    IF(SUMM.GE.DOIMAX(II)) THEN
                      AIRR=DOIMAX(II)-ADIW(II)
                    ENDIF
                  ENDIF
C                 WRITE (70,*) 'ROOTS EXTEND TO LAYER :',NDRZ
C                 WRITE (70,710) IDOI(II)
                  WRITE(70,1500) AIRR,XKU(II,NCOLK)*100.d0
                ELSEIF(IDOI(II).EQ.3) THEN
                  DO 83 NCOLK=1,NKU(II)
                    IF(JGROW.LE.JKU(II,NCOLK+1)) GOTO 93
   83             CONTINUE
                  NCOLK=NKU(II)
   93             DO 103 L=1,NDRZ
                    ULOI=XKU(II,NCOLK)*(FC(L)-XWP(L))*TL(L)
c                    IF (ULOI.LT.1.0) GOTO 120                        !IRRIGATE 1.0 CM MINIMUM
                    AIRR=AIRR+ULOI-(THETA(L)-XWP(L))*TL(L)   !to allow negative demand as a storage
  103             CONTINUE
                  if (id_depth.gt.0) then
                  ULOI=XKU(II,NCOLK)*(FC(NDRZ+1)-XWP(NDRZ+1))
     &                   *max((dble(id_depth)-TLT(NDRZ)),0.0d0)            !TPAW*%refill
c                    IF (ULOI.LT.1.0) GOTO 120                        !IRRIGATE 1.0 CM MINIMUM
                  AIRR=AIRR+ULOI-(THETA(NDRZ+1)-XWP(NDRZ+1))      !TPAW*%refill-PAW, to allow negative demand as a storage
     &                   *max((dble(id_depth)-TLT(NDRZ)),0.0d0)            !TPAW*%refill
                  endif
c                  if (sumAT.eq.0.0d0) AIRR=0.0d0    !no auto irrigation when no plant 
                  AIRR=MAX(AIRR,0.0D0)
                  AIRR=Min(AIRR,amxirr)
                    airr=min(airr,totalavail)
                  if (airr.lt.amirr) then  !.or.sumAT.eq.0.0d0) then
                    print*,'skip irrigation because amount is too small'
                    airr=0.0d0
                    goto 120
                  endif
                  IF(AIRR.GT.0.0D0) JDOPI=JULDAY
                  IF(DOIMAX(II).NE.0.0D0) THEN
                    SUMM=ADIW(II)+AIRR
                    IF(SUMM.GE.DOIMAX(II)) THEN
                      AIRR=DOIMAX(II)-ADIW(II)
                    ENDIF
                  ENDIF
C                 WRITE (70,*) 'ROOTS EXTEND TO LAYER :',NDRZ
C                 WRITE (70,710) IDOI(II)
                  WRITE(70,1300) AIRR,XKU(II,NCOLK)*100.0D0
                  else
                  STOP'INPUT INCORRECT FOR IRRIGATION (AMT)'
                ENDIF
              ELSE
                STOP'INPUT INCORRECT FOR IRRIGATION (DATES)'
              ENDIF
c
              ADIW(II)=ADIW(II)+AIRR
              curradiw=curradiw+airr
              IRLOC=II
              GOTO 120
            ENDIF
      ELSE   !lIMITED BY MONTHLY WATER AVAILABILITY
C  
            IF(ADIWMONTH(IM,iyi).LT.DOIMONTH(IM,iyi)) THEN
C
C=========================================================
C             *** 1) FIXED INTERVAL OR 2) SPECIFIED DATES FOR SEASON
C=========================================================
C
              IF(ITOI(II).EQ.1.OR.ITOI(II).EQ.2) THEN
                DO 11 K=1,MXAPP
                  IF(JDOI(II,K).EQ.JULDAY) THEN
                    PRINT *, "RZMAN CALLING MANOUT"
                    CALL MANOUT(19,0,JDAY,0,IYYY)
                    WRITE(70,1000) IM,ID,IYYY,textmethod(ITOI(II))
C=========================================================
C                   *** 1) FIXED AMOUNT PER IRRIGATION
C=========================================================
C
                    IF(IDOI(II).EQ.1) THEN
                      AIRR=FDOI(II)
                      IF(DOIMONTH(IM,iyi).NE.0.0D0) THEN
                        SUMM=ADIWMONTH(IM,iyi)+AIRR
                        IF(SUMM.GE.DOIMONTH(IM,iyi)) THEN
                          AIRR=DOIMONTH(IM,iyi)-ADIWMONTH(IM,iyi)
                        ENDIF
                      ENDIF
C=========================================================
C                   *** 2) VARYING AMOUNT PER IRRIGATION
C=========================================================
C
                    ELSEIF(IDOI(II).EQ.2) THEN
                      ISPT(II)=ISPT(II)+1
                      AIRR=VDOI(II,ISPT(II))
                      IF(DOIMONTH(IM,iyi).NE.0.0D0) THEN
                        SUMM=ADIWMONTH(IM,iyi)+AIRR
                        IF(SUMM.GE.DOIMONTH(IM,iyi)) THEN
                          AIRR=DOIMONTH(IM,iyi)-ADIWMONTH(IM,iyi)
                        ENDIF
                      ENDIF
                    ELSE
                      STOP'INPUT INCORRECT FOR IRRIGATION (TIMING)'
                    ENDIF
                    airr=min(airr,totalavail)
                    WRITE(70,1200) AIRR,airrtype(irrtyp(II))
                    WRITE(70,*) ' '
                  ENDIF
   11           CONTINUE
C=========================================================
C             *** 3) SOIL WATER DEPLETION USING ONLY THE ROOT ZONE
C=========================================================
C
              ELSEIF(ITOI(II).EQ.3) THEN
                  DO 62 L=1,NN
                    IF ((id_depth.eq.0).and.
     &                 (RDF(L).EQ.0.0D0)) GOTO 72
                    IF ((id_depth.gt.0).and.
     &                 (tlt(l).gt.dble(id_depth))) GOTO 72
                    NDRZ=L
   62             CONTINUE
   72             DO 21 L=1,NN
                  JH=NDXN2H(L)
c                  FC(L)=SOILHP(6,JH)*AEF
C                  FC(L)=SOILHP(7,JH)
C                  XWP(II,L)=SOILHP(9,JH)
                  FC(L)=WC(HFC,SOILHP(1,JH),JH,0)
                  XWP(L)=WC(HWP,SOILHP(1,JH),JH,0)
   21           CONTINUE
                TAWIRZ=0.0D0
                TPAWRZ=0.0D0
                DO 31 L=1,NDRZ
                    TPAWRZ=TPAWRZ+(FC(L)-XWP(L))*TL(L)
                    TAWIRZ=TAWIRZ+(THETA(L)-XWP(L))*TL(L)
   31           CONTINUE
               if(id_depth.gt.0) then
                TPAWRZ=TPAWRZ+(FC(NDRZ+1)-XWP(NDRZ+1))
     &                *max(0.0d0,(dble(id_depth)-TLT(NDRZ)))
                TAWIRZ=TAWIRZ+(THETA(NDRZ+1)-XWP(NDRZ+1))
     &                *max(0.0d0,(dble(id_depth)-TLT(NDRZ)))
               endif
                IF(TPAWRZ.EQ.0.0D0) GOTO 120
c                TPAWRZ=TPAWRZ+(FC(1)-XWP(II,1))*TL(1)
c                TAWIRZ=TAWIRZ+(THETA(1)-XWP(II,1))*TL(1)
                SMDEP=TAWIRZ/TPAWRZ
                DO 41 NCOLM=1,NMAD(II)
                  IF(JGROW.LE.JMAD(II,NCOLM+1)) GOTO 51
   41           CONTINUE
                NCOLM=NMAD(II)
   51           IF(nint(SMDEP*100.d0).GT.nint(XMAD(II,NCOLM)*100.d0))
     &             GOTO 120
                IF(JULDAY.LT.JDOPI+NDAIRR(II)) GOTO 120
                PRINT *, "RZMAN CALLING MANOUT"
                CALL MANOUT(19,0,JDAY,0,IYYY)
                WRITE(70,1000) IM,ID,IYYY,textmethod(ITOI(II))
                WRITE(70,1100) SMDEP*100.0D0,XMAD(II,NCOLM)*100.0D0
C=========================================================
C               *** 3) REFILL ROOTZONE TO UPPER LIMIT OF AVAILABLE WATER
C=========================================================
C
                IF(IDOI(II).EQ.3) THEN
                  DO 81 NCOLK=1,NKU(II)
                    IF(JGROW.LE.JKU(II,NCOLK+1)) GOTO 91
   81             CONTINUE
                  NCOLK=NKU(II)
   91             DO 101 L=1,NDRZ
                    ULOI=XKU(II,NCOLK)*(FC(L)-XWP(L))*TL(L)
c                    IF (ULOI.LT.1.0) GOTO 120                        !IRRIGATE 1.0 CM MINIMUM
                    AIRR=AIRR+ULOI-(THETA(L)-XWP(L))*TL(L)   !to allow negative demand as a storage
  101             CONTINUE
                  if (id_depth.gt.0) then
                  ULOI=XKU(II,NCOLK)*(FC(NDRZ+1)-XWP(NDRZ+1))
     &                   *max((dble(id_depth)-TLT(NDRZ)),0.0d0)            !TPAW*%refill
c                    IF (ULOI.LT.1.0) GOTO 120                        !IRRIGATE 1.0 CM MINIMUM
                  AIRR=AIRR+ULOI-(THETA(NDRZ+1)-XWP(NDRZ+1))      !TPAW*%refill-PAW, to allow negative demand as a storage
     &                   *max((dble(id_depth)-TLT(NDRZ)),0.0d0)            !TPAW*%refill
                  endif
                  AIRR=MAX(AIRR,0.0D0)
                  AIRR=Min(AIRR,amxirr)
                    airr=min(airr,totalavail)
                  if (airr.lt.amirr) then !.or.sumAT.eq.0.0d0) then
                    print*,'skip irrigation because amount is too small'
                    airr=0.0d0
                    goto 120
                  endif
                  IF(AIRR.GT.0.0D0) JDOPI=JULDAY
                  IF(DOIMONTH(IM,iyi).NE.0.0D0) THEN
                    SUMM=ADIWMONTH(IM,iyi)+AIRR
                    IF(SUMM.GE.DOIMONTH(IM,iyi)) THEN
                      AIRR=DOIMONTH(IM,iyi)-ADIWMONTH(IM,iyi)
                    ENDIF
                  ENDIF
C                 WRITE (70,*) 'ROOTS EXTEND TO LAYER :',NDRZ
C                 WRITE (70,710) IDOI(II)
                  WRITE(70,1300) AIRR,XKU(II,NCOLK)*100.0D0
                ELSE
                  STOP'INPUT INCORRECT FOR IRRIGATION (AMT)'
                ENDIF
C=========================================================
C             *** 4) ET based irrigation
C=========================================================
C
              ELSEIF(ITOI(II).EQ.4) THEN
                IF(sumPET1.EQ.0.0D0.or.sumAET1.eq.0.0d0) GOTO 120
                 SMDEP=sumAET1/sumPET1
c                IF(sumPT1.EQ.0.0D0.or.sumAT1.eq.0.0d0) GOTO 120
c                 SMDEP=sumAT1/sumPT1
                DO 141 NCOLM=1,NMAD(II)
                  IF(JGROW.LE.JMAD(II,NCOLM+1)) GOTO 151
  141           CONTINUE
                NCOLM=NMAD(II)
  151       continue
                IF(nint(SMDEP*100.d0).GT.nint(XMAD(II,NCOLM)*100.d0))
     &               GOTO 120
                IF(JULDAY.LT.JDOPI+NDAIRR(II)) GOTO 120
                PRINT *, "RZMAN CALLING MANOUT"
                CALL MANOUT(19,0,JDAY,0,IYYY)
                WRITE(70,1000) IM,ID,IYYY,textmethod(ITOI(II))
                WRITE(70,1400) SMDEP*100.d0,XMAD(II,NCOLM)*100.d0
C=========================================================
C               *** 4) ET based refill
C=========================================================
C
                IF(IDOI(II).EQ.4) THEN
                DO 221 L=1,NN
                  JH=NDXN2H(L)
C                  FC(L)=SOILHP(7,JH)
C                  XWP(II,L)=SOILHP(9,JH)
                  FC(L)=WC(HFC,SOILHP(1,JH),JH,0)
                  XWP(L)=WC(HWP,SOILHP(1,JH),JH,0)
  221           continue
                  DO 161 L=1,NN
                    IF (RDF(L).EQ.0.0D0) GOTO 171
                    NDRZ=L
  161             CONTINUE
  171             DO 181 NCOLK=1,NKU(II)
                    IF(JGROW.LE.JKU(II,NCOLK+1)) GOTO 191
  181             CONTINUE
                  NCOLK=NKU(II)
  191             ULOI=0.0d0
               DO 201 L=1,NDRZ                  !to only the root zone?
                    ULOI=ULOI+(FC(L)-theta(L))*TL(L)    !maximum irrigation amount
c                    AIRR=AIRR+MAX(0.0D0,ULOI-THETA(L)*TL(L))
  201             CONTINUE
                    ULOIET=max(0.0d0,XKU(II,NCOLK)*sumPET1-sumrain1)
c                    IF (ULOIET.LT.1.0) GOTO 120                       !IRRIGATE 1.0 CM MINIMUM
                    AIRR=AIRR+MAX(0.0D0,min(ULOIET,ULOI))
                  AIRR=Min(AIRR,amxirr)
                    airr=min(airr,totalavail)
                  if (airr.lt.amirr) then  !.or.sumAT.eq.0.0d0) then
                    print*,'skip irrigation because amount is too small'
                    airr=0.0d0
                    goto 120
                  endif
c                  sumRain1=0.0d0
C                  sumRain1=max(0.0d0,sumRain1-XKU(II,NCOLK)*sumPET1)  !excess rainfall is saved for next irrigation event
                  DO JJ=1,30
                  sumPET2(JJ)=0.0d0
                  sumAET2(JJ)=0.0d0
                  sumPT2(JJ)=0.0d0
                  sumAT2(JJ)=0.0d0
                  sumCropET2(JJ)=0.0d0
                  SUMRAIN2(JJ)=0.0D0
                  ENDDO
c                    AIRR=MAX(AIRR,0.0D0)
c                  IF(AIRR.GT.0.0D0) JDOPI=JULDAY
                  JDOPI=JULDAY
                  IF(DOIMONTH(IM,iyi).NE.0.0D0) THEN
                    SUMM=ADIWMONTH(IM,iyi)+AIRR
                    IF(SUMM.GE.DOIMONTH(IM,iyi)) THEN
                      AIRR=DOIMONTH(IM,iyi)-ADIWMONTH(IM,iyi)
                    ENDIF
                  ENDIF
C                 WRITE (70,*) 'ROOTS EXTEND TO LAYER :',NDRZ
C                 WRITE (70,710) IDOI(II)
                  WRITE(70,1500) AIRR,XKU(II,NCOLK)*100.d0
                ELSE
                  STOP'INPUT INCORRECT FOR IRRIGATION (AMT)'
                ENDIF
C=========================================================
C             *** 5) Canopy T based irrigation:  Need to be modified by Fang
C=========================================================
C
              ELSEIF(ITOI(II).EQ.5) THEN
                IF(sumPET1.EQ.0.0D0.or.sumAET1.eq.0.0d0) GOTO 120
                IF ((TLEAFU-T14)-(TLEAFL-T14).GT.0.0D0) THEN
                 CWSI=min(((TLCSHAW-T14)-(TLEAFL-T14))/
     &                    ((TLEAFU-T14)-(TLEAFL-T14)),1.0d0)
                 CWSI = MAX(0.0D0,CWSI)
                 ELSE
                 CWSI = -1.0D0
                 ENDIF
                DO 145 NCOLM=1,NMAD(II)
                  IF(JGROW.LE.JMAD(II,NCOLM+1)) GOTO 155
  145           CONTINUE
                NCOLM=NMAD(II)
  155       continue
                IF(nint(CWSI).LT.nint(XMAD(II,NCOLM))) GOTO 120
                IF(JULDAY.LT.JDOPI+NDAIRR(II)) GOTO 120
                PRINT *, "RZMAN CALLING MANOUT"
                CALL MANOUT(19,0,JDAY,0,IYYY)
                WRITE(70,1000) IM,ID,IYYY,textmethod(ITOI(II))
                WRITE(70,1400) CWSI,XMAD(II,NCOLM)
C=========================================================
C               *** 5) ET based refill (no changes needed)
C=========================================================
C
                IF(IDOI(II).EQ.4) THEN
                DO 225 L=1,NN
                  JH=NDXN2H(L)
C                  FC(L)=SOILHP(7,JH)
C                  XWP(II,L)=SOILHP(9,JH)
                  FC(L)=WC(HFC,SOILHP(1,JH),JH,0)
                  XWP(L)=WC(HWP,SOILHP(1,JH),JH,0)
  225           continue
                  DO 165 L=1,NN
                    IF (RDF(L).EQ.0.0D0) GOTO 175
                    NDRZ=L
  165             CONTINUE
  175             DO 185 NCOLK=1,NKU(II)
                    IF(JGROW.LE.JKU(II,NCOLK+1)) GOTO 195
  185             CONTINUE
                  NCOLK=NKU(II)
  195             ULOI=0.0d0
               DO 205 L=1,NDRZ                  !to only the root zone?
                    ULOI=ULOI+(FC(L)-theta(L))*TL(L)    !maximum irrigation amount
c                    AIRR=AIRR+MAX(0.0D0,ULOI-THETA(L)*TL(L))
  205             CONTINUE
                    ULOIET=max(0.0d0,XKU(II,NCOLK)*sumPET1-sumrain1)
c                    IF (ULOIET.LT.1.0) GOTO 120                       !IRRIGATE 1.0 CM MINIMUM
                    AIRR=AIRR+MAX(0.0D0,min(ULOIET,ULOI))
                  AIRR=Min(AIRR,amxirr)
                    airr=min(airr,totalavail)
                  if (airr.lt.amirr) then  !.or.sumAT.eq.0.0d0) then
                    print*,'skip irrigation because amount is too small'
                    airr=0.0d0
                    goto 120
                  endif
c                  sumRain1=0.0d0
C                  sumRain1=max(0.0d0,sumRain1-XKU(II,NCOLK)*sumPET1)  !excess rainfall is saved for next irrigation event
                  DO JJ=1,30
                  sumPET2(JJ)=0.0d0
                  sumAET2(JJ)=0.0d0
                  sumPT2(JJ)=0.0d0
                  sumAT2(JJ)=0.0d0
                  sumCropET2(JJ)=0.0d0
                  SUMRAIN2(JJ)=0.0D0
                  ENDDO
c                    AIRR=MAX(AIRR,0.0D0)
c                  IF(AIRR.GT.0.0D0) JDOPI=JULDAY
                  JDOPI=JULDAY
                  IF(DOIMONTH(IM,iyi).NE.0.0D0) THEN
                    SUMM=ADIWMONTH(IM,iyi)+AIRR
                    IF(SUMM.GE.DOIMONTH(IM,iyi)) THEN
                      AIRR=DOIMONTH(IM,iyi)-ADIWMONTH(IM,iyi)
                    ENDIF
                  ENDIF
C                 WRITE (70,*) 'ROOTS EXTEND TO LAYER :',NDRZ
C                 WRITE (70,710) IDOI(II)
                  WRITE(70,1500) AIRR,XKU(II,NCOLK)*100.d0
                ELSE IF(IDOI(II).EQ.3) THEN     !Rootzone depletion method
                  DO 82 NCOLK=1,NKU(II)
                    IF(JGROW.LE.JKU(II,NCOLK+1)) GOTO 92
   82             CONTINUE
                  NCOLK=NKU(II)
   92             DO 102 L=1,NDRZ
                    ULOI=XKU(II,NCOLK)*(FC(L)-XWP(L))*TL(L)
c                    IF (ULOI.LT.1.0) GOTO 120                        !IRRIGATE 1.0 CM MINIMUM
                    AIRR=AIRR+ULOI-(THETA(L)-XWP(L))*TL(L)   !to allow negative demand as a storage
  102             CONTINUE
                  if (id_depth.gt.0) then
                  ULOI=XKU(II,NCOLK)*(FC(NDRZ+1)-XWP(NDRZ+1))
     &                   *max((dble(id_depth)-TLT(NDRZ)),0.0d0)            !TPAW*%refill
c                    IF (ULOI.LT.1.0) GOTO 120                        !IRRIGATE 1.0 CM MINIMUM
                  AIRR=AIRR+ULOI-(THETA(NDRZ+1)-XWP(NDRZ+1))      !TPAW*%refill-PAW, to allow negative demand as a storage
     &                   *max((dble(id_depth)-TLT(NDRZ)),0.0d0)            !TPAW*%refill
                  endif
                  AIRR=MAX(AIRR,0.0D0)
                  AIRR=Min(AIRR,amxirr)
                    airr=min(airr,totalavail)
                  if (airr.lt.amirr) then !.or.sumAT.eq.0.0d0) then
                    print*,'skip irrigation because amount is too small'
                    airr=0.0d0
                    goto 120
                  endif
                  IF(AIRR.GT.0.0D0) JDOPI=JULDAY
                  IF(DOIMONTH(IM,iyi).NE.0.0D0) THEN
                    SUMM=ADIWMONTH(IM,iyi)+AIRR
                    IF(SUMM.GE.DOIMONTH(IM,iyi)) THEN
                      AIRR=DOIMONTH(IM,iyi)-ADIWMONTH(IM,iyi)
                    ENDIF
                  ENDIF
C                 WRITE (70,*) 'ROOTS EXTEND TO LAYER :',NDRZ
C                 WRITE (70,710) IDOI(II)
                  WRITE(70,1300) AIRR,XKU(II,NCOLK)*100.0D0
                ELSE

                  STOP'INPUT INCORRECT FOR IRRIGATION (AMT)'
                ENDIF
              ELSE
                STOP'INPUT INCORRECT FOR IRRIGATION (DATES)'
              ENDIF
              ADIWMONTH(IM,iyi)=ADIWMONTH(IM,iyi)+AIRR
              curradiw=curradiw+airr
              IRLOC=II
              GOTO 120
            ENDIF
          ENDIF
        ENDIF
        ENDIF
  110 CONTINUE
  120 continue
c
      RETURN
 1000 FORMAT('IRRIGATION OCCURRED ON ',I2,'/',I2,'/',I4,2X,
     +    'BASED ON IRRIGATION OF ',A30)
 1100 FORMAT('IRRIGATION WAS NECESSARY BECAUSE SOIL MOISTURE HAS ',
     +    'DROPPED TO ',F5.1,'% OF TPAW (<THE TRIGGER OF ',F5.1,'%)')
 1200 FORMAT('DEPTH OF IRRIGATION OF ',F5.2,' CM',' (',A10,' Irrigation)
     +')
 1300 FORMAT('DEPTH OF IRRIGATION OF ',F5.2,' CM',
     +    ' BY REFILLING TO ', F5.1, '% OF TPAW')
 1400 FORMAT('IRRIGATION WAS NECESSARY BECAUSE AET HAD ',
     +    'DROPPED TO ',F5.1,'% PET (<THE TRIGGER OF ',F5.1,'%)')
 1500 FORMAT('DEPTH OF IRRIGATION IS ',F5.2,' CM',
     +    ' BY REFILLING ', F5.1, '% OF PET')
      END
C
      FUNCTION JDATE(ID,MM,IYYY)
C
C======================================================================
C
C       PURPOSE:  THIS IS A GENERIC JULIAN DATE CALCULATOR.  THE DATE
C             RETURNED IS THE POSITION OF THE JULIAN DATE IN THE
C             SPECIFIED YEAR (EX. 12,05,1988 = 133RD DAY IN 1988)
C
C       REF: "NUMERICAL RECIPES: THE ART OF SCIENTIFIC COMPUTING",
C          CAMBRIDGE UNIVERSITY PRESS, 1986. PG 10.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       ID         I  --DAY OF MONTH [EX 12).
C       IYYY   I  THE YEAR WHICH THE JULIAN DATE IS CONTAINED (EX 1987).
C       JA         L  USED IN CALCULATING CORRECTION FOR LEAP YEAR IN
C                 INPUTTED DATE.
C       JAA        L  USED IN CALCULATING CORRECTION FOR LEAP YEAR ON
C                 FIRST DAY OF THE INPUTTED YEAR.
C       JDATE  O  DAY OF YEAR (1-366)
C       JM         L  MODIFIED MONTH.
C       JULIAN     L  DATE WITHOUT CORRECTION FOR LEAP YEAR.
C       JY         L  MODIFIED YEAR.
C       MM         I  THE MONTH OF THE YEAR (EX 12 = DEC, 5 = MAY).
C       NJ         L  JULIAN DATE FOR FIRST DAY OF THE INPUTTED YEAR.
C
C       CALLED FROM:
C
C       PROGRAMMER:  NUMERICAL RECIPES PROGRAMMER.
C
C       VERSION: 2.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      IF(IYYY.LE.0) THEN
          STOP'DATE SPECIFIED IN A MANAGEMENT PRACTICE IS INCORRECT'
      ENDIF
      IF(MM.GT.2) THEN
        JY=IYYY
        JM=MM+1
      ELSE
        JY=IYYY-1
        JM=MM+13
      ENDIF
C
C     --CALCULATE THE JULIAN DATE FOR THE FIRST DAY OF THE SPECIFIED
C     YEAR.
C
      NJ=INT(365.25*(IYYY-1))+1721423
      JAA=INT(0.01*JY)
      NJ=NJ+2-JAA+INT(0.25*JAA)
C
C     --CALCULATE THE JULIAN DATE FOR THE SPECIFIED DATE.
C
      JULIAN=INT(365.25*JY)+INT(30.6001*JM)+ID+1720995
      JA=INT(0.01*JY)
      JDATE=JULIAN+2-JA+INT(0.25*JA)-NJ
C
      RETURN
      END
C
      FUNCTION JULDATE(ID,MM,IYYY)
C
C======================================================================
C
C       PURPOSE:  THIS IS A GENERIC JULIAN DATE CALCULATOR.  THE DATE
C             RETURNED IS THE ACTUAL JULIAN DATE
C             (EX. MAY 23, 1968 = 2,440,000)
C
C       REF: "NUMERICAL RECIPES: THE ART OF SCIENTIFIC COMPUTING",
C          CAMBRIDGE UNIVERSITY PRESS, 1986. PG 10.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       ID         I  DAY OF MONTH (BETWEEN 1-31)
C       IYYY   I  YEAR WHICH THE JULIAN DATE IS CONTAINED (EX 1987)
C       JA         L  USED IN CALCULATING CORRECTION FOR LEAP YEAR IN
C                 INPUTTED DATE
C       JULDATE    O  JULIAN DATE BASED ON GREGORIAN CALENDAR
C       JM         L  MODIFIED MONTH
C       JULIAN     L  DATE WITHOUT CORRECTION FOR LEAP YEAR
C       JY         L  MODIFIED YEAR
C       MM         I  THE MONTH OF THE YEAR (EX 12 = DEC, 5 = MAY)
C
C       CALLED FROM:
C
C       PROGRAMMER:  NUMERICAL RECIPES PROGRAMMER.
C
C       VERSION: 3.2
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      IF(IYYY.LE.0) THEN
        STOP'DATE SPECIFIED IN A MANAGEMENT PRACTICE IS INCORRECT'
      ENDIF
      IF(MM.GT.2) THEN
        JY=IYYY
        JM=MM+1
      ELSE
        JY=IYYY-1
        JM=MM+13
      ENDIF
C
C     --CALCULATE THE JULIAN DATE FOR THE SPECIFIED DATE.
C
      JULIAN=INT(365.25*JY)+INT(30.6001*JM)+ID+1720995
      JA=INT(0.01*JY)
      JULDATE=JULIAN+2-JA+INT(0.25*JA)
C another way to calculate JUlIAN
      JUL=INT(1461*(IYYY+4800+INT((MM-14)/12))/4)+
     +    INT((367*(MM-2-12*INT((MM-14)/12)))/12)-
     +    INT((3*INT((IYYY+4900+INT((MM-14)/12))/100))/4)
     +       +ID-32075
      RETURN
      END
C
C
      SUBROUTINE MEQUE(IQUE)
C
C======================================================================
C
C       PURPOSE:  THIS ROUTINE CLEARS OUT USED MANAGEMENT EVENTS AND
C             ADVANCES THE QUE TO THE NEXT SET
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C
C
C       CALLED FROM:   RZWQM
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:   2.1
C
C-----------------------------------------------------------------------
C
      INTEGER MXAPP
      PARAMETER(MXAPP=200)
C
      INTEGER I,IC,J,K,IQUE(4,MXAPP)
C
C     ..FIND NEXT QUE ENTRY
      IF(IQUE(1,1).NE.-1) THEN
        DO 10 I=1,MXAPP
          IF(IQUE(1,I).EQ.-1) GOTO 20
   10   CONTINUE
   20   I=I+1
        IC=0
C
C       ..GET RID OF ANTIQUATED ENTRIES
        DO 30 J=I,MXAPP
          IC=IC+1
          IQUE(1,IC)=IQUE(1,J)
          IQUE(2,IC)=IQUE(2,J)
          IQUE(3,IC)=IQUE(3,J)
          IQUE(4,IC)=IQUE(4,J)
   30   CONTINUE
C
C       ..PAD REST OF QUE WITH -1'S
        DO 40 K=IC+1,MXAPP
          IQUE(1,K)=-1
          IQUE(2,K)=-1
          IQUE(3,K)=-1
          IQUE(4,K)=-1
   40   CONTINUE
      ENDIF
      RETURN
      END
C
      SUBROUTINE MAQUE(JDAY,THETA,NDXH2N,NDXN2H,SOILHP,TWL,JGS,IQUE,
     +    AIRR,NN,FRACOM,TLT,BD,CC,CHLRFL,INXPL,FT,IYYY,IRLOC,JGROW,RDF,
     +    IPL,pori,tapmun,iyb,irrtyp,airrtype,niw,ipsw_depth,TLCSHAW,
     +    TLEAFU,TLEAFL,T14)
C
C======================================================================
C
C       PURPOSE:  THIS ROUTINE CREATES THE MANAGEMENT QUE AND ADDS NEW
C             EVENTS ONTO THE QUE.  ALL MANAGEMENT OPERATIONS ARE SET
C             FOR EXECUTION THROUGH THIS ROUTINE.  IT LOOKS AT THE
C             TOTAL WATER CONTENT FOR THE 1ST HORIZON VERSUS A USER
C             SPECIFIED THRESHOLD VALUE.  ALSO VERIFIES THAT CERTAIN
C             MANAGEMENT PRACTICES HAVE ALL COMPONENTS AVAILABLE TO
C             OPERATE PROPERLY (IE, FERTIGATING HAS TO HAVE AN IRR.
C             EVENT CONCURRENT).
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       JPLNT  I  PLANTING DATE (JULIAN)
C       SUFNDX     I  NITROGEN SUFFICIENCY INDEX, THRESHOLD BELOW WHICH
C                 FERTILIZER APPLICAITONS ARE TRIGGERED
C       IDSPLT     I  MIN NUMBER OF DAYS IN BETWEEN SPLIT FERTLIZER APPS
C       SPLTAP     I  MAXIMUM AMT OF N TO APPLY FOR EACH SPLIT APP
C       SPLTST     I  FRACTION OF RECOMMENDED FERTILIZER APPLICATION THAT
C                 IS APPLIED AS STARTER.
C       TAPMUN        TOTAL APPLIED MANURE FOR YEAR (KG N/HA)
C       CHLRFL     I  LOGICAL FLAG TRIGGERED BY CHLOROPHYLL METER
C       JSPLT  I  DATE OF NEXT SPLIT FERT APP.
C
C
C       COMMENTS:  IQUE CONTAINS 4 ELEMENTS WHERE:
C
C     1)  THE MASTER MANAGEMENT SEQUENCE (MMS)
C     2)  POINTER TO ACTUAL VARIABLE IN DATA STRUCTURE
C     3)  THE DAY THE EVENT WAS PUT ON THE QUE (JULIAN)
C     4)  ACTIVE TILLAGE IMPLIMENT WHEN PUT ON THE QUE.
C
C     MMS DICTIONARY:
C     ---------------
C     1)  FERTILIZING BROADCAST (LEAVE ON SURFACE)
C     2)  FERTILIZING BROADCAST (INCORPORATE WITH TILLAGE)
C     3)  FERTILIZER NH3 INJECTOR APPLICATION
C     4)  FERTILIZER APPLICATION THROUGH IRRIGATION
C     5)  PESTICIDE APPLICATION-BROADCAST (LEAVE ON SURFACE)
C     6)  PESTICIDE APPLICATION-BROADCAST (INCORPORATE WITH TILLAGE)
C     7)  PESTICIDE APPLICATION ON FOLIAGE
C     8)  PESTICIDE APPLICATION THROUGH IRRIGATION
C     9)  MICROENCAPSULATED PESTICIDE APPLICATION (ON SURFACE)
C     10) MICROENCAPSULATED PESTICIDE APPLICATION (INCORPORATED)
C     11) PESTICIDE APPLICATION ON SOIL SURFACE ONLY
C     12) PESTICIDE APPLICATION INJECTED
C     13) MANURE APPLICATION BROADCAST (LEAVE ON SURFACE)
C     14) MANURE APPLICATION BROADCAST (INCORPORATE WITH TILLAGE)
C     15) MANURE APPLICATION INJECTOR APPLICATION
C     16) MANURE APPLICATION APPLICATION THROUGH IRRIGATION
C     17) SPECIFIC DATE TILLAGE EVENT
C     18) PLANTING OPERATION
C     19) IRRIGATION EVENT
C     20) INCORPORATION TILLAGE EVENT
C     21) FERTILIZER APP CANCELLED W/O IRRIGATION SPECIFIED
C     22) PESTICIDE APP CANCELLED W/O IRRIGATION SPECIFIED
C     23) MANURE APP CANCELLED W/O IRRIGATION SPECIFIED
C     24) FERTILIZER APP CANCELLED W/O INJECTOR SPECIFIED
C     25) PESTICIDE APP CANCELLED W/O INJECTOR SPECIFIED
C     26) MANURE APP CANCELLED W/O INJECTOR SPECIFIED
C     27) FERTILIZER NH3 INJECTOR WITH (N-SERVE)
C
C
C     AN EVENT GROUP FOR A DAY DIVIDED FROM ANOTHER DAY'S BY -1'S
C
C       CALLED FROM:  RZWQM
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:   3.0
C
C-----------------------------------------------------------------------
C     
      USE VARIABLE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,MAXHOR=12,MXAPP=200,MXCHEM=15,MXSPEC=10,
     +          MXTG=500,MXPEST=3)
C
      PARAMETER(MXTAPP=10)
C
      COMMON /PLNTG/ PLDEN(MXAPP),PLTDAY(MXAPP,2),ROWSP(MXAPP),
     +    PLDEPTH(MXAPP),sdwtplr(mxapp),sdager(mxapp),sprlapr(mxapp),
     +    PLNTSW(MXAPP),PLNTWD(MXAPP),LAYNDX(MXAPP),NPGS,NPR(MXAPP),
     +    NGPL(MXAPP),iptype(mxapp),iemrg(mxapp)
C
C     Zhiming added Pmanuremix
      COMMON /TILLIN/ OBD(2),OFDP(2),TDEPTH(30),TILINT(30,2),
     +    TNDX(MXAPP),UDEPTH(MXAPP), PManureMix(MXAPP), IMP(MXAPP),  
     +    ITL(MXAPP), ITOFF(MXAPP,2),IWTILL(MXAPP),NUMTL, ITPL(MXAPP)
C
      COMMON /MFERT/ FNH4IN(MXAPP),FNO3IN(MXAPP),FUREIN(MXAPP),
     +    SPLTST(MXAPP),SPLTAP(MXAPP),IFOFF(MXAPP,2),IFMETH(MXAPP),
     +    IWFERT(MXAPP),NAPPF,IBMPAP(MXAPP),IBMPMD(MXAPP),
     +    IDSPLT(MXAPP),IFPL(MXAPP)
C
      COMMON /MPEST/ APP(MXAPP),PDR(MXAPP),IAPP(MXAPP),IPOFF(MXAPP,2),
     +    IWPEST(MXAPP),IPMETH(MXAPP),NAPP,IPPL(MXAPP)
C
      COMMON /MMNUR/ AMNH4(MXAPP),AMOW(MXAPP),AMBED(MXAPP),
     +    CNBED(MXAPP),AMCN(15),CNMANR(MXAPP),FCMANR(MXAPP),
     +    IWMANR(MXAPP),IMOFF(MXAPP,2),IMMETH(MXAPP),IMTYPE(MXAPP),
     +    NAPPM,IMPL(MXAPP),IMANDECOMP(MXAPP)
C
      COMMON /BMPRC/ YGOAL,OMADJ,USAMDP,UMISCC,ISTATE,ISOYB,BMPIRR,
     +    BMPTIL,FMC,YGSF,SOYYLD
C
      COMMON /THGIN/ THGDEP(MXTG),THGCUR,ITHG(MXTG),IWTHG(MXTG),
     +    ITHGOFF(MXTG,2),ITHGDUR(MXTG),NUMTHG,IDP
C
      DIMENSION NDXH2N(MAXHOR),SOILHP(13,MAXHOR),THETA(MXNOD),JGS(2),
     +    IQUE(4,MXAPP),ITQUE(2,MXTAPP),FRACOM(MXNOD),TLT(MXNOD),
     +    BD(MXNOD),CC(MXNOD,MXCHEM),INXPL(MXSPEC),ADIW(MXAPP),
     +    NDXN2H(MXNOD),RDF(MXNOD),pori(mxnod),NYRP(MXPEST),
     +    ADIWMONTH(12,200),irrtyp(mxapp),totadiw(20),FRACON(MXNOD)
C
      CHARACTER*15 BMPNAME(5)
      character*10 airrtype(5)
      LOGICAL T1,T2,T3,CHLRFL,SPLT1,FIRST7
      DATA SPLT1 /.TRUE./,JSPLT /0/,FIRST7 /.TRUE./,totadiw/20*0.0d0/
      DATA BMPNAME/'MISSOURI BMP','NEBRASKA BMP','IOWA YIELD GOAL BMP',
     +     'IOWA LSNT BMP','COLORADO BMP'/
      DATA ADIW /MXAPP*0.0D0/,IRPL /1/  !,ADIWMONTH/12*100*0.0D0/

      CALL MAQUE_Memory('GET',ADIW,JSPLT,SPLT1,FIRST7,NYR,IRPL,NYRC,
     +  NYRP,ADIWMONTH,totadiw,curradiw)
C
C     ..SAVE CURRENT YEAR
      IF(FIRST7) THEN
        NYR=IYYY
        NYRC=IYYY
        DO I=1,MXPEST
        NYRP(I)=IYYY
        END DO
        FIRST7=.FALSE.
      ENDIF
C
        if (mod(iyyy-1,4).eq.0) then
            iyp=366
        else
            iyp=365
        endif
C     ..FIND END OF QUE
      DO 10 IE=MXAPP,1,-1
        IF(IQUE(1,IE).NE.-1) GOTO 20
   10 CONTINUE
   20 TDAY=DBLE(JDAY)
      YR=DBLE(IYYY)
      T3=.TRUE.
      JPLNT=999   !SET HIGH, HAVE TO BE YEAR OF PLANTING TO BE REAL
C
C     ..RESET SPLIT DAY IF NEW YEAR
      IF((IYYY.NE.NYR).and.(ipl.eq.0)) THEN
        JSPLT=0
        NYR=IYYY
        NYRC=IYYY
c        totadiw(iyyy-iyb+1)=0.0d0
        DO I=1,MXPEST
        NYRP(I)=IYYY
        END DO
        DO 30 I=1,MXAPP
c          totadiw(iyyy-iyb+1)=totadiw(iyyy-iyb+1)+adiw(i)
          ADIW(I)=0.0D0
   30   CONTINUE
         if (niw.gt.1) Tadiw=AVEMOV(totadiw,niw-1,curradiw,Iyyy-iyb)
         curradiw=0.0d0
        DO I=1,12
          ADIWMONTH(I,iyyy-iyb+1)=0.0D0
        ENDDO
      ENDIF
C
C     ..INITIALIZE VARIABLES FOR CHECKING SOIL DAMPNESS
      SUMTH=0.D0
      DO 40 I=1,NDXH2N(1)
        SUMTH=SUMTH+THETA(I)
   40 CONTINUE
C
      TOTSW=THETA(1)*TLT(1)
      TOTFC=SOILHP(7,1)*TLT(1)
      DO I=2,NN
          IF (TLT(I).LE.dble(ipsw_depth)) THEN   !LIMIT TO 100 CM
          IH=NDXN2H(I)
          TOTSW=TOTSW+THETA(I)*(TLT(I)-TLT(I-1))
          TOTFC=TOTFC+SOILHP(7,IH)*(TLT(I)-TLT(I-1))
          ENDIF
      ENDDO
C     ..FIND MEAN THETA VALUE, THBAR
      THBAR=SUMTH/NDXH2N(1)
C
C     ..COMPARE TO THRESHOLD VALUE
      IF((THBAR/SOILHP(6,1)).GT.TWL) THEN      !SHOULD IT CHANGED TO FIELD CAPACITY, LIWANG MA, 12-27-2014
        WRITE(70,*) 'SOIL WATER CONTENT EXCEEDS THRESHOLD ',
     +      'WATER LEVEL-- DAY ',JDAY
        WRITE(70,*) 'DAMPNESS:',THBAR/SOILHP(6,1),'<-->  TRESHOLD:',TWL
        WRITE(70,*) 'MANAGEMENT PRACTICES ARE POSTPONED.'
        IE=IE+1
        IF(IQUE(1,1).NE.-1) THEN
          DO 50 I=MXAPP,2,-1
            IQUE(1,I)=IQUE(1,I-1)
            IQUE(2,I)=IQUE(2,I-1)
            IQUE(3,I)=IQUE(3,I-1)
            IQUE(4,I)=IQUE(4,I-1)
   50     CONTINUE
          IQUE(1,1)=-1
          IQUE(2,1)=-1
          IQUE(3,1)=-1
          IQUE(4,1)=-1
        ENDIF
      ELSEIF(IQUE(1,1).EQ.-1.AND.IQUE(1,2).NE.-1) THEN
C
C       ..ACTIVATE STORED VALUES ON QUE
        DO 60 I=1,MXAPP-1
          IQUE(1,I)=IQUE(1,I+1)
          IQUE(2,I)=IQUE(2,I+1)
          IQUE(3,I)=IQUE(3,I+1)
          IQUE(4,I)=IQUE(4,I+1)
   60   CONTINUE
        IQUE(1,MXAPP)=-1
        IQUE(2,MXAPP)=-1
        IQUE(3,MXAPP)=-1
        IQUE(4,MXAPP)=-1
      ENDIF
C
C     ..START BUILDING TEMPORARY QUE WITH NEW EVENTS
      IC=0
C
C     ..ZERO OUT TEMP QUE
      DO 70 I=1,MXTAPP
        ITQUE(1,I)=0
        ITQUE(2,I)=0
   70 CONTINUE
C     JPLNT = -10
C
C     ..CHECK FOR PLANTING DATE
      IF(IPL.EQ.0) THEN
        DO 80 IP=1,NPGS
          IF(YR.EQ.PLTDAY(IP,2)) THEN
            JPLNT=INT(PLTDAY(IP,1))
            IRPL=NPR(IP)
            ICPLNT=INXPL(IRPL)
            IF((TDAY.GE.PLTDAY(IP,1)).AND.(TDAY.LE.(PLTDAY(IP,1)+
     &        PLNTWD(IP))).AND.(TOTSW.GE.PLNTSW(IP))) THEN
              IC=IC+1
              ITQUE(1,IC)=18
              ITQUE(2,IC)=IP
              isumpet1=1
              GOTO 100
            ELSEIF(TDAY.LT.PLTDAY(IP,1)) THEN
              GOTO 100
            ENDIF
          ENDIF
   80   CONTINUE
      ELSE
        DO 90 IP=1,NPGS
          IF(IPL.EQ.(NPR(IP)).AND.YR.EQ.PLTDAY(IP,2)) THEN
            IRPL=IPL
            JPLNT=INT(PLTDAY(IP,1))
            ICPLNT=INXPL(IPL)
          ENDIF
   90   CONTINUE
      ENDIF
C
C
  100 CALL MAIRR(JDAY,NDXN2H,RDF,AIRR,JGROW,IYYY,IRPL,ADIW,IRLOC,pori,
     +  ADIWMONTH,IYB,isumpet1,irrtyp,airrtype,tadiw,niw,curradiw,ipl,
     +  TLCSHAW,TLEAFU,TLEAFL,T14)
C
C     ..CHECK FOR TILLAGE EVENTS
      DO 110 IT=1,NUMTL
C
C       ..CHECK IF WE ARE MANAGEING THE CURRENT CROP ONLY
        IF(ITPL(IT).EQ.IRPL) THEN
            PLT = IRPL
          IF(((IWTILL(IT).EQ.1).AND.(JDAY.EQ.JPLNT-ITOFF(IT,1))).OR.((
     +       IWTILL(IT).EQ.2).AND.(JDAY.EQ.JPLNT+ITOFF(IT,1))).OR.((
     +    IWTILL(IT).EQ.2).AND.(JDAY+iyp.EQ.JPLNT+ITOFF(IT,1))).OR.((
     +       IWTILL(IT).EQ.3).AND.(JDAY.EQ.JGS(1)+1+ITOFF(IT,1))).OR.((
     +    IWTILL(IT).EQ.3).AND.(JDAY+iyp.EQ.JGS(1)+1+ITOFF(IT,1))).OR.((
     +       IWTILL(IT).EQ.4).AND.(JDAY.EQ.JGS(2)+1+ITOFF(IT,1))).OR.((
     +    IWTILL(IT).EQ.4).AND.(JDAY+iyp.EQ.JGS(2)+1+ITOFF(IT,1))).OR.((
     +       IWTILL(IT).EQ.5).AND.(JDAY.EQ.ITOFF(IT,1).AND.IYYY.EQ.
     +       ITOFF(IT,2)))) THEN
            IC=IC+1
            ITQUE(1,IC)=17
            ITQUE(2,IC)=IT
            ITT=IT
            T3=.FALSE.
            GOTO 120
          ENDIF
        ENDIF
  110 CONTINUE
      IF(T3) ITT=MXAPP
C
C     ..CHECK FOR FERTILIZATION EVENTS
  120 DO 140 IR=1,NAPPF
C
C       ..CHECK IF WE ARE MANAGEING THE CURRENT CROP ONLY
        IF(IFPL(IR).EQ.IRPL) THEN
          IF(((IWFERT(IR).EQ.1).AND.(JDAY.EQ.JPLNT-IFOFF(IR,1))).OR.
     +        ((IWFERT(IR).EQ.2).AND.(JDAY.EQ.JPLNT+IFOFF(IR,1))).OR.
     +    ((IWFERT(IR).EQ.2).AND.(JDAY+iyp.EQ.JPLNT+IFOFF(IR,1))).OR.
     +        ((IWFERT(IR).EQ.3).AND.(JDAY.GE.JPLNT).AND.
     +     (JDAY.EQ.JGS(1)+1+IFOFF(IR,1))).OR.
     +    ((IWFERT(IR).EQ.3).AND.(JDAY+IYP.GE.JPLNT).AND.
     +     (JDAY+iyp.EQ.JGS(1)+1+IFOFF(IR,1))).OR.
     +        ((IWFERT(IR).EQ.4).AND.(JDAY.EQ.JGS(2)+1+IFOFF(IR,1))).OR.
     +    ((IWFERT(IR).EQ.4).AND.(JDAY+iyp.EQ.JGS(2)+1+IFOFF(IR,1))).OR.
     +        ((IWFERT(IR).EQ.5).AND.(JDAY.EQ.IFOFF(IR,1).AND.IYYY.EQ.
     +        IFOFF(IR,2))).OR.((IWFERT(IR).EQ.6).AND.((JDAY.EQ.JPLNT-
     +    IFOFF(IR,1)).OR.(JDAY.EQ.JSPLT).or.(jday+iyp.eq.jsplt))).OR.
     +         ((IWFERT(IR).EQ.7)
     +        .AND.((JDAY.EQ.JPLNT-IFOFF(IR,1)).OR.(CHLRFL.AND.JDAY.GE.
     +        JSPLT)))) THEN
C
            IF(IFMETH(IR).EQ.6.AND.JSPLT.EQ.0.AND.(ICPLNT.LT.100.OR.
     +          ICPLNT.GT.200.OR.IWFERT(IR).EQ.4)) THEN
C
C             ..CHECK IF USING  B M P  PRACTICES
C
C             ..DETERMINE HOW MUCH FERTLIZER TO ADD BASED ON BMP OPTIONS
              PRINT *, "RZMAN CALLING BMPNIT"
              CALL BMPNIT(FRACOM,PLDEN(IRPL),TLT,NN,BD,CC(1,9),THETA,
     +            FUREIN,FNH4IN,FNO3IN,IR,BMPREC,IBMPAP,TAPMUN)
              SPLT1=.TRUE.
              CALL CDATE(JDAY,IDD,IMM,IYYY)
      WRITE(70,1001) IDD,IMM,IYYY,JDAY,BMPREC,BMPNAME(ISTATE),USAMDP
 1001 FORMAT(70('*')//'=> ESTIMATED BMP FERTLIZER ON ',I2,'/',I2,'/',I4,
     +          '(DAY',I4,')',
     +         ' IS  ',F7.2,1X,'[KG-N/HA] BASED ON  ',A15//,56X,
     +         'SOIL TEST DEPTH  ',F5.1,' CM'/)
            ENDIF
C
C           ..SET MINIMUM DAY FOR SPLIT APPLICATIONS
            CALL SPLIT(JDAY,T3,ITQUE,FNH4IN,FNO3IN,IFMETH,FUREIN,IWFERT,
     +          IFOFF,SPLTAP,IDSPLT,SPLTST,IR,IC,ICPLNT,JPLNT,JSPLT,
     +          SPLT1,IBMPAP,IBMPMD,ITT,BMPIRR,AIRR,IYYY,FT)
C
            IF(IFMETH(IR).LE.4.AND.JSPLT.LT.731) THEN   !change from 367 to 731 to cover winter wheat Pat suggestion
              IF(IFMETH(IR).LE.2) THEN
                IC=IC+1
                ITQUE(1,IC)=IFMETH(IR)
                ITQUE(2,IC)=IR
                IF(ITQUE(1,IC).EQ.2) THEN
                  IC=IC+1
                  ITQUE(1,IC)=20
                  ITQUE(2,IC)=4
                ENDIF
              ELSEIF(IFMETH(IR).LE.4) THEN
C
                IF(IFMETH(IR).EQ.3) THEN
C
C                 ..REGULAR ANHYDROUS-APP
                  IC=IC+1
                  ITQUE(1,IC)=3
                  ITQUE(2,IC)=IR
                ELSE
C
C                 ..SPECIAL CASE FOR ANHYDROUS-APP WITH (N-SERVE)
C                 NITRIFICATION INHIBITOR
                  IC=IC+1
                  ITQUE(1,IC)=27
                  ITQUE(2,IC)=IR
                ENDIF
C
C               ..SET TILLAGE IMP POINTER AT INJECTOR
                ITT=MXAPP
C
C               ..BURN EXISTING TILLAGE APP
                IF(.NOT.T3) THEN
                  DO 130 J=1,IC
                    IF(ITQUE(1,J).EQ.17) ITQUE(2,J)=ITT
  130             CONTINUE
                ELSE
C
C                 ..BUILD NEW TILLAGE OPERATION WITH INJECTOR
                  IC=IC+1
                  ITQUE(1,IC)=17
                  ITQUE(2,IC)=MXAPP
                  T3=.FALSE.
                ENDIF
C
              ELSE
C
C               ..NO INJECTOR IMP SPECIFIED
                PRINT *, "RZMAN CALLING MANOUT"
                CALL MANOUT(24,IR,JDAY,0,IYYY)
              ENDIF
            ELSEIF(AIRR.NE.0.0D0.AND.IFMETH(IR).EQ.5) THEN
C
C             .. HAVE IRRIGATION EVENT SO SCHEDULE EVENT
              IC=IC+1
              ITQUE(1,IC)=4
              ITQUE(2,IC)=IR
            ELSEIF(IFMETH(IR).EQ.5) THEN
C
C             ..NO IRRIGATION EVENT COINSIDING SO
C             .. SCHEDULE DEFAULT IRRIGATION APPLICATION
              IF(AIRR.EQ.0.0D0) THEN
                PRINT *, "RZMAN CALLING MANOUT"
                CALL MANOUT(19,0,JDAY,0,IYYY)
                AIRR=BMPIRR
                WRITE(70,1000) AIRR
              ENDIF
              IC=IC+1
              ITQUE(1,IC)=4
              ITQUE(2,IC)=IR
C           CALL MANOUT(21,IR,JDAY,0,IYYY)
            ENDIF
            GOTO 150
          ENDIF
        ENDIF
  140 CONTINUE
C
C     ..CHECK FOR PESTICIDE EVENTS
  150 DO 170 IS=1,NAPP
C
C       ..CHECK IF WE ARE MANAGEING THE CURRENT CROP ONLY
        IF(IPPL(IS).EQ.IRPL) THEN
          IF(((IWPEST(IS).EQ.1).AND.(JDAY.EQ.JPLNT-IPOFF(IS,1)).OR.(
     +        IWPEST(IS).EQ.2).AND.(JDAY.EQ.JPLNT+IPOFF(IS,1)).OR.(
     +     IWPEST(IS).EQ.2).AND.(JDAY+iyp.EQ.JPLNT+IPOFF(IS,1)).OR.(
     +        IWPEST(IS).EQ.3).AND.(JDAY.EQ.JGS(1)+1+IPOFF(IS,1)).OR.(
     +     IWPEST(IS).EQ.3).AND.(JDAY+iyp.EQ.JGS(1)+1+IPOFF(IS,1)).OR.(
     +        IWPEST(IS).EQ.4).AND.(JDAY.EQ.JGS(2)+1+IPOFF(IS,1)).OR.(
     +     IWPEST(IS).EQ.4).AND.(JDAY+iyp.EQ.JGS(2)+1+IPOFF(IS,1)).OR.(
     +        IWPEST(IS).EQ.5).AND.(JDAY.EQ.IPOFF(IS,1).AND.IYYY.EQ.
     +        IPOFF(IS,2)))) THEN
            IF(IPMETH(IS).NE.4) THEN
              IF(IPMETH(IS).NE.8) THEN
                IC=IC+1
                ITQUE(1,IC)=IPMETH(IS)+4
                ITQUE(2,IC)=IS
                IF(ITQUE(1,IC).EQ.6.OR.ITQUE(1,IC).EQ.10) THEN
                  IC=IC+1
                  ITQUE(1,IC)=20
C
C                 .. POINT AT THE DEFAULT TILLAGE IMP (FIELD CULTIVATOR)
                  ITQUE(2,IC)=4
                ENDIF
              ELSEIF(IPMETH(IS).EQ.8) THEN
                IC=IC+1
                ITQUE(1,IC)=IPMETH(IS)+4
                ITQUE(2,IC)=IS
C
C               ..SET TILLAGE IMP POINTER AT INJECTOR
                ITT=MXAPP
C
C               ..BURN EXISTING TILLAGE APP IF PRESENT
                IF(.NOT.T3) THEN
                  DO 160 J=1,IC
                    IF(ITQUE(1,J).EQ.17) ITQUE(2,J)=ITT
  160             CONTINUE
                ELSE
C
C                 ..BUILD NEW TILLAGE OPERATION WITH INJECTOR
                  IC=IC+1
                  ITQUE(1,IC)=17
                  ITQUE(2,IC)=MXAPP
                  T3=.FALSE.
                ENDIF
              ELSE
C
C               ..NO INJECTOR IMP SPECIFIED
                PRINT *, "RZMAN CALLING MANOUT"
                CALL MANOUT(25,IS,JDAY,0,IYYY)
              ENDIF
C
C           ..CHECK FOR PRESENCE OF IRRIGATION EVENT
            ELSEIF(AIRR.NE.0.0D0) THEN
              IC=IC+1
              ITQUE(1,IC)=IPMETH(IS)+4
              ITQUE(2,IC)=IS
            ELSE
C             ..NO IRRIGATION EVENT COINSIDING SO
C             .. SCHEDULE DEFAULT IRRIGATION APPLICATION
              IF(AIRR.EQ.0.0D0) THEN
                PRINT *, "RZMAN CALLING MANOUT"
                CALL MANOUT(19,0,JDAY,0,IYYY)
                AIRR=BMPIRR
                WRITE(70,1000) AIRR
              ENDIF
              IC=IC+1
              ITQUE(1,IC)=IPMETH(IS)+4
              ITQUE(2,IC)=IS
CC          CALL MANOUT(22,IS,JDAY,0,IYYY)
            ENDIF
          ENDIF
        ENDIF
  170 CONTINUE
C
C     ..CHECK FOR MANURE EVENTS
      DO 190 IM=1,NAPPM
C
C       ..CHECK IF WE ARE MANAGEING THE CURRENT CROP ONLY
        IF(IMPL(IM).EQ.IRPL) THEN
            PLN = IRPL
          IF(((IWMANR(IM).EQ.1).AND.(JDAY.EQ.JPLNT-IMOFF(IM,1))).OR.((
     +        IWMANR(IM).EQ.2).AND.(JDAY.EQ.JPLNT+IMOFF(IM,1))).OR.((
     +    IWMANR(IM).EQ.2).AND.(JDAY+iyp.EQ.JPLNT+IMOFF(IM,1))).OR.((
     +        IWMANR(IM).EQ.3).AND.(JDAY.EQ.JGS(1)+1+IMOFF(IM,1))).OR.((
     +    IWMANR(IM).EQ.3).AND.(JDAY+iyp.EQ.JGS(1)+1+IMOFF(IM,1))).OR.((
     +        IWMANR(IM).EQ.4).AND.(JDAY.EQ.JGS(2)+1+IMOFF(IM,1))).OR.((
     +    IWMANR(IM).EQ.4).AND.(JDAY+iyp.EQ.JGS(2)+1+IMOFF(IM,1))).OR.((
     +        IWMANR(IM).EQ.5).AND.(JDAY.EQ.IMOFF(IM,1).AND.IYYY.EQ.
     +        IMOFF(IM,2)))) THEN
            IF(IMMETH(IM).NE.4) THEN
              IF(IMMETH(IM).EQ.3) THEN
                IC=IC+1
                ITQUE(1,IC)=IMMETH(IM)+12
                ITQUE(2,IC)=IM
C
C               ..SET TILLAGE IMP POINTER AT INJECTOR
                ITT=MXAPP
C
C               ..BURN EXISTING TILLAGE APP IF PRESENT
                IF(.NOT.T3) THEN
                  DO 180 J=1,IC
                    IF(ITQUE(1,J).EQ.17) ITQUE(2,J)=ITT
  180             CONTINUE
                ELSE
C
C                 ..BUILD NEW TILLAGE OPERATION WITH INJECTOR
                  IC=IC+1
                  ITQUE(1,IC)=17
                  ITQUE(2,IC)=MXAPP
                  T3=.FALSE.
                ENDIF
C
C             .. SET NH3 INJECTOR
              ELSEIF(IMMETH(IM).NE.3) THEN
                IC=IC+1
                ITQUE(1,IC)=IMMETH(IM)+12
                ITQUE(2,IC)=IM
c
                IF(ITQUE(1,IC).EQ.14) THEN
C
C                 .. DEFAULT TILLAGE IMP
                  IC=IC+1
                  ITQUE(1,IC)=20
                  ITQUE(2,IC)=4
                ENDIF
              ELSE
C
C               ..NO INJECTOR IMP SPECIFIED
                PRINT *, "RZMAN CALLING MANOUT"
                CALL MANOUT(26,IR,JDAY,0,IYYY)
              ENDIF
            ELSEIF(AIRR.NE.0.0D0) THEN
C             .. HAVE IRRIGATION EVENT FOR SCHEDULE EVENT
              IC=IC+1
              ITQUE(1,IC)=IMMETH(IM)+12
              ITQUE(2,IC)=IM
            ELSE
C             ..NO IRRIGATION EVENT COINSIDING SO
C             .. SCHEDULE DEFAULT IRRIGATION APPLICATION
              IF(AIRR.EQ.0.0D0) THEN
                PRINT *, "RZMAN CALLING MANOUT"
                CALL MANOUT(19,0,JDAY,0,IYYY)
                AIRR=BMPIRR
                WRITE(70,1000) AIRR
              ENDIF
              IC=IC+1
              ITQUE(1,IC)=IMMETH(IM)+12
              ITQUE(2,IC)=IM
C           CALL MANOUT(23,IM,JDAY,0,IYYY)
            ENDIF
            GOTO 200
          ENDIF
        ENDIF
  190 CONTINUE
      IF(T3) ITT=0
C
C     .. GO THROUGH TILE HEADGATE SEQUENCE -- THIS WILL NOT CHANGE IQUE
  200 DO 205 IH=1,NUMTHG
        IF(JDAY.EQ.ITHGOFF(IH,1).AND.IYYY.EQ.ITHGOFF(IH,2)) THEN
          THGCUR=THGDEP(IH)
          GOTO 208
        ENDIF
  205 CONTINUE
C
C     .. SORT THROUGH TEMPORARY STACK
  208 IF(IC.GT.0) THEN
        II=IE
        T1=.TRUE.
        T2=.TRUE.
        DO 240 I=1,IC
          IF(ITQUE(1,I).EQ.17.AND.T1) THEN
C
C           .. DATED TILLAGE EVENT
            T1=.FALSE.
            II=II+1
            IQUE(1,II)=ITQUE(1,I)
            IQUE(2,II)=ITQUE(2,I)
            IQUE(3,II)=JDAY
            IQUE(4,II)=ITT
          ELSEIF(ITQUE(1,I).EQ.20.AND.T2.AND.T1) THEN
C
C           .. INCORPORATION TILLAGE EVENT
            T2=.FALSE.
            II=II+1
            IQUE(1,II)=ITQUE(1,I)
            IQUE(2,II)=ITQUE(2,I)
            IQUE(3,II)=JDAY
            IQUE(4,II)=ITT
          ELSEIF(ITQUE(1,I).NE.17.AND.ITQUE(1,I).NE.20) THEN
            IF(ITQUE(1,I).EQ.4.OR.ITQUE(1,I).EQ.8.OR.ITQUE(1,I).EQ.16)
     +          THEN
C
C             ..IRRIGATION CONTROLLED PRACTICES HAVE TO PUT AT FRONT
              DO 210 J=1,MXAPP
                IF(IQUE(1,J).EQ.-1) GOTO 220
  210         CONTINUE
C
C             ..MAKE ROOM FOR NEW PRACTICE
  220         DO 230 JJ=MXAPP,J+1,-1
                IQUE(1,JJ)=IQUE(1,JJ-1)
                IQUE(2,JJ)=IQUE(2,JJ-1)
                IQUE(3,JJ)=IQUE(3,JJ-1)
                IQUE(4,JJ)=IQUE(4,JJ-1)
  230         CONTINUE
C
C             .. FOUND PLACE TO PUT PRACTICE
              IQUE(1,J)=ITQUE(1,I)
              IQUE(2,J)=ITQUE(2,I)
              IQUE(3,J)=JDAY
              IQUE(4,J)=ITT
              II=II+1   !to fix a problem when fertigation and manure were applied the same day
            ELSE
C
C             .. ALL OTHER PRACTICES
              II=II+1
              IQUE(1,II)=ITQUE(1,I)
              IQUE(2,II)=ITQUE(2,I)
              IQUE(3,II)=JDAY
              IQUE(4,II)=ITT
            ENDIF
          ENDIF
C
  240   CONTINUE
      ENDIF
C
      CALL MAQUE_Memory('PUT',ADIW,JSPLT,SPLT1,FIRST7,NYR,IRPL,NYRC,
     +  NYRP,ADIWMONTH,totadiw,curradiw)
      RETURN
C
 1000 FORMAT(' AUTOMATIC MANAGEMENT IRRIGATION EVENT USING DEFAULT',/
     +    ' DEPTH OF IRRIGATION AT ',F5.2,' CM'/)
      END
C
      SUBROUTINE SPLIT(JDAY,T3,ITQUE,FNH4IN,FNO3IN,IFMETH,FUREIN,IWFERT,
     +    IFOFF,SPLTAP,IDSPLT,SPLTST,IR,IC,ICPLNT,JPLNT,JSPLT,SPLT1,
     +    IBMPAP,IBMPMD,ITT,BMPIRR,AIRR,IYYY,FT)
C======================================================================
C
C       PURPOSE:  THIS ROUTINE DETERMINES THE PARTITIONING AND APPLICATION
C             TIMING FOR SPLIT APPLICATIONS.  IT ASSUMES THAT THERE
C             IS A STARTER THAT IS A PERCENTAGE OF THE TOTAL
C             FOLLOWED BY A SIDEDRESS WHICH IS A SET AMOUNT PREDETERMINED
C             BY THE USER.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       AIRR      AMOUNT OF IRRIGATION SCHEDULED FOR TODAY (CM)
C       BMPIRR        IRRIGATION AMOUNT SPECIFIED BY USER FOR BMP (CM)
C       FNH4IN        AMOUNT OF NH4-N FERTILIZER APPLIED (KG/HA)
C       FNO3IN
C       FUREIN
C       IBMPAP        BMP CHEMICAL APPLICATION OPTION (1..5)
C       IBMPMD        BMP APPLICATION METHOD OPTION (1..4)
C       IC            CURRENT COUNTER FOR TEMP QUE
C       ICPLNT
C       IDSPLT
C       IFOFF
C       IR            LOOP INDEX SPECIFING CURRENT FERT OPTION
C       ITQUE     TEMPORARY QUE
C       ITT           CURRENT INDEX FOR TILLAGE OPERATIONS
C       IWFERT
C       JSPLT
C       NUMFN
C       SPLINC
C       SPLT1
C       SPLTAP
C       T3            LOGICAL FLAG INDICATING IF TILL OP IS SET.
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                 NONE
C
C       CALLED FROM:  MAQUE
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:   3.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXTAPP=10,MXAPP=200)
C
      LOGICAL T3,SPLT1
C
      DIMENSION ITQUE(2,MXTAPP),FNH4IN(MXAPP),FNO3IN(MXAPP),
     +    FUREIN(MXAPP),IWFERT(MXAPP),IFOFF(MXAPP,2),SPLTAP(MXAPP),
     +    IDSPLT(MXAPP),SPLTST(MXAPP),IFMETH(MXAPP),IBMPMD(MXAPP),
     +    IBMPAP(MXAPP)
C
      SAVE SAVNH4,SAVNO3,SAVURE,TMPNH4,TMPNO3,TMPURE,NUMFN
C
      DATA SAVNH4 /0.0D0/,SAVNO3 /0.0D0/,SAVURE /0.0D0/,TMPNH4 /0.0D0/,
     +    TMPNO3 /0.0D0/,TMPURE /0.0D0/,NUMFN /0/
C
      IF((IWFERT(IR).EQ.6.OR.IWFERT(IR).EQ.7).AND.(ICPLNT.LT.100.OR.
     +    ICPLNT.GT.200.OR.IWFERT(IR).EQ.4)) THEN
        IF(JDAY.EQ.JPLNT-IFOFF(IR,1)) THEN
          IF(SPLT1) THEN
            SAVNH4=FNH4IN(IR)
            SAVNO3=FNO3IN(IR)
            SAVURE=FUREIN(IR)
            NUMFN=0
            IF(SAVNH4.GT.0.0D0) NUMFN=NUMFN+1
            IF(SAVNO3.GT.0.0D0) NUMFN=NUMFN+1
            IF(SAVURE.GT.0.0D0) NUMFN=NUMFN+2
            IF(NUMFN.GT.0) THEN
              SPLINC=SPLTAP(IR)/DBLE(NUMFN)
              SPLT1=.FALSE.
            ENDIF
          ENDIF
          FNH4IN(IR)=SAVNH4*SPLTST(IR)
          FNO3IN(IR)=SAVNO3*SPLTST(IR)
          FUREIN(IR)=SAVURE*SPLTST(IR)
          TMPNH4=SAVNH4-FNH4IN(IR)
          TMPNO3=SAVNO3-FNO3IN(IR)
          TMPURE=SAVURE-FUREIN(IR)
          IF(NUMFN.GT.0) THEN
            JSPLT=JDAY+IDSPLT(IR)
C
C           ..CONFIGURE QUE FOR BMP OPTIONS
            IF(IFMETH(IR).EQ.6) CALL BMPOPT(IBMPAP,IBMPMD,IR,ITQUE,IC,
     +          T3,ITT,BMPIRR,AIRR,JDAY,IYYY)
          ELSE
            JSPLT=731
          ENDIF
        ELSEIF((TMPNH4.GT.0.0D0.OR.TMPNO3.GT.0.0D0.OR.TMPURE.GT.0.0D0)
     +      .AND.FT.GT.0.0) THEN
          JSPLT=JDAY+IDSPLT(IR)
          IF(TMPNH4.GT.0.0D0) THEN
            FNH4IN(IR)=MIN(SPLINC,TMPNH4)
            TMPNH4=TMPNH4-FNH4IN(IR)
          ELSE
            FNH4IN(IR)=0.0D0
          ENDIF
          IF(TMPNO3.GT.0.0D0) THEN
            FNO3IN(IR)=MIN(SPLINC,TMPNO3)
            TMPNO3=TMPNO3-FNO3IN(IR)
          ELSE
            FNO3IN(IR)=0.0D0
          ENDIF
          IF(TMPURE.GT.0.0D0) THEN
            FUREIN(IR)=MIN(SPLINC*2,TMPURE)
            TMPURE=TMPURE-FUREIN(IR)
          ELSE
            FUREIN(IR)=0.0D0
          ENDIF
C
C         ..CONFIGURE QUE FOR BMP OPTIONS
          IF(IFMETH(IR).EQ.6) CALL BMPOPT(IBMPAP,IBMPMD,IR,ITQUE,IC,T3,
     +        ITT,BMPIRR,AIRR,JDAY,IYYY)
        ELSEIF((TMPNH4.GT.0.0D0.OR.TMPNO3.GT.0.0D0.OR.TMPURE.GT.0.0D0)
     +      .AND.FT.EQ.0.0) THEN
          IF((IFMETH(IR).EQ.6.AND.IBMPMD(IR).NE.4).OR.(IFMETH(IR).NE.5))
     +        THEN
C
C           ..CANCELS FERT APP'S FOR ALL IMPLIMENTS EXCEPT IRRIGATION.
            WRITE(70,*) ' REMAINING SPLIT FERTILIZER APPS CANCELLED DUE'
            WRITE(70,*) ' TO CANOPY CLOSURE'
            JSPLT=731
            FNH4IN(IR)=0.0D0
            FNO3IN(IR)=0.0D0
            FUREIN(IR)=0.0D0
          ENDIF
        ELSE
          JSPLT=731
          FNH4IN(IR)=0.0D0
          FNO3IN(IR)=0.0D0
          FUREIN(IR)=0.0D0
        ENDIF
      ELSEIF(IFMETH(IR).EQ.6.AND.(ICPLNT.LT.100.OR.ICPLNT.GT.200.OR.
     +    IWFERT(IR).EQ.4)) THEN
C
C       ..CONFIGURE QUE FOR BMP OPTIONS
        PRINT *, "RZMAN CALLING BMPOPT"
        CALL BMPOPT(IBMPAP,IBMPMD,IR,ITQUE,IC,T3,ITT,BMPIRR,AIRR,JDAY,
     +      IYYY)
      ENDIF
      RETURN
      END
C
      SUBROUTINE BMPOPT(IBMPAP,IBMPMD,IR,ITQUE,IC,T3,ITT,BMPIRR,AIRR,
     +    JDAY,IYYY)
C
C======================================================================
C
C       PURPOSE:  THIS ROUTINE DETERMINES THEN PARTITIONING AND APPLICATION
C             METHODS TO APPLY WHEN USING BMP OPTIONS.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       AIRR      AMOUNT OF IRRIGATION SCHEDULED FOR TODAY (CM)
C       BMPIRR        IRRIGATION AMOUNT SPECIFIED BY USER FOR BMP (CM)
C       IBMPAP        BMP CHEMICAL APPLICATION OPTION (1..5)
C       IBMPMD        BMP APPLICATION METHOD OPTION (1..4)
C       IC            CURRENT COUNTER FOR TEMP QUE
C       IR            LOOP INDEX SPECIFING CURRENT FERT OPTION
C       ITQUE     TEMPORARY QUE
C       ITT           CURRENT INDEX FOR TILLAGE OPERATIONS
C       T3            LOGICAL FLAG INDICATING IF TILL OP IS SET.
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                 NONE
C
C       CALLED FROM:  MAQUE
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:   3.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXTAPP=10,MXAPP=200)
C
      LOGICAL T3
C
      DIMENSION ITQUE(2,MXTAPP),IBMPAP(MXAPP),IBMPMD(MXAPP)
C
C     ..DETERMINE APPLICATION METHOD
      IF(IBMPMD(IR).LE.2) THEN
C
C       ..BROADCAST LEAVE ON SURFACE
        IC=IC+1
        ITQUE(1,IC)=IBMPMD(IR)
        ITQUE(2,IC)=IR
        IF(IBMPMD(IR).EQ.2) THEN
C
C         ..BROADCAST WITH INCORP
          IC=IC+1
          ITQUE(1,IC)=20
          ITQUE(2,IC)=4
        ENDIF
      ELSEIF(IBMPMD(IR).EQ.3) THEN
C
C       ..INJECTED WITH NH3-N APPLICATOR
        IC=IC+1
        IF(IBMPAP(IR).EQ.4) THEN
          ITQUE(1,IC)=IBMPMD(IR)
          ITQUE(2,IC)=IR
        ELSE
          ITQUE(1,IC)=27
          ITQUE(2,IC)=IR
        ENDIF
C
C       ..SET TILLAGE IMP POINTER AT INJECTOR
        ITT=MXAPP
C
C       ..BURN EXISTING TILLAGE APP
        IF(.NOT.T3) THEN
          DO 10 J=1,IC
            IF(ITQUE(1,J).EQ.17) ITQUE(2,J)=ITT
   10     CONTINUE
        ELSE
C
C         ..BUILD NEW OPERATION WITH INJECTOR
          IC=IC+1
          ITQUE(1,IC)=17
          ITQUE(2,IC)=MXAPP
          T3=.FALSE.
        ENDIF
      ELSE
C
C       ..IRRIGATION APPLICATION
        IF(AIRR.EQ.0.0D0) THEN
          PRINT *, "RZMAN CALLING MANOUT"
          CALL MANOUT(19,0,JDAY,0,IYYY)
          AIRR=BMPIRR
          WRITE(70,1000) AIRR
        ENDIF
        IC=IC+1
        ITQUE(1,IC)=4
        ITQUE(2,IC)=IR
      ENDIF
C
      RETURN
 1000 FORMAT(' BMP MANAGEMENT IRRIGATION EVENT USING DEFAULT',/
     +    ' DEPTH OF IRRIGATION AT ',F5.2,' CM'/)
      END
C
      SUBROUTINE BMPNIT(FRACOM,PLTPOP,TLT,NN,BD,XNO3,THETA,FUREIN,
     +    FNH4IN,FNO3IN,IR,BMPREC,IBMPAP,TAPMUN)
C
C======================================================================
C
C       PURPOSE:  THIS ROUTINE DETERMINES THE AMOUNT OF FERTILIZER TO APPLY
C             BASED ON THE BMP OPTIONS SET BY THE USER.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       BD            CURRENT SOIL BULK DENSITY [G/CM^3]
C       BMPREC        BMP RECOMMENDATION OF FERTILIZER ADDITION (KG/HA)
C       CONV5  P  CONVERTS LBS/AC ==> KG/HA
C       CONV6  P  CONVERTS BU/AC ==> KG/HA
C       FNH4IN        STANDARD FERTILIZER INPUT FOR NH4-N (KG/HA)
C       FNO3IN        STANDARD FERTILIZER INPUT FOR NO3-N (KG/HA)
C       FRACOM        FRACTION OF ORGANIC MATTER IN THE SOIL
C       FUREIN        STANDARD FERTILIZER INPUT FOR UREA-N (KG/HA)
C       ISTATE        SPECIFIES WHICH STATE ALGORITHM TO USE
C       OMADJ     ORGANIC MATTER ADJUSTMENT FACTOR
C       ONEFT  P  ONE FOOT DEPTH IN CM.
C       PLTPOP        PLANT SEEDING RATE (#/HA)
C       SOYYLD        SOYBEAN YIELD FROM PREVIOUS YEAR (KG/HA)
C       TAPMUN        TOTAL APPLIED MANURE FOR YEAR (KG N/HA)
C       THETA     VOL WATER CONTENT [CM3/CM3]
C       UMISCC        USER SPECIFIED MISC. CREDITS (LB/AC)
C       USAMDP        USER SPECIFIED DEPTH OF SOIL SAMPLING (IN)
C       YGOAL     YIELD GOAL [BU/AC]
C
C       COMMENTS:
C
C       STATE CODES:  1- MISSOURI
C                 2- NEBRASKA
C                 3- IOWA-FALL TEST
C                 4- IOWA-LATE SPRING TEST
C                 5- COLORADO
C
C       EXTERNAL REFERENCES:
C                 NONE
C
C       CALLED FROM:  MAFERT
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:   3.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(CONV5=1.121D0,ONEFT=12.0D0*2.54D0,MXAPP=200,CONV6=
     +    62.71D0)
C
      COMMON /BMPRC/ YGOAL,OMADJ,USAMDP,UMISCC,ISTATE,ISOYB,BMPIRR,
     +    BMPTIL,FMC,YGSF,SOYYLD
C
      LOGICAL FIRST8
C
      DIMENSION FRACOM(NN),TLT(NN),BD(NN),XNO3(NN),THETA(NN),
     +    FNH4IN(MXAPP),FNO3IN(MXAPP),FUREIN(MXAPP),IBMPAP(MXAPP)
C
      SAVE FIRST8,USAMCM
      DATA FIRST8 /.TRUE./
C
      IF(FIRST8) THEN
        USAMCM=USAMDP   !*2.54D0 Liwang Ma, chage to cm 2-20-2009
        FIRST8=.FALSE.
      ENDIF
C
C     ..DETERMINE AVERAGE FRACTION ORGANIC MATTER IN THE TOP 12 IN.
      OMFRAC=FRACOM(1)*TLT(1)/ONEFT
      DO 10 I=2,NN
        OMFRAC=OMFRAC+FRACOM(I)*(TLT(I)-TLT(I-1))/ONEFT
        IF(TLT(I).GE.ONEFT) THEN
C
C         ..CONVERT FRACTION ==> PERCENTAGE
          OMFRAC=OMFRAC*100.0D0
          GOTO 20
        ENDIF
   10 CONTINUE
C
C     ..DETERMINE DEPTH-WEIGHTED AVERAGE NITRATE VALUE
   20 AVGN=XNO3(1)*THETA(1)/BD(1)*TLT(1)/USAMCM
      DO 30 I=2,NN
        AVGN=AVGN+XNO3(I)*THETA(I)/BD(I)*(TLT(I)-TLT(I-1))/USAMCM
        IF(TLT(I).GE.USAMCM) THEN
          GOTO 40
        ENDIF
   30 CONTINUE
C
C     ..DETERMINE THE NITROGEN RECOMMENDATION BASED ON STATE CODE.
   40 IF(ISTATE.EQ.1) THEN
C
C       ..M I S S O U R I   ALGORITHM
        SOMADJ=5.0D0*OMADJ*OMFRAC
        RECN=PLTPOP*1.0D-3*4.0D0+0.9D0*YGOAL-SOMADJ
      ELSEIF(ISTATE.EQ.2) THEN
C
C       ..N E B R A S K A   ALGORITHM
        RECN=35.0D0+1.2D0*YGOAL-8.0D0*AVGN-0.14D0*YGOAL*OMFRAC
C
C       ..DETERMINE IF LAST YEAR THERE WAS A SOYBEAN CROP
        IF(ISOYB.EQ.1) RECN=RECN-45.0D0
      ELSEIF(ISTATE.EQ.3) THEN
C
C       ..I O W A   FALL TEST ALGORITHM
        IF(ISOYB.EQ.1) THEN
          SOY=MIN(SOYYLD/CONV6,40.0D0)
        ELSE
          SOY=0.0D0
        ENDIF
c        RECN=YGOAL*YGSF-SOY-TAPMUN/CONV5*FMC !changed by Liwang Ma to give N credit directly
        RECN=YGOAL*YGSF-SOY-FMC
      ELSEIF(ISTATE.EQ.4) THEN
C
C       ..I O W A   LATE SPRING TEST ALGORITHM
c Liwang Ma, change from 25 to 20, Oct 8,2005
        RECN=(25.0D0-AVGN)*8.0D0
      ELSEIF(ISTATE.EQ.5) THEN
C
C       ..C O L O R A D O   ALGORITHM
        RECN=35.0D0+1.2D0*YGOAL-8.0D0*AVGN-0.14D0*YGOAL*OMFRAC
      ENDIF
C
C     ..SUBTRACT OUT THE USER MISC. CREDITS
      RECN=RECN-UMISCC
C
      BMPREC=MAX(RECN*CONV5,0.0D0)
      PRINT*,'==> ESTIMATED BMP FERTLIZER APP [KG/HA]: ',BMPREC
c      WRITE(70,1000) BMPREC
C
C     ..ZERO OUT CHEMICAL AMOUNT VARIABLES
      FNH4IN(IR)=0.0D0
      FNO3IN(IR)=0.0D0
      FUREIN(IR)=0.0D0
C
C     ..DETERMINE CHEMICAL PARTITIIONING
      IF(IBMPAP(IR).EQ.1) THEN
        FNO3IN(IR)=BMPREC
      ELSEIF(IBMPAP(IR).EQ.2.OR.IBMPAP(IR).EQ.4.OR.IBMPAP(IR).EQ.5) THEN
        FNH4IN(IR)=BMPREC
      ELSEIF(IBMPAP(IR).EQ.3) THEN
        FNO3IN(IR)=BMPREC*0.5D0
        FNH4IN(IR)=BMPREC*0.5D0
      ELSEIF(IBMPAP(IR).EQ.6) THEN
        FNO3IN(IR)=BMPREC*0.25D0
        FNH4IN(IR)=BMPREC*0.25D0
        FUREIN(IR)=BMPREC*0.50D0
      ENDIF
C
      RETURN
c 1000 FORMAT(' ==> ESTIMATED BMP FERTLIZER APP [KG/HA]: ',F12.2)
      END
C
      SUBROUTINE MATILL(JDAY,SOILPP,RPOOL,SDEAD,SDCN,XNU,CC,THETA,SLKS,
     +    T,CONCX2,SOLTP1,SOILHP,IQUE,RM,RCN,EK2,CORES,BD,IYYY,FRACOM,
     +    FREUND,H,HWP,FRACON)
C
C======================================================================
C
C       PURPOSE:
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       EK2        I  EQUIL.CONST. FOR ADSORPTION [CM3 H2O/G SOIL]
C       FCR        P  FRACTION OF CARBON IN BIOMASS
C
C
C       COMMENTS:
C
C       CALLED FROM:
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:   2
C
C-----------------------------------------------------------------------
C
      USE VARIABLE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,MXNODT=3001,MAXHOR=12,MXCHEM=15,MXPEST=3,
     +    MXAPP=200,FCR=0.4D0,CARBCV=1.0D0/0.58D0)
C
      COMMON /IPMACF/ AGSZ(MAXHOR),FDEP(MAXHOR),MCPOR(MAXHOR),PI,
     +    PL(MAXHOR),RP(MAXHOR),SFCT,TWOPI,WP(MAXHOR),NHCMP,YESMAC,
     +    EFFWR,EFFWC,XPRESF
C
      COMMON /IPINF/ CRUSTK,OCRUST,ICRUST,INFLPD,NHZ,NSLT,POND,RR,
     +    DHB(MAXHOR),NSL(MAXHOR),WI(MXnod),DSL(MXNODT),VWC(MXNODT),
     +    IUGFLW
      DOUBLE PRECISION MCPOR
      INTEGER YESMAC
C
      COMMON /GEOMTR/XLONG,ASPECT,DELZ(MXNOD),ELEV,HORTHK(MAXHOR),SLOPE,
     +    AREA,TL(MXNOD),TLT(MXNOD),XLAT,ZN(MXNOD),NHOR,NN,NNT
C
      COMMON /NDEX/ NDXN2H(MXNOD),NDXH2N(MAXHOR),NDXN2T(MXNOD),
     +    NDXT2N(MXNODT)
C
      COMMON /NINFO/ TSOTN0,TSOTN,TOTA,TOTL,TAMIN,TANIT,TADRT(2),
     +    TARAIN(2),TAIRR(3),TAFERT(3),TARES(3),TLIMM,TLDEN,TLVOL,
     +    TLRO(3),TLSEP(3),TLDRN(3),TLLAT(3),TAMANR,TMANH4,tamanr_bal
     +    ,TLN2O,TLNXO,TLNITN2O,TLNITNXO,TLGHGADS,TLGHGADSden
C
      COMMON /CINFO/TAMANC,TADRTC(2),TARESC(3),TOTCO2,TACUREA,YIELDC
     +             ,TAMANC_BAL,RCO2,TCAG,TCBG,TOTCH4,TOTCO2NI
C
      COMMON /NUTPAR/ R14,R23,R34,R43,R45,R53,R15,R25,EFFMAX,EFFNIT,
     +    EFFDEN,DENC,RPOP(3),ADCAY,ANIT,ADENIT,AUREA,ADEATH(3),CN(9),
     +    ADCAYV(5),XKP(3),AMETH,FRN2O_NIT,FRN2O_DEN,RNO_N2O,RAINNO,
     +    O2LIM,EFFMETH,ISBUG
C
C     Zhiming added Pmanuremix
      COMMON /TILLIN/ OBD(2),OFDP(2),TDEPTH(30),TILINT(30,2),
     +    TNDX(MXAPP),UDEPTH(MXAPP), PManureMix(MXAPP), IMP(MXAPP),  
     +    ITL(MXAPP), ITOFF(MXAPP,2),IWTILL(MXAPP),NUMTL, ITPL(MXAPP)
     
C
      DIMENSION RPOOL(MXNOD,2),SOILPP(8,MAXHOR),TN(20),TC(MXCHEM),
     +    TCX(MXPEST),XNU(MXNOD,25),CC(MXNOD,MXCHEM),THETA(mxnod),
     +    SLKS(MXNOD,MXPEST,2),T(mxnod),PBD(MAXHOR),IQUE(4,MXAPP),
     +    CONCX2(MXNOD,MXPEST),SOLTP1(MAXHOR,5),SOILHP(13,MAXHOR),
     +    EK2(MXNOD,MXPEST),CORES(MXPEST),BD(mxnod),FRACOM(mxnod)
     +    ,FREUND(MXPEST),H(mxnod),FRACON(MXNOD)

      INTEGER :: ddd1,mmm1,kk1
C
      DATA PBD /MAXHOR*0.0D0/
C
      TARES(1)=0.0D0
      TARES(2)=0.0D0
      TARES(3)=0.0D0
      TARESC(1)=0.0D0
      TARESC(2)=0.0D0
      TARESC(3)=0.0D0
      
      CALL CDATE(JDAY,ddd1,mmm1,IYYY)
C
C     ..CHECK QUE FOR TILLAGE EVENT
      DO 190 IS=1,MXAPP
C
C       ..KICK OUT IF AT END OF GROUP
        IF(IQUE(1,IS).EQ.-1) GOTO 200
        IF(IQUE(1,IS).EQ.17.OR.IQUE(1,IS).EQ.20) THEN
          IPRAC=IQUE(1,IS)
          IT=IQUE(2,IS)
          
          Counttill = Counttill + 1
          Tday1(Counttill)= ddd1
          Tmon(Counttill)= mmm1
          Tyear(Counttill) = IYYY
C
c        if (tndx(it).eq.0.0d0) goto 200   !Liwang was trying to fix N balance problem, but this is the wrong place
C         ..REPORT WHAT I'M DOING
          IF(IPRAC.EQ.20) THEN
            PRINT *, "RZMAN CALLING MANOUT"
            CALL MANOUT(IPRAC,IT,JDAY,(JDAY-IQUE(3,IS)),IYYY)
          ELSE
            PRINT *, "RZMAN CALLING MANOUT"
            CALL MANOUT(IPRAC,IMP(IT),JDAY,(JDAY-IQUE(3,IS)),IYYY)
          ENDIF
          
              Tilld(Counttill) = Tilld1(IT) 
              Tillinceffi(Counttill) =Tillinceffi1(IT)  
              Tillmixeffi(Counttill)= Tillmixeffi1(IT)      
          
          
          
C
C         .. ZERO OUT ACCUMULATORS
          DO 10 J=1,MXCHEM
            TC(J)=0.0D0
            IF(J.GT.12) TCX(J-12)=0.0D0
   10     CONTINUE
c Liwang Ma, 
          DO 20 J=1,20
            TN(J)=0.0D0
   20     CONTINUE
          TOTW=0.0D0
          TOTT=0.0D0
C
C========================= MODIFIED BY LIWANG MA TO AVOID MASS BALANCE DUE TO TILLAGE DEPTH IRREGULARITY=====
          IF(ITL(IT).LT.2.AND..NOT.(IPRAC.EQ.20).AND.
     +      (UDEPTH(IT).GT.HORTHK(1))) THEN
            IHT=2
          ELSE
            IHT=1
          ENDIF
C====================================================================================
C         .. DETERMINE HOW MANY HORIZONS ARE AFFECTED
c          IF(ITL(IT).LT.2.AND..NOT.(IPRAC.EQ.20)) THEN
c            IHT=2
c          ELSE
c            IHT=1
c          ENDIF
C
C         .. DETERMINE IF TILLAGE OPERATION OR JUST COMPACTION
          IF((ITL(IT).EQ.1.OR.ITL(IT).EQ.2).OR.(IPRAC.EQ.20)) THEN
C
C           ..DETERMINE TILLAGE INTENSITY
            IF(IPRAC.EQ.20) THEN
C
C             ..DEFAULT TILLAGE INTENSITY FOR APPLICATION OF CHEMICALS
              TI=0.25D0
            ELSE
              IF(TNDX(IT).GE.1.0) THEN
                I=INT(TNDX(IT))
                TI=TILINT(IMP(IT),I)
              ELSE
                TI=TNDX(IT)
              ENDIF
            ENDIF
C
C           ..ACCUMULATED MASSES FOR MIXING PURPOSES
cc-ma            IF(TI.NE.0.0D0) THEN  ! commented out by Liwang Ma on 4-7-2010 to correct the mass balance problem with zero tillage intensity.
              DO 50 I=1,NN
                IH=NDXN2H(I)
                IF(IH.LE.IHT) THEN
                  TW=THETA(I)*TL(I)
                  TB=SOILPP(3,IH)*TL(I)
                  TOTW=TOTW+TW
                  TOTT=TOTT+T(I)*SOLTP1(IH,5)*TL(I)
                  DO 30 J=1,MXCHEM
                    IF(J.LE.12) THEN
                      TC(J)=TC(J)+CC(I,J)*TW
                    ELSE
                      IP=J-12
C LIWANG MA, 7-3-2006
                      TC(J)=TC(J)+CC(I,J)*(THETA(I)+
     +                  FSLKS(SLKS(I,IP,1),CC(I,J),FREUND(IP),
     +                    theta(i),soilpp(3,ih))*
     +                    SOILPP(3,IH))*TL(I)
                      TCX(IP)=TCX(IP)+CONCX2(I,IP)*TB
                    ENDIF
   30             CONTINUE
c Liwang Ma, 
                  DO 40 J=1,20
                    IF(J.LE.9.OR.J.GE.13) THEN
                      TN(J)=TN(J)+XNU(I,J)*TB
                    ELSE
                      TN(J)=TN(J)+XNU(I,J)*TW
                    ENDIF
   40             CONTINUE
                ENDIF
   50         CONTINUE
cc-ma            ENDIF ! commented out by Liwang Ma on 4-7-2010 to correct the mass balance problem with zero tillage intensity.
C
C           ..ADJUST BULK DENSITY AND FRACTION OF DEAD END PORES
            DO 60 IH=1,IHT
              SOILPP(3,IH)=SOILPP(3,IH)-(SOILPP(3,IH)-0.667D0*OBD(IH))*
     +            TI
              SOILPP(3,IH)=MAX(SOILPP(3,IH),0.8D0*OBD(IH))
              FDEP(IH)=1.0D0
   60       CONTINUE
C
C           ..DESTROY SURFACE CRUST
            CRUSTK=0.0D0
C
C           ..ADJUST RESIDUE MIXING FACTOR
            RMF=1.0D0-TI
            IF(.NOT.(IPRAC.EQ.20)) THEN
              IM=IMP(IT)
              IF(TDEPTH(IM).NE.0.0D0.AND.TDEPTH(IM).GT.UDEPTH(IT)) THEN
                RMF=RMF+RMF*(TDEPTH(IM)-UDEPTH(IT))/TDEPTH(IM)
                RMF=MAX(RMF,0.0D0)
                RMF=MIN(RMF,1.0D0)
              ENDIF
            ENDIF
C
C           ..CONVERT STANDING RESIDUE TO FLAT (ON THE GROUND) RESIDUE
            DSDEAD=SDEAD*(1.0D0-EXP(-8.535D0*TI*TI))
            SDEAD=SDEAD-DSDEAD
C
C           ..STICK THE NEW KNOCKDOWN RESIDUE INTO THE RESIDUE POOL
            RCN=CNEW(RM,DSDEAD,RCN,SDCN,0)
C           RCN = MAX(MIN(RCN,CN(2)),CN(1))
            RM=RM+DSDEAD
C
C           ..MIX RESIDUE INTO THE TILLAGE LAYER
            IF(RCN.LE.CN(1)) THEN
              S=0.0D0
            ELSEIF(RCN.GE.CN(2)) THEN
              S=1.0D0
            ELSE
              S=(1.0D0/RCN-1.0D0/CN(1))/(1.0D0/CN(2)-1.0D0/CN(1))
            ENDIF
            RP1MIX=(1.0D0-RMF)*RM*(1.0D0-S)
            RP2MIX=(1.0D0-RMF)*RM*S
C
C           .. SAVE FOR MASS BALANCE CALCULATIONS
            TARES(1)=RP1MIX*FCR/CN(1)
            TARES(2)=RP2MIX*FCR/CN(2)
            TARESC(1)=RP1MIX*FCR
            TARESC(2)=RP2MIX*FCR
C
            IF(RCN.LE.CN(1)) THEN
         RCO2=RCO2+RM*(1.0D0-RMF)*FCR-(RP1MIX*FCR/RCN)*CN(1)
            ELSEIF(RCN.GE.CN(2)) THEN
         RCO2=RCO2+RM*(1.0D0-RMF)*FCR-(RP2MIX*FCR/RCN)*CN(2)
            endif
c
C           ..UPDATE SURFACE RESIDUE POOLS
            RM=RM*RMF
C
C           ..ADD PESTICIDE ON RESIDUE TO PESTICIDE POOL
            DO 70 J=1,MXCHEM
              IF(J.GT.12) THEN
                IP=J-12
                IF(CORES(IP).GT.0.0D0) THEN
                  PSOIL=CORES(IP)*(1.0D0-RMF)
                  CORES(IP)=MAX(CORES(IP)*RMF,0.0D0)
C LIWANG MA, 7-3-2006  residue to pesticide pool
                  CONV2=1.0D0/((THETA(1)+SOILPP(3,1)*
     +                FSLKS(SLKS(1,IP,1),psoil,FREUND(IP),
     +                theta(1),soilpp(3,1))+
     +                SOILPP(3,1)*EK2(1,IP))*TL(1))
                  XCONC=PSOIL*CONV2
                  TC(J)=TC(J)+XCONC*(THETA(1)+SOILPP(3,1)*
     +                FSLKS(SLKS(1,IP,1),psoil,FREUND(IP),
     +                theta(1),soilpp(3,1)))*tl(1)
                  TCX(IP)=TCX(IP)+XCONC*EK2(1,IP)
     +                   *SOILPP(3,1)*EK2(1,IP)*TL(1)
                ENDIF
              ENDIF
   70       CONTINUE
C
C           ..MIX CHEMICALS AND MAKE PROFILE UNIFORM FOR AREA AFFECTED
            DO 100 I=1,NN
              IH=NDXN2H(I)
              IF(IH.LE.IHT) THEN
                FACT=TL(I)/HORTHK(IHT)
                THETA(I)=TOTW*FACT/TL(I)
                TW=FACT/(THETA(I)*TL(I))
                TB=FACT/(SOILPP(3,IH)*TL(I))
                TP=SOILPP(3,IH)*TL(I)/1.0D6
                DO 80 J=1,MXCHEM
                  IF(J.LE.12) THEN
                    CC(I,J)=TC(J)*TW
                  ELSE
                    IP=J-12
C LIWANG MA, 7-3-2006
                    CC(I,J)=TC(J)*FACT/((THETA(I)+
     +                  FSLKS(SLKS(I,IP,1),TC(J),FREUND(IP),
     +                   theta(i),soilpp(3,ih))*
     +                  SOILPP(3,IH))*TL(I))
                    CONCX2(I,IP)=TCX(IP)*TB
                  ENDIF
   80           CONTINUE
c  Liwang Ma, 9-9-2005
                DO 90 J=1,20
                  IF(J.LE.9.OR.J.GE.13) THEN
                    XNU(I,J)=TN(J)*TB
                    IF(J.EQ.1.OR.J.EQ.2) RPOOL(I,J)=XNU(I,J)*TP
                  ELSE
                    XNU(I,J)=TN(J)*TW
                  ENDIF
   90           CONTINUE
C               THETA(I) = TOTW * FACT / TL(I)
                T(I)=TOTT*FACT/(SOLTP1(IH,5)*TL(I))
              ENDIF
  100       CONTINUE
C
C           ..MIX IN RESIDUES AND ADJUST CARBON POOLS FOR NEW RESIDUE
            DO 110 I=1,NN
              IH=NDXN2H(I)
              IF(IH.LE.IHT) THEN
                FACT=TL(I)/HORTHK(IHT)
                RPOOL(I,1)=RPOOL(I,1)+RP1MIX*FACT*1.0D-5*FCR
                RPOOL(I,2)=RPOOL(I,2)+RP2MIX*FACT*1.0D-5*FCR
                FACTN=1.0D6/(TL(I)*SOILPP(3,IH))
                XNU(I,1)=RPOOL(I,1)*FACTN
                XNU(I,2)=RPOOL(I,2)*FACTN
              ENDIF
  110       CONTINUE
C
          ELSEIF(ITL(IT).EQ.3) THEN
C
C           ..COMPACTION RUN OF TRACTOR
            DO 120 IH=1,IHT
              PBD(IH)=SOILPP(3,IH)
C
C             ..ADJUST BULK DENSITY
              DBD=(OBD(IH)-SOILPP(3,IH))*0.3D0
              SOILPP(3,IH)=MIN((SOILPP(3,IH)+DBD),OBD(IH))
C
C             ..ADJUST FRACTION OF DEAD END PORES
              DDP=(FDEP(IH)-OFDP(IH))*0.3D0
              FDEP(IH)=MAX((FDEP(IH)+DDP),OFDP(IH))
  120       CONTINUE
            DO 150 I=1,NN
              IH=NDXN2H(I)
              IF(IH.LE.IHT) THEN
C
C               ..ADJUST PESTICIDE MASS-BASED CONCENTRATIONS
                DO 130 IP=1,MXPEST
                  J=IP+12
C LIWANG MA, 7-3-2006
                  tCC=CC(I,J)*(THETA(I)+
     +                FSLKS(SLKS(I,IP,1),CC(I,J),FREUND(IP),
     +                 theta(i),pbd(ih))*PBD(IH))
                  CC(I,J)=tcc/(
     +                THETA(I)+
     +                FSLKS(SLKS(I,IP,1),CC(I,J),FREUND(IP),
     +                theta(i),soilpp(3,ih))
     +                *SOILPP(3,IH))
c                  CC(I,J)=CC(I,J)*(THETA(I)+
c     +                FSLKS(SLKS(I,IP,1),CC(I,J),FREUND(IP),
c     +                 theta(i),pbd(ih))*PBD(IH))/(
c     +                THETA(I)+
c     +                FSLKS(SLKS(I,IP,1),CC(I,J),FREUND(IP),
c     +                theta(i),soilpp(3,ih))
c     +                *SOILPP(3,IH))
                  CONCX2(I,IP)=CONCX2(I,IP)*PBD(IH)/SOILPP(3,IH)
  130           CONTINUE
C
C               ..ADJUST NUTRIENT MASS-BASED CONCENTRATIONS
                DO 140 IN=1,20
                  IF(IN.LT.10.OR.IN.GT.12) XNU(I,IN)=XNU(I,IN)*PBD(IH)/
     +                SOILPP(3,IH)
  140           CONTINUE
              ENDIF
  150       CONTINUE
C
C           ..DESTROY SURFACE CRUST
            CRUSTK=0.0D0
C
          ELSE
            PRINT*,'>>>>> ILLEGAL TILLAGE OPERATION <<<<<'
C
          ENDIF
C
C         ... INITIALIZE SOIL HYDRAULIC PARAMETERS AFTER MIXING UP SOIL
          DO 160 J=1,NHOR
            PCLAY=SOILPP(7,J)*100.0D0
            PSILT=SOILPP(6,J)*100.0D0
            PSAND=SOILPP(5,J)*100.0D0
            TBD=SOILPP(3,J)
            ITYPE=INT(SOILPP(1,J))
            PDEN=SOILPP(2,J)
            CALL SOILPR(J,SOILHP,TBD,PCLAY,PSAND,PSILT,ITYPE,PDEN,0,
     +                  HWP)
            SOILPP(4,J)=SOILHP(6,J)
  160     CONTINUE
C
C         ..UPDATE BULK DENSITY (BD)
          DO 170 J=1,NN
            IH=NDXN2H(J)
            BD(J)=SOILPP(3,IH)
            H(J)=WCH(H(J),THETA(J),SOILHP(1,IH),IH,0)
  170     CONTINUE
C
C         ..UPDATE FRACTION OF ORGANIC MATTER IN SOIL
          DO 180 J=1,NN
            TOTWT=(XNU(J,1)+XNU(J,2)+XNU(J,3)+XNU(J,4)+XNU(J,5))*CARBCV
            TOTWT=TOTWT+(XNU(J,7)/RPOP(1)+XNU(J,8)/RPOP(2)+XNU(J,9)/
     +          RPOP(3))*CARBCV
            WHUMUS=TOTWT*TL(J)*BD(J)/1.0D6
            WTSOIL=TL(J)*BD(J)
            FRACOM(J)=WHUMUS/WTSOIL
        TOTON=XNU(J,1)/CN(1)+XNU(J,2)/CN(2)+XNU(J,3)/CN(3)+
     &        XNU(J,4)/CN(4)+XNU(J,5)/CN(5)+XNU(J,7)/RPOP(1)/CN(7)+
     &        XNU(J,8)/RPOP(2)/CN(8)+XNU(J,9)/RPOP(3)/CN(9)
        FRACON(J)=TOTON*TL(J)*BD(J)/1.0D6/WTSOIL
  180     CONTINUE
          GOTO 200
        ENDIF
  190 CONTINUE
  200 RETURN
      END
C
      SUBROUTINE MAFERT(CC,THETA,AIRR,JDAY,NN,IQUE,NDXT2N,NITNHB,ANHYD,
     +    IYYY,subirr,cr,cs,ccl)
C
C======================================================================
C
C       PURPOSE:
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       ANHYD  L  ANHYDROUS-NH3 POOL, USED TO STORE NH4 DURING THE
C                 APPLICATION DELAY PERIOD [MG/L]
C       NITNHB     I  FLAG CONTROLLING NITRIFICATION SHUTDOWN DUE TO
C                 MANAGEMENT PRACTICE. (0=OFF, 1=REG NH3 APP,
C                 2=NH3 APP W/ N-SERVE)
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C
C       CALLED FROM:  MAIN PROGRAM
C
C       PROGRAMMER:   KEN ROJAS
C
C       VERSION:  2.2
C  fertigation with subsurface irritation is needed. Dec. 13, 2013, Liwang Ma
C======================================================================
C
      USE VARIABLE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,MXCHEM=15,MXPEST=3,MXAPP=200,MXNODT=3001)
C
      PARAMETER(NFMETH=4)
C
      COMMON /MFERT/ FNH4IN(MXAPP),FNO3IN(MXAPP),FUREIN(MXAPP),
     +    SPLTST(MXAPP),SPLTAP(MXAPP),IFOFF(MXAPP,2),IFMETH(MXAPP),
     +    IWFERT(MXAPP),NAPPF,IBMPAP(MXAPP),IBMPMD(MXAPP),
     +    IDSPLT(MXAPP),IFPL(MXAPP)
C
      COMMON /INFILC/ RAC(11),RRC(11),FERTIR(4),PESTIR(MXPEST),FMNRIR
C     Zhiming added Pmanuremix
      COMMON /TILLIN/ OBD(2),OFDP(2),TDEPTH(30),TILINT(30,2),
     +    TNDX(MXAPP),UDEPTH(MXAPP), PManureMix(MXAPP), IMP(MXAPP),  
     +    ITL(MXAPP), ITOFF(MXAPP,2),IWTILL(MXAPP),NUMTL, ITPL(MXAPP)
C
      COMMON /NINFO/ TSOTN0,TSOTN,TOTA,TOTL,TAMIN,TANIT,TADRT(2),
     +    TARAIN(2),TAIRR(3),TAFERT(3),TARES(3),TLIMM,TLDEN,TLVOL,
     +    TLRO(3),TLSEP(3),TLDRN(3),TLLAT(3),TAMANR,TMANH4,tamanr_bal
     +    ,TLN2O,TLNXO,TLNITN2O,TLNITNXO,TLGHGADS,TLGHGADSden
C
C
      INTEGER :: ddd1,mmm1,kk1
      
      COMMON /CINFO/TAMANC,TADRTC(2),TARESC(3),TOTCO2,TACUREA,YIELDC
     +             ,TAMANC_BAL,RCO2,TCAG,TCBG,TOTCH4,TOTCO2NI
C
      DIMENSION CC(MXNOD,MXCHEM),THETA(NN),IQUE(4,MXAPP),QFRT(20),
     +    NDXT2N(MXNODT),ANHYD(NN)
      LOGICAL FIRST9
      SAVE FIRST9
      DATA FIRST9 /.TRUE./
C
      IF(FIRST9) THEN
C
C       ..CREATE PARTITIONING ARRAY FOR NH3-N APPLICATIONS
        DO 10 I=1,10
          QFRT(I)=DBLE(I)/110.0D0
          QFRT(21-I)=QFRT(I)
   10   CONTINUE
        FIRST9=.FALSE.
      ENDIF
C
C     ..INITIALIZE VARIABLES FOR CHECKING SOIL DAMPNESS
      DO 20 I=1,4
        FERTIR(I)=0.0D0
   20 CONTINUE
      TAFERT(1)=0.0D0
      TAFERT(2)=0.0D0
      TAFERT(3)=0.0D0
      NITNHB=0
      TACUREA = 0.0D0
C
C     ..CONVERT FROM KG/HA ==> MG/L(WATER)
C     FIRST LAYER THICKNESS = 1 CM
      CONV=10.0D0/THETA(1)
      
       CALL CDATE(JDAY,ddd1,mmm1,IYYY)

       
       
       
C     ..CHECK QUE FOR FERTILIZER EVENT
      DO 50 IS=1,MXAPP
C
C       ..KICK OUT IF AT END OF GROUP
        IF(IQUE(1,IS).EQ.-1) GOTO 60
        DO 40 IP=1,NFMETH
          IF(IQUE(1,IS).EQ.IP.OR.(IQUE(1,IS).EQ.27.AND.IP.EQ.NFMETH))
     +        THEN
            IPRAC=IQUE(1,IS)
            IA=IQUE(2,IS)
            ITT=IQUE(4,IS)
C
C           ..REPORT WHAT I'M DOING
            PRINT *, "RZMAN CALLING MANOUT"
            CALL MANOUT(IPRAC,IA,JDAY,(JDAY-IQUE(3,IS)),IYYY)
            
              Countfert = Countfert + 1
              Fday(Countfert)= ddd1
              Fmon(Countfert)= mmm1
              Fyear(Countfert) = IYYY
              
              Fertp(Countfert)=Fertp1(IA)
              IF (IFMETH(IA).EQ.1) THEN
              Fertd(Countfert)= 0.0D0      !Fertd1(IA)     !NEED TO SELECT BASED ON FERTILIZER APPLICATION METHODS, LIWANG MA 
              Flefts(Countfert)= 100.0D0   !Flefts1(IA)   !NEED TO SELECT BASED ON FERTILIZER APPLICATION METHODS, LIWANG MA 
              ELSE IF (IFMETH(IA).EQ.2) THEN
              Fertd(Countfert)= 0.15D0       
              Flefts(Countfert)= 60.0D0    
              ELSE IF (IFMETH(IA).EQ.3) THEN
              Fertd(Countfert)= 0.15D0       
              Flefts(Countfert)= 0.0D0    
              ELSE IF (IFMETH(IA).EQ.4) THEN
              Fertd(Countfert)= 0.0D0       
              Flefts(Countfert)= 100.0D0  
              ELSE IF (IFMETH(IA).EQ.5) THEN
              Fertd(Countfert)= 0.0D0       
              Flefts(Countfert)= 100.0D0
              ELSE
              Fertd(Countfert)= 0.0D0       
              Flefts(Countfert)= 100.0D0
              ENDIF              
              IF (CS.EQ.MAX(CS,CR,CCL)) THEN
              Scov(Countfert)= 1               !(CR+CCL)*100.D0   !Scov1(IA)  LIWANG MA, IS IT CORRECT TO TAKE THE DOMINANT TYPE?
              ELSEIF (CR.EQ.MAX(CS,CR,CCL)) THEN
              Scov(Countfert)= 2   
              ELSEIF (CCL.EQ.MAX(CS,CR,CCL)) THEN
              Scov(Countfert)= 3   
              ENDIF
              
              
C           ..FIND THE METHOD OF APPLICATION FOR FERTILIZER
            IF(IPRAC.LE.3.OR.IPRAC.EQ.27) THEN
C
C             .. SAVE FOR MASS BALANCE CALCULATIONS
              TAFERT(1)=FNO3IN(IA)
              TAFERT(2)=FNH4IN(IA)
              TAFERT(3)=FUREIN(IA)
C              TACUREA = FUREIN(IA)*12.0D0/28.0D0  !COVERT FROM N TO C IN UREA (CO(NH2)2)
C
C             ..CHECK IF NH3 APPLICATOR
              IF(IPRAC.EQ.3.OR.IPRAC.EQ.27) THEN
C
C               .. ADD ANHYDROUS-NH3 TO LAYER WITH TRIANGLUAR DISTRIBUTE
                IDTH=INT(MAX(UDEPTH(ITT)-10.0D0,1.0D0))
                DO 30 J=1,20
                  IL=NDXT2N(J+IDTH)
                  ANHYD(IL)=ANHYD(IL)+FNH4IN(IA)*QFRT(J)
   30           CONTINUE
                IF(IPRAC.EQ.27) THEN
C
C                 ..ANHYDROUS WITH N-SERVE
                  NITNHB=2
                ELSE
C
C                 ..ANHYDROUS WITHOUT N-SERVE
                  NITNHB=1
                ENDIF
C
C               ..IN CASE OF UAN APPLICATION ADDIN CHEMICAL NOT SUCRESTURED
                FNO3=FNO3IN(IA)*CONV
                FUREA=FUREIN(IA)*CONV
                CC(1,9)=CC(1,9)+FNO3
                CC(1,12)=CC(1,12)+FUREA
              ELSE
C
C               ..CONVERT FROM KG/HA ==> MG/L
                FNO3=FNO3IN(IA)*CONV
                FNH4=FNH4IN(IA)*CONV
                FUREA=FUREIN(IA)*CONV
C
C               ..ADDIN NEW CONCENTRATIONS INTO THE EQUILIBRIUM SYSTEM
                CC(1,9)=CC(1,9)+FNO3
                CC(1,10)=CC(1,10)+FNH4
                CC(1,12)=CC(1,12)+FUREA
C
              ENDIF
            ELSE
              if (airr.gt.0.0d0) CONV=10.0D0/AIRR
C
C             ..CONVERT FROM KG/HA ==> MG/L (IRRIGATION WATER)
              FERTIR(1)=FNO3IN(IA)*CONV
              FERTIR(2)=FNH4IN(IA)*CONV
              FERTIR(3)=0.0D0
              FERTIR(4)=FUREIN(IA)*CONV
            ENDIF
          ENDIF
   40   CONTINUE
   50 CONTINUE
C
   60 RETURN
      END
C
      SUBROUTINE MAPEST(FT,FTR,CC,THETA,JDAY,COPLNT,CORES,BD,SLKS,H,
     +    IQUE,AIRR,IYYY)
C
C======================================================================
C
C       PURPOSE:
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DECRIPTION
C       --------  --- -----------
C       NPMETH     P  NUMBER OF POSSIBLE PESTICIDE APP METHODS
C       CONV   P  CONVERTS FROM KG/HA ==> UG/CM^2
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C
C       CALLED FROM:  MAIN PROGRAM
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:  2.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,MAXHOR=12,MXCHEM=15,MXPEST=3,MXAPP=200)
C
      PARAMETER(NPMETH=8,CONV=10.0D0)
C
C     PESTICIDE COMMON VARIABLES
C
      COMMON /RZPEST/ HALFF(MXPEST),HALFFP(MXPEST),HALFFB(MXPEST),
     +    HALFR(MXPEST),HALFRP(MXPEST),HALFRB(MXPEST),HALFSSA(MXPEST),
     +    HALFSSV(MXPEST),HALFSSP(MXPEST),HALFSSB(MXPEST),
     +    HALFSA(MXPEST),HALFSN(MXPEST),HALFSB(MXPEST),HALFBD(MXPEST),
     +    XKH(MXPEST),XKOC(MXPEST),XMW(MXPEST),THETAR(MXPEST),
     +    TMPR(MXPEST),EA(MXPEST),EN(MXPEST),WALKERB(MXPEST),
     +    VMAX(MXPEST),DYIELD(4,MXPEST),FREUND(MXPEST),XPKA(MXPEST),
     +    XPKB(MXPEST),REFPH(MXPEST),CATKOC(MXPEST),ANKOC(MXPEST),
     +    PCOEFF(MXPEST),PPOWER(MXPEST),RCOEFF(MXPEST),RPOWER(MXPEST),
     +    XKOW(MXPEST),PBIND(MXPEST),IPCODE(4,MXPEST),IPANC(MXPEST),
     +    IPDEP(MXPEST),IPACTV(MXPEST),NPEST
C
      COMMON /MPEST/ APP(MXAPP),PDR(MXAPP),IAPP(MXAPP),IPOFF(MXAPP,2),
     +    IWPEST(MXAPP),IPMETH(MXAPP),NAPP,IPPL(MXAPP)
      COMMON /INFILC/ RAC(11),RRC(11),FERTIR(4),PESTIR(MXPEST),FMNRIR
C
      COMMON /KNTCS/ CONCX2(MXNOD,MXPEST),EK2(MXNOD,MXPEST),
     +    RK2(MXPEST),RDIF(MXPEST),FEK2(MXPEST),IEK2(MXPEST)
C
C     Zhiming added Pmanuremix
      COMMON /TILLIN/ OBD(2),OFDP(2),TDEPTH(30),TILINT(30,2),
     +    TNDX(MXAPP),UDEPTH(MXAPP), PManureMix(MXAPP), IMP(MXAPP),  
     +    ITL(MXAPP), ITOFF(MXAPP,2),IWTILL(MXAPP),NUMTL, ITPL(MXAPP)
C
      COMMON /GEOMTR/XLONG,ASPECT,DELZ(MXNOD),ELEV,HORTHK(MAXHOR),SLOPE,
     +    AREA,TL(MXNOD),TLT(MXNOD),XLAT,ZN(MXNOD),NHOR,NN,NNT
C
      COMMON /PINFO/ TSOTP0(MXPEST),TAPAP(MXPEST),TLPRS(MXPEST),
     +    TLPCN(MXPEST),TLPSL(MXPEST),TLPRO(MXPEST),TLPLC(MXPEST),
     +    TDPPLNT(MXPEST),TLPDT(MXPEST),TLPDTD(MXPEST),
     +    TLPDTL(MXPEST),TLPDMA(MXPEST)
C
C
      DIMENSION CC(MXNOD,MXCHEM),THETA(NN),COPLNT(MXPEST),
     +    CORES(MXPEST),BD(NN),NDAY(MXAPP),SLKS(MXNOD,MXPEST,2),
     +    FIRST10(MXAPP),H(NN),PLWDEP(MXAPP),IQUE(4,MXAPP)
C
      LOGICAL FIRST10,CONT1,CONT2,F1,F2
C
      SAVE FIRST10,NDAY,PLWDEP,CONT1,CONT2,I1,I2
C
      DATA FIRST10 /MXAPP*.TRUE./,NDAY /MXAPP*0/
      DATA CONT1 /.FALSE./,CONT2 /.FALSE./,I1 /0/,I2 /0/
C
      F1=.TRUE.
      F2=.TRUE.
C
C     ..CHECK QUE FOR PESTICIDE EVENT
      DO 40 IS=1,MXAPP
C
C       ..KICK OUT IF AT END OF GROUP
c commented out by Liwang Ma, June 2, 2005
c        IF(IQUE(1,IS).EQ.-1) GOTO 50
        DO 30 IP=1,NPMETH
          IF(IQUE(1,IS).EQ.IP+4.OR.(CONT1.AND.F1).OR.(CONT2.AND.F2))
     +        THEN
            IF(CONT1) THEN
              IPRAC=5
              IA=I1
            ELSEIF(CONT2) THEN
              IPRAC=6
              IA=I2
            ELSE
              IPRAC=IP
              IA=IQUE(2,IS)
C
C             ..REPORT WHAT I'M DOING
              PRINT *, "RZMAN CALLING MANOUT"
              CALL MANOUT(IP+4,IA,JDAY,(JDAY-IQUE(3,IS)),IYYY)
            ENDIF
            ITT=IQUE(4,IS)
C
            IPACTV(IAPP(IA))=1
C
C           ..FIND THE INDEX OF PEST INTO CC
            K=12+IAPP(IA)
            KR=IAPP(IA)
            PESTIR(KR)=0.0D0
            PSOIL=APP(IA)*CONV
            TAPAP(KR)=APP(IA)
C
C           ..FIND THE METHOD OF APPLICATION FOR PEST
            IF(IPRAC.EQ.1.OR.IPRAC.EQ.2) THEN
C
C             ..SURFACE APPLICATION
              PLNT=(1.0D0-FT)*PSOIL
              RES=(1.0D0-FTR)*(PSOIL-PLNT)
              PSOIL=PSOIL-RES-PLNT
              COPLNT(KR)=COPLNT(KR)+PLNT
              CORES(KR)=CORES(KR)+RES
C LIWANG MA, 7-3-2006
              CONV2=1.0D0/((THETA(1)+BD(1)*
     +            FSLKS(SLKS(1,KR,1),CC(1,K),FREUND(KR),
     +                theta(1),bd(1))+BD(1)*
     +            EK2(1,KR))*TL(1))
              XCONC=PSOIL*CONV2
              CC(1,K)=CC(1,K)+XCONC
              CONCX2(1,KR)=CONCX2(1,KR)+XCONC*EK2(1,KR)
            ELSEIF(IPRAC.EQ.3) THEN
C
C             ..FOLIAGE APPLICATION
              IF(FT.LT.1.0D0) THEN
                COPLNT(KR)=COPLNT(KR)+PSOIL
              ELSE
                TAPAP(KR)=0.0D0
                PRINT *, "RZMAN CALLING MANOUT"
                CALL MANOUT(28,IA,JDAY,(JDAY-IQUE(3,IS)),IYYY)
              ENDIF
C
C           ... DON WAUCHOPE AND KEN ROJAS DECIDED THAT RESIDUE WOULD NOT
C           GET ANY OF THE CHEMICAL APPLIED, AND THE FOLIAR APP WOULD
C           BE APPLIED AT 100% EFFICIENCY (9/21/2001) REF @ 7.6.3 IN BOOK
C
C           IF (FTR.LT.1.0D0) THEN
C           CORES(KR) = CORES(KR) + PSOIL - COPLNT(KR)
C           ELSE
C           CORES(KR) = 0.0D0
C           END IF
C           IF (FT.GE.1.0D0.AND.FTR.GE.1.0D0) TAPAP(KR) = 0.0D0
            ELSEIF(IPRAC.EQ.4) THEN
C
C             ..IRRIGATION WATER APPLICATION
              PESTIR(KR)=PSOIL/AIRR
            ELSEIF(IPRAC.EQ.7) THEN
C
C             ..SOIL SURFACE ONLY APPLICATION
              CONV2=1.0D0/((THETA(1)+BD(1)*
     +            FSLKS(SLKS(1,KR,1),CC(1,K),FREUND(KR),
     +             theta(1),bd(1))+BD(1)*
     +            EK2(1,KR))*TL(1))
              XCONC=PSOIL*CONV2
              CC(1,K)=CC(1,K)+XCONC
              CONCX2(1,KR)=CONCX2(1,KR)+XCONC*EK2(1,KR)
            ELSEIF(IPRAC.EQ.8) THEN
C
C             ..INJECTED APPLICATION
C
C             .. DETERMINE LAYER PEST IS INJECTED INTO
              DO 10 I=1,NN
                IF(TLT(I).LE.UDEPTH(ITT)) IL=I
   10         CONTINUE
C
C             .. ADD PEST TO LAYER
C LIWANG MA, 7-3-2006
              CONV2=1.0D0/((THETA(IL)+BD(IL)*
     +            FSLKS(SLKS(IL,KR,1),CC(IL,K),FREUND(KR),
     +             theta(il),bd(il))+BD(IL)*
     +            EK2(IL,KR))*TL(IL))
              XCONC=PSOIL*CONV2
              CC(IL,K)=CC(IL,K)+XCONC
              CONCX2(IL,KR)=CONCX2(IL,KR)+XCONC*EK2(IL,KR)
            ENDIF
            IF(IPRAC.EQ.5.OR.CONT1) THEN
C
C             ..SLOW RELEASE SURFACE APPLICATION
              IF(FIRST10(IA)) THEN
                I1=IA
                FIRST10(IA)=.FALSE.
                CONT1=.TRUE.
                NDAY(IA)=MAX(2,INT(1.0D0/PDR(IA)))
              ENDIF
              NDAY(IA)=NDAY(IA)-1
              IF(NDAY(IA).LE.0) THEN
                FIRST10(IA)=.TRUE.
                CONT1=.FALSE.
              ENDIF
              WS=ABS(H(1)*9.81D-2)
C LIWNAG MA, 7-3-2006
              CONV2=1.0D0/((THETA(1)+BD(1)*
     +           FSLKS(SLKS(1,KR,1),CC(1,K),FREUND(KR),
     +            theta(1),bd(1))+BD(1)*
     +            EK2(1,KR))*TL(1))
              APPMT=PSOIL*PDR(IA)
              TAPAP(KR)=APPMT/CONV
              PSOIL=APPMT*(100.0D0-0.063D0*WS)*0.01D0
              XCONC=PSOIL*CONV2
              CC(1,K)=CC(1,K)+XCONC
              CONCX2(1,KR)=CONCX2(1,KR)+XCONC*EK2(1,KR)
              F1=.FALSE.
            ENDIF
            IF(IPRAC.EQ.6.OR.CONT2) THEN
C
C             ..SLOW RELEASE APPLICATION WITH TILLAGE INCORPORATION
              IF(FIRST10(IA)) THEN
                I2=IA
                FIRST10(IA)=.FALSE.
                CONT2=.TRUE.
C
C               ..FIND NUMBER OF DAYS TO RELEASE
                NDAY(IA)=MAX(2,INT(1.0D0/PDR(IA)))
C
C               ..FIND DEPTH OF INCORPORATION
                PLWDEP(IA)=HORTHK(1)
c Liwang Ma, June 2, 2005. ITT = 0 why???
c               IF(ITL(ITT).EQ.1) PLWDEP(IA)=HORTHK(2)
              ENDIF
              NDAY(IA)=NDAY(IA)-1
              IF(NDAY(IA).LE.0) THEN
                FIRST10(IA)=.TRUE.
                CONT2=.FALSE.
              ENDIF
              CONV3=1.0D0/PLWDEP(IA)
c  changed by Liwang Ma, June 2, 2005
c this section still need further work done
                  APPMT=PSOIL*PDR(IA)
              DO 20 I=1,NN
C
C               ..CONVERSION FROM KG/HA ==> MG/L(WATER)
                IF(TLT(I).LE.PLWDEP(IA)) THEN
C LIWANG MA, 7-3-2006
                  CONV2=1.0D0/((THETA(I)+BD(I)*
     +              FSLKS(SLKS(I,KR,1),CC(I,K),FREUND(KR),
     +              theta(i),bd(i))+BD(I)*
     +                EK2(I,KR))*TL(I))
                  WS=ABS(H(I)*9.81D-2)
cLiwang                  APPMT=PSOIL*PDR(IA)
                  TAPAP(KR)=APPMT/CONV
cLiwang                  PSOIL=APPMT*(100.0D0-0.063D0*WS)*0.01D0*CONV2*CONV3*
cLiwang     +                TL(I)
cLiwang                  CC(I,K)=CC(I,K)+PSOIL
cLiwang                  CONCX2(I,KR)=CONCX2(I,KR)+PSOIL*EK2(I,KR)
                  PSOIL=APPMT*(100.0D0-0.063D0*WS)*0.01D0*CONV3*TL(I)
                  CC(I,K)=CC(I,K)+PSOIL*conv2
                  CONCX2(I,KR)=CONCX2(I,KR)+PSOIL*conv2*EK2(I,KR)
                ENDIF
   20         CONTINUE
              F2=.FALSE.
            ENDIF
          ENDIF
   30   CONTINUE
   40 CONTINUE
C
   50 RETURN
      END
C
      SUBROUTINE MANURE(CC,THETA,AIRR,JDAY,NN,TL,TLT,IQUE,RPOOL,RM,RCN,
     +    CN,IYYY,ANHYD,xnu,NDXN2H,subirr,cr,cs,ccl)
C
C======================================================================
C
C       PURPOSE:
C  Liwang Ma made changes to the injection option on July 28, 2006.
c         Rpool and Xnu were updated the same time. otherwise it will not
c         working.
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       CKG        P  CONVERTS FROM KG/HA >> G/CM^2
C       CNMANR     I  C:N RATIO OF NEW MANURE WASTE
C       CNBED  I  C:N RATIO OF NEW MANURE BEDDING
C       FCMANR     I  FRACTION OF CARBON OF NEW MANURE WASTE
C       TCAMOW     L  TOTAL CARBON OF NEW MANURE WASTE
C       TNAMOW     L  TOTAL NITROGEN OF NEW MANURE WASTE
C       TCAMBED    L  TOTAL CARBON OF NEW MANURE BEDDING
C       TNAMBED    L  TOTAL NITROGEN OF NEW MANURE BEDDING
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C
C       CALLED FROM:  MAIN PROGRAM
C
C       PROGRAMMER:   KEN ROJAS
C
C       VERSION:  2.0
C
C======================================================================
C
      USE VARIABLE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,MXCHEM=15,MXPEST=3,MXAPP=200)
      PARAMETER(NFMETH=4,CKG=1.0D-5,FCR=0.4D0,maxhor=12)
C
      COMMON /MMNUR/ AMNH4(MXAPP),AMOW(MXAPP),AMBED(MXAPP),
     +    CNBED(MXAPP),AMCN(15),CNMANR(MXAPP),FCMANR(MXAPP),
     +    IWMANR(MXAPP),IMOFF(MXAPP,2),IMMETH(MXAPP),IMTYPE(MXAPP),
     +    NAPPM,IMPL(MXAPP),IMANDECOMP(MXAPP)
C
      COMMON /INFILC/ RAC(11),RRC(11),FERTIR(4),PESTIR(MXPEST),FMNRIR
C
C     Zhiming added Pmanuremix
      COMMON /TILLIN/ OBD(2),OFDP(2),TDEPTH(30),TILINT(30,2),
     +    TNDX(MXAPP),UDEPTH(MXAPP), PManureMix(MXAPP), IMP(MXAPP),  
     +    ITL(MXAPP), ITOFF(MXAPP,2),IWTILL(MXAPP),NUMTL, ITPL(MXAPP)
C
      COMMON /NINFO/ TSOTN0,TSOTN,TOTA,TOTL,TAMIN,TANIT,TADRT(2),
     +    TARAIN(2),TAIRR(3),TAFERT(3),TARES(3),TLIMM,TLDEN,TLVOL,
     +    TLRO(3),TLSEP(3),TLDRN(3),TLLAT(3),TAMANR,TMANH4,tamanr_bal
     +    ,TLN2O,TLNXO,TLNITN2O,TLNITNXO,TLGHGADS,TLGHGADSden
      COMMON /CINFO/TAMANC,TADRTC(2),TARESC(3),TOTCO2,TACUREA,YIELDC
     +             ,TAMANC_BAL,RCO2,TCAG,TCBG,TOTCH4,TOTCO2NI
     
      INTEGER :: ddd1,mmm1,kk1
C Liwang Ma, July 28, 2006
      COMMON /SOIL/ SOILPP(8,MAXHOR),pori(mxnod)
C
      DIMENSION CC(MXNOD,MXCHEM),THETA(NN),TL(NN),TLT(NN),
     +    IQUE(4,MXAPP),RPOOL(MXNOD,2),CN(9),ANHYD(MXNOD),
     +    XNU(MXNOD,25),NDXN2H(mxnod)
C
C     ..INITIALIZE VARIABLES FOR CHECKING SOIL DAMPNESS
      FMNRIR=0.0D0
      TAMANR=0.0D0
	TMANH4=0.0D0
      TAMANC=0.0D0
      TAMANR_bal=0.0D0
      TAMANC_bal=0.0D0
      
      CALL CDATE(JDAY,ddd1,mmm1,IYYY)
      

C
C     ..CHECK QUE FOR MANURE EVENT
      DO 30 IS=1,MXAPP
C
C       ..KICK OUT IF AT END OF GROUP
        IF(IQUE(1,IS).EQ.-1) GOTO 40
        DO 20 IM=1,NFMETH
          IF(IQUE(1,IS).EQ.IM+12) THEN
            IPRAC=IM
            IA=IQUE(2,IS)
C           IK = IMTYPE(IA)
            ITT=IQUE(4,IS)
C
C           .. REPORT WHAT I'M DOING
            PRINT *, "RZMAN CALLING MANOUT"
            CALL MANOUT(IM+12,IA,JDAY,(JDAY-IQUE(3,IS)),IYYY)
              Countman = Countman + 1
              Mday1(Countman)= ddd1
              Mmon(Countman)= mmm1
              Myear1(Countman) = IYYY
              
              AppdayManmass(Countman) = AppdayManmass1(IA)             
              Mantpper(Countman)= Mantpper1(IA)
              Manweipper(Countman)= Manweipper1(IA)
              Manweopper(Countman)=Manweopper1(IA)
              Manwcper(Countman) = 100.0D0 - Manwcper1(IA)
              Manfc(Countman)= 100.0D0           !Manfc1(IA), Is it integer?  No it is % :Debasis
              IF (IMMETH(IA).EQ.1) THEN
              Mand(Countman)=0.0D0    !Mand1(IA)       !NEED TO SELECT BASED ON FERTILIZER APPLICATION METHODS, LIWANG MA
              Mlefts(Countman)= 100.D0  !Mlefts1(IA)  !NEED TO SELECT BASED ON FERTILIZER APPLICATION METHODS, LIWANG MA 
              ELSE IF (IMMETH(IA).EQ.2) THEN
              Mand(Countman)=0.15D0    
              Mlefts(Countman)= 60.D0   
              ELSE IF (IMMETH(IA).EQ.3) THEN
              Mand(Countman)=0.15D0    
              Mlefts(Countman)= 0.D0   
              ELSE IF (IMMETH(IA).EQ.4) THEN
              Mand(Countman)=0.0D0    
              Mlefts(Countman)= 100.D0  
              ENDIF              
              
              Mtype(Countman) = Mtype1(IA)   !NEED TO SELECT BASED ON FERTILIZER APPLICATION METHODS, LIWANG MA 
                 
            
             
C
C           .. CONVERT FROM BIOMASS UNITS TO CARBON UNITS.
            TCAMOW=AMOW(IA)*FCMANR(IA)
            IF(TCAMOW.GT.0.0D0.AND.CNMANR(IA).GT.0.0D0) THEN
              TNAMOW=TCAMOW/CNMANR(IA)
            ELSE
              TNAMOW=0.0D0
            ENDIF
C
            TCAMBED=AMBED(IA)*FCR
            IF(TCAMBED.GT.0.0D0.AND.CNBED(IA).GT.0.0D0) THEN
              TNAMBED=TCAMBED/CNBED(IA)
            ELSE
              TNAMBED=0.0D0
            ENDIF
C
C           ..FIND THE METHOD OF APPLICATION FOR MANURE
            IF(IPRAC.EQ.3) THEN
C             ..NH3 INJECTOR
C
C             .. SAVE FOR MASS BALANCE CALCULATIONS
c Liwang Ma, July 28, 2006 for mass balance purpose
              TAMANR=AMNH4(IA)+TNAMOW+TNAMBED
              TAMANR_bal=TNAMOW+TNAMBED
              TAMANC=TCAMOW+TCAMBED
              TAMANC_bal=TCAMOW+TCAMBED
c              TAMANR=AMNH4(IA)
              TMANH4=AMNH4(IA)
C
C             ..GET PARTITIONING VALUES
              SOW=(1.0D0/CNMANR(IA)-1.0D0/CN(1))/(1.0D0/CN(2)-1.0D0/
     +            CN(1))
              SOBED=(1.0D0/CNBED(IA)-1.0D0/CN(1))/(1.0D0/CN(2)-1.0D0/
     +            CN(1))
              SOW=MAX(SOW,0.0D0)
              SOW=MIN(SOW,1.0D0)
              SOBED=MAX(SOBED,0.0D0)
              SOBED=MIN(SOBED,1.0D0)
C
C             .. DETERMINE LAYER MANURE IS INJECTED INTO
              DO 10 I=1,NN
                IF(TLT(I).LE.UDEPTH(ITT)) IL=I
   10         CONTINUE
C
C             .. ADD MANURE TO LAYER
C  Liwang Ma, 9-9-2005 to put NH4 into anhydrous NH4 pools
              CC(IL,10)=CC(IL,10)+AMNH4(IA)*10.0D0/(TL(IL)*THETA(IL))
c              ANHYD(IL)=ANHYD(IL)+AMNH4(IA)
C
C             ..UPDATE RESIDUE POOLS WITHIN THE LAYER
              RPOOL(IL,1)=RPOOL(IL,1)+TCAMOW*(1.0D0-SOW)*CKG
              RPOOL(IL,1)=RPOOL(IL,1)+TCAMBED*(1.0D0-SOBED)*CKG
              RPOOL(IL,2)=RPOOL(IL,2)+TCAMOW*SOW*CKG
              RPOOL(IL,2)=RPOOL(IL,2)+TCAMBED*SOBED*CKG
c Liwang Ma, July 28, 2006
                IH=NDXN2H(IL)
                FACTN=1.0D6/(TL(IL)*SOILPP(3,IH))
                XNU(IL,1)=RPOOL(IL,1)*FACTN
                XNU(IL,2)=RPOOL(IL,2)*FACTN
            ELSE
C
C             .. SAVE FOR MASS BALANCE CALCULATIONS
              TAMANR_bal=TNAMOW+TNAMBED
              TAMANC_bal=TCAMOW+TCAMBED
              TAMANR=AMNH4(IA)
              TAMANC=0.0d0
              TMANH4=AMNH4(IA)
              IF(IPRAC.EQ.4) THEN
C               .. IRRIGATION SPECIAL
                CONV=10.0D0/AIRR
C               .. SAVE FOR MASS BALANCE CALCULATIONS
                TAMANR_bal=TNAMOW+TNAMBED
                TAMANC_bal=TCAMOW+TCAMBED
                TAMANR=AMNH4(IA)
                TAMANC=0.0d0
                TMANH4=AMNH4(IA)
C               ..CONVERT FROM KG/HA ==> MG/L (IRRIGATION WATER)
                FMNRIR=AMNH4(IA)*CONV
              ELSE
C
C               ..CONVERT FROM KG/HA ==> MG/L(WATER)
                CONV=10.0D0/THETA(1)
C               ..CONVERT FROM KG/HA ==> MG/L
                TAMNH4=AMNH4(IA)*CONV
C               ..ADDIN NEW CONCENTRATIONS INTO THE EQUILIBRIUM SYSTEM
                CC(1,10)=CC(1,10)+TAMNH4
              ENDIF
C
C             ..UPDATE SURFACE RESIDUE POOLS
              CRM=RM*FCR
              RCN=(CRM+TCAMOW)/((CRM/RCN)+TNAMOW)
C             ..STANDARDIZED THE MASS FOR DIFFERENT MANURE FRACT CARBON WITH
C             RESIDUE STANDARD FCR=0.4
              RM=RM+TCAMOW/FCR
C
C             ADD BEDDING MATERIAL INTO SURFACE RESIDUE (ASSUME FCR(BEDDING)=0.4)
              CRM=RM*FCR
              RCN=(CRM+TCAMBED)/((CRM/RCN)+TNAMBED)
              RM=RM+AMBED(IA)
            ENDIF
            GOTO 40
          ENDIF
   20   CONTINUE
   30 CONTINUE
C
   40 RETURN
      END
C
      SUBROUTINE MANOUT(IQ,IND,JDAY,DELAY,IYYY)
C
C======================================================================
C
C       PURPOSE: THIS SUBROUTINE OUTPUTS MANAGEMENT EVENTS TO THE FILE
C             MANAGE.OUT
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DECRIPTION
C       --------  --- -----------
C       NEVNTS     P  NUMBER OF POSSIBLE EVENTS
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C
C       CALLED FROM:
C
C       PROGRAMMER:   KEN ROJAS
C
C       VERSION:  2.0
C
C======================================================================
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MXAPP=200,MXPEST=3,MAXHOR=12,MXSPEC=10)
      PARAMETER(NEVNTS=28,MXT1=29,MXTNM=MXAPP-MXT1-1)
C
      COMMON /MFERT/ FNH4IN(MXAPP),FNO3IN(MXAPP),FUREIN(MXAPP),
     +    SPLTST(MXAPP),SPLTAP(MXAPP),IFOFF(MXAPP,2),IFMETH(MXAPP),
     +    IWFERT(MXAPP),NAPPF,IBMPAP(MXAPP),IBMPMD(MXAPP),
     +    IDSPLT(MXAPP),IFPL(MXAPP)
C
      COMMON /MPEST/ APP(MXAPP),PDR(MXAPP),IAPP(MXAPP),IPOFF(MXAPP,2),
     +    IWPEST(MXAPP),IPMETH(MXAPP),NAPP,IPPL(MXAPP)
C
      CHARACTER SOLTYP*10,NMPEST*30,PLNAME*30
      COMMON /NAMES/ NMPEST(MXPEST),SOLTYP(MAXHOR),PLNAME(MXSPEC)
C
      COMMON /MMNUR/ AMNH4(MXAPP),AMOW(MXAPP),AMBED(MXAPP),
     +    CNBED(MXAPP),AMCN(15),CNMANR(MXAPP),FCMANR(MXAPP),
     +    IWMANR(MXAPP),IMOFF(MXAPP,2),IMMETH(MXAPP),IMTYPE(MXAPP),
     +    NAPPM,IMPL(MXAPP),IMANDECOMP(MXAPP)
C
      COMMON /PLNTG/ PLDEN(MXAPP),PLTDAY(MXAPP,2),ROWSP(MXAPP),
     +    PLDEPTH(MXAPP),sdwtplr(mxapp),sdager(mxapp),sprlapr(mxapp),
     +    PLNTSW(MXAPP),PLNTWD(MXAPP),LAYNDX(MXAPP),NPGS,NPR(MXAPP),
     +    NGPL(MXAPP),iptype(mxapp),iemrg(mxapp)
C
      CHARACTER EVENT(NEVNTS)*60
      CHARACTER TILLNM(MXAPP)*30
      INTEGER DELAY
      SAVE KYYY,KDAY,EVENT,TILLNM
C
      DATA(TILLNM(I),I=1,MXT1) /'MOLDBOARD PLOW','CHISEL PLOW-STRAIGHT',
     +    'CHISEL PLOW-TWISTED','FIELD CULTIVATOR','TANDEM DISK',
     +    'OFFSET DISK','ONE-WAY DISK','PARAPLOW ','SPIKE TOOTH HARROW',
     +    'SPRING TOOTH HARROW','ROTARY HOE','BEDDER RIDGE,',
     +    'V-BLADE SWEEP','SUBSOILER','ROTOTILLER','ROLLER PACKAGE',
     +    'ROW PLANTER W/ SMOOTH COULTER',
     +    'ROW PLANTER W/ FLUTED COULTER','ROW PLANTER W/ SWEEPS',
     +    'LISTER PLANTER','DRILL','DRILL W/CHAIN DRAG',
     +    'ROW CULTIVATOR W/SWEEPS','ROW CULTIVATOR W/SPIDER WHEELS',
     +    'ROD WEEDER','ROLLING CULTIVATOR','NH3 APPLICATOR',
     +    'RIDGE-TILL CULTIVATOR','RIDGE-TILL PLANTER'/
      DATA(TILLNM(I),I=MXT1+1,MXAPP-1) /MXTNM*' '/
      DATA TILLNM(MXAPP) /'NH3 APPLICATOR'/
C
      DATA KDAY /0/, KYYY/0/
      DATA(EVENT(I),I=1,19) /'FERTILIZING BROADCAST (LEAVE ON SURFACE)',
     +    'FERTILIZING BROADCAST (INCORPORATE WITH TILLAGE)',
     +    'FERTILIZER NH3 INJECTOR APPLICATION',
     +    'FERTILIZER APPLICATION THROUGH IRRIGATION',
     +    'PESTICIDE APPLICATION-BROADCAST (LEAVE ON SURFACE)',
     +    'PESTICIDE APPLICATION-BROADCAST (INCORPORATE WITH TILLAGE)',
     +    'PESTICIDE APPLICATION ON FOLIAGE ONLY',
     +    'PESTICIDE APPLICATION THROUGH IRRIGATION',
     +    'MICROENCAPSULATED PESTICIDE APPLICATION (ON SURFACE)',
     +    'MICROENCAPSULATED PESTICIDE APPLICATION (INCORPORATED)',
     +    'PESTICIDE APPLICATION ON SOIL SURFACE ONLY',
     +    'PESTICIDE APPLICATION INJECTED',
     +    'MANURE APPLICATION BROADCAST (LEAVE ON SURFACE)',
     +    'MANURE APPLICATION BROADCAST (INCORPORATE WITH TILLAGE)',
     +    'MANURE APPLICATION INJECTOR APPLICATION',
     +    'MANURE APPLICATION THROUGH IRRIGATION',
     +    'SPECIFIC DATE TILLAGE EVENT','PLANTING OPERATION',
     +    'IRRIGATION EVENT'/
      DATA(EVENT(I),I=20,NEVNTS) /'INCORPORATION TILLAGE EVENT',
     +    'FERTILIZER APP CANCELLED W/O IRRIGATION SPECIFIED',
     +    'PESTICIDE APP CANCELLED W/O IRRIGATION SPECIFIED',
     +    'MANURE APP CANCELLED W/O IRRIGATION SPECIFIED',
     +    'FERTILIZER APP CANCELLED W/O INJECTOR SPECIFIED',
     +    'PESTICIDE APP CANCELLED W/O INJECTOR SPECIFIED',
     +    'MANURE APP CANCELLED W/O INJECTOR SPECIFIED',
     +    'FERTILIZER NH3 INJECTOR WITH (N-SERVE)',
     +    'PESTICIDE APP CANCELLED NO FOLIAGE PRESENT'/
C
      IF((KDAY.NE.JDAY).OR.(KYYY.NE.IYYY)) THEN
        CALL CDATE(JDAY,ID,IM,IYYY)
        WRITE(70,1000)
        WRITE(70,1600)
        IF(IYYY.LT.1900) THEN
          IYYYY=IYYY+1900
          WRITE(70,1100) ID,IM,IYYYY,JDAY
        ELSE
          WRITE(70,1100) ID,IM,IYYY,JDAY
        ENDIF
        KDAY=JDAY
        KYYY=IYYY
      ENDIF
      IF(IQ.LT.0) THEN
C       ..CANCEL IRRIGATION EVENT -- LOOK IN PHYSCL ROUTINE FOR CALL
        WRITE(70,1400) JDAY
      ELSE
        WRITE(70,1200) EVENT(IQ)
        IF(DELAY.NE.0.0D0) WRITE(70,1300) DELAY
        IF(IQ.LE.4.OR.IQ.EQ.27) THEN
          WRITE(70,1700) FNH4IN(IND)
          WRITE(70,1800) FNO3IN(IND)
          WRITE(70,1900) FUREIN(IND)
        ELSEIF(IQ.LE.12) THEN
          WRITE(70,2000) NMPEST(IAPP(IND)),APP(IND)
        ELSEIF(IQ.LE.16) THEN
          WRITE(70,1700) AMNH4(IND)
          WRITE(70,2100) AMOW(IND)
          WRITE(70,2200) AMBED(IND)
        ELSEIF(IQ.EQ.17.OR.IQ.EQ.20) THEN
          WRITE(70,2300) TILLNM(IND)
        ELSEIF(IQ.EQ.18) THEN
          IIPL=NPR(IND)
          WRITE(70,2400) PLNAME(IIPL)
          WRITE(70,2500) PLDEN(IND)*1000.0D0
        ENDIF
      ENDIF
      WRITE(70,1600)
C
      RETURN
 1000 FORMAT(70('*'))
 
 
 1100 FORMAT(/4('-'),I3,'/',I2,'/',I4,' -----',I6,'  ----'/)
 1200 FORMAT('EVENT ==> ',A60)
 1300 FORMAT('DELAY FOR THIS EVENT WAS ',I3,' DAY(S)')
 1400 FORMAT('***IRRIGATION APPLICATION FOR DAY ',I3,' CANCELLED',
     +    ' DUE TO RAINFALL***')
c 1500 FORMAT('NO DELAY FOR IRRIGATION EVENTS')
 1600 FORMAT(' ')
 1700 FORMAT(T11,'AMOUNT OF NH4 [KG/HA] ',T40,G12.5)
 1800 FORMAT(T11,'AMOUNT OF NO3 [KG/HA] ',T40,G12.5)
 1900 FORMAT(T11,'AMOUNT OF UREA [KG/HA] ',T40,G12.5)
 2000 FORMAT(T11,'AMOUNT OF ',A30,T50,G10.5,'[KG/HA]')
 2100 FORMAT(T11,'AMOUNT OF ORGANIC WASTE [KG/HA] ',T40,G12.5)
 2200 FORMAT(T11,'AMOUNT OF BEDDING [KG/HA] ',T40,G12.5)
 2300 FORMAT(T11,'WITH IMPLIMENT: ',A30)
 2400 FORMAT(T13,'CROP PLANTED: ',A30)
 2500 FORMAT(T13,'PLANTING DENSITY: ',F12.1)
      END
C
      SUBROUTINE MAPLNT(RFDD,JDAY,IYYY,TMAX,TMIN,RTS,TPP,W,HEIGHT,LAI,T,
     +    SOILPP,NDXN2H,RDF,JGS,NSC,PLTSLV,RPOOL,UPNIT,EWP,SDEAD,TLPLNT,
     +    XNIT,BASE,FIXN,IPL,IPM,JGROW,IMAGIC,ODMDN,CN,RM,RESAGE,RCN,
     +    SDCN,CORES,COPLNT,NPEST,CHLRFL,TDAY,IQUE,INXPL,IRTYPE,OMSEA,
     +    TLAI,IPR,SDEAD_HEIGHT,AIRR,CC,RZPAR,JBDAY,IYB,CO2R,ndxt2n,
     +    iswpar,tmday,iweather,ssurft,IPDEPTH,TUP,DAYL)
C
C======================================================================
C
C       PURPOSE: THIS ROUTINE CONTROLS WHICH PLANT MODEL IS CALLED TO
C             GROW THE CROP.  USES THE CROP NAME READ IN TO DETERMINE
C             WHICH MODEL TO CALL.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DECRIPTION
C       --------  --- -----------
C       AHLDG  I
C       ALA        I  ALUMINUM SATURATION BELOW WHICH ROOT GROWTH IS
C                 UNAFFECTED (%)
C       ALNAVL     I
C       ALNGER     I
C       ALNLW  I
C       ALPHA  I  TEMPORARY STORAGE OF COEFF
C       ALX        I  ALUMINUM SATURATION ABOVE WHICH ROOT GROWTH IS
C                 NEGLIGIBLE (%)
C       BASE  I/O BASAL AREA OF A PLANT [M^2]
C       BETA   I  TEMPORARY STORAGE OF COEFF
C       BGSEED     I
C       CAA        I  CALCIUM SATURATION BELOW WHICH ROOT GROWTH IS
C                 REDUCED (%)
C       CANK   I
C       CAX        I  CALCIUM SATURATION BELOW WHICH ROOT GROWTH IS
C                 NEGLIGIBLE (CMOL/KG)
C       CLSIZE     I
C       CN         L  9-LONG VECTOR OF OM & BM C:N RATIOS (IN SAME ORDER
C                 AS IN C VECTOR)
C       CNST   I
C       CNUP1  I
C       CNUP2  I
C       CONVLA     I
C       CVLBIO     I
C       DAY        L
C       DEADLF     L
C       DEVRAT     I
C       DMDNIT    I/O NITROGEN DEMAND FROM PLANT (G-N/PLANT)
C       DROTOL     I
C       DTSEED     I
C       EFFN   I
C       EWP        I
C       FIRST  L  TRUE IF FIRST TIME THROUGH ROUTINE
C       FIXN   I
C       GDDFLG     I
C       GITEMP     I
C       GIWAT  I
C       GMN        I  FRACTION OF NORMAL ROOT GROWTH WHEN PORE SPACE IS
C                 SATURATED [0-1]
C       GRMRAT     I
C       GS         I  GROWTH STAGE [0..1]
C       GSGDD  I
C       GSR        I  GROWTH STAGE WHEN ROOT DEPTH REACHES MAXIMUM [0..1]
C       H      I  NODAL SOIL WATER PRESSURE HEADS [CM]
C       HEIGHT    I/O
C       HFMAX  I
C       I      L  INDEX VARIABLE
C       IEMERG     L
C       IGPL   L
C       IMAGIC     I  INDICATOR OF DEBUG OUTPUT
C       INITJD     L
C       IPL       I/O PLANT CURRENTLY MODELING [1..MXSPEC]
C       IPLTYP     I
C       IPM       I/O
C       ISOW   L
C       IYYY   I  --YEAR
C       J      L  INDEX VARIABLE
C       JDAY   I  JULIAN DAY    [1..366]
C       JGROW I/O --NUMBER OF DAYS SINCE PLANTING.
C                 AFTER HARVEST JGROW= -999.
C       JGS       I/O [1..2];1:JULIAN DAY PLANT EMERGES
C                 2:JULIAN DAY PLANT IS RIPE
C       JN         L  INDEX VARIABLE
C       K      L  VON KARMAN CONSTANT ( =.41)
C       LAI       I/O LEAF AREA INDEX (LIVE LEAVES ONLY)
C       TLAI  I/O LEAF AREA INDEX (LIVE AND DEAD LEAVES)
C       LAYNDX     I
C       LL         L  LOWER LIMIT OF PLANT-EXTRACTABLE WATER [M/M]
C       LWM        I  RATIO OF ROOT LENGTH TO WEIGHT IN PLOW LAYER
C                 AT MATURITY [M/G)
C       LWS        I  NORMAL RATIO OF ROOT LENGTH TO WEIGHT IN
C                 SEEDLING [M/G)
C       MATJD  L
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODT     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       MXSPEC     P
C       NDXN2H     I  INDEX FOR NUMERICAL LAYERS TO HORIZONS,
C                   IE. WHICH HORIZON IS NUMERICAL LAYER IN.
C       NGPL   I
C       NH4        L
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       NPGS   I
C       NPL        I
C       NPR        I
C       NSC        I  CURRENT OUTPUT SCENARIO NUMBER
C       PCLDG  I
C       PDTRAN     L
C       PGDVRT     I
C       PGNTGT     I
C       PLALFA     I
C       PLANTS     L
C       PLBIO  I
C       PLDEN  I
C       PLDIAM     I
C       PLHGHT     I
C       PLTDAY     I
C       PLTMNT     I
C       PLTMXT     I
C       PLTOPT     I
C       PLTSLV    I/O NUMBER OF LIVE PLANTS [#/HA]
C       PLWTS  L
C       PMAX   I
C       PMNNIT     I
C       PNIT   L
C       PRB        I
C       PRNLW  I
C       PSTATE     I  1-ALLOCABLE CARBON 
C                     2-LEAF CARBON
C                     3-STEM CARBON
C                     4-PROPAGULE CARBON
C                     5-ROOT CARBON
C                     6-SEED CARBON
C                     7-STANDING DEAD
C                     8-LITTER CARBON
C                     9-DEAD ROOT
C       PTLAMX     I
C       PWRTS I/O NUMBER OF PLANT WITH ACTIVE ROOTS [#/HA]
C       R20        I
C       RATLS  I
C       RATRSN     I
C       RATRSX     I
C       RDF       I/O FRACTIONAL DISTRIBUTION OF ROOTS BY LAYER [0..1]
C       RDR        I  MAXIMUM, NORMAL ROOT DEATH RATE
C                 (PROP. OF ROOT GROWTH)
C       RDX        I  NORMAL MAXIMUM ROOT SYSTEM DEPTH [M]
C       RFDD   I  TOTAL RAINFALL DEPTH [CM]
C       RLV        L  ROOT LENGTH DENSITY [CM/CM3]
C       ROWSP  I
C       RPOOL  I  RESIDUE POOL, BOTH FAST AND SLOW DECOMPOSERS
C                 [0]--SURFACE RESIDUE,  [1] SLOW DECOMP RESIDUE
C                 [1..NN]--MIXED SOIL RES  [2] FAST DECOMP RESIDUE
C                 UNITS = [G-CARBON / CM^2-SOIL]
C       RQ10   I
C       RTNLW  I
C       RTS        I  TOTAL S-W RADIATION INCOMING [MJ/M^2/DAY]
C       SDAMAX     I
C       SDDVRT     I
C       SDEAD  I  STANDING DEAD MATERIAL [KG/HA]
C       SDCN   I  STANDING DEAD C:N RATIO
C       SDSVRT     I
C       SDTMGM     I
C       SDWMAX     I
C       SFREEZ     I
C       SLA1   I
C       SLA2   I
C       SLA3   I
C       SLA4   I
C       SOILHP     I  MODIFIED BROOKS-COREY PARAMETERS
C                   (1):   HB    - BUBBLING PRESSURE O(H) [CM]
C                   (2):   LAMDA - PORE SIZE DISTRIBUTION INDEX
C                   (3):   EPS   - EXPONENT FOR K(H) CURVE
C                   (4):   KSAT  - SAT HYDRAULIC CONDUCT [CM/HR]
C                   (5):   WR    - RESIDUAL WATER CONTENT
C                   (6):   WS    - SATURATION WATER CONTENT
C                   (7):   WFC   - FIELD CAPACITY (1/3 BAR) WC
C                   (8):   WFC   - FIELD CAPACITY (1/10 BAR) WC
C                   (9):   WWP   - WILTING POINT (15 BAR) WC
C                   (10):  HB    - BUBBLING PRESSURE K(H) CURVE [CM]
C                   (11):  C2    - SECOND INTRCEPT ON K(H) CURVE
C                   (12):  N1    - FIRST EXPONENT FOR K(H) CURVE
C                   (13):  A1    - CONSTANT FOR O(H) CURVE
C       SOILPP     I
C       STEND  I
C       STNAVL     I
C       STNGER     I
C       STNLW  I
C       T      I  SOIL TEMPERATURE IN NUMERICAL CONFIGURATION [C]
C       TBS        I  BASE TEMPERATURE FOR ROOT GROWTH (C)
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       TLPLNT    I/O TOTAL DAILY NITROGEN UPTAKE [KG-N/HA]
C       TLXPLT     L  TOTAL DAILY LUXURIOUS NITROGEN UPTAKE [KG-N/HA]
C       TTPLNT     L  ACCUMULATED TOTAL PLANT UPTAKE [KG-N/HA]
C       TMPLNT     L  ACCUMULATED PASSIVE PLANT UPTAKE [KG-N/HA]
C       TXPLNT     L  ACCUMULATED LUXURIOUS PLANT UPTAKE [KG-N/HA]
C       TLT        I  DEPTH TO BOTTOM OF NUMERICAL LAYERS [CM]
C       TMAX   I  MAXIMUM AIR TEMPERATURE [C]
C       TMIN   I  MINIMUM AIR TEMPERATURE [C]
C       TNITUP     I  TOTAL NITROGEN UPTAKE FOR WHOLE PROFILE [G/PLANT]
C       TOP        I  OPTIMUM TEMPERATURE FOR ROOT GROWTH (C)
C       TOT4WT     I
C       TPP       I/O
C       TPUP   I
C       UL         L  DRAINED UPPER LIMIT OF SOIL WATER FOR THE
C                 FINE EARTH FRACTION [0..1]
C       UPNIT  I  UPTAKE OF NITROGEN FOR EACH LAYER [G/PLANT/LAYER]
C       VRNLZ  I
C       W      I  HOURLY SOLAR CONSTANT = 5.016 [MJ/M^2/HR]
C       WCG        I  WEIGHTING COEFFICIENT - GEOTROPISM
C       WCP        I  WEIGHTING COEFFICIENT FOR PLASTICITY
C       WDLDG  I
C       XLAT   I  LATITUDE OF FIELD
C       XNIT   I
C       XPLANT     L
C       YIELD  L
C       :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C       ::::   THE FOLLOWING ITEMS ARE NEEDED BY THE ROOT ZONE MODEL   ::::
C       :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C       BASE   O  BASAL AREA OF A PLANT [M^2]
C       DMDNIT     O  PLANT REQUIREMENT OF NITROGEN FOR THE DAY [G-N/PLANT]
C       JGS        O  [1..2];1:JULIAN DAY PLANT EMERGES
C                      2:JULIAN DAY PLANT IS RIPE
C       LAI        O  LEAF AREA INDEX
C       PLTSLV     O  NUMBER OF LIVE PLANTS [#/HA]
C       PWRTS  O  NUMBER OF PLANT WITH ACTIVE ROOTS [#/HA]
C       RDF        O  FRACTIONAL DISTRIBUTION OF ROOTS BY LAYER
C       RPOOL  O  RESIDUE POOL, BOTH FAST AND SLOW DECOMPOSERS
C                 [0]--SURFACE RESIDUE, [1..NN]--MIXED SOIL RES
C                 UNITS = [G-CARBON / CM^2-SOIL]
C       SDEAD  O  STANDING DEAD MATERIAL [KG/HA]
C       TLPLNT     O  TOTAL DAILY UPTAKE OF NITROGEN [KG-N/HA]
C       :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C       :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C
C       COMMENTS: CURRENT MODELS INCLUDED: PGMAIN - GENERIC CROP MODEL
C                            POTATO - SIM POTATO CROP MODEL
C                            CERESMZ- CEREZ MAIZE CORN MODEL
C
C       EXTERNAL REFERENCES:
C                 CERESM
C                 PGMAIN
C                 PHOPER
C                 POTATO
C                 SGATE
C                 TMHARV
C                 VGATE
C
C       CALLED FROM: MAIN
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:     2.0
C
C======================================================================
C
      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,MAXHOR=12,MXAPP=200,MXPEST=3,MXSPEC=10,
     +    MXANA=135,mxnodt=3001,mhr=24)
C-GH    Liwang Ma
      PARAMETER (PI=3.141592654D0, R2D=180.0D0/PI, MXCHEM=15)
      COMMON /IPOTEV/ A0,AW,AC,ARI,XW,FSUN,COEPAN,rss,RST(MXSPEC)
     +    ,RXANGLE(MXSPEC),RTCCRIT(MXSPEC),RPLEAF0(MXSPEC),
     +      RRSTEXP(MXSPEC),RRLEAF0(MXSPEC)
C-GH     Liwang Ma
C
      COMMON /GEOMTR/XLONG,ASPECT,DELZ(MXNOD),ELEV,HORTHK(MAXHOR),SLOPE,
     +    AREA,TL(MXNOD),TLT(MXNOD),XLAT,ZN(MXNOD),NHOR,NN,NNT
C
      COMMON /HYDROL/ AEF,BOTFLX,DRDEP,DRSPAC,DRRAD,CLAT(MAXHOR),
     +    H(MXNOD),HKBAR(MXNOD),QF(MXNOD),SOILHP(13,MAXHOR),HFC,HWP,
     +    THETA(MXNOD),thetai(mxnod),RICH_EPS,IREBOT,ITBL,IDRAIN,
     +    MAXITER_RICH,MAXCYCLE_RICH
C
      COMMON /NINFO/ TSOTN0,TSOTN,TOTA,TOTL,TAMIN,TANIT,TADRT(2),
     +    TARAIN(2),TAIRR(3),TAFERT(3),TARES(3),TLIMM,TLDEN,TLVOL,
     +    TLRO(3),TLSEP(3),TLDRN(3),TLLAT(3),TAMANR,TMANH4,tamanr_bal
     +    ,TLN2O,TLNXO,TLNITN2O,TLNITNXO,TLGHGADS,TLGHGADSden
C
      COMMON /CINFO/TAMANC,TADRTC(2),TARESC(3),TOTCO2,TACUREA,YIELDC
     +             ,TAMANC_BAL,RCO2,TCAG,TCBG,TOTCH4,TOTCO2NI
C
      COMMON /PLNTDA/ CLSIZE(0:7),PSTATE(9),PWRTS,DMDNIT,TNITUP,GS,
     +    CNUP1(MXSPEC),CNUP2(MXSPEC),UPLUX
C
C-GH Liwang MA
C     ..DSSAT PARAMETERS
C=======================================================================
C
C     VARIABLES PERTAINING TO CROPGRO and CERES Water Balance Calculations
C
C-----------------------------------------------------------------------
      COMMON /CROPH2O/ ACTEVP,PET,ACTTRN,PER,PES,TOTPUP,PTRANSDAY
     &    ,RET_day_T,RET_day_S,ipet
C     &     ,TTRFD, SMELT,TTRO,DTSEEP,TTIRR
      REAL CGTH(MXNOD), CGT(MXNOD), CGDUL(MXNOD), CGTSHP,
     &   CGLL(MXNOD), CGSAT(MXNOD), RDFR(mxnod), RLVR(mxnod),DSLT(20),
     &  CGNO3(MXNOD), CGNH4(MXNOD), CGTLT(MXNOD), TOTPUPP,
     &  RAIN,RUNOFF,DRAIN,DEPIR,CGANO3,CGANH4,CGTOTO,CGTOTP,CGTOTF,
     &  TLTR(mxnod),cgbd(mxnod),cgph(mxnod),RZOC(MXNOD),CRAINR,
     &  RZON(MXNOD),upnitr(mxnod)
c
      COMMON/WATER_C/TRFDD_C,AIRR_C,SMELT_C,TROI_C,DTSEEP_C,TTRFDD_C,
     +               TTIRR_C
      COMMON/NITROGEN_C/TSNO3_C,TSNH4_C,TSNH4P_C,TSUR_C,TOTO_C,TOTP_C,
     +       TOTF_C,TSHP_C,RZSOILC(mxnod),RZSOILN(mxnod),
     +       totminr,totimm,totden,totvol,TOTFERT,TOTMANU
	common/rootdepth/ IR
      integer Hdate(3)
C-GH Liwang MA
C     DECLARE CONTENTS OF PLNTIN COMMON BLOCK
      DOUBLE PRECISION AHLDG,ALA,ALNAVL,ALNGER,ALNLW,ALPHA,ALX,BETA,
     +    BGSEED,CAA,CANK,CAX,CONVLA,CVLBIO,DEVRAT,DROTOL,DTSEED,GITEMP,
     +    GIWAT,GMN,GRMRAT,GSR,HFMAX,LWM,LWS,PCLDG,PGDVRT,PGNTGT,PLALFA,
     +    PLBIO,PLDIAM,PLHGHT,PLTMNT,PLTMXT,PLTOPT,PMAX,PMNNIT,PRNLW,
     +    PTLAMX,R20,RATLS,RATRSX,RATRSN,RDR,RDX,RQ10,RTNLW,SDAMAX,
     +    SDDVRT,SDSVRT,SDTMGM,SDWMAX,SFREEZ,SLA1,SLA2,SLA3,SLA4,STEND,
     +    STNAVL,STNGER,STNLW,TBS,TOP,TOT4WT,WCG,WCP,WDLDG,EFFN,CNST,
     +    PRB,GSGDD,PMAXN,PNRED,rwl(mxnod),rootn(mxnod),C1(MXNOD)
      INTEGER IPLTYP,NPL,VRNLZ,GDDFLG,EDATE,ADATE,MDATE
      COMMON /PLNTIN/ AHLDG(MXSPEC),ALA(MXSPEC),ALNAVL(MXSPEC),
     +    ALNGER(MXSPEC),ALNLW(MXSPEC),ALPHA(MXSPEC),ALX(MXSPEC),
     +    BETA(MXSPEC),BGSEED(MXSPEC),CAA(MXSPEC),CANK(MXSPEC),
     +    CAX(MXSPEC),CONVLA(MXSPEC),CVLBIO(MXSPEC),DEVRAT(2:7,MXSPEC),
     +    DROTOL(MXSPEC),EFFN(MXSPEC),DTSEED(MXSPEC),GITEMP(MXSPEC),
     +    GIWAT(MXSPEC),GMN(MXSPEC),GRMRAT(MXSPEC),GSR(MXSPEC),
     +    HFMAX(MXSPEC),PCLDG(MXSPEC),PGDVRT(MXSPEC),PGNTGT(MXSPEC),
     +    PLALFA(MXSPEC),PLBIO(MXSPEC),PLDIAM(MXSPEC),PLHGHT(MXSPEC),
     +    PLTMNT(MXSPEC),PLTMXT(MXSPEC),PLTOPT(MXSPEC),PMAX(MXSPEC),
     +    PMAXN(MXSPEC),PNRED(MXSPEC),PMNNIT(MXSPEC),PRNLW(MXSPEC),
     +    PTLAMX(MXSPEC),R20(MXSPEC),RATLS(MXSPEC),RATRSX(MXSPEC),
     +    RATRSN(MXSPEC),RDR(MXSPEC),RDX(MXSPEC),RQ10(MXSPEC),
     +    RTNLW(MXSPEC),SDAMAX(MXSPEC),SDDVRT(MXSPEC),SDSVRT(MXSPEC),
     +    SDTMGM(MXSPEC),SDWMAX(MXSPEC),SFREEZ(MXSPEC),SLA1(MXSPEC),
     +    SLA2(MXSPEC),SLA3(MXSPEC),SLA4(MXSPEC),STEND(2:6,MXSPEC),
     +    STNAVL(MXSPEC),STNGER(MXSPEC),STNLW(MXSPEC),TBS(MXSPEC),
     +    TOP(MXSPEC),TOT4WT(MXSPEC),WCG(MXSPEC),WCP(MXSPEC),
     +    WDLDG(MXSPEC),CNST(MXSPEC),PRB(7,7,MXSPEC),GSGDD(2:6,MXSPEC),
     +    GDDFLG(MXSPEC),VRNLZ(MXSPEC),SUFNDX(MXSPEC),IPLTYP(MXSPEC),
     +    LWM(MXSPEC),LWS(MXSPEC),NPL,INFIXN(MXSPEC)
C
      COMMON /PLNTG/ PLDEN(MXAPP),PLTDAY(MXAPP,2),ROWSP(MXAPP),
     +    PLDEPTH(MXAPP),sdwtplr(mxapp),sdager(mxapp),sprlapr(mxapp),
     +    PLNTSW(MXAPP),PLNTWD(MXAPP),LAYNDX(MXAPP),NPGS,NPR(MXAPP),
     +    NGPL(MXAPP),iptype(mxapp),iemrg(mxapp)
C
      CHARACTER SOLTYP*10,NMPEST*30,PLNAME*30
      COMMON /NAMES/ NMPEST(MXPEST),SOLTYP(MAXHOR),PLNAME(MXSPEC)
C
      COMMON /BMPRC/ YGOAL,OMADJ,USAMDP,UMISCC,ISTATE,ISOYB,BMPIRR,
     +    BMPTIL,FMC,YGSF,SOYYLD
C
      INTEGER IHTYPE,IHDAY,IHARV,IHCL,KOMP(5),NRKOB,DAPCALC
      COMMON /HRVST/ HSTAGE(MXAPP),HPERC(MXAPP),HRVEFF(MXAPP),YIELD(3),
     +    STUBHT(MXAPP),IHTYPE(MXAPP),IHDAY(MXAPP,3),IHARV(MXAPP),
     +    IHCL(MXAPP)
C
C     ..PASSED VARIABLES
      INTEGER JGS(2),NDXN2H(MXNOD),JGST(2),IQUE(4,MXAPP),INXPL(MXSPEC),
     +    IRTYPE(MXSPEC),ndxt2n(mxnodt),LUMDAY
      DOUBLE PRECISION RDF(MXNOD),RPOOL(MXNOD,2),SOILPP(8,MAXHOR),
     +    T(NN),UPNIT(MXNOD),PCN(0:MXNOD),PNIT(9),CN(9),CORES(MXPEST),
     +    COPLNT(MXPEST),OMSEA(MXANA),hrptrans(mhr),hrevap(mhr),
     +    ssurft(mhr),TUP(MXNOD),WORG(5),GORG(5),
     +    DGORG(5),WDORG(5),PE(MXNOD),LAIFKT(10),LUKRIT(10)    !only four plant compartments for now
C
C-GH   Liwang Ma
      DOUBLE PRECISION CC(MXNOD,MXCHEM),RLV(MXNOD),DSTL(20),tnup(mxnod)
C-GH   Liwang Ma
C     ..LOCAL VARIABLES
      DOUBLE PRECISION LAI,LFWT,LFWTS,LFWTSD,ad,lured,LURMAX,
     &     GDUL(MXNOD),GLL(MXNOD),GSAT(MXNOD)
C
      double precision obmas,wumas,wumalt,obalt
C     ..FLAG FOR CROP SWITCHING
C-GH    Liwang Ma
      CHARACTER CROPR*2,VARNOR*6, VRNAME*16,iswpar*1,DAUERKULT*1
      CHARACTER*80 MOW80
      INTEGER YRSIMR,YRSIMRS,wurz,YRPLT,PLYEAR,PLDAY,EDAP,MDAP,ADAP
      INTEGER iresetlai
      common /wsi/ wsi, alaireset, heightset, iresetlai, iresetht1
      DATA VARNOR/'     '/
      DATA VRNAME/'                '/
      save nreps
C-GH   Liwang Ma
      LOGICAL FIRST11,CHLRFL,first15,FHARV,FEXIST
      SAVE FIRST11,first15
      SAVE PCN,TTPLNT,TMPLNT,TXPLNT,IJ
C
      DATA FIRST11 /.TRUE./,istage/1/,lukrit/10*0.0d0/,IPLS/2/,ij/0/
C-MA Liwang
      DATA FIRST15/.TRUE./,FHARV/.FALSE./,NUMORGANS/5/,DAUERKULT/'N'/
C
C      PRINT*,'DMDNIT: ',DMDNIT,'  TNITUP:  ',TNITUP
C      TNITUP = DMDNIT
C      TONIT = TONIT + TNITUP
C      PRINT*,' TOTAL NITROGEN UPTAKEN',TONIT
C
      DAY=DBLE(JDAY)
      TLPLNT=0.0D0
      TADRT(1)=0.0D0
      TADRT(2)=0.0D0
      TADRTC(1)=0.0D0
      TADRTC(2)=0.0D0
      ODMDN=0.0D0
      FIXN=0.0d0
C
      IF(IMAGIC.EQ.-8) THEN
        EWP=1.0D0
      endif
c
C     ..FIND NEW PLANT IF NEEDED
      IF(NPGS.EQ.0) IPL=0
      IF(FIRST11) THEN
        IPM=1
        IPL=0
        DO 10 K=1,MXAPP
          IF(IQUE(1,K).EQ.-1) GOTO 20
          IF(IQUE(1,K).EQ.18) THEN
            IPM=IQUE(2,K)
            IPL=NPR(IPM)

            PRINT *, "RZMAN CALLING MANOUT"
            CALL MANOUT(18,IPM,JDAY,(JDAY-IQUE(3,K)),IYYY)
            JGROW=0
            TTPLNT=0.0D0
            TMPLNT=0.0D0
            TXPLNT=0.0D0
            JGS(1)=366
            JGS(2)=366
            YRPLT =  IYYY * 1000 + JDAY
            PLYEAR = IYYY
            PLDAY = JDAY
          ENDIF
   10   CONTINUE
      ENDIF
C
C     ..HAVE A CROP PLANTED LET'S GROW IT, SHALL WE
   20 IF(IPL.NE.0) THEN
C
      IPDEPTH=NDXT2N(INT(PLDEPTH(IPM)))
C       .. CALCULATE PHOTOPERIOD
        TPP=PHOPER(XLAT,DAY)
C
C       .. SAVE NITROGEN UPTAKE LIMITS AND AMOUNTS IN KG/HA
        TLPLNT=TNITUP*PWRTS*1.0D-3
        TLXPLT=UPLUX*PWRTS*1.0D-3
        ODMDN=DMDNIT*PWRTS*1.0D-3
        Oldpwrts=PWRTS
        IF(IMAGIC.EQ.-7.OR.IMAGIC.EQ.-8) THEN
          WRITE(97,*) 'TLPLNT==',TLPLNT,' ODMDN==',ODMDN
          WRITE(97,*) 'TNITUP==',TNITUP,' PWRTS==',PWRTS
        ENDIF
C
C
        IF(INXPL(IPL).GE.8000.AND.INXPL(IPL).LT.9000) THEN
C=================================================================
C
C         .. Q U I C K T U R F    M O D E L
C         ..... CREATED FOR TORSTEN BEHL
C         ..... BY KEN ROJAS, Liwang Ma
C         ..... LAST MODIFIED 4/22/09
C
C=================================================================
          JGST(1)=JGS(1)
          JGST(2)=JGS(2)
          TM=(TMAX+TMIN)*0.5D0
          PRINT *, "RZMAN CALLING QUICKTURF"
          CALL QUICKTURF(FIRST11,JDAY,NN,CN,RDF,HEIGHT,LAI,TLAI,PLTSLV,
     +        PWRTS,DMDNIT,RM,RCN,RPOOL,PLDEN(IPM),JGS,TLT,TL,
     +        int(pldepth(IPM)),TADRT,TADRTC,iyyy,OMSEA,TM,RCO2,resage)
C
          CALL SGATE(TDAY,11,LAI)
          CALL SGATE(TDAY,12,HEIGHT)
          CALL SGATE(TDAY,50,PLTSLV)
          CALL SGATE(TDAY,14,OMSEA(63))
          OMSEA(13)=EWP
C          OMSEA(43)=LAI
C          OMSEA(62)=HEIGHT
          IPR=3
          irtype(ipl)=3   !treated as wheat
C
          IF(FIRST11) THEN
            BASE=0.0D0
            HEIGHT=0.0D0
            DMDNIT=0.0D0
            JGS(1)=366
            LAI=0.0D0
            TLAI=0.0D0
            PLTSLV=0.0D0
            PWRTS=0.0D0
c            FIXN = 0.0D0
            DO 30 I=1,NN
              RDF(I)=0.0D0
   30       CONTINUE
          ENDIF
C
C         ...DETERMINE PLANT STAGE FOR MANAGEMENT APPLICATIONS
          DO 40 I=1,2
            IF(JGS(I).EQ.1.AND.JGST(I).EQ.366) THEN
              JGS(I)=JDAY
            ELSE
              JGS(I)=JGST(I)
            ENDIF
   40     CONTINUE
          JGROW=JGROW+1
C
        ELSE IF(INXPL(IPL).GE.9000. AND.INXPL(IPL).LT.9500) THEN
C=================================================================
C
C         .. Q U I C K P L A N T   M O D E L
C         ..... CREATED FOR TORSTEN BEHL
C         ..... BY Ken Rojas, LIWANG MA
C         ..... LAST MODIFIED 4/15/09
C
C=================================================================
          JGST(1)=JGS(1)
          JGST(2)=JGS(2)
          TM=(TMAX+TMIN)*0.5D0
          IF (FIRST11) THEN
            IF(INDEX(PLNAME(IPL),'SOYBEAN').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'soybean').NE.0 ) THEN
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'alfalfa').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'ALFALFA').NE.0 ) THEN
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'peanut').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'PEANUT').NE.0 ) THEN
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'DRYBEAN').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'drybean').NE.0 ) THEN
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'FABABEAN').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'fababean').NE.0 ) THEN
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'CANOLA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'canola').NE.0 ) THEN
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'VELVETBEAN').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'velvetbean').NE.0 ) THEN
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'BRACHIARIA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'brachiaria').NE.0 ) THEN
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'CABBAGE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'cabbage').NE.0 ) THEN
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'CHICKPEA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'chickpea').NE.0 ) THEN
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'COWPEA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'cowpea').NE.0 ) THEN
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'PEPPER').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'pepper').NE.0 ) THEN
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'SUNFLOWER').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'sunflower').NE.0 ) THEN
 	          IRTYPE(IPL)=1
            ELSE IF(INDEX(PLNAME(IPL),'PINEAPPLE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'pineapple').NE.0 ) THEN
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'BAHIA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'bahia').NE.0 ) THEN
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'COTTON').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'cotton').NE.0 ) THEN
 	          IRTYPE(IPL)=1
            ELSE IF(INDEX(PLNAME(IPL),'SUGARCANE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'sugarcane').NE.0 ) THEN
 	          IRTYPE(IPL)=1
            ELSE IF(INDEX(PLNAME(IPL),'TOMATO').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'tomato').NE.0 ) THEN
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'MAIZE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'maize').NE.0 ) THEN
	          IRTYPE(IPL)=1
            ELSE IF(INDEX(PLNAME(IPL),'SORGHUM').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'sorghum').NE.0 ) THEN
	          IRTYPE(IPL)=1
            ELSE IF(INDEX(PLNAME(IPL),'POTATO').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'potato').NE.0 ) THEN
	          IRTYPE(IPL)=2               ! need to redefine irtype for potato, Liwang Ma, 1-29-2007
            ELSE IF(INDEX(PLNAME(IPL),'WHEAT').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'wheat').NE.0 ) THEN
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'BERMUDAGRASS').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'bermudagrass').NE.0 ) THEN
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'SPRING BARLEY').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'spring barley').NE.0 ) THEN
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'MILLET-PRL').NE.0 .OR.    !pearl millet
     +        INDEX(PLNAME(IPL),'millet-prl').NE.0 ) THEN
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'MILLET-PRO').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'millet-pro').NE.0 ) THEN
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'MILLET-FOX').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'millet-fox').NE.0 ) THEN
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'TRITICALE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'triticale').NE.0 ) THEN
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'SUGARBEET').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'sugarbeet').NE.0 ) THEN
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'ALFALFA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'alfalfa').NE.0 ) THEN
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'WINTER RYE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'winter rye').NE.0 ) THEN
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'OILRADDISH-SPRING').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'oilraddish-spring').NE.0 ) THEN
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'OILRADDISH-FALL').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'oilraddish-fall').NE.0 ) THEN
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'OILSEED RAPE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'oilseed rape').NE.0 ) THEN
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'GRASS').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'grass').NE.0 ) THEN
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'MUSTARD').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'mustard').NE.0 ) THEN
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'TURNIP RAPE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'turnip rape').NE.0 ) THEN
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'WINTER BARLEY').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'winter barley').NE.0 ) THEN
	          IRTYPE(IPL)=3
            ELSE
                IRTYPE(IPL)=2
            ENDIF            
          ENDIF
c
          PRINT *, "RZMAN CALLING QUICKPLANT"
          CALL QUICKPLANT(FIRST11,JDAY,NN,CN,RDF,HEIGHT,LAI,TLAI,PLTSLV,
     +        PWRTS,DMDNIT,RM,RCN,RPOOL,PLDEN(IPM),JGS,TM,PLNAME(IPL),
     +        TLT,TL,int(PLdepth(IPM)),TADRT,TADRTC,iyyy,OMSEA,RCO2,
     +        RWL,ROOTN,resage)
C
          CALL SGATE(TDAY,11,LAI)
          CALL SGATE(TDAY,12,HEIGHT)
          CALL SGATE(TDAY,50,PLTSLV)
          CALL SGATE(TDAY,14,OMSEA(63))
          CALL SGATE(TDAY,79,OMSEA(41))
          CALL SGATE(TDAY,81,OMSEA(42))
          OMSEA(13)=EWP
        CALL VGATE(TDAY,51,RWL/pwrts)
        CALL VGATE(TDAY,35,ROOTN/pwrts)
C          OMSEA(43)=LAI
C          OMSEA(62)=HEIGHT
         if (resage.eq.0.0d0) ipr=irtype(ipl)
c          IPR=1
C
          IF(FIRST11) THEN
            BASE=0.0D0
            HEIGHT=0.0D0
            DMDNIT=0.0D0
            JGS(1)=366
            LAI=0.0D0
            TLAI=0.0D0
            PLTSLV=0.0D0
            PWRTS=0.0D0
c            FIXN = 0.0D0
            DO 50 I=1,NN
              RDF(I)=0.0D0
   50       CONTINUE
          ENDIF
C
C         ...DETERMINE PLANT STAGE FOR MANAGEMENT APPLICATIONS
          DO 60 I=1,2
            IF(JGS(I).EQ.1.AND.JGST(I).EQ.366) THEN
              JGS(I)=JDAY
            ELSE
              JGS(I)=JGST(I)
            ENDIF
   60     CONTINUE
          JGROW=JGROW+1
C
        ELSE IF((INXPL(IPL).GE.9500).and.(INXPL(IPL).LT.9700)) THEN
C=================================================================
C
C         .. Q U I C K T R E E   M O D E L
C         ..... CREATED FOR CALIFORNIA 
C         ..... BY Liwang Ma
C
C=================================================================
          JGST(1)=JGS(1)
          JGST(2)=JGS(2)
          TM=(TMAX+TMIN)*0.5D0
          PRINT *, "RZMAN CALLING QUICKTREE"
          CALL QUICKTREE(FIRST11,JDAY,NN,CN,RDF,HEIGHT,LAI,TLAI,PLTSLV,
     +        PWRTS,DMDNIT,RM,RCN,RPOOL,PLDEN(IPM),JGS,TM,PLNAME(IPL),
     +        TLT,TL,int(PLdepth(IPM)),TADRT,TADRTC,iyyy,OMSEA,RCO2,
     +        resage)
C
          CALL SGATE(TDAY,11,LAI)
          CALL SGATE(TDAY,12,HEIGHT)
          CALL SGATE(TDAY,50,PLTSLV)
          CALL SGATE(TDAY,14,OMSEA(63))
          OMSEA(13)=EWP
C          OMSEA(43)=LAI
C          OMSEA(62)=HEIGHT
          IPR=2
          irtype(ipl)=2   !only leaf as residue, no branches, treated as sobyean leaves
C
          IF(FIRST11) THEN
            BASE=0.0D0
            HEIGHT=0.0D0
            DMDNIT=0.0D0
            JGS(1)=366
            LAI=0.0D0
            TLAI=0.0D0
            PLTSLV=0.0D0
            PWRTS=0.0D0
c            FIXN = 0.0D0
            DO 51 I=1,NN
              RDF(I)=0.0D0
   51       CONTINUE
          ENDIF
C
C         ...DETERMINE PLANT STAGE FOR MANAGEMENT APPLICATIONS
          DO 61 I=1,2
            IF(JGS(I).EQ.1.AND.JGST(I).EQ.366) THEN
              JGS(I)=JDAY
            ELSE
              JGS(I)=JGST(I)
            ENDIF
   61     CONTINUE
          JGROW=JGROW+1
C
        ELSE IF(INXPL(IPL).GE.9700) THEN
C=================================================================
C
C         .. SUCROS PLANT MODEL FROM HERMES
C         ..... CREATED FOR Rob Malone 
C         ..... BY Liwang Ma
C
C=================================================================
          JGST(1)=JGS(1)
          JGST(2)=JGS(2)
          TM=(TMAX+TMIN)*0.5D0
           temppop=plden(ipm)
          IF(DAUERKULT.EQ.'D'.and.temppop.eq.0.0d0) then
           do i=1,mxapp
              Temppop=plden(ipm-i)
              if (temppop.gt.0.0d0) goto 223
           enddo
           endif
223       continue
          PLTSLV=temppop*1.0D3
          PWRTS=temppop*1.0D3
          Pltpop=temppop/10.D0
          
          DO I = 1,NN
            JH = NDXN2H(I)
              GSAT(I) = SOILHP(6,JH)
              GDUL(I) = SOILHP(7,JH)
              GLL(I)  = SOILHP(9,JH)
              C1(I) = CC(I,9)*THETA(I)*TL(I)*0.1D0   !CHECK UNIT GOING TO DSSAT AND HERMES MODELS
C              CGNH4(I) = REAL(CC(I,10))*THETA(I)*TL(I)*0.1D0
              PORLU=0.0D0
              IF (TLT(I).LE.30.D0) THEN
                  PORLU=PORLU+(SOILHP(6,JH)-THETA(I))*TL(I)
                  IDEXPOR=I
              ENDIF
          ENDDO
          PORLU=PORLU/TLT(IDEXPOR)
           IF (PORLU < LUKRIT(ISTAGE)) then
            ! counting days of oxygen deficit
              LUMDAY = LUMDAY + 1
              IF (LUMDAY > 4) LUMDAY = 4
              IF (PORLU < 0)  PORLU = 0.0D0
              LURMAX = PORLU/LUKRIT(ISTAGE)
              LURED = 1.0D0-LUMDAY/4.0D0*(1-LURMAX)
           ELSE
              LUMDAY = 0
              LURED = 1.0D0
           END IF
!   THE FOLLOWING IS NOT IMPLEMENTED IN RZWQM
!           IF LURED > 1 then LET LURED = 1
!           FOR I = 1 TO N
!               IF I > MIN(WURZ,GRW) THEN
!                  LET TP(I) = 0
!               ELSE
!                  IF WUEFF(I)*WUDICH(I) > 0 then
!                    Reduction of transpirationin all layers by LURED
!                     LET TP(I) = TRAMAX*WUEFF(I)*WUDICH(I)/WEFF * LURED
!                  ELSE
!                     LET TP(i) = 0
!                  END IF
!               END IF
!           NEXT I
          
          IF (FIRST11) THEN
            IF(INDEX(PLNAME(IPL),'SOYBEAN').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'soybean').NE.0 ) THEN
                CROPR="SB"
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'SUNFLOWER').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'sunflower').NE.0 ) THEN
                CROPR="SU"
	          IRTYPE(IPL)=1
            ELSE IF(INDEX(PLNAME(IPL),'ALFALFA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'alfalfa').NE.0 ) THEN
                CROPR="AL"
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'peanut').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'PEANUT').NE.0 ) THEN
                CROPR="PN"
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'DRYBEAN').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'drybean').NE.0 ) THEN
                CROPR="BN"
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'FABABEAN').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'fababean').NE.0 ) THEN
                CROPR="FB"
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'CANOLA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'canola').NE.0 ) THEN
                CROPR="CA"
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'VELVETBEAN').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'velvetbean').NE.0 ) THEN
                CROPR="VB"
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'BRACHIARIA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'brachiaria').NE.0 ) THEN
                CROPR="BR"
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'CABBAGE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'cabbage').NE.0 ) THEN
                CROPR="CB"
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'CHICKPEA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'chickpea').NE.0 ) THEN
                CROPR="CH"
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'COWPEA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'cowpea').NE.0 ) THEN
                CROPR="CP"
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'PEPPER').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'pepper').NE.0 ) THEN
                CROPR="PR"
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'SUNFLOWER').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'sunflower').NE.0 ) THEN
                CROPR="SU"
 	          IRTYPE(IPL)=1
            ELSE IF(INDEX(PLNAME(IPL),'ALFALFA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'alfalfa').NE.0 ) THEN
                CROPR="AL"
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'PINEAPPLE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'pineapple').NE.0 ) THEN
                CROPR="PI"
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'BAHIA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'bahia').NE.0 ) THEN
                CROPR="G0"
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'COTTON').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'cotton').NE.0 ) THEN
                CROPR="CO"
 	          IRTYPE(IPL)=1
            ELSE IF(INDEX(PLNAME(IPL),'SUGARCANE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'sugarcane').NE.0 ) THEN
                CROPR="SC"
 	          IRTYPE(IPL)=1
            ELSE IF(INDEX(PLNAME(IPL),'TOMATO').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'tomato').NE.0 ) THEN
                CROPR="TM"
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'MAIZE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'maize').NE.0 ) THEN
                CROPR="MZ"
	          IRTYPE(IPL)=1
            ELSE IF(INDEX(PLNAME(IPL),'SORGHUM').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'sorghum').NE.0 ) THEN
                CROPR="SG"
	          IRTYPE(IPL)=1
            ELSE IF(INDEX(PLNAME(IPL),'POTATO').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'potato').NE.0 ) THEN
                CROPR="PT"
	          IRTYPE(IPL)=2               ! need to redefine irtype for potato, Liwang Ma, 1-29-2007
            ELSE IF(INDEX(PLNAME(IPL),'WINTER WHEAT').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'winter wheat').NE.0 ) THEN
                CROPR="WH"
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'BERMUDAGRASS').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'bermudagrass').NE.0 ) THEN
                CROPR="BM"
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'SPRING WHEAT').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'spring wheat').NE.0 ) THEN
                CROPR="SW"
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'SPRING BARLEY').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'spring barley').NE.0 ) THEN
                CROPR="BA"
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'MILLET-PRL').NE.0 .OR.    !pearl millet
     +        INDEX(PLNAME(IPL),'millet-prl').NE.0 ) THEN
                CROPR="ML"
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'MILLET-PRO').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'millet-pro').NE.0 ) THEN
                CROPR="MO"
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'MILLET-FOX').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'millet-fox').NE.0 ) THEN
                CROPR="MX"
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'TRITICALE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'triticale').NE.0 ) THEN
                CROPR="TR"
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'SUGARBEET').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'sugarbeet').NE.0 ) THEN
                CROPR="BS"
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'ALFALFA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'alfalfa').NE.0 ) THEN
                CROPR="AA"
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'WINTER RYE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'winter rye').NE.0 ) THEN
                CROPR="WR"
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'OILRADDISH-SPRING').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'oilraddish-spring').NE.0 ) THEN
                CROPR="OS"
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'OILRADDISH-FALL').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'oilraddish-fall').NE.0 ) THEN
                CROPR="OF"
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'OILSEED RAPE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'oilseed rape').NE.0 ) THEN
                CROPR="OR"
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'GRASS').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'grass').NE.0 ) THEN
                CROPR="GR"
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'MUSTARD').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'mustard').NE.0 ) THEN
                CROPR="MT"
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'TURNIP RAPE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'turnip rape').NE.0 ) THEN
                CROPR="WT"
	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'WINTER BARLEY').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'winter barley').NE.0 ) THEN
                CROPR="WG"
	          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'OAT').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'oat').NE.0 ) THEN
                CROPR="OT"
	          IRTYPE(IPL)=3
            ELSE
                CROPR="UD"
                IRTYPE(IPL)=2
            ENDIF            
!
          DO J=1,NHOR
            PCLAY=SOILPP(7,J)*100.0D0
            PSILT=SOILPP(6,J)*100.0D0
            PSAND=SOILPP(5,J)*100.0D0
          enddo
!     set up AD values for HERMES model     
        IF(PSAND.GE.85.0D0.AND.(PSILT+1.5D0*PCLAY).LE.15.0D0) THEN
          AD= 0.004D0     !         NCL=1
        ELSEIF(PSAND.GE.85.0D0.AND.PSAND.LE.90.0D0.AND.(PSILT+1.5D0*
     +      PCLAY).GE.15.0D0) THEN
          AD= 0.004D0    !NCL=2
        ELSEIF(PSAND.GE.70.0D0.AND.PSAND.LE.85.0D0.AND.(PSILT+2.0D0*
     +      PCLAY).LE.30.0D0) THEN
          AD = 0.004D0   !NCL=2
        ELSEIF(PSAND.GE.52.0D0.AND.PCLAY.LE.20.0D0.AND.(PSILT+2.0D0*
     +      PCLAY).GT.30.0D0) THEN
          AD = 0.005D0   !NCL=3
        ELSEIF(PSAND.GE.43.0D0.AND.PSAND.LE.52.0D0.AND.(PCLAY.LT.7.0D0
     +      .AND.PSILT.LT.50.0D0)) THEN
          AD = 0.005D0   !NCL=3
        ELSEIF(PSAND.LT.52.0D0.AND.PCLAY.GE.7.0D0.AND.(PCLAY.LT.27.0D0
     +      .AND.PSILT.GE.28.0D0.AND.PSILT.LE.50.0D0)) THEN
          AD = 0.005D0        !NCL=4
        ELSEIF(PSILT.GE.50.0D0.AND.PCLAY.GE.12.0D0.AND.(PCLAY.LT.27.0D0)
     +      ) THEN
          AD = 0.005D0    !NCL=5
        ELSEIF(PSILT.GE.50.0D0.AND.PSILT.LE.80.0D0.AND.(PCLAY.LT.12.0D0)
     +      ) THEN
          AD = 0.005D0    !NCL=5
        ELSEIF(PSILT.GE.80.0D0.AND.PCLAY.LT.12.0D0) THEN
           AD = 0.002D0    !NCL=6
        ELSEIF(PSAND.GE.45.0D0.AND.PCLAY.GE.20.0D0.AND.(PCLAY.LE.35.0D0
     +      .AND.PSILT.LT.28.0D0)) THEN
          AD = 0.005D0   !NCL=7
        ELSEIF(PSAND.GE.20.0D0.AND.PSAND.LE.45.0D0.AND.PCLAY.GE.27.0D0
     +      .AND.PCLAY.LE.40.0D0) THEN
          AD = 0.001D0   !NCL=8
        ELSEIF(PCLAY.GE.27.0D0.AND.PCLAY.LE.40.0D0.AND.PSAND.LT.20.0D0)
     +      THEN
          AD = 0.001D0  !NCL=9
        ELSEIF(PCLAY.GE.35.0D0.AND.PSAND.GE.45.0D0) THEN
          AD = 0.001D0   !NCL=10
        ELSEIF(PCLAY.GE.40.0D0.AND.PSILT.GE.40.0D0) THEN
          AD = 0.001D0   !NCL=11
        ELSEIF(PCLAY.GE.40.0D0.AND.PSILT.LT.40.0D0.AND.PSAND.LT.45.0D0)
     +      THEN
          AD = 0.001D0   !NCL=12
        ELSE
           AD = 0.005D0   !NCL=13 ASSUME TO BE LOAM
        ENDIF
          ENDIF
          YRSIMR =  IYYY * 1000 + JDAY
            DO I = 1, 3
              if (ihday(ipm,3).ne.0) then
              HDATE(I) = IHDAY(IPM,3)*1000 + 
     +         JDATE(IHDAY(IPM,1),IHDAY(IPM,2),IHDAY(IPM,3))
              else
              hdate(i) = -99	
              endif		 
            ENDDO
            cxlat = xlat*180.d0/3.1415d0 
!RMarquez 08-31-16 -> Current HERMES_SUCROS call header
      PRINT *, "RZMAN CALLING HERMES_SUCROS"
      CALL HERMES_SUCROS(FIRST11,T,TM,THETA,GDUL,GLL,GSAT,            
     + RDF,C1,TUP,PE,RTS/2.d0,DAYL,LAI,pltpop,plden(ipm+1),             
     + OBMAS,WUMAS,TL/10.0D0,TLT/10.0D0,WORG,GORG,DGORG,WDORG,JGS,TLAI, 
     + FIXN,CXLAT,CO2R,ADATE,MDATE,GEHOB,WUGEH,DAUERKULT,EWP,           
     + PESUM,RTDEP,JDAY,ISTAGE,IPLS,NN,DMDNIT,SUMPE,CROPR,LUKRIT,
     + RM,CN,OMSEA,RPOOL,TADRT,TADRTC,RCO2,FHARV,LAIFKT,KOMP,
     + RCN,PLNAME(IPL),ihtype(ipm),iharv(ipm),HRVEFF(ipm),HDATE,yrsimr,
     + resage,NRKOM,AD,lured,PLHGHT(IPL),PLALFA(IPL),plden(ipm),sdead,
     + sdcn,stubht(ipm),HEIGHT,wumalt,obalt,WORG2MIN,WORG3MIN,imagic,
     + ipm-npgs,EDATE,GS)
!
C RESET LAI 12-16-2018
       IF (IRESETLAI.EQ.1.AND.ALAIRESET.GT.0.D0) THEN
          LAI=ALAIRESET
          TLAI = LAI                !Leaf area index, m2/m2
C          WOrg(2) = LAI/LAIFKT(2)
          LAIFKT(2) = LAI/WOrg(2)
       ENDIF
       IF (IRESETHT1.EQ.1) HEIGHT=HEIGHTSET
C END OF RESETTING
       RTDEP=RTDEP*10.0D0
          STEM2=(WORG(2)+WORG(3))/10.D0/PLTPOP
          HEIGHT=PLHGHT(IPL)*(1.0D0-EXP(-PLALFA(IPL)*STEM2/
     &          (2.0D0*PLHGHT(IPL))))
C
! output to RZWQM
! total weight of the organs, NAME FROM DSSAT CONVENTION, BUT THEY ARE DOUBLE PRECISION.
           RTWT=WORG(1)
           LFWT=WORG(2)
           STWT=WORG(3)
           SDWT=WORG(4)
           TUWT=WORG(5)
! total dead of each organ           
           RTWTS=WDORG(1)
           LFWTS=WDORG(2)
           STWTS=WDORG(3)
           SDWTS=WDORG(4)
           TUWTS=WDORG(5)
! daily death of each organ          
           RTWTSD=DGORG(1)
           LFWTSD=DGORG(2)
           STWTSD=DGORG(3)
           SDWTSD=DGORG(4)
           TUWTSD=DGORG(5)
!
      CALL SGATE (TDAYR,49,GS)
      CALL SGATE (TDAYR,61,RTWT/pwrts)
      CALL SGATE (TDAYR,65,RTWTS/pwrts)
      CALL SGATE (TDAYR,14,RTDEP)
      CALL SGATE (TDAYR,59,STWT/pwrts)
      CALL SGATE (TDAYR,58,LFWT/pwrts)
      CALL SGATE (TDAYR,11,LAI)
      CALL SGATE (TDAYR,79,LFWT+STWT+SDWT)  !WORG(2)+WORG(3)+WORG(4))   !OBMAS)
      CALL SGATE (TDAYR,81,RTWT)             !WUMAS)
      CALL SGATE (TDAYR,66,GEHOB*LFWT/PWRTS)
      CALL SGATE (TDAYR,67,GEHOB*STWT/PWRTS)
      CALL SGATE (TDAYR,69,WUGEH*RTWT/PWRTS)
      CALL SGATE (TDAYR,70,GEHOB*SDWT/PWRTS)
      OMSEA(22) = GEHOB*(LFWT+STWT+SDWT)  !(WORG(2)+WORG(3)+WORG(4))  !OBMAS
      OMSEA(23) = WUGEH*RTWT  !WORG(1)                    !WUMAS
      OMSEA(24) = GEHOB*SDWT
      OMSEA(25) = FIXN
      OMSEA(41) = LFWT+STWT+SDWT   !WORG(2)+WORG(3)+WORG(4)          !OBMAS
      OMSEA(42) = RTWT              !WUMAS
      OMSEA(43) = LAI
      OMSEA(44) = SDWT
      OMSEA(62) = HEIGHT
      OMSEA(63) = RTDEP
      OMSEA(65) = LFWT/PWRTS
      OMSEA(66) = STWT/PWRTS
      OMSEA(67) = RTWT/PWRTS
      OMSEA(68) = SDWT/PWRTS
c      OMSEA(45) = dble(ISTAGE)
      OMSEA(45) = GS
      OMSEA(13) = EWP
!
          DO I = 1,NN
              CC(I,9)=C1(I)/(THETA(I)*TL(I)*0.1D0)   !CHECK UNIT GOING TO DSSAT AND HERMES MODELS
C              CC(I,10)=DBLE(CGNH4(I))/(THETA(I)*TL(I)*0.1D0)
              rwl(i)=wumas*rdf(i)/(pwrts*1.0d-3)
              ROOTN(I)=RWL(I)*WUGEH
              UPNIT(i)=PE(i)/(PWRTS*1.0D-3)
          ENDDO
          TNITUP=(SUMPE+FIXN)/(PWRTS*1.0D-3)
          TLPLNT = TNITUP * PWRTS * 1.0D-3
          CALL VGATE(TDAY,36,TNUP)
C
          CALL SGATE(TDAY,50,PLTSLV)
c          IPR=1
C         ..ACCUMULATE NITROGEN UPTAKEN BY PLANTS
c          FIXN=NFIX     !From HERMES  kg N/ha
c          FIXN=FIXN*oldPWRTS*1.0D-3  !no conversion is necessary 
C          TLXPLT=0.0d0  !no luxurious N uptake
C          TTPLNT=TTPLNT+TLPLNT+TLXPLT
C          TMPLNT=TMPLNT+TLPLNT
C          TXPLNT=TXPLNT+TLXPLT
C          TLPLNT=TLPLNT+TLXPLT
C
C====================== ALFALFA CUTTING ========================================
C      IF (DAUERKULT.EQ.'D'.AND.PLDEN(IPM+1).EQ.0.and.ipm.lt.npgs) THEN  !PERRENIAL CROPS
      IF((INDEX(PLNAME(IPL),'ALFALFA').NE.0).or.
     &   (INDEX(PLNAME(IPL),'BERMUDAGRASS').NE.0)) THEN
	  INQUIRE (FILE = 'FORGMOW.DAT', EXIST = FEXIST)
        IF (FEXIST) THEN
        MOWLUN=999
        OPEN (UNIT=MOWLUN,FILE='FORGMOW.DAT',STATUS='OLD')
C        I=0
        ISECT = 0
        DO WHILE (ISECT.EQ.0)
          READ (MOWLUN,'(A80)',IOSTAT=ISECT) MOW80
          IF (MOW80(1:1).NE."@"
     &       .AND.MOW80(1:1).NE."!"
     &       .AND.MOW80(1:20).NE."                    "
C     &       .and.mow80(1:6)==trtchar
     &       .AND.ISECT.EQ.0)THEN
            IJ = IJ + 1
            READ (MOW80,'(I6,I8,4F6.0)',IOSTAT=ISECT)
     &                ITRT,IDATE,FORMOW,RSPLF,FORGS,FORHT
C     &                TRNO(I),DATE(I),MOW(I),RSPLF(I),MVS(I),rsht(i)
C
C            INQUIRE(file='FORAGE.OUT',EXIST=FEXIST)
            IF (IJ.EQ.1) THEN
               OPEN(UNIT=987,FILE='FORAGE.OUT')
           WRITE (987,'(A97)') 'CUTTING DATE     HARV_LEAF     HARV_STEM
     &         TOTAL     LEAF_LEFT     STEM_LEFT    LEAF/STEM'
C            REWIND(987)
            ELSE
              OPEN(UNIT=987,FILE='FORAGE.OUT',POSITION='APPEND')
            ENDIF
        if (IDATE.EQ.(IYYY*1000+JDAY)) THEN
             WORG(2)=max(WORG2MIN,FORMOW*RSPLF/100.D0)
             WORG(3)=max(WORG3MIN,FORMOW*(1.0-RSPLF/100.D0))
                FHLEAF=LFWT-WORG(2)       !HARVESTED LEAF
                FHSTEM=STWT-WORG(3)       !HARVESTED STEM
                FHTOTAL=FHLEAF+FHSTEM+WORG(4)    !LEAF+STEM+SEEDS, WHICH NEED TO PRINT OUT IN A FILE
             WORG(4)=0.0D0                !SET SEEDS TO ZERO
             LAI = (WOrg(2))*LAIFKT(2)
             TLAI = (WOrg(2))*LAIFKT(2)
             Pesum = (WORG(2)+WORG(3))*gehob+Worg(1)*wugeh
             OBMAS=WORG(2)+WORG(3)
             OBALT = obmas
             WUMAS = Worg(1)
             wumalt = Worg(1)
             ISTAGE=FORGS                             !HOW TO SET THE GROWTH STAGE AFTER HARVEST A PERRENIAL? start istage with 2 or 3?
C  RESET PLANT HEIGHT BASED ON LEAF AND STEM WEIGHT
          STEM2=(WORG(2)+WORG(3))/10.D0/PLTPOP
          HEIGHT=PLHGHT(IPL)*(1.0D0-EXP(-PLALFA(IPL)*STEM2/
     &          (2.0D0*PLHGHT(IPL))))
cc        if ((ipm.lt.npgs).and.(plden(IPM+1).eq.0)) then
cc            IPM=IPM+1
cc            IPLS=IPLS+1
          WRITE (987,'(4X,I8, 6(5X,F9.2))') IDATE,FHLEAF,FHSTEM,FHTOTAL
     &                                  ,WORG(2),WORG(3),WORG(2)/WORG(3)
          ENDIF
          END IF
        END DO
        CLOSE (MOWLUN)
        CLOSE (987)
        ELSE
        PRINT*,'FORGMOW.DAT IS MISSING AND NO CUTTING IS SCHEDULED
     &         PLEASE COPY A TEMPLATE FROM /RZWQM2/STATUP/ FOLDER'
        ENDIF
        ENDIF
C====================== END OF ALFALFA CUTTING =================================
       IF(FHARV) THEN
      write(unit=101,fmt=*) yrplt,EDATE-yrplt,ADATE-yrplt,
     &                      MDATE-yrplt,CROPR
      TNAG = GEHOB*(WORG(2)+WORG(3)+WORG(4))   !OBMAS  !(WORG(2)+WORG(3)+WORG(4))
      TNBG = WUGEH*WORG(1)
      TCAG = WORG(2)+WORG(3)+WORG(4)   !OBMAS  !
      TCBG = WORG(1)
      IF (iharv(ipm).EQ.3) then   !seed harvest
          YIELD(1)=WORG(4)*HRVEFF(ipm)  
          YIELD(2)=(WORG(2)+WORG(3))*
     +        max(0.0d0,(1.0d0-stubht(ipm)/height))
          YIELD(3)=WORG(1)
      endif
!
      IF (iharv(ipm).EQ.4) then    !aboveground biomass harvest
          YIELD(1)=WORG(4)
          YIELD(2)=(WORG(2)+WORG(3))*
     +        max(0.0d0,(1.0d0-stubht(ipm)/height))*HRVEFF(ipm)+WORG(4)
          YIELD(3)=WORG(1)
      endif
!
      IF (iharv(ipm).EQ.5) then    !root harvest
          YIELD(1)=WORG(4)  
          YIELD(2)=(WORG(2)+WORG(3))*
     +             max(0.0d0,(1.0d0-stubht(ipm)/height))+WORG(4)
          YIELD(3)=WORG(1)*HRVEFF(ipm)
      endif
! 
      IF (iharv(ipm).EQ.6) then    !all above and below ground
          YIELD(1)=WORG(4)*HRVEFF(ipm)  
          YIELD(2)=(WORG(2)+WORG(3)+WORG(4))*HRVEFF(ipm)   !OBMAS
          YIELD(3)=WORG(1)*HRVEFF(ipm)
      endif
cc      IF (DAUERKULT.EQ.'D'.AND.PLDEN(IPM+1).EQ.0.and.ipm.lt.npgs) THEN  !PERRENIAL CROPS
C
c        if (plden(IPM).gt.0.0d0) FIRST11=.true.
c           DO I=2,NUMORGANS
c             WORG(2)=max(720d0,WORG(2)*MIN(1.0D0,STUBHT(IPM)/HEIGHT))  !(1.0D0-HRVEFF(ipm))
c             WORG(3)=max(100d0,WORG(3)*MIN(1.0D0,STUBHT(IPM)/HEIGHT))  !(1.0D0-HRVEFF(ipm))
cc             WORG(2)=max(WORG2MIN,WORG(2)*MIN(1.0D0,STUBHT(IPM)/HEIGHT))  !(1.0D0-HRVEFF(ipm))
cc             WORG(3)=max(WORG3MIN,WORG(3)*MIN(1.0D0,STUBHT(IPM)/HEIGHT))  !(1.0D0-HRVEFF(ipm))
cc             WORG(4)=0.0d0                                  !(1.0D0-HRVEFF(ipm))
cc             WORG(5)=0.0d0                                  !(1.0D0-HRVEFF(ipm))
cc             yield(2)=yield(2)
cc     &       -max(0.0d0,WORG2MIN-WORG(2)*MIN(1.0D0,STUBHT(IPM)/HEIGHT))
cc     &       -max(0.0d0,WORG3MIN-WORG(3)*MIN(1.0D0,STUBHT(IPM)/HEIGHT))
c           WDORG(I)=0.0D0
c           ENDDO
cc             LAI = (WOrg(2))*LAIFKT(2)
cc             TLAI = (WOrg(2))*LAIFKT(2)
C             Pesum = Max((Pesum-Worg(1)*wugeh) * STUBHT(IPM)/HEIGHT,
C     &            820d0*gehob+Worg(1)*wugeh)
cc             Pesum = Max((Pesum-Worg(1)*wugeh) * STUBHT(IPM)/HEIGHT,
cc     &            (WORG2MIN+WORG3MIN)*gehob+Worg(1)*wugeh)
cc             OBMAS=WORG(2)+WORG(3)
cc             OBALT = obmas
cc             WUMAS = Worg(1)
cc             wumalt = Worg(1)
cc             ISTAGE=2                            !HOW TO SET THE GROWTH STAGE AFTER HARVEST A PERRENIAL? start istage with 2 or 3?
cc        if ((ipm.lt.npgs).and.(plden(IPM+1).eq.0)) then
cc            IPM=IPM+1
cc            IPLS=IPLS+1
cc        ENDIF
cc      ELSE
! RESET ALL THE CROP VARIABLES FOR ANNUAL CROP
      IPLS=2
      DO I=1,NUMORGANS
      WORG(I)=0.0D0
      WDORG(I)=0.0D0
      ENDDO
!
      LAI=0.0D0
      TLAI=0.0d0
      GEHOB=0.0D0
      WUGEH=0.0D0
      ISTAGE=1                                !=0 AFTER HARVEST?
      wumas=0.0d0
      obmas=0.0d0
cc      ENDIF
!  SUM UP TOTAL N IN THE PLANT IN PESUM AFTER CUTTING/HARVEST   
              nrkob = size(KOMP)
              OBMAS=0.0D0
              do i = 1, nrkob
                  !RMarquez - accumulate above ground organ mass using aboveground organ #'s
                    !komp= VAL(PROGIP1$(i:i)) !original
                    !OBMAS=OBMAS + WORG(komp) !original
                    if (KOMP(I).NE.0) OBMAS=OBMAS + WORG(KOMP(I))
                    enddo
             wumas=worg(1)
!           ! Including beet/tuber mass in calculation of N-content for sugar beets and potatoes 
           IF (CROPR == "BS" .or. CROPR == "PT") then
                PESUM = (OBMAS+WORG(4))*gehob+WUMAS*WUGEH
           ELSE
                PESUM =(OBMAS*gehob+WUMAS*WUGEH)    !*.007
           END IF
c
            FHARV=.FALSE.
            CALL CDATE(JDAY,ID,IM,IYYY)
            WRITE(70,1000)
            WRITE(70,1110) plname(ipl),ID,IM,IYYY,JDAY
c            TCAG=OMSEA(41)
c            TCBG=OMSEA(42)
c            TNAG=OMSEA(22)
c            TNBG=OMSEA(23)
            HI=YIELD(1)/TCAG
            WRITE(70,1200) PLTSLV,(YIELD(I),I=1,3),HI,TNAG,
     +                     TNBG,TCAG,TCBG
c            WRITE(70,1000)
      ENDIF
          IF(FIRST11.AND.IHARV(IPM).GT.2) THEN
            BASE=0.0D0
            HEIGHT=0.0D0
            DMDNIT=0.0D0
            JGS(1)=366
            LAI=0.0D0
            TLAI=0.0D0
            PLTSLV=0.0D0
            PWRTS=0.0D0
c            FIXN = 0.0D0
            DO I=1, NN
              RDF(I)=0.0D0
            ENDDO
          ENDIF
C
          if (resage.eq.0.d0) ipr=irtype(IPL)
c          IPR=irtype(IPL)  !SET RES TYPE TO LAST HARVEST PLANT TYPE
C
C         ...DETERMINE PLANT STAGE FOR MANAGEMENT APPLICATIONS
           DO I=1,2
!          DO 61 I=1,2
            IF(JGS(I).EQ.1.AND.JGST(I).EQ.366) THEN
              JGS(I)=JDAY
            ELSE
              JGS(I)=JGST(I)
            ENDIF
           ENDDO
!   61     CONTINUE
          JGROW=JGROW+1
C
        ELSE IF(INXPL(IPL).LT.2000) THEN
C
C         ==========================================
C         .. G E N E R I C   P L A N T   M O D E L
C         ==========================================
C
          JGST(1)=JGS(1)
          JGST(2)=JGS(2)
          IGPL=NGPL(IPL)
C
            DO I = 1,NN
              JH = NDXN2H(I)
C              CGTH(I)  = REAL(THETA(I))
C              CGT(I)   = REAL(T(I))
               GSAT(I) = SOILHP(6,JH)
C              CGDUL(I)=real(WC(HFC,SOILHP(1,JH),JH,0))
C              CGLL(I)=real(WC(HWP,SOILHP(1,JH),JH,0))
C              CGBD(I) = real(soilpp(3,jh))
           ENDDO

C         ..CALL PLANT GROWTH MODEL: GENERIC CROP
          PRINT *, "RZMAN CALLING PGMAIN"
          CALL PGMAIN(RFDD,DAY,TMAX,TMIN,RTS,TPP,W,HEIGHT,LAI,THETA,H,T,
     +        TLT,SOILPP,SOILHP,NDXN2H,NN,RDF,JGS,NSC,PLTSLV,RPOOL,
     +        UPNIT,EWP,SDEAD,TLPLNT,XNIT,BASE,FIXN,IGPL,IPM,FIRST11,
     +        PCN,IMAGIC,PNIT,TADRT,CN,RM,RCN,SDCN,CHLRFL,TDAY,
     +        IRTYPE(IPL),OMSEA,TLAI,Jday,iyyy,CO2R,tadrtc,ndxt2n,RCO2,
     +        GSAT,TL,plantNseed,rwl,rootn)
C
C RESET LAI 12-16-2018
       IF (IRESETLAI.EQ.1.AND.ALAIRESET.GT.0.D0) THEN
          LAI=ALAIRESET
          TLAI = LAI                !Leaf area index, m2/m2
C          PSTATE(2) = LAI*(CONVLA(IPL)/PLTS27)/C2BM
          CONVLA(IPL) = PSTATE(2)*C2BM/LAI*PLTS27
C          STEM2=(PSTATE(2)+PSTATE(3))*C2BM
C          HEIGHT=PLHGHT(IPL)*(1.0D0-EXP(-PLALFA(IPL)*STEM2/
C     &          (2.0D0*PLHGHT(IPL))))
       ENDIF
       IF (IRESETHT1.EQ.1) HEIGHT=HEIGHTSET
C END OF RESETTING

          TCLSIZE=CLSIZE(1)+CLSIZE(2)+CLSIZE(3)+CLSIZE(4)+CLSIZE(5)
     &            +CLSIZE(6)+CLSIZE(7)
C      EMERGENCE DATE
        IF (CLSIZE(3)/TCLSIZE.GE.0.5D0.AND. EDAP.LE.0) THEN
              edap = Dapcalc(IYYY*1000+JDAY,plyear,plday)
        ENDIF
C      ANTHESIS DATE
        IF (CLSIZE(6)/TCLSIZE.GE.0.1D0.AND.ADAP.LE.0) THEN
              Adap = Dapcalc(IYYY*1000+JDAY,plyear,plday)
        ENDIF
C      MATURITY DATE
        IF (CLSIZE(7)/TCLSIZE.GE.0.95D0.AND.MDAP.LE.0) THEN
              Mdap = Dapcalc(IYYY*1000+JDAY,plyear,plday)
        ENDIF
C
        CALL VGATE(TDAY,51,RWL*2.5d0)
        CALL VGATE(TDAY,35,ROOTN)
C         ..ACCUMULATE NITROGEN UPTAKEN BY PLANTS
          FIXN=FIXN*oldPWRTS*1.0D-3
          TTPLNT=TTPLNT+TLPLNT+TLXPLT
          TMPLNT=TMPLNT+TLPLNT
          TXPLNT=TXPLNT+TLXPLT
          TLPLNT=TLPLNT+TLXPLT
C
C         .. SEND OUTPUT TO PRINTING SECTION
          IF(NSC.GE.1) THEN
            CALL SGATE(TDAY,79,(PSTATE(2)+PSTATE(3)+PSTATE(4)+
     +          PSTATE(6))*PLTSLV*1.0D-3*2.5D0)
            CALL SGATE(TDAY,80,(PSTATE(7)+PSTATE(8))*PLTSLV*1.0D-3*2.5D0
     +          )
            CALL SGATE(TDAY,81,PSTATE(5)*PLTSLV*1.0D-3*2.5D0)
          ENDIF
C
C
C====================== ALFALFA CUTTING ========================================
C      IF (DAUERKULT.EQ.'D'.AND.PLDEN(IPM+1).EQ.0.and.ipm.lt.npgs) THEN  !PERRENIAL CROPS
      IF((INDEX(PLNAME(IPL),'ALFALFA').NE.0).OR.
     &   (INDEX(PLNAME(IPL),'BERMUDAGRASS').NE.0)) THEN
	  INQUIRE (FILE = 'FORGMOW.DAT', EXIST = FEXIST)
        IF (FEXIST) THEN
        MOWLUN=999
        OPEN (UNIT=MOWLUN,FILE='FORGMOW.DAT',STATUS='OLD')
C        I=0
        ISECT = 0
        DO WHILE (ISECT.EQ.0)
          READ (MOWLUN,'(A80)',IOSTAT=ISECT) MOW80
          IF (MOW80(1:1).NE."@"
     &       .AND.MOW80(1:1).NE."!"
     &       .AND.MOW80(1:20).NE."                    "
C     &       .and.mow80(1:6)==trtchar
     &       .AND.ISECT.EQ.0)THEN
            IJ = IJ + 1
            READ (MOW80,'(I6,I8,4F6.0)',IOSTAT=ISECT)
     &                ITRT,IDATE,FORMOW,RSPLF,FORGS,FORHT
C
C            INQUIRE(file='FORAGE.OUT',EXIST=FEXIST)
            IF (IJ.EQ.1) THEN
               OPEN(UNIT=987,FILE='FORAGE.OUT')
           WRITE (987,'(A97)') 'CUTTING DATE     HARV_LEAF     HARV_STEM
     &         TOTAL     LEAF_LEFT     STEM_LEFT    LEAF/STEM'
            ELSE
               OPEN(UNIT=987,FILE='FORAGE.OUT',POSITION='APPEND')
C           REWIND(987)
            ENDIF
        if (IDATE.EQ.(IYYY*1000+JDAY)) THEN
cc        DWNOD1=0.0D0    !NODULES ARE NOT HARVESTED
cc        YIELD(1)=PSTATE(6)*HRVEFF(IPM)
cc        YIELD(2)=(PSTATE(2)+PSTATE(3))*HRVEFF(IPM)*(1.0D0-RMPCT)
cc        YIELD(3)=0.0D0
cc        TP=TP*(1.0D0-RMPCT)
cc        TPN=TPN*(1.0D0-RMPCT)
c Liwang Ma        TOT8=PSTATE(4)+(1.0D0-HRVEFF(IPM))*(PSTATE(6)+TP)
c Liwang Ma        TOT8N=PNIT(4)+(1.0D0-HRVEFF(IPM))*(PNIT(6)+TPN)
C FOR ALFALFA HARVEST, EDITED BY LIWANG MA
C        TOT8=(1.0D0-HRVEFF(IPM))*(PSTATE(6)+PSTATE(4)+TP)
C        TOT8N=(1.0D0-HRVEFF(IPM))*(PNIT(6)+PNIT(4)+TPN)
cc        TOT8=0.0D0
cc        TOT8N=0.0D0
cc        TOT9=0.0D0
cc        PSTATE(8)=PSTATE(8)+TOT8
        TOTALPLNT=CLSIZE(4)+CLSIZE(5)+CLSIZE(6)+CLSIZE(7)
        FHLEAF=PSTATE(2)*TOTALPLNT*C2BM-FORMOW*RSPLF/100.D0
        FHSTEM=PSTATE(3)*TOTALPLNT*C2BM-FORMOW*(1.0D0-RSPLF/100.D0)
        FHTOTAL=FHLEAF+FHSTEM+(PSTATE(4)+PSTATE(6))*TOTALPLNT*C2BM  !FHLEAF, FHSTEM, WHICH NEED TO PRINT OUT IN A FILE
        PSTATE(2)=FORMOW/TOTALPLNT/C2BM*RSPLF/100.D0
        PSTATE(3)=FORMOW/TOTALPLNT/C2BM*(1.0D0-RSPLF/100.D0)
        PSTATE(4)=0.0D0                                        !ALL PROPAGULE GONE AT CUTTING
cc        PSTATE(5)=PSTATE(5)                                  !NO CHANGE TO ROOT
        PSTATE(6)=0.0D0                                        !ALL SEEDS ARE GONE AT CUTTING
        GS=FORGS
        PNIT(2)=PNIT(2)*FORMOW*RSPLF/100.D0/(PSTATE(2)*TOTALPLNT*C2BM)
        PNIT(3)=PNIT(3)*FORMOW*(1.0D0-RSPLF/100.D0)/
     &          (PSTATE(3)*TOTALPLNT*C2BM)
        PNIT(4)=0.0D0
        PNIT(6)=0.0D0
cc        PSTATE(7)=PSTATE(7)*RMPCT
        CLSIZE(5)=CLSIZE(5)+CLSIZE(6)+CLSIZE(7)
        CLSIZE(6)=0.0D0
        CLSIZE(7)=0.0D0
        CLSIZE(2)=0.0D0
        CLSIZE(3)=0.0D0
        CLSIZE(4)=0.0D0
CC        GS=(STEND(5)+3.0D0*STEND(4))*0.25D0
C  RESET PLANT HEIGHT BASED ON LEAF AND STEM WEIGHT
          STEM2=(PSTATE(2)+PSTATE(3))*C2BM
          HEIGHT=PLHGHT(IPL)*(1.0D0-EXP(-PLALFA(IPL)*STEM2/
     &          (2.0D0*PLHGHT(IPL))))
C         ...DETERMINE LEAF AREA
          LAI=C2BM*PSTATE(2)/(CONVLA(IPL)/PLTS27)
          TLAI=LAI                                         !NEED TO BE TESTED     
          WRITE (987,'(4X,I8, 6(5X,F9.2))') IDATE,FHLEAF,FHSTEM,FHTOTAL
     &          ,PSTATE(2)*TOTALPLNT*C2BM,PSTATE(3)*TOTALPLNT*C2BM,
     &          PSTATE(2)/PSTATE(3)
        DO IP=1,NPEST
          CORES(IP)=0.0D0
          COPLNT(IP)=0.0D0
        ENDDO
          ENDIF
          END IF
        END DO
        CLOSE (MOWLUN)
        CLOSE (987)
        ELSE
        PRINT*,'FORGMOW.DAT IS MISSING AND NO CUTTING IS SCHEDULED
     &          PLEASE COPY A TEMPLATE FROM /RZWQM2/STATUP/ FOLDER'
        ENDIF
        ENDIF
C====================== END OF ALFALFA CUTTING =================================
C         ...DETERMINE IF WE HARVEST THE CROPS
c add inxpl for soybean credit, Liwang Ma
          PRINT *, "RZMAN CALLING TMHARV"
          CALL TMHARV(CLSIZE,FIRST11,GS,HEIGHT,JDAY,PSTATE,
     +        STEND(2,IGPL),RPOOL,PCN,RDF,SDEAD,NN,IPM,PNIT,TTPLNT,
     +        TADRT,CN,RM,RESAGE,RCN,SDCN,CORES,COPLNT,NPEST,JGS,IYYY,
     +        TMPLNT,TXPLNT,IRTYPE(IPL),OMSEA,SDEAD_HEIGHT,inxpl(ipl),
     +        tadrtc,TCAG,TCBG,RCO2,PLNAME(IPL),NPGS,plden(ipm+1),
     +        plantNseed)
c          IPR=irtype(IPL)  !SET RES TYPE TO LAST HARVEST PLANT TYPE
          if (resage.eq.0.d0) ipr=irtype(IPL)
C
C         ...DETERMINE PLANT STAGE FOR MANAGEMENT APPLICATIONS
          DO 70 I=1,2
            IF(JGS(I).EQ.1.AND.JGST(I).EQ.366) THEN
              JGS(I)=JDAY
            ELSE
              JGS(I)=JGST(I)
            ENDIF
   70     CONTINUE
C
C         ..SETUP BMP OPTIONS
          IF( INDEX(PLNAME(IPL),'Soybean').NE.0.OR.
     +        INDEX(PLNAME(IPL),'soybean').NE.0.or.
     +        INDEX(PLNAME(IPL),'SOYBEAN').NE.0) THEN
c     +     .or.(INXPL(IGPL).GT.100.AND.INXPL(IGPL).LT.200)) 
            ISOYB=1
            SOYYLD=YIELD(1)/1.12D0  !change to lb/ac, liwang Ma, 2-20-2009
          ELSE
            ISOYB=0
            SOYYLD=0.0D0
          ENDIF
C
          IF(FIRST11.AND.IHARV(IPM).GT.2) THEN
            BASE=0.0D0
            HEIGHT=0.0D0
            DMDNIT=0.0D0
            JGS(1)=366
            LAI=0.0D0
            TLAI=0.0D0
            PLTSLV=0.0D0
            PWRTS=0.0D0
c            FIXN = 0.0D0     !do not know why FIXN needs to set to 0, Liwang Ma, 7-30-2008
            DO 80 I=1,NN
              RDF(I)=0.0D0
   80       CONTINUE
            write(unit=101,fmt=*) yrplt,EDAP,ADAP,MDAP,PLNAME(IPL)
            EDAP=0
            ADAP=0
            MDAP=0
          ENDIF
C-GH Liwang Ma
c        ENDIF
        JGROW=JGROW+1
C-GH     Liwang Ma
C
        ELSE IF ((INXPL(IPL).GE. 7000).AND.(INXPL(IPL).LT.8000)) THEN
C        ELSE IF(INDEX(PLNAME(IPL),'SOYGRO').NE.0 .OR.
C     +          INDEX(PLNAME(IPL),'soygro').NE.0 .OR.
C     +          INDEX(PLNAME(IPL),'pnutgro').NE.0 .OR.
C     +          INDEX(PLNAME(IPL),'PNUTGRO').NE.0 .OR.
C     +          INDEX(PLNAME(IPL),'BEANGRO').NE.0 .OR.
C     +          INDEX(PLNAME(IPL),'beangro').NE.0 .OR.
C     +          INDEX(PLNAME(IPL),'CERES').NE.0  .OR.
C     +          INDEX(PLNAME(IPL),'ceres').NE.0) THEN
C
C         ..INITIALIZE
          JGST(1) = JGS(1)
          JGST(2) = JGS(2)
c
          IF (FIRST11) THEN
C            IP2 = INDEX(PLNAME(IPL)(IP:IPEND),' ')+1
C            IPEND = INDEX(PLNAME(IPL),'  ')
C            VARNOR = PLNAME(IPL)(IP+1:IP+7)
C            VRNAME = PLNAME(IPL)(IP+8:IPEND)
            IF(INDEX(PLNAME(IPL),'SOYBEAN').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'soybean').NE.0 ) THEN
                CROPR="SB"
	          IRTYPE(IPL)=2
                IOFF=9
            ELSE IF(INDEX(PLNAME(IPL),'SUNFLOWER').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'sunflower').NE.0 ) THEN
                CROPR="SU"
	          IRTYPE(IPL)=1
                IOFF=11
            ELSE IF(INDEX(PLNAME(IPL),'bermudagrass').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'BERMUDAGRASS').NE.0 ) THEN
                CROPR="BM"
	          IRTYPE(IPL)=3
                IOFF=14
            ELSE IF(INDEX(PLNAME(IPL),'peanut').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'PEANUT').NE.0 ) THEN
                CROPR="PN"
	          IRTYPE(IPL)=2
                IOFF=8
            ELSE IF(INDEX(PLNAME(IPL),'DRYBEAN').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'drybean').NE.0 ) THEN
                CROPR="BN"
 	          IRTYPE(IPL)=2
               IOFF=9
            ELSE IF(INDEX(PLNAME(IPL),'FABABEAN').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'fababean').NE.0 ) THEN
                CROPR="FB"
 	          IRTYPE(IPL)=2
               IOFF=10
            ELSE IF(INDEX(PLNAME(IPL),'CANOLA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'canola').NE.0 ) THEN
                CROPR="CA"
 	          IRTYPE(IPL)=2
               IOFF=8
            ELSE IF(INDEX(PLNAME(IPL),'VELVETBEAN').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'velvetbean').NE.0 ) THEN
                CROPR="VB"
 	          IRTYPE(IPL)=2
               IOFF=12
            ELSE IF(INDEX(PLNAME(IPL),'BRACHIARIA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'brachiaria').NE.0 ) THEN
                CROPR="BR"
 	          IRTYPE(IPL)=2
               IOFF=12
            ELSE IF(INDEX(PLNAME(IPL),'CABBAGE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'cabbage').NE.0 ) THEN
                CROPR="CB"
 	          IRTYPE(IPL)=2
               IOFF=9
            ELSE IF(INDEX(PLNAME(IPL),'CHICKPEA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'chickpea').NE.0 ) THEN
                CROPR="CH"
 	          IRTYPE(IPL)=2
               IOFF=10
            ELSE IF(INDEX(PLNAME(IPL),'COWPEA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'cowpea').NE.0 ) THEN
                CROPR="CP"
 	          IRTYPE(IPL)=2
               IOFF=8
            ELSE IF(INDEX(PLNAME(IPL),'PEPPER').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'pepper').NE.0 ) THEN
                CROPR="PR"
 	          IRTYPE(IPL)=2
               IOFF=8
            ELSE IF(INDEX(PLNAME(IPL),'SUNFLOWER').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'sunflower').NE.0 ) THEN
                CROPR="SU"
 	          IRTYPE(IPL)=1
               IOFF=11
            ELSE IF(INDEX(PLNAME(IPL),'OILCROP-SUN').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'oilcrop-sun').NE.0 ) THEN
                CROPR="SF"
 	          IRTYPE(IPL)=1
               IOFF=13
            ELSE IF(INDEX(PLNAME(IPL),'ALFALFA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'alfalfa').NE.0 ) THEN
                CROPR="AL"
 	          IRTYPE(IPL)=2
               IOFF=9
            ELSE IF(INDEX(PLNAME(IPL),'PINEAPPLE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'pineapple').NE.0 ) THEN
                CROPR="PI"
 	          IRTYPE(IPL)=2
               IOFF=11
            ELSE IF(INDEX(PLNAME(IPL),'BAHIA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'bahia').NE.0 ) THEN
                CROPR="G0"
 	          IRTYPE(IPL)=2
               IOFF=7
            ELSE IF(INDEX(PLNAME(IPL),'COTTON').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'cotton').NE.0 ) THEN
                CROPR="CO"
 	          IRTYPE(IPL)=1
               IOFF=8
            ELSE IF(INDEX(PLNAME(IPL),'SUGARCANE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'sugarcane').NE.0 ) THEN
                CROPR="SC"
 	          IRTYPE(IPL)=1
               IOFF=11
            ELSE IF(INDEX(PLNAME(IPL),'TOMATO').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'tomato').NE.0 ) THEN
                CROPR="TM"
 	          IRTYPE(IPL)=2
               IOFF=8
            ELSE IF(INDEX(PLNAME(IPL),'MAIZE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'maize').NE.0 ) THEN
                CROPR="MZ"
	          IRTYPE(IPL)=1
                IOFF=7
            ELSE IF(INDEX(PLNAME(IPL),'SORGHUM').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'sorghum').NE.0 ) THEN
                CROPR="SG"
	          IRTYPE(IPL)=1
                IOFF=9
            ELSE IF(INDEX(PLNAME(IPL),'POTATO').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'potato').NE.0 ) THEN
                CROPR="PT"
	          IRTYPE(IPL)=2               ! need to redefine irtype for potato, Liwang Ma, 1-29-2007
                IOFF=8
            ELSE IF(INDEX(PLNAME(IPL),'WHEAT').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'wheat').NE.0 ) THEN
                CROPR="WH"
	          IRTYPE(IPL)=3
                IOFF=7
            ELSE IF(INDEX(PLNAME(IPL),'BARLEY').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'barley').NE.0 ) THEN
                CROPR="BA"
	          IRTYPE(IPL)=3
                IOFF=8
            ELSE IF(INDEX(PLNAME(IPL),'MILLET-PRL').NE.0 .OR.    !pearl millet
     +        INDEX(PLNAME(IPL),'millet-prl').NE.0 ) THEN
                CROPR="ML"
	          IRTYPE(IPL)=3
                IOFF=12
            ELSE IF(INDEX(PLNAME(IPL),'MILLET-PRO').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'millet-pro').NE.0 ) THEN
                CROPR="MO"
	          IRTYPE(IPL)=3
                IOFF=12
            ELSE IF(INDEX(PLNAME(IPL),'MILLET-FOX').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'millet-fox').NE.0 ) THEN
                CROPR="MX"
	          IRTYPE(IPL)=3
                IOFF=12
            ELSE IF(INDEX(PLNAME(IPL),'TRITICALE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'triticale').NE.0 ) THEN
                CROPR="TR"
	          IRTYPE(IPL)=3
                IOFF=11
            ELSE IF(INDEX(PLNAME(IPL),'SUGARBEET').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'sugarbeet').NE.0 ) THEN
                CROPR="BS"
	          IRTYPE(IPL)=2
                IOFF=11
            ENDIF            
            IPEND = LEN_TRIM(PLNAME(IPL))
            VARNOR = PLNAME(IPL)(IOFF:IOFF+6)
            VRNAME = PLNAME(IPL)(IOFF+7:IPEND)

C             DO 301 I = 1, 10
            DO 301 I = 1, 3
              if (ihday(ipm,3).ne.0) then
              HDATE(I) = IHDAY(IPM,3)*1000 + 
     +         JDATE(IHDAY(IPM,1),IHDAY(IPM,2),IHDAY(IPM,3))
              else
              hdate(i) = -99	
              endif		 
C              HDATE(I) = IYYY * 1000 + IHDAY(IPM,I)
  301       CONTINUE
C
          ENDIF

C         .. SOIL LAYER INPUTS FOR DSSAT MODELS
          DO 68 I = 1,NN
            JH = NDXN2H(I)
C             ..ADD IN ALL THIS LAYER
              CGTLT(I) = real(TLT(I))
              CGTH(I)  = REAL(THETA(I))
              CGT(I)   = REAL(T(I))
c changed to take account of field saturation AEF
              CGSAT(I) = REAL(SOILHP(6,JH))
c              CGSAT(I) = REAL(SOILHP(6,JH)*AEF)
cc              CGDUL(I) = REAL(SOILHP(7,JH))
cc              CGLL(I)  = REAL(SOILHP(9,JH))
c              CGDUL(I)  = real(WC(-100.0d0,SOILHP(1,JH),jh,0))
c              CGLL(I)  = real(WC(-20000.0d0,SOILHP(1,JH),jh,0))
               CGDUL(I)=real(WC(HFC,SOILHP(1,JH),JH,0))
               CGLL(I)=real(WC(HWP,SOILHP(1,JH),JH,0))
              CGBD(I) = real(soilpp(3,jh))
c Liwang Ma 8/13/2003
c              CGNO3(I) = REAL(CC(I,9)*THETA(I)/SOILPP(3,JH))
c              CGNH4(I) = REAL(CC(I,10)*THETA(I)/SOILPP(3,JH))
              CGNO3(I) = REAL(CC(I,9)*THETA(I))*0.1
              CGNH4(I) = REAL(CC(I,10)*THETA(I))*0.1
              TOTPUPP = REAL(TOTPUP)
c              TLTR(I) = REAL(TLT(I))
              CGPH(I) = real(-LOG10(CC(I,1)))
              RZOC(I) = REAL(RZSOILC(I))
              RZON(I) = REAL(RZSOILN(I))
 68       CONTINUE
CZ-MA
              CGANO3 = REAL (TSNO3_C)
              CGANH4 = REAL (TSNH4_C + TSNH4P_C + TSUR_C)
              CGTOTO = REAL (TOTO_C)
              CGTOTP = REAL (TOTP_C)
              CGTOTF = REAL (TOTF_C)
              CGTSHP = REAL (TSHP_C)
              TDAYR = TDAY
C              DO I=1,MHR
C              IF (SSURFT(I).GT.-273.0D0) THEN
C                  STMAX=REAL(SSURFT(I))
C                  STMIN=REAL(SSURFT(I))
C              ENDIF
C              ENDDO
C              DO I=1,MHR
C              IF (SSURFT(I).GT.-273.0D0) THEN
C                  STMAX=MAX(STMAX,REAL(SSURFT(I)))
C                  STMIN=MIN(STMIN,REAL(SSURFT(I)))
C              ENDIF
C              ENDDO
CZ-MA
C
C         ..CONVERT PLANT DENSITY FROM 10 PLANTS/M^2 TO PLANTS/M^2
c          PLTPPR = PLDEN(IPM) / 10.0d0
c           temppop=plden(ipm)
c          IF(INDEX(PLNAME(IPL),'ALFALFA').NE.0.
c     &        and.temppop.eq.0.0d0) then
c           do i=1,mxapp
c              Temppop=plden(ipm-i)
c              if (temppop.gt.0.0d0) goto 123
c           enddo
c           endif
c123       continue
          PLTPPR = PLDEN(IPM) / 10.0d0
C
C         ..CONVERT LATITUDE AND ASPECT FROM RADIANS TO DEGREES
          XLATR = XLAT * R2D   
          AZIRR = ASPECT * R2D 
          XLONGR = XLONG * R2D
          YRSIMRS = IYB  * 1000 + JBDAY
          YRSIMR =  IYYY * 1000 + JDAY
C-GH
C          Obtain water balance variables
c          RAIN   = REAL(TRFDD_C - AIRR_C + SMELT_C)
           RAIN   = REAL(TRFDD_C - AIRR_C)
           CRAINR  = REAL(TTRFDD_C)
C           CIRR    = REAL (TTIRR_C)
C
          RUNOFF = REAL(TROI_C)
          DRAIN  = REAL(DTSEEP_C)
          DEPIR  = max(REAL(AIRR_C - SMELT_C),0.0)
C
C          RADHGT=(PLDIAM(IPL)*0.5D0)/PLHGHT(IPL)
c         write(*,234)TPUP,PDTRAN,DMDNIT,TNITUP,yrsimr
c234       FORMAT(/,' POTENTIAL UPTAKE (CM)',G12.2,
c     +        ' POTENTIAL TRANSPIRATION (CM)',G12.3,/,
c     +        ' POTENTIAL NITROGEN DEMAND (G/PLT)',F8.2,
c     +        ' ACTUAL NITROGEN UPTAKE (G/PLT)',F8.2,
c     +        ' YRDOY ',I8)
c         write(*,235)ACTTRN,ACTEVP,yrsimr
c235       FORMAT(/,' ACTUAL TRANSP (CM)',G12.2,
c     +        ' ACTUAL EVAP (CM)',G12.3,/,
c     +        ' YRDOY ',I8)
c         write(*,236) ACTTRN,TPUP,PET,ACTEVP,(PET+PER+PES),YRSIMR
c236       FORMAT(/,' ACTUAL TRANSPIRATION (CM)',G12.3,
c     +        ' POTENTIAL UPTAKE (CM)',G12.3,
c     +        ' POTENTIAL TRANSPIRATION (CM)',G12.3,/,
c     +        ' SOIL EVAPORATION (CM)',G12.3,
c     +        ' POTENTIAL EVAPOTRANSPIRATION (CM)',G12.3,
c     +        ' YRDOY ',I8)
C
C
C         .. CALL CROPGRO ROUTINES
c-liwang ma          IF (CROPR .EQ. 'SB' .OR. CROPR .EQ. 'PN' .OR. CROPR .EQ.
c-liwang ma     &        'BN') THEN

C          CALL GRODRV(XLATR,TMAX,TMIN,RTS,XW,XW,
C     &            CGTH,CGT,CGSAT,CGDUL,CGLL,DSLT,NN,CGNO3,CGNH4,CGTLT,
C     &            ACTTRN,PET,ACTEVP,PER,PES,
C     &            CROPR,VARNOR,YRSIMR,PLTPPR,ROWSP(IPM),AZIRR,
C     &            TLT(LAYNDX(IPM)),
C     &            BASE,DMDNIT,FIRST11,FIXN,GS,HEIGHT,JGS,LAI,
C     &            PLTSLV,PWRTS,RDFR,RLVR,TNITUP,WCG(IPL),TOTPUPP,
C     &            RAIN,RUNOFF,DRAIN,DEPIR,CGANO3,CGANH4,CGTOTO,CGTOTP,
C     &            CGTOTF,CGTSHP,TDAYR,cgbd,RZPAR,CGPH,RZOC,YRSIMRS,
C     &            HDATE,OMSEA,TLAI,CN,RPOOL,TADRT,VRNAME,CRAINR,RM,RCN,
C     &            FIRST15,resage,nreps,yield,ihtype(ipl),iharv(ipl),
C     &            PLHGHT(IPL),PLALFA(IPL),RADHGT,NLAYRI)
c	     if (resage.eq.0) ipr=IPL
C
c-liwang ma          ELSE IF (CROPR .EQ. 'MZ'.OR.CROPR.EQ.'WH') THEN
C         .. CALL CERES ROUTINES
          PRINT *, "RZMAN CALLING DSSATDRV"
          CALL DSSATDRV(XLATR,TMAX,TMIN,RTS,XW,XW,
     &            CGTH,CGT,CGSAT,CGDUL,CGLL,DSLT,NN,CGNO3,CGNH4,CGTLT,
     &            ACTTRN,PET,ACTEVP,PER,PES,
     &            CROPR,VARNOR,YRSIMR,PLTPPR,ROWSP(IPM),AZIRR,
     &            PLDEPTH(IPM),iptype(ipm),totminr,totimm,totden,totvol,
     &            BASE,DMDNIT,FIRST11,FIXN,GS,HEIGHT,JGS,LAI,
     &            PLTSLV,PWRTS,RDFR,RLVR,TNITUP,WCG(IPL),TOTPUPP,
     &            RAIN,RUNOFF,DRAIN,DEPIR,CGANO3,CGANH4,CGTOTO,CGTOTP,
     &            CGTOTF,CGTSHP,TDAYR,cgbd,RZPAR,CGPH,RZOC,RZON,YRSIMRS,
     &            HDATE,OMSEA,TLAI,CN,RPOOL,TADRT,VRNAME,CRAINR,RM,RCN,
     &            FIRST15,nreps,yield,ihtype(ipm),iharv(ipm),
     &            RADHGT,NLAYRI,
     &            REAL(HRVEFF(IPm))*100.0,TADRTC,CO2R,W,iswpar,
     &            real(sdwtplr(ipm)),real(sdager(ipm)),
     &            real(sprlapr(ipm)),iemrg(ipm),soltyp(1),
     &            TOTFERT,TOTMANU,REAL(TTIRR_C),real(A0),iweather,
     &            upnitr,SSURFT,RCO2,resage,rwl,rootn,imagic)
c
          if (resage.eq.0.d0) ipr=irtype(IPL)
        CALL VGATE(TDAY,51,RWL)
        CALL VGATE(TDAY,35,ROOTN)
C
c-liwang ma          ENDIF
          if (omsea(63).gt.1.0d0) then
            IR=NDXT2N(INT(OMSEA(63)+0.5D0))
          else 
            ir = 1
          endif
C-rz liwang Ma
          DO 69 I = 1,NLAYRI
              JH = NDXN2H(I)
              CC(I,9)=DBLE(CGNO3(I))/(THETA(I)*0.1D0)
              CC(I,10)=DBLE(CGNH4(I))/(THETA(I)*0.1D0)
              RDF(I) = DBLE(RDFR(I))
              tnup(i)=dble(upnitr(i)*TL(i))
 69       CONTINUE
          TLPLNT = TNITUP * PWRTS * 1.0D-3
          CALL VGATE(TDAY,36,TNUP)
c-rz Liwang Ma
C            PRINT*,' TOTAL ROOT FRACTION DIST: ', TRDF
C
C         .. CHECK FOR HARVEST AND REINITIALIZE
          IF (FIRST11) THEN
            CALL CDATE(JDAY,ID,IM,IYYY)
            WRITE(70,1000)
            WRITE(70,1100) plname(ipl),ID,IM,IYYY,JDAY
c            WRITE(70,1200)PLTSLV,YIELD(1)
            TCAG=OMSEA(41)
            TCBG=OMSEA(42)
            TNAG=OMSEA(22)
            TNBG=OMSEA(23)
            IF (TCAG.gt.0.0d0) HI=YIELD(1)/TCAG
            WRITE(70,1200) PLTSLV,(YIELD(I),I=1,3),HI,TNAG,
     +                     TNBG,TCAG,TCBG
            WRITE(70,1000)
            BASE   = 0.0D0
            HEIGHT = 0.0D0
            DMDNIT = 0.0D0
            JGS(1) = 366
            TLAI    = 0.0D0
            LAI    = 0.0D0
            PLTSLV = 0.0D0
            PWRTS  = 0.0D0
            TOTFERT = 0.0D0
            TOTMANU = 0.0D0
            TTIRR_C = 0.0D0
c            FIXN = 0.0D0    !do not know why FIXN needs to set to 0, Liwang Ma, 7-30-2008
            DO 43 I = 1,NN
              RDF(I) = 0.0D0
 43         CONTINUE
          ENDIF
C         ...DETERMINE PLANT STAGE FOR MANAGEMENT APPLICATIONS
          DO 416 I = 1, 2
            IF (JGS(I).EQ.1.AND.JGST(I).EQ.366) THEN
              JGS(I) = JDAY
            ELSE
              JGS(I) = JGST(I)
            END IF
  416     CONTINUE
          JGROW = JGROW + 1
        END IF
C-GH
c
      ELSE
C
C
C       ..ZERO OUT ALL THE PLANT GROWTH OUTPUT VARIABLES
        IF(NSC.GE.1) THEN
          DO 90 I=11,14
            CALL SGATE(TDAY,I,0.0D0)
   90     CONTINUE
          DO 100 I=16,18
            CALL SGATE(TDAY,I,0.0D0)
  100     CONTINUE
          CALL SGATE(TDAY,41,0.0D0)
          DO 110 I=49,73
            CALL SGATE(TDAY,I,0.0D0)
  110     CONTINUE
          DO 120 I=79,81
            CALL SGATE(TDAY,I,0.0D0)
  120     CONTINUE
          CALL VGATE(TDAY,51,RDF)
          CALL VGATE(TDAY,35,RDF)
        ENDIF
      ENDIF
C
C     .. ERROR DETECTION FOR TOO LARGE LAI
      IF(LAI.GT.40) THEN
        PRINT*,' LEAF AREA INDEX HAS EXCEEDED MAX (40.0) ==>',LAI
        STOP' TOO LARGE LAI'
      ENDIF
C find the soil layer where roots are grown.
        ir=0
        do i=1,nn
           if (tlt(i).le.omsea(63)) then
               ir=i
           endif
        enddo
C
      RETURN
 1000 FORMAT(70('*'))
 1100 FORMAT(/4('-'),'DSSAT Crop Harvest','----',A30,/
     +       4x,'ON ',I3,'/',I2,'/',I4,' -----',I6,'  ----'/)
 1110 FORMAT(/4('-'),'HERMES Crop Harvest','----',A30,/
     +       4x,'ON ',I3,'/',I2,'/',I4,' -----',I6,'  ----'/)
c 1200 FORMAT(//,'HARVEST YIELDS FROM ',F12.1,' PLANTS',/50('='),/,
c     +    ' YIELD FROM SEEDS [KG/HA]:',T50,F12.2//)
 1200 FORMAT(/,'HARVEST YIELDS FROM ',F12.1,' PLANTS',/50('='),/,
     +    ' YIELD FROM SEEDS/TUBER [KG/HA]:',T50,F12.2,/,
     +    ' YIELD FROM ABOVE GROUND BIOMASS [KG/HA]:',T50,F12.2,/,
     +    ' YIELD FROM ROOT BIOMASS [KG/HA]:',T50,F12.2,/,
     +    ' HARVEST INDEX:',T50,F12.2,//,
     +    ' ----NITROGEN DISTRIBUTION AT TIME OF HARVEST----',/,
     +    '     --TOTAL ABOVE GROUND NITROGEN [KG-N/HA]:',T50,F12.2,/,
     +    '     --TOTAL BELOW GROUND NITROGEN [KG-N/HA]:',T50,F12.2,//,
     +    ' ----BIOMASS DISTRIBUTION AT TIME OF HARVEST----',/,
     +    '           (BIOMASS ARE IN DRY WEIGHT ONLY)'/,
     +    '     --TOTAL ABOVE GROUND BIOMASS [KG-BM/HA]:',T50,F12.2,/,
     +    '     --TOTAL BELOW GROUND BIOMASS [KG-BM/HA]:',T50,F12.2,//)
      END
C
      SUBROUTINE TMHARV(CLSIZE,FIRST12,GS,HEIGHT,JDAY,PSTATE,STEND,
     +    RPOOL,PCN,RDF,SDEAD,NN,IPM,PNIT,TTPLNT,TADRT,CN,RM,RESAGE,RCN,
     +    SDCN,CORES,COPLNT,NPEST,JGS,IYYY,TMPLNT,TXPLNT,IRES,OMSEA,
     +    SDEAD_HEIGHT,Iinxpl,TADRTC,TCAG,TCBG,RCO2,PLNAME,NPGS,plden1,
     +    plantNseed)
C======================================================================
C
C       PURPOSE:
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       CLSIZE     I
C       PCN        I  PLANT C:N RATIOS BY LAYER
C       FIRST  I  TRUE IF FIRST TIME THROUGH ROUTINE
C       GS         I  GROWTH STAGE [0..1]
C       HEIGHT     I
C       HPERC  I
C       HRVEFF     I
C       HSTAGE     I
C       I      L  INDEX VARIABLE
C       IHARV  I
C       IHCL   I
C       IHDAY  I
C       IHTYPE     I
C       IPM        I
C       IRES   I  INDEX TO RESIDUE TYPE (CORN,SOYBEAN,WHEAT...)
C       JDAY   I  JULIAN DAY    [1..366]
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODT     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       MXSPEC     P
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       PERC   L  PERCOLATION INTO IMAGE LAYER = FLOW PAST
C                 LOWER BOUNDARY OF BOTTOM-MOST REAL LAYER.[CM/DAY]
C       PNIT   I
C       PSTATE     I  1-ALLOCABLE CARBON 
C                     2-LEAF CARBON
C                     3-STEM CARBON
C                     4-PROPAGULE CARBON
C                     5-ROOT CARBON
C                     6-SEED CARBON
C                     7-STANDING DEAD
C                     8-LITTER CARBON
C                     9-DEAD ROOT
C       RDF        I  FRACTIONAL DISTRIBUTION OF ROOTS BY LAYER [0..1]
C       RPOOL  I  RESIDUE POOL, BOTH FAST AND SLOW DECOMPOSERS
C                 [0]--SURFACE RESIDUE,  [1] SLOW DECOMP RESIDUE
C                 [1..NN]--MIXED SOIL RES  [2] FAST DECOMP RESIDUE
C                 UNITS = [G-CARBON / CM^2-SOIL]
C       SDEAD  I  STANDING DEAD MATERIAL [KG/HA]
C       SDCN   I  STANDING DEAD C:N RATIO
C       SINCL  L
C       STEND  I
C       STUBHT     I
C       SUMC27     L
C       SUMCL  L
C       TTPLNT     I  CUMMULATED PLANT NITROGEN UPTAKE [KG/HA]
C       YIELD  I
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                 HARVST
C                 C
C
C       CALLED FROM:
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:   2
C
C-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,MXAPP=200,C2BM=2.5D0,MXANA=135)
C
C  PASSED VARIABLES
      DIMENSION CLSIZE(0:7),PSTATE(9),STEND(2:6),RPOOL(MXNOD,2),
     +    PCN(0:NN),RDF(NN),PNIT(9),SUMCL(4),TADRT(2),CN(9),
     +    CORES(NPEST),COPLNT(NPEST),JGS(2),OMSEA(MXANA),TADRTC(2)
      LOGICAL FIRST12,FHARV
      CHARACTER PLNAME*30
C
      INTEGER IHTYPE,IHDAY,IHARV,IHCL,IRES
      COMMON /HRVST/ HSTAGE(MXAPP),HPERC(MXAPP),HRVEFF(MXAPP),YIELD(3),
     +    STUBHT(MXAPP),IHTYPE(MXAPP),IHDAY(MXAPP,3),IHARV(MXAPP),
     +    IHCL(MXAPP)
      SAVE FHARV
      DATA FHARV /.FALSE./
C
      SINCL=0.0D0
      SUMCL(4)=0.0D0
      SUMC27=0.0D0
      DO 10 I=2,7
        SUMC27=SUMC27+CLSIZE(I)
        IF(I.GE.IHCL(IPM)) SINCL=SINCL+CLSIZE(I)
        IF(I.GE.5.AND.I.LE.7) SUMCL(4)=SUMCL(4)+CLSIZE(I)
   10 CONTINUE
      DO 20 I=1,3
        SUMCL(I)=0.0D0
        IF(SUMCL(4).GT.0.0D0) SUMCL(I)=CLSIZE(4+I)/SUMCL(4)
   20 CONTINUE
C
C     ..DETERMINE HARVEST PLANT TOTALS FOR BIOMASS AND NITROGEN
      TCA=PSTATE(2)+PSTATE(3)+PSTATE(7)
      TCAG=((TCA*SUMCL(1)+(TCA+PSTATE(4))*SUMCL(2)+(TCA+PSTATE(4)+
     +    PSTATE(6))*SUMCL(3))*C2BM*SUMCL(4))
      TCBG=PSTATE(5)*SUMCL(4)*C2BM
      TNBG=PNIT(5)*SUMCL(4)
      TNGG=PNIT(6)*SUMCL(4)
      TNAG=TTPLNT-TNBG+plantNseed*sumcl(4)
      TGRAIN=PSTATE(6)*SUMCL(4)*C2BM
C
C     ..HARVEST BASED ON GROWTH STAGE THRESHOLD
      IF(IHTYPE(IPM).EQ.1) THEN
        IF(GS.GE.HSTAGE(IPM)) FHARV=.TRUE.
C
C     ..HARVEST BASED ON PHENOLOGICAL CLASS THRESHOLD
      ELSEIF(IHTYPE(IPM).EQ.2) THEN
        IF(SUMC27.EQ.0.0D0) THEN
          PERC=0.0D0
        ELSE
          PERC=SINCL/SUMC27
        ENDIF
        IF(PERC.GE.HPERC(IPM)) FHARV=.TRUE.
C
C     ..HARVEST BASED ON USER SPECIFIED HARVEST DATE
      ELSE
        DO 30 I=1,3
          if ((IYYY*1000+JDAY).eq.(IHDAY(IPM,3)*1000+ 
     +     JDATE(IHDAY(IPM,1),IHDAY(IPM,2),IHDAY(IPM,3)))) FHARV=.TRUE.
c          IF(JDAY.EQ.IHDAY(IPM,I)) FHARV=.TRUE.
   30   CONTINUE
      ENDIF
C
C     ..HARVEST ACTIVITY WAS TRIGGERED
      IF(FHARV) THEN
c          if ((plden1.gt.0.0d0).and.(INDEX(PLNAME,'ALFALFA').NE.0)) then
c             plname='Final-Alf'
c             iharv(ipm)=4
c          endif
c
        PRINT *, "RZMAN CALLING HARVST"
        CALL HARVST(PSTATE,IHARV,HRVEFF,YIELD,CLSIZE,STEND,GS,STUBHT,
     +      HEIGHT,SUMCL,FIRST12,PCN,RDF,JDAY,SDEAD,NN,IPM,RPOOL,PNIT,
     +      TADRT,CN,RM,RESAGE,RCN,SDCN,CORES,COPLNT,NPEST,IYYY,TMPLNT,
     +      TXPLNT,IRES,TCAG,TCBG,TNAG,TNBG,Iinxpl,TADRTC,RCO2,PLNAME,
     +      plantNseed)
C        
c      IF (INDEX(PLNAME,'ALFALFA').EQ.0) THEN
        JGS(2)=1
        SDEAD_HEIGHT=HEIGHT
        FHARV=.FALSE.
c      ELSE
C          JGS(2)=366
C          FHARV=.TRUE.
c        JGS(2)=1
c        if ((ipm.lt.npgs).and.(plden1.eq.0)) IPM=IPM+1
c        if (plden1.gt.0.0d0) FIRST12=.true.
c           FHARV=.FALSE.
c          ipm=ipm+1
      ENDIF
C
c      ENDIF
C
C     ..SETOUT MSEA OUTPUT
      OMSEA(21)=TTPLNT
      OMSEA(22)=TNAG
      OMSEA(23)=TNBG
      OMSEA(24)=TNGG
      OMSEA(41)=TCAG
      OMSEA(42)=TCBG
      OMSEA(44)=TGRAIN
C
      RETURN
      END
C
      SUBROUTINE HARVST(PSTATE,IHARV,HRVEFF,YIELD,CLSIZE,STEND,GS,
     +    STUBHT,HEIGHT,SUMCL,FIRST13,PCN,RDF,JDAY,SDEAD,NN,IPM,RPOOL,
     +    PNIT,TADRT,CN,RM,RESAGE,RCN,SDCN,CORES,COPLNT,NPEST,IYYY,
     +    TMPLNT,TXPLNT,IRES,TCAG,TCBG,TNAG,TNBG,Iinxpl,TADRTC,RCO2,
     +    PLNAME,plantNseed)
C
C======================================================================
C
C       PURPOSE:  THIS ROUTINE DETERMINES HARVEST YIELD AND AMOUNT OF
C             LITTER AND DEAD ROOTS ADDED TO CARBON POOLS
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       C2BM   P  CONVERTS FROM CARBON TO BIOMASS UNITS
C       CKG        P  CONVERTS FROM G-C/CM^2 ==> KG/HA
C       CLSIZE    I/O
C       PCN        I  PLANT C:N RATIOS BY LAYER
C       FACT   L
C       FIRST I/O TRUE IF FIRST TIME THROUGH ROUTINE
C       GS        I/O GROWTH STAGE [0..1]
C       HEIGHT    I/O
C       HRVEFF     I
C       I      L  INDEX VARIABLE
C       IHARV I/O
C       IPM        I
C       JDAY   I  JULIAN DAY    [1..366]
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       PNIT   I
C       PSTATE     I  1-ALLOCABLE CARBON 
C                     2-LEAF CARBON
C                     3-STEM CARBON
C                     4-PROPAGULE CARBON
C                     5-ROOT CARBON
C                     6-SEED CARBON
C                     7-STANDING DEAD
C                     8-LITTER CARBON
C                     9-DEAD ROOT
C       RDF        I  FRACTIONAL DISTRIBUTION OF ROOTS BY LAYER [0..1]
C       RMPCT  L
C       RPOOL I/O RESIDUE POOL, BOTH FAST AND SLOW DECOMPOSERS
C                 [0]--SURFACE RESIDUE,  [1] SLOW DECOMP RESIDUE
C                 [1..NN]--MIXED SOIL RES  [2] FAST DECOMP RESIDUE
C                 UNITS = [G-CARBON / CM^2-SOIL]
C       S      L  SLOPE OF FIELD [RAD]
C       SDEAD I/O STANDING DEAD MATERIAL [KG/HA]
C       SDCN  I/O STANDING DEAD C:N RATIO
C       STEND  I
C       STUBHT     I
C       SUMCL  I
C       TCA        L
C       TCAG   L
C       TCBG   L
C       TNAG   L
C       TNBG   L
C       TOT7   L
C       TOT8   L
C       TOT9   L
C       TP         L  DAYS ELAPSED SINCE PESTICIDE SIMULATION BEGINS
C       YIELD I/O
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                 NONE
C
C       CALLED FROM:
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:   2
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,C2BM=2.5D0,MXAPP=200,CKG=1.0D5,MXPEST=3)
      DIMENSION CLSIZE(0:7),HRVEFF(MXAPP),PSTATE(9),STEND(2:6),
     +    STUBHT(MXAPP),YIELD(3),IHARV(MXAPP),PCN(0:NN),RDF(NN),
     +    RPOOL(MXNOD,2),PNIT(9),SUMCL(4),TADRT(2),CN(9),CORES(MXPEST),
     +    COPLNT(MXPEST),TADRTC(2)
      LOGICAL FIRST13
      CHARACTER PLNAME*30
      double precision CNOD,DWNOD,DWNODA,NDTH,NFIXN,NODGR,WTNFX,
     +  SENNOD(300),DNOD,CTONOD,CTONODS,WNDOT,WTNOO,NNOFF,WTNNO,PRONOD
      COMMON/NFIX1/CNOD,DWNOD,DWNODA,NDTH,NFIXN,NODGR,WTNFX,
     +  SENNOD,DNOD,CTONOD,CTONODS,WNDOT,WTNOO,NNOFF,WTNNO,PRONOD
C
C     INITIALIZE
      TOT7=0.0D0
      TOT8=0.0D0
      TOT9=0.0D0
C
      IF(HEIGHT.GT.0.0D0) THEN
        RMPCT=STUBHT(IPM)/HEIGHT
        RMPCT=MIN(RMPCT,1.0D0)
      ELSE
        RMPCT=0.0D0
      ENDIF
C
      IF(IHARV(IPM).NE.5) THEN
        HEIGHT=STUBHT(IPM)
      ELSE
        HEIGHT=0.0D0
      ENDIF
C
C     ..GET TOTAL CARBON AND NITROGEN (+ LUXURY AMT) IN THE LEAVES, STEMS,
c take soybean credit into account for corn-soybean rotation, Liwang Ma
        if ((Iinxpl.gt.100).and.(Iinxpl.lt.200)) then
          ANODULE = MIN(pstate(6)*2.0D-2,45.0D0)
     +            *6.25D0/0.3D0
c from Schapers amd Moiser (1991), NLEAP book edited by Ron Follett
c          ANODULE = MIN(yield(1)*sumcl(4)*c2bm/67.2d0*1.5d0,45.0D0)
c     +            /cn(2)*c2bm
        ELSE
          ANODULE = 0.0D0
        ENDIF
c
C     ..PARTITION DEAD ROOT CARBON  :::USE DWL FOR LAYER DIST
C CHNAGE MADE BY LIWANG MA TO ACCOUT FOR SOYBEAN CREDIT
C     AND STANDING DEAD
C
      TP=PSTATE(2)+PSTATE(3)+PSTATE(7)
      IF(SUMCL(4).GT.0.0D0) THEN
        TPN=PNIT(2)+PNIT(3)+PNIT(7)+(TXPLNT/SUMCL(4))
      ELSE
        TPN=PNIT(2)+PNIT(3)+PNIT(7)
      ENDIF
C
cc      IF (INDEX(PLNAME,'ALFALFA').NE.0) THEN
C        FIRST13=.TRUE.
cc        DWNOD1=0.0D0    !NODULES ARE NOT HARVESTED
cc        YIELD(1)=PSTATE(6)*HRVEFF(IPM)
cc        YIELD(2)=(PSTATE(2)+PSTATE(3))*HRVEFF(IPM)*(1.0D0-RMPCT)
cc        YIELD(3)=0.0D0
cc        TP=TP*(1.0D0-RMPCT)
cc        TPN=TPN*(1.0D0-RMPCT)
c Liwang Ma        TOT8=PSTATE(4)+(1.0D0-HRVEFF(IPM))*(PSTATE(6)+TP)
c Liwang Ma        TOT8N=PNIT(4)+(1.0D0-HRVEFF(IPM))*(PNIT(6)+TPN)
C FOR ALFALFA HARVEST, EDITED BY LIWANG MA
C        TOT8=(1.0D0-HRVEFF(IPM))*(PSTATE(6)+PSTATE(4)+TP)
C        TOT8N=(1.0D0-HRVEFF(IPM))*(PNIT(6)+PNIT(4)+TPN)
cc        TOT8=0.0D0
cc        TOT8N=0.0D0
cc        TOT9=0.0D0
cc        PSTATE(8)=PSTATE(8)+TOT8
cc        PSTATE(2)=PSTATE(2)*RMPCT
cc        PSTATE(3)=PSTATE(3)*RMPCT
cc        PSTATE(4)=PSTATE(4)*RMPCT
cc        PSTATE(5)=PSTATE(5)
cc        PSTATE(6)=PSTATE(6)*RMPCT
c        IF(PNIT(7).GT.0.0D0) THEN
c          SDCN=PSTATE(7)/PNIT(7)
c        ELSE
c          SDCN=CN(1)
c        ENDIF
cc        PSTATE(7)=PSTATE(7)*RMPCT
cc        CLSIZE(5)=CLSIZE(5)+CLSIZE(6)+CLSIZE(7)
cc        CLSIZE(6)=0.0D0
cc        CLSIZE(7)=0.0D0
cc        CLSIZE(2)=0.0D0
cc        CLSIZE(3)=0.0D0
cc        CLSIZE(4)=0.0D0
cc        GS=(STEND(5)+3.0D0*STEND(4))*0.25D0
cc        DO IP=1,NPEST
cc          CORES(IP)=0.0D0
cc          COPLNT(IP)=0.0D0
cc        ENDDO
!***********************************************************************
!     HARVEST NODULES
!***********************************************************************
C      CNOD   = 0.0D0
C      DWNOD  = 0.0D0    
C      NDTH   = 0.0D0    
C      NFIXN  = 0.0D0    
C      NODGR  = 0.0D0    
C      WTNFX  = 0.0D0    
C      do i=1,nn
C      SENNOD(i) = 0.0D0 
C      enddo
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
C      DWNODA = 0.0D0  
C      DNOD   = 30.0D0   !why set up to 50 later?
!***********************************************************************
cc      ELSE
C
cc       DWNOD1=DWNOD     !RETURN ALL NODULES TO ROOT BIOMASS LATER
cc       DWNOD=0.0D0
C     ..HARVEST TYPE:: MULTIPLE HARVEST, YIELD SEEDS ONLY
      IF(IHARV(IPM).EQ.1) THEN
        YIELD(1)=PSTATE(6)*HRVEFF(IPM)
        YIELD(2)=0.0D0
        YIELD(3)=0.0D0
        TOT8=PSTATE(4)+(1.0D0-HRVEFF(IPM))*PSTATE(6)+(1.0D0-RMPCT)*TP
        TOT8N=PNIT(4)+(1.0D0-HRVEFF(IPM))*PNIT(6)+(1.0D0-RMPCT)*TPN
        PSTATE(8)=PSTATE(8)+TOT8
        PSTATE(2)=PSTATE(2)*RMPCT
        PSTATE(3)=PSTATE(3)*RMPCT
        PSTATE(4)=0.0D0
        PSTATE(6)=0.0D0
c        IF(PNIT(7).GT.0.0D0) THEN
c          SDCN=PSTATE(7)/PNIT(7)
c        ELSE
c          SDCN=CN(1)
c        ENDIF
        PSTATE(7)=PSTATE(7)*RMPCT
C
        CLSIZE(5)=CLSIZE(5)+CLSIZE(6)+CLSIZE(7)
        CLSIZE(6)=0.0D0
        CLSIZE(7)=0.0D0
C
        GS=(STEND(5)+3.0D0*STEND(4))*0.25D0
        DO 10 IP=1,NPEST
          CORES(IP)=CORES(IP)+COPLNT(IP)
          COPLNT(IP)=0.0D0
   10   CONTINUE
C
C     ..HARVEST TYPE:: MULTIPLE HARVEST, YIELD ALL ABOVE GROUND BIOMASS
      ELSEIF(IHARV(IPM).EQ.2) THEN
        YIELD(1)=PSTATE(6)*HRVEFF(IPM)
        YIELD(2)=(PSTATE(2)+PSTATE(3))*HRVEFF(IPM)*(1.0D0-RMPCT)
        YIELD(3)=0.0D0
        TP=TP*(1.0D0-RMPCT)
        TPN=TPN*(1.0D0-RMPCT)
c Liwang Ma        TOT8=PSTATE(4)+(1.0D0-HRVEFF(IPM))*(PSTATE(6)+TP)
c Liwang Ma        TOT8N=PNIT(4)+(1.0D0-HRVEFF(IPM))*(PNIT(6)+TPN)
        TOT8=(1.0D0-HRVEFF(IPM))*(PSTATE(6)+PSTATE(4)+TP)
        TOT8N=(1.0D0-HRVEFF(IPM))*(PNIT(6)+PNIT(4)+TPN)
        PSTATE(8)=PSTATE(8)+TOT8
        PSTATE(2)=PSTATE(2)*RMPCT
        PSTATE(3)=PSTATE(3)*RMPCT
        PSTATE(4)=0.0D0
        PSTATE(6)=0.0D0
c        IF(PNIT(7).GT.0.0D0) THEN
c          SDCN=PSTATE(7)/PNIT(7)
c        ELSE
c          SDCN=CN(1)
c        ENDIF
        PSTATE(7)=PSTATE(7)*RMPCT
        CLSIZE(5)=CLSIZE(5)+CLSIZE(6)+CLSIZE(7)
        CLSIZE(6)=0.0D0
        CLSIZE(7)=0.0D0
        GS=(STEND(5)+3.0D0*STEND(4))*0.25D0
        DO 20 IP=1,NPEST
          CORES(IP)=0.0D0
          COPLNT(IP)=0.0D0
   20   CONTINUE
C
C     ..HARVEST TYPE:: SINGLE HARVEST, YIELD SEEDS ONLY
      ELSEIF(IHARV(IPM).EQ.3) THEN
        FIRST13=.TRUE.
        YIELD(1)=PSTATE(6)*HRVEFF(IPM)
        YIELD(2)=0.0D0
        YIELD(3)=0.0D0
        TOT7=RMPCT*TP
        TOT8=(1.0D0-RMPCT)*TP+PSTATE(6)*(1.0D0-HRVEFF(IPM))+PSTATE(4)
        TOT8N=(1.0D0-RMPCT)*TPN+PNIT(6)*(1.0D0-HRVEFF(IPM))+PNIT(4)
        TOT9=PSTATE(5)
        PSTATE(9)=PSTATE(9)+TOT9
        PSTATE(8)=PSTATE(8)+TOT8
        PSTATE(7)=TOT7
        PSTATE(6)=0.0D0
        PSTATE(5)=0.0D0
        PSTATE(4)=0.0D0
        PSTATE(3)=0.0D0
        PSTATE(2)=0.0D0
        DO 30 I=2,7
          CLSIZE(I)=0.0D0
   30   CONTINUE
        GS=0.0D0
        DO 40 IP=1,NPEST
          CORES(IP)=CORES(IP)+COPLNT(IP)
          COPLNT(IP)=0.0D0
   40   CONTINUE
C
C     ..HARVEST TYPE:: SINGLE HARVEST, YIELD ALL ABOVE GROUND BIOMASS
      ELSEIF(IHARV(IPM).EQ.4) THEN
        FIRST13=.TRUE.
        YIELD(1)=PSTATE(6)  !*HRVEFF(IPM)  assuming 100% grain harvested 6-5-2016
        YIELD(2)=TP*(1.0D0-RMPCT)*HRVEFF(IPM)
        YIELD(3)=0.0D0
        TOT7=RMPCT*TP
c Liwang Ma 3/20/08       TOT8=PSTATE(4)+(1.0D0-HRVEFF(IPM))*(PSTATE(6)+(1.0D0-RMPCT)*TP)
c Liwang Ma 3/20/08       TOT8N=PNIT(4)+(1.0D0-HRVEFF(IPM))*(PNIT(6)+(1.0D0-RMPCT)*TPN)
c        TOT8=(1.0D0-HRVEFF(IPM))*(PSTATE(6)+PSTATE(4)+(1.0D0-RMPCT)*TP)
c        TOT8N=(1.0D0-HRVEFF(IPM))*(PNIT(6)+PNIT(4)+(1.0D0-RMPCT)*TPN)
        TOT8=PSTATE(4)+(1.0D0-HRVEFF(IPM))*(1.0D0-RMPCT)*TP
        TOT8N=PNIT(4)+(1.0D0-HRVEFF(IPM))*(1.0D0-RMPCT)*TPN
        TOT9=PSTATE(5)
        PSTATE(9)=PSTATE(9)+TOT9
        PSTATE(8)=PSTATE(8)+TOT8
        PSTATE(7)=TOT7
        PSTATE(6)=0.0D0
        PSTATE(5)=0.0D0
        PSTATE(4)=0.0D0
        PSTATE(3)=0.0D0
        PSTATE(2)=0.0D0
        DO 50 I=2,7
          CLSIZE(I)=0.0D0
   50   CONTINUE
        GS=0.0D0
        DO 60 IP=1,NPEST
          CORES(IP)=CORES(IP)+COPLNT(IP)*(1.0D0-HRVEFF(IPM))
          COPLNT(IP)=0.0D0
   60   CONTINUE
C
C     ..HARVEST TYPE:: SINGLE HARVEST, YIELD ROOTS ONLY
      ELSEIF(IHARV(IPM).EQ.5) THEN
        FIRST13=.TRUE.
        YIELD(1)=0.0D0
        YIELD(2)=0.0D0
        YIELD(3)=PSTATE(5)*HRVEFF(IPM)
        TP=TP+PSTATE(4)+PSTATE(6)
        TPN=TPN+PNIT(4)+PNIT(6)
        TOT9=(1.0D0-HRVEFF(IPM))*PSTATE(5)
        TOT8=TP
        TOT8N=TPN
        PSTATE(9)=PSTATE(9)+TOT9
        PSTATE(8)=PSTATE(8)+TOT8
        PSTATE(7)=0.0D0
        PSTATE(6)=0.0D0
        PSTATE(5)=0.0D0
        PSTATE(4)=0.0D0
        PSTATE(3)=0.0D0
        PSTATE(2)=0.0D0
        DO 70 I=2,7
          CLSIZE(I)=0.0D0
   70   CONTINUE
        GS=0.0D0
        DO 80 IP=1,NPEST
          CORES(IP)=CORES(IP)+COPLNT(IP)
          COPLNT(IP)=0.0D0
   80   CONTINUE
C
      ELSEIF(IHARV(IPM).EQ.6) THEN
        FIRST13=.TRUE.
        YIELD(1)=PSTATE(6)*HRVEFF(IPM)
        YIELD(2)=TP*(1.0D0-RMPCT)*HRVEFF(IPM)
        YIELD(3)=PSTATE(5)*HRVEFF(IPM)
        TP=TP+PSTATE(4)+(1.0D0-HRVEFF(IPM))*PSTATE(6)+
     &     (1.0D0-HRVEFF(IPM))*(1.0D0-RMPCT)*TP
        TPN=TPN+PNIT(4)+(1.0D0-HRVEFF(IPM))*PNIT(6)+
     &     (1.0D0-HRVEFF(IPM))*(1.0D0-RMPCT)*TPN
        TOT9=(1.0D0-HRVEFF(IPM))*PSTATE(5)
        TOT8=TP
        TOT8N=TPN
        PSTATE(9)=PSTATE(9)+TOT9
        PSTATE(8)=PSTATE(8)+TOT8
        PSTATE(7)=0.0D0
        PSTATE(6)=0.0D0
        PSTATE(5)=0.0D0
        PSTATE(4)=0.0D0
        PSTATE(3)=0.0D0
        PSTATE(2)=0.0D0
        DO  I=2,7
          CLSIZE(I)=0.0D0
        Enddo
        GS=0.0D0
        DO IP=1,NPEST
          CORES(IP)=CORES(IP)+COPLNT(IP)
          COPLNT(IP)=0.0D0
        enddo
C
      ENDIF
cc      ENDIF
C
C     ..ADJUST RESIDUE AGE (AGE IS RESET IN HARVST ROUTINE)
      RESAGE=0.0d0
C
C     ..CONVERSION FACTOR FOR G-CARBON/PLANT ==> G-CARBON/CM^2
      FACT=SUMCL(4)*1.0D3*1.0D-8
C
C     ..CALCULATE STANDING DEAD [KG/HA]-- SDCN DOESN'T CHANGE SINCE WE
C     ARE ALWAYS TAKING FROM IT.
c      SDEAD=PSTATE(7)*SUMCL(4)*C2BM
C
C     ..PUT LITTER CARBON INTO SURFACE RESIDUE POOL (KG/HA)
      IF(TOT8N.GT.0.0D0) THEN
        RMN=TOT8*SUMCL(4)*C2BM
        TOT8CN=TOT8/TOT8N
        RCN=CNEW(RM,RMN,RCN,TOT8CN,IRES)
C       RCN = MAX(MIN(RCN,CN(2)),CN(1))
        RM=RM+RMN
c standing residue after harvest
        SDMN=PSTATE(7)*SUMCL(4)*C2BM
        SDCN=CNEW(SDEAD,SDMN,SDCN,TOT8CN,IRES)
        SDEAD=SDEAD+SDMN
      ENDIF
C
c take soybean credit into account for corn-soybean rotation, Liwang Ma
c        if ((Iinxpl.gt.100).and.(Iinxpl.lt.200)) then
c          ANODULE = MIN(yield(1)*2.0D-2,45.0D0)
c     +            *6.25D0/0.3D0
c from Schapers amd Moiser (1991), NLEAP book edited by Ron Follett
c          ANODULE = MIN(yield(1)*sumcl(4)*c2bm/67.2d0*1.5d0,45.0D0)
c     +            /cn(2)*c2bm
c        ELSE
c          ANODULE = 0.0D0
c        ENDIF
c
C     ..PARTITION DEAD ROOT CARBON  :::USE DWL FOR LAYER DIST
C CHNAGE MADE BY LIWANG MA TO ACCOUT FOR SOYBEAN CREDIT
c      TOT10 = TOT9 + ANODULE
      TOTNODULE=0.0D0
      DO J=1,NN
      TOTNODULE=TOTNODULE+DWNOD1/C2BM/(SUMCL(4)/10.0D0)*PCN(J)*
     &       (C2BM*0.16D0*PRONOD)*RDF(J)
      ENDDO
      TOT10 = TOT9 + TOTNODULE
      DO 90 I=1,NN
        IF(PCN(I).GT.0.0D0) THEN
          IF(PCN(I).LE.CN(1)) THEN
            S=0.0D0
          ELSEIF(PCN(I).GE.CN(2)) THEN
            S=1.0D0
          ELSE
            S=(1.0D0/PCN(I)-1.0D0/CN(1))/(1.0D0/CN(2)-1.0D0/CN(1))
          ENDIF
          RPOOL(I,1)=RPOOL(I,1)+RDF(I)*TOT10*(1.0D0-S)*FACT
          RPOOL(I,2)=RPOOL(I,2)+RDF(I)*TOT10*S*FACT
C
C         .. SAVE FOR MASS BALANCE CALCULATIONS (CONVERT TO KG/HA)
          TADRT(1)=TADRT(1)+RDF(I)*TOT10*(1.0D0-S)*FACT*CKG/CN(1)
          TADRT(2)=TADRT(2)+RDF(I)*TOT10*S*FACT*CKG/CN(2)
          TADRTC(1)=TADRTC(1)+RDF(I)*TOT10*(1.0D0-S)*FACT*CKG
          TADRTC(2)=TADRTC(2)+RDF(I)*TOT10*S*FACT*CKG
          RMASS=RDF(I)*TOT10*FACT*CKG
          IF(PCN(I).LE.CN(1)) THEN
          RCO2=RCO2+RMASS-(RMASS*(1.0D0-S)/PCN(I))*CN(1)
          ELSEIF(PCN(I).GE.CN(2)) THEN
          RCO2=RCO2+RMASS-(RMASS*S/PCN(I))*CN(2)
          endif
c
        ENDIF
   90 CONTINUE
C
C     ..CALCULATE YIELD TOTALS [KG/HA]
        YIELDC = 0.0D0
      DO 100 I=1,3
        YIELD(I)=YIELD(I)*SUMCL(4)*C2BM
        YIELDC = YIELDC + YIELD(I)/C2BM
  100 CONTINUE
C

C     ..PRINTOUT YIELD TOTALS
      IF(TCAG.GT.0.0D0) THEN
        HI=YIELD(1)/TCAG
      ELSE
        HI=0.0D0
      ENDIF
      CALL CDATE(JDAY,ID,IM,IYYY)
      WRITE(70,1000)
      WRITE(70,1100) plname,ID,IM,IYYY,JDAY
      WRITE(70,1200) SUMCL(4)*1.0D3,(YIELD(I),I=1,3),HI,TNAG,TMPLNT-
     +    TNBG+plantNseed*sumcl(4),TXPLNT,TNBG,plantNseed*sumcl(4),
     +    TCAG,TCBG
      WRITE(70,1000)
C
      RETURN
 1000 FORMAT(70('*'))
 1100 FORMAT(/4('-'),'Generic Plant Harvest','---',A30,/
     &       4x,'ON ',I3,'/',I2,'/',I4,' -----',I6,'  ----'/)
 1200 FORMAT(/,'HARVEST YIELDS FROM ',F12.1,' PLANTS',/50('='),/,
     +    ' YIELD FROM SEEDS/TUBER [KG/HA]:',T50,F12.2,/,
     +    ' YIELD FROM ABOVE GROUND BIOMASS [KG/HA]:',T50,F12.2,/,
     +    ' YIELD FROM ROOT BIOMASS [KG/HA]:',T50,F12.2,/,
     +    ' HARVEST INDEX:',T50,F12.2,//,
     +    ' ----NITROGEN DISTRIBUTION AT TIME OF HARVEST----',/,
     +    '     --TOTAL ABOVE GROUND NITROGEN [KG-N/HA]:',T50,F12.2,/,
     +    '       --PLANT MAINTENANCE NITROGEN [KG-N/HA]:',T45,F7.2,/,
     +    '       --PLANT LUXURIOUS NITROGEN [KG-N/HA]:',T45,F7.2,/,
     +    '     --TOTAL BELOW GROUND NITROGEN [KG-N/HA]:',T50,F12.2,//,
     +    '     [TOTAL N FROM SEEDS AT GERMINATION[KG-N/HA]:',T50,
     +F12.2,']',//,
     +    ' ----BIOMASS DISTRIBUTION AT TIME OF HARVEST----',/,
     +    '           (BIOMASS ARE IN DRY WEIGHT ONLY)'/,
     +    '     --TOTAL ABOVE GROUND BIOMASS [KG-BM/HA]:',T50,F12.2,/,
     +    '     --TOTAL BELOW GROUND BIOMASS [KG-BM/HA]:',T50,F12.2,//)
      END
C
