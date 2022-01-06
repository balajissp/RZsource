       SUBROUTINE COMP2EXP(NN,JBDAY,JULBDAY,JULEDAY,TLT,DRSPAC,soilpss,
     &            scenname)
C =================================================================
C purpose:
C       PROGRAMED BY LIWANG MA, MARCH 23, 2002
C       this was modified on April 1, 2003 for Dr. Qiang Yu to
c          include temperature in the soil profile.
C       1. READ DATA FROM LAYER.PLT AND EXTRACT ONLY THE DATES WITH
C          EXPERIMENTAL MEASUREMENTS
C       2. TRANSFORM RESULTS FOR NUMBERICAL LAYERS TO HORIZON (OR MEASURED DEPTH)
C =================================================================
C EXPERIMENTAL DATA ARE READ FROM EXPDATA.DAT
C SIMULATION DATA ARE READ FROM TWO SOURCES: LAYER.PLT(VECTOR) AND *.ANA(SCALAR) 
C COMPARISON BETWEEN EXPERIMENTAL AND SIMULATION RESULTS IS IN COMP.OUT
C
C IDEXP:  NUMBER OF EXPERIMENTAL VARIABLES MEASURED WITH SOIL DEPTH (VECTOR)
C NEDATA: NUMBER OF TIME SERIES DATA FOR EACH VECTOR VARIABLE
C MHORZ:  NUMBER OF DEPTH FOR EACH SAMPLING DATE
C DEP:    SOIL DEPTH WHERE EXPERIMENTAL DATA WERE COLLECTED (CM)
C EXPD: EXPERIMENTAL MEASUREMENT AT A PARTICULAER SOIL DEPTH
C ISDATA: NUMBER OF EXPERIMENTAL VARIABLES MEASURED (SCALAR)
C NYIELD: NUMBER OF YIELD MEASUREMENTS
C EYIELD: MEASURED YIELD (KG/HA)
C HAREFF: HARVEST EFFICIENT (0-1)
C NBIOM:  NUMBER OF BIOMASS MEASUREMENTS (KG/HA)
C EBIOM:  MEASURED ABOVE GROUND BIOMASS (KG/HA)
C NNUP: NUMBER OF PLANT N UPTAKE MEASUREMENTS 
C ENUP: MEASURED TOTAL PLANT N UPTAKE (KG N/HA)
C NGRAINN:NUMBER OF GRAIN N UPTAKE MEASUREMENTS
C EGRAINN:MEASURED GRAIN N UPTAKE (KG N/HA)
C NTF:  NUMBER OF TILE FLOW MEASUREMENTS 
C ETILEF: ACCUMULATIVE TILE FLOW (CM)
C ETNPPM: N IN TILE FLOW (PPM)
C EP1PPM: PESTICIDE 1 IN TILE FLOW (PPM)
C EP2PPM: PESTICIDE 2 IN TILE FLOW (PPM)
C EP3PPM: PESTICIDE 3 IN TILE FLOW (PPM)
C NWTB: NUMBER OF WATER TABLE MEASUREMENTS 
C EWTB: MEASURED WATER TABLE DEPTH (CM)
C NROF: NUMBER OF SURFACE RUNOFF MEASUREMENTS
C EROF: MEASURED RUNOFF (UNIT ??)
C NPHT: NUMBER OF PLANT HEIGHT MEASUREMENTS
C EPHT: MEASURED PLANT HEIGHT (CM)
C NET:  NUMBER OF ET MEASUREMENT
C EAET: MEASURED ET AT A GIVEN TIME INTERVAL (CM)
C NLAI: NUMBER OF LAI MEASUREMENTS
C ELAI: MEASURED LAI
C ANADATA:SIMULATED DAILY OUTPUT FROM ANALYSIS FILE (TOTAL 74 VARIALBES)
C 
C====================================================================== 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       parameter (mxnod=300,maxhor=25,MXANA=113)
       DIMENSION PDATA(24,mxnod),EAVGHZ(1500)  !,EAVGTM(3000)
       DIMENSION ZHOR(12),EXPD(24,1500,maxhor),DEP(24,1500,maxhor)
       Dimension AVGS(24,1500,maxhor),RMSE(1500),!DIFF(1200),
     +           IJDAY(24,1500),ipcrop(100)
       DIMENSION IDAY(24,1500),IMONTH(24,1500),IYEAR(24,1500)
       DIMENSION IDEXP(24),NEDATA(24),SAVGHZ(1500)   !,SAVGTM(3000)
       DIMENSION JNAME(25), MHORZ(24,1500),TLT(mxnod),PRED1(mxnod)
       DIMENSION DS(mxnod), SOILP(24,1500,mxnod),pred2(mxnod),
     +           soilps(mxnod),soilpss(mxnod),soilpe(mxnod),PRED3(MXNOD)
       DIMENSION ISDATA(12),EYIELD(100),DAYHAR(100),EBIOM(100),
     +           DAYBIOM(100),PBIOM(100),EAPHEN(100),EMPHEN(100),
     +           ENUP(100),DAYNUP(100),ANADATA(MXANA),PYIELD(100),
     +           PNUP(100),ETILEF(1200),DAYTFE(1200),PTILEN(1200),
     +           PTILEF(1200),NDAYS(1200),ETNPPM(1200),ETILEFD(1200),
     +           EP1PPM(1200),EP2PPM(1200),EP3PPM(1200),PACCTN(1200),
     +           PTILEP1(1200),PTILEP2(1200),PTILEP3(1200),PACCTF(1200)
       DIMENSION EWTB(1200),PWTB(1200),EROF(100),PROF(100),
     +           DAYWTB(1200),DAYROF(100),EGRAINN(100),PGRAINN(100),
     +           DAYGN(100),HAREFF(100),EACCTN(1200),EACCTF(1200)
       DIMENSION ENROF(100),PNROF(100),EP1ROF(100),PP1ROF(100),
     +           EP2ROF(100),PP2ROF(100),EP3ROF(100),PP3ROF(100)
       DIMENSION PPHT(500),EPHT(500),PAET(100),EAET(100),DAYPHEN(100),
     +           DAYPHT(500),DAYETS(100),DAYETE(100),ELAI(500),
     +           PLAI(500),DAYLAI(500),ptfyear(100),etfyear(100),
     +           etnyear(100),ptnyear(100),IYCROP(100),IBCROP(100),
     +           INUCROP(100),INGCROP(100),indgn(100),indyield(100),
     +           indbiom(100),indnup(100),pldate(100),adap(100),
     +           ptilefmon(100,12),ptileNmon(100,12),amdap(100),
     +           edap(100),EEPHEN(100)
       CHARACTER*80 TITLE
       CHARACTER*8 FMAT(24),ctime
       CHARACTER*255 SCENNAME,string
       character*3 mon(12)
       character*4 cyr
       character*2 cmon,cday
       character*10 day_mon_yr
       integer DATE_TIME(8)
       logical first/.true./
c       COMMON SCENNAME
       DATA FMAT/'SWC','SRP','FRP','FHP','IHP','SHP',
     +         'NO3N','NH4N','P1AD','P1SLN','P1TOT',
     +         'P2AD','P2SLN','P2TOT','P3AD','P3SLN','P3TOT',
     +         'ST','SOM','SP','N2O','NxO','CH4','CO2'/
C
       DATA IDEXP/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
       DATA JNAME/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
       DATA MON /'Jan','Feb','Mar','Apr','May','Jun','Jul'
     &         ,'Aug','Sep','Oct','Nov','Dec'/
C
        OPEN (UNIT=1,FILE='LAYER.PLT',STATUS='OLD',err=13)
        OPEN (UNIT=3,FILE='EXPDATA.DAT',STATUS='OLD',err=23)
c        OPEN (UNIT=4,FILE='COMP2001.OUT',STATUS='unknown')
        OPEN (UNIT=6,FILE='COMP2EXP.OUT',STATUS='unknown')
C READ ANALYSIS FILE
        OPEN(UNIT=5, FILE=SCENNAME, STATUS='UNKNOWN')
        OPEN(UNIT=7, FILE='tileF.out', STATUS='UNKNOWN')
        OPEN(UNIT=8, FILE='tileN.out', STATUS='UNKNOWN')
        OPEN(UNIT=9, FILE='runoff.out', STATUS='UNKNOWN')
        OPEN(UNIT=10, FILE='runoffN.out', STATUS='UNKNOWN')
        OPEN(UNIT=11, FILE='phenology.out', STATUS='UNKNOWN')
C===FIST COMPARE MEASURED DATA BY SOIL LAYER =====================
C ================================================================
C      READ EXPERIMENTAL DATA FOR vectors by LAYER
C ================================================================
	CALL DATE_AND_TIME (VALUES=DATE_TIME)
!      date_time(1)  The 4-digit year  
!      date_time(2)  The month of the year  
!      date_time(3)  The day of the month  
!      date_time(4)  The time difference with respect to Coordinated Universal Time (UTC) in minutes  
!      date_time(5)  The hour of the day (range 0 to 23) - local time  
!      date_time(6)  The minutes of the hour (range 0 to 59) - local time  
!      date_time(7)  The seconds of the minute (range 0 to 59) - local time  
!      date_time(8)  The milliseconds of the second (range 0 to 999) - local time  
      
!     Version information stored in ModuleDefs.for
	WRITE (6,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
C SET RANGE OF DAYS TO OUTPUT
          IDAYS=JULEDAY-JULBDAY+JBDAY
          CALL CJULDATE(JULBDAY,IDB,IMB,IYYYB)
          CALL CJULDATE(JULEDAY,IDE,IME,IYYYE)
          DAYB = DBLE(IYYYB)+DBLE(JDATE(IDB,IMB,IYYYB))*1.0D-3
          DAYE = DBLE(IYYYE)+DBLE(JDATE(IDE,IME,IYYYE))*1.0D-3
C
      CALL ECHO(3)
      read (3,1000,err=27) string
        knumb=inumb(string)
      backspace (3)
      READ (UNIT=3,FMT=*,err=27) (IDEXP(I),I=1,knumb)
      ITOTAL = 0
      DO 944 I=1,knumb
         ITOTAL = ITOTAL + IDEXP(I)
944   CONTINUE
      IF (ITOTAL.EQ.0) GOTO 943
C
      DO 111 II=1,knumb
        IF (IDEXP(II).EQ.1) THEN
        CALL ECHO(3)
C
      IF (II.EQ.1) THEN
        READ (UNIT=3,FMT=*,err=27) NEDATA(II), ITYPESWC
      ELSE if (ii.eq.7) then
        READ (UNIT=3,FMT=*,err=27) NEDATA(II), ITYPENO3
      ELSE if (ii.eq.8) then
        READ (UNIT=3,FMT=*,err=27) NEDATA(II), ITYPENH4
      ELSE
        READ (UNIT=3,FMT=*,err=27) NEDATA(II)
      ENDIF
        DO 15 K=1,NEDATA(II)
           MHORZ(II,K) = 1
           I = 1
           READ (UNIT=3,FMT=*,err=27) IDAY(II,K),IMONTH(II,K),
     +        IYEAR(II,K),DEP(II,K,1),EXPD(II,K,1)
           DEPTHTRK = DEP(II,K,1)
255        IF (K.EQ.NEDATA(II)) THEN
             READ (UNIT=3,FMT=*,err=27) TITLE
             IF (TITLE(1:1) .EQ. '=') THEN
              GOTO 15
             ELSE
             BACKSPACE (UNIT=3)
             ENDIF
           ENDIF
              I = I + 1
               READ (UNIT=3,FMT=*,err=27) IDUM1,IDUM2,IDUM3,
     +             DEP(II,K,I),EXPD(II,K,I)
           IF (DEP(II,K,I).GT.DEPTHTRK) THEN
              DEPTHTRK = DEP(II,K,I)
              MHORZ(II,K) = MHORZ(II,K) + 1
              GOTO 255
           ELSE
              BACKSPACE (UNIT=3)
              GOTO 15
           ENDIF
C  20      CONTINUE
  15    CONTINUE
        ENDIF
111   CONTINUE
C
c
C ===== Read DAILY AVERAGE RESULTS ===========================
C
C  READ EXPERIMENTAL RESULTS
943   CALL ECHO(3)
      read (3,1000,err=36) string
        iknumb=inumb(string)
      backspace (3)
      READ (UNIT=3,FMT=*,err=36) (ISDATA(I),I=1,iKNUMB)
      CALL ECHO(3)
C
C  READ FINAL YIELD
      IF (ISDATA(1).EQ.1) THEN
      CALL ECHO(3)
      READ (UNIT=3, FMT=*,err=36) NYIELD     !maximum 100
        if (nyield.gt.100) then
        print*, 'Number of yield observations exceed 100'
        stop
        endif
      DO 46 K=1,NYIELD
      READ (UNIT=3, FMT=*,err=36) IHARVD,IHARVM,IHARVY,EYIELD(K),
     +                     HAREFF(K),IYCROP(K)
         DAYHAR(K) = DBLE(IHARVY)+DBLE(JDATE(IHARVD,IHARVM,IHARVY))
     +                  *1.0D-3
        indyield(k)=iharvy
46    CONTINUE
      END IF
C
C  READ PLANT BIOMASS DATA
      IF (ISDATA(2).EQ.1) THEN
      CALL ECHO(3)
      READ (UNIT=3, FMT=*,err=36) NBIOM               !maximum 100
        if (nbiom.gt.100) then
        print*, 'Number of biomass observations exceed 100'
        stop
        endif
      DO 45 K=1,NBIOM
      READ (UNIT=3,FMT=*,err=36)IDBIOM,IMBIOM,IYBIOM,EBIOM(K),
     +   IBCROP(K)
         DAYBIOM(K) = DBLE(IYBIOM)+DBLE(JDATE(IDBIOM,IMBIOM,IYBIOM))
     +                  *1.0D-3
        indbiom(k)=iybiom
45    CONTINUE
      ENDIF
C
C  READ PLANT N UPTAKE DATA
      IF (ISDATA(3).EQ.1) THEN
      CALL ECHO(3)
      READ (UNIT=3, FMT=*,err=36) NNUP         !maximum 100
        if (nnup.gt.100) then
        print*, 'Number of N uptake observations exceed 100'
        stop
        endif
      DO 47 K=1,NNUP
      READ (UNIT=3,FMT=*,err=36) IDNUP,IMNUP,IYNUP,ENUP(K),INUCROP(K)
         DAYNUP(K) = DBLE(IYNUP)+DBLE(JDATE(IDNUP,IMNUP,IYNUP))
     +                  *1.0D-3
        indnup(k)=iynup
47    CONTINUE
      ENDIF
C
C  READ GRAIN N UPTAKE DATA
      IF (ISDATA(4).EQ.1) THEN
      CALL ECHO(3)
      READ (UNIT=3, FMT=*,err=36) NGRAINN      !maximum 100
        if (ngrainn.gt.100) then
        print*, 'Number of grain N observations exceed 100'
        stop
        endif
      DO 89 K=1,NGRAINN
      READ (UNIT=3,FMT=*,err=36) IDGN,IMGN,IYGN,EGRAINN(K),INGCROP(K)
         DAYGN(K) = DBLE(IYGN)+DBLE(JDATE(IDGN,IMGN,IYGN))
     +                  *1.0D-3
        indgn(k)=iygn
89    CONTINUE
      ENDIF
C
C  READ DAILY TILE FLOW DATA
      IF (ISDATA(5).EQ.1) THEN
      CALL ECHO(3)
      READ (UNIT=3, FMT=*,err=36) NTF     !maximum 1200
        if (ntf.gt.1200) then
        print*, 'Number of tile flow observations exceed 1200'
        stop
        endif
      DO 48 K=1,NTF
      READ (UNIT=3,FMT=*,err=36)IDTFE,IMTFE,IYTFE,ETILEF(K),ETNPPM(K),
     +       EP1PPM(K),EP2PPM(K),EP3PPM(K)
         DAYTFE(K) = DBLE(IYTFE)+DBLE(JDATE(IDTFE,IMTFE,IYTFE))
     +                  *1.0D-3
C adjust tile flow based on drainage spacing
c      ETILEF(K) = ETILEF(K) * 2900/DRSPAC
C
         if (juliand.gt. JulDATE(IDTFE,IMTFE,IYTFE)) then
           print*,'problem with TILE FLOW data AT LINE  ',K
           stop
         else
           juliand = JulDATE(IDTFE,IMTFE,IYTFE)
         endif
c      read (3,*) sdate,etilef(k),etnppm(k)
c     read (sdate,890) idtfe,imtfe,iytfe
48    CONTINUE
      ENDIF
c
C  READ WATER TABLE DATA
      IF (ISDATA(6).EQ.1) THEN
      CALL ECHO(3)
      READ (UNIT=3, FMT=*,err=36) NWTB            !maximum 1200
        if (nwtb.gt.1200) then
        print*, 'Number of water table observations exceed 1200'
        stop
        endif
      DO 49 K=1,NWTB
      READ (UNIT=3,FMT=*,err=36) IDWTB,IMWTB,IYWTB,EWTB(K)
         DAYWTB(K) = DBLE(IYWTB)+DBLE(JDATE(IDWTB,IMWTB,IYWTB))
     +                  *1.0D-3
49    CONTINUE
      ENDIF
C
C  READ SURFACE RUNOFF DATA
      IF (ISDATA(7).EQ.1) THEN
      CALL ECHO(3)
      READ (UNIT=3, FMT=*,err=36) NROF   !maximum 100
        if (nrof.gt.100) then
        print*, 'Number of runoff observations exceed 100'
        stop
        endif
      DO 57 K=1,NROF
      READ (UNIT=3,FMT=*,err=36) IDROF,IMROF,IYROF,EROF(K),ENROF(K),
     +                    EP1ROF(K),EP2ROF(K),EP3ROF(K)
         DAYROF(K) = DBLE(IYROF)+DBLE(JDATE(IDROF,IMROF,IYROF))
     +                  *1.0D-3
57    CONTINUE
      ENDIF
C
C  READ PLANT HEIGHT (CM)
      IF (ISDATA(8).EQ.1) THEN
      CALL ECHO(3)
      READ (UNIT=3, FMT=*,err=36) NPHT   !maximum 100
        if (npht.gt.500) then
        print*, 'Number of plant height observations exceed 100'
        stop
        endif
      DO 77 K=1,NPHT
      READ (UNIT=3,FMT=*,err=36) IDPHT,IMPHT,IYPHT,EPHT(K)
         DAYPHT(K) = DBLE(IYPHT)+DBLE(JDATE(IDPHT,IMPHT,IYPHT))
     +                  *1.0D-3
77    CONTINUE
      ENDIF
C
C  READ ET MEASUREMENTS (CM)
      IF (ISDATA(9).EQ.1) THEN
      CALL ECHO(3)
      READ (UNIT=3, FMT=*,err=36) NET   !maximum 100
        if (net.gt.100) then
        print*, 'Number of ET observations exceed 100'
        stop
        endif
      DO 87 K=1,NET
      READ (UNIT=3,FMT=*,err=36) IDETS,IMETS,IYETS,IDETE,IMETE,IYETE,
     +   EAET(K)
         DAYETS(K) = DBLE(IYETS)+DBLE(JDATE(IDETS,IMETS,IYETS))*1.0D-3
         DAYETE(K) = DBLE(IYETE)+DBLE(JDATE(IDETE,IMETE,IYETE))*1.0D-3
87    CONTINUE
      ENDIF
C
C  READ LAI MEASUREMENTS (CM)
      IF (ISDATA(10).EQ.1) THEN
      CALL ECHO(3)
      READ (UNIT=3, FMT=*,err=36) NLAI  !maximum 200
        if (nlai.gt.500) then
        print*, 'Number of LAI observations exceed 200'
        stop
        endif
      DO 97 K=1,NLAI
      READ (UNIT=3,FMT=*,err=36) IDLAI,IMLAI,IYLAI,ELAI(K)
         DAYLAI(K) = DBLE(IYLAI)+DBLE(JDATE(IDLAI,IMLAI,IYLAI))
     +                  *1.0D-3
97    CONTINUE
      ENDIF
C  READ phenology MEASUREMENTS, anthesis and physiological maturity dates (DAP)
      IF (ISDATA(11).EQ.1) THEN
      CALL ECHO(3)
      READ (UNIT=3, FMT=*,err=36) NPHEN  !maximum 100
        if (nphen.gt.100) then
        print*, 'Number of phenology observations exceed 100'
        stop
        endif
      DO K=1,NPHEN
      READ (UNIT=3,FMT=*,err=36)IDPHEN,IMPHEN,IYPHEN,EEPHEN(K),
     &                 EAPHEN(K),EMPHEN(K) !,IPCROP(K)
         DAYPHEN(K) = DBLE(IYPHEN)*1000+
     +                DBLE(JDATE(IDPHEN,IMPHEN,IYPHEN))
      ENDDO
      ENDIF
C================================================================
C ===== NOW COMPARE DAILY AVERAGE RESULTS ===========================
C  READ DATA FROM ANALYSIS OUTPUT FILE
      READ (UNIT=5,FMT=55,err=28) TITLE
55    FORMAT (22/,A80)
       IJK = 2
       PTILEF(IJK) = 0.0D0
       PTILEN(IJK) = 0.0D0
       PTILEP1(IJK) = 0.0D0
       PTILEP2(IJK) = 0.0D0
       PTILEP3(IJK) = 0.0D0
       PACCTF(IJK) = 0.0D0
       PACCTN(IJK) = 0.0D0
       EACCTF(IJK) = 0.0D0
       EACCTN(IJK) = 0.0D0
       NDAYS(IJK) = 0
       RMSEYD = 0.0D0
       RMSEBM = 0.0D0
       RMSENB = 0.0D0
       RMSENG = 0.0D0
       RMSEWT = 0.0D0
       RMSERO = 0.0D0
       RMSENRO = 0.0D0
       RMSEP1RO = 0.0D0
       RMSEP2RO = 0.0D0
       RMSEP3RO = 0.0D0
       RMSETF = 0.0D0
       RMSETN = 0.0D0
       RMSETP1 = 0.0D0
       RMSETP2 = 0.0D0
       RMSETP3 = 0.0D0
       NTFN0 = 0
       NTFP1 = 0
       NTFP2 = 0
       NTFP3 = 0
       NROFN0 = 0
       NROFP1 = 0
       NROFP2 = 0
       NROFP3 = 0
       KJ = 1
       PAET(KJ) = 0.0D0
       RMSEAET = 0.0D0
       RMSELAI = 0.0D0
       RMSEPHT = 0.0D0
       RMSEACCTF = 0.0D0
C
      DO WHILE (.true.)
      READ (UNIT=5,FMT=*,err=28,end=66) (ANADATA(I),I=1,MXANA)
          if ((plant63.eq.0.0d0).and.(anadata(63).gt.0.0d0)) then
          plant41=0.0d0
          plant42=0.0d0
          plant62=0.0d0
          plant44=0.0d0
          else
          plant41=max(plant41,anadata(41))
          plant42=max(plant42,anadata(42))
          plant62=max(plant62,anadata(62))
          plant44=max(plant44,anadata(44))
          endif
          plant63=anadata(63)        

C
      if (first) iyrstart=int(anadata(1))
c101   FORMAT(F8.3,1X,12(F8.4,1x),27(G11.3,1x),4(F10.2,1x),60(G10.3,1x))
101   FORMAT (F8.3,1X,99(G15.6,1x))
C  CALCULATE GRAIN YIELD FROM ANALYSIS FILE
      DO 60 I=1,NYIELD
        IF (DAYHAR(I).EQ.ANADATA(1).AND.
     +      DAYHAR(I).GE.DAYB.AND.DAYHAR(I).LE.DAYE) THEN
c Liwang Ma, do not use harvest index at this time.
c            PYIELD(I)= ANADATA(44)*HAREFF(I)
            PYIELD(I)= max(ANADATA(44),plant44)
            plant44=0.0d0
            RMSEYD = RMSEYD + (EYIELD(I)-PYIELD(I))**2
        ENDIF
60    CONTINUE
C
C  CALCULATE ABOVEGROUND BIOMASS FROM ANALYSIS FILE
      DO 61 I=1,NBIOM
        IF (DAYBIOM(I).EQ.ANADATA(1).AND.   !) THEN
     +      DAYBIOM(I).GE.DAYB.AND.DAYBIOM(I).LE.DAYE) THEN
            PBIOM(I)= max(ANADATA(41),plant41)
            plant41=0.0d0
            RMSEBM = RMSEBM + (EBIOM(I)-PBIOM(I))**2
        ENDIF
61    CONTINUE
C
C  CALCULATE N UPTAKE BY ABOVEGROUND BIOMASS FROM ANALYSIS FILE
      DO 62 I=1,NNUP
        IF (DAYNUP(I).EQ.ANADATA(1).AND. !) THEN
     +      DAYNUP(I).GE.DAYB.AND.DAYNUP(I).LE.DAYE) THEN
            PNUP(I)= ANADATA(22)
            RMSENB = RMSENB + (ENUP(I)-PNUP(I))**2
        ENDIF
62    CONTINUE
C
C
C  CALCULATE N IN GRAIN FROM ANALYSIS FILE
      DO 99 I=1,NGRAINN
        IF (DAYGN(I).EQ.ANADATA(1).AND.  !) THEN
     +      DAYGN(I).GE.DAYB.AND.DAYGN(I).LE.DAYE) THEN
            PGRAINN(I)= ANADATA(24)
            RMSENG = RMSENG + (EGRAINN(I)-PGRAINN(I))**2
        ENDIF
99    CONTINUE
C
C  CALCULATE WATER TABLE FROM ANALYSIS FILE
      DO 68 I=1,NWTB
        IF (DAYWTB(I).EQ.ANADATA(1).AND. !) THEN
     +      DAYWTB(I).GE.DAYB.AND.DAYWTB(I).LE.DAYE) THEN
            PWTB(I)= ANADATA(61)
            RMSEWT = RMSEWT + (EWTB(I)-PWTB(I))**2
        ENDIF
68    CONTINUE
C
C  CALCULATE SURFACE RUNOFF FROM ANALYSIS FILE
      DO 69 I=1,NROF
        IF (DAYROF(I).EQ.ANADATA(1).AND. !) THEN
     +      DAYROF(I).GE.DAYB.AND.DAYROF(I).LE.DAYE) THEN
            PROF(I)= ANADATA(12)
            RMSERO = RMSERO + (EROF(I)-PROF(I))**2
          IF (ANADATA(12).GT.0.0D0) THEN
            PNROF(I) = ANADATA(29)/ANADATA(12)*10.0D0
            PP1ROF(I) = ANADATA(50)/ANADATA(12)*10.0D0
            PP2ROF(I) = ANADATA(55)/ANADATA(12)*10.0D0
            PP3ROF(I) = ANADATA(60)/ANADATA(12)*10.0D0
          ELSE
            PNROF(I) =  0.0D0
            PP1ROF(I) = 0.0D0
            PP2ROF(I) = 0.0D0
            PP3ROF(I) = 0.0D0
          ENDIF
          IF ((PROF(I).GT.0.0D0).AND.(EROF(I).GT.0.0D0)) THEN
            IF (ENROF(I).GT.0.0D0) THEN
            NROFN0 = NROFN0 + 1
            RMSENRO = RMSENRO + (ENROF(I)-PNROF(I))**2
            ENDIF
            IF (EP1ROF(I).GT.0.0D0) THEN
            NROFP1 = NROFP1 + 1
            RMSEP1RO = RMSEP1RO + (EP1ROF(I)-PP1ROF(I))**2
            ENDIF
            IF (EP2ROF(I).GT.0.0D0) THEN
            NROFP2 = NROFP2 + 1
            RMSEP2RO = RMSEP2RO + (EP2ROF(I)-PP2ROF(I))**2
            ENDIF
            IF (EP3ROF(I).GT.0.0D0) THEN
            NROFP3 = NROFP3 + 1
            RMSEP3RO = RMSEP3RO + (EP3ROF(I)-PP3ROF(I))**2
            ENDIF
          ENDIF
        ENDIF
69    CONTINUE

C  CALCULATE PLANT HEIGHT FROM ANALYSIS FILE
C
      DO 32 I=1,NPHT
        IF (DAYPHT(I).EQ.ANADATA(1).AND. !) THEN
     +      DAYPHT(I).GE.DAYB.AND.DAYPHT(I).LE.DAYE) THEN
            PPHT(I)= max(ANADATA(62),plant62)
            plant62=0.0d0
            RMSEPHT = RMSEPHT + (EPHT(I)-PPHT(I))**2
        ENDIF
32    CONTINUE
C
C  CALCULATE PLANT HEIGHT FROM ANALYSIS FILE
      DO 39 I=1,NLAI
        IF (DAYLAI(I).EQ.ANADATA(1).AND. !) THEN
     +      DAYLAI(I).GE.DAYB.AND.DAYLAI(I).LE.DAYE) THEN
            PLAI(I)= max(ANADATA(43),plant43)
            plant43=0.0d0
            RMSELAI = RMSELAI + (ELAI(I)-PLAI(I))**2
        ENDIF
39    CONTINUE
C  CALCULATE ET FROM ANALYSIS FILE
        IF ((ANADATA(1).GE.DAYETS(KJ)).AND.
     +     (ANADATA(1).LE.DAYETE(KJ))) THEN
            PAET(KJ)= PAET(KJ) + (ANADATA(6)+ANADATA(7))
        ENDIF
        IF ((ANADATA(1).EQ.DAYETE(KJ))) THEN
            RMSEAET = RMSEAET + (EAET(KJ)-PAET(KJ))**2
            KJ = KJ + 1
            PAET(KJ) = 0.0D0
            IF (KJ.GT.NET) GOTO 65
        ENDIF
C
C  CALCULATE Monthly TILE-FLOW and N in tile flow FROM ANALYSIS FILE
C
      CALL CDATE(NINT((ANADATA(1)-INT(ANADATA(1)))*1000),IDi,IMi,
     +           INT(ANADATA(1)))
            iyi = int(anadata(1))-iyrstart+1
            PTILEFmon(iyi,imi)= PTILEFmon(Iyi,imi)+ANADATA(11)
            PTILENmon(Iyi,imi)= PTILENmon(Iyi,imi)+ANADATA(31)    
c
            write(cday,'(i2.2)'),idi
            write(cyr,'(i4.4)'),int(anadata(1))
            write(cmon,'(i2.2)'),imi
            day_mon_yr=cday//"/"//cmon//"/"//cyr
            Ctime="0"//":"//"00"//":"//"00"
      write (unit=7,fmt=500) 'tileF',day_mon_yr,ctime,anadata(11) 
      write (unit=8,fmt=500) 'tileN',day_mon_yr,ctime,anadata(31) 
      write (unit=9,fmt=500) 'runoff',day_mon_yr,ctime,
     &         anadata(12) 
      write (unit=10,fmt=500) 'runoffN',day_mon_yr,ctime,
     &         anadata(29) 
500         format (A8,2x,A10,2x,A8,2x,f13.6)
C  CALCULATE TILE-FLOW FROM ANALYSIS FILE
C
        IF ((ANADATA(1).GT.DAYTFE(IJK-1)).AND.
     +     (ANADATA(1).LE.DAYTFE(IJK))) THEN
              PTILEF(IJK)= PTILEF(IJK) + ANADATA(11)
c             PACCTF(IJK)= PACCTF(IJK) + ANADATA(11)
              PTILEN(IJK)= PTILEN(IJK) + ANADATA(31)*10.0D0
c             PACCTN(IJK)= PACCTN(IJK) + ANADATA(31)
              PTILEP1(IJK)= PTILEP1(IJK)+ANADATA(48)*10.0D0
              PTILEP2(IJK)= PTILEP2(IJK)+ANADATA(53)*10.0D0
              PTILEP3(IJK)= PTILEP3(IJK)+ANADATA(58)*10.0D0
              NDAYS(IJK) = NDAYS(IJK) + 1
        ENDIF
C CONVERT TOTAL N-MASS TO PPM
c        print*,ijk,anadata(11),ptilef(ijk),daytfe(ijk),anadata(1)
c        IF ((PTILEF(IJK).GT.0.0D0).AND.
         if ((ANADATA(1).EQ.DAYTFE(IJK))) THEN
      IF ((PTILEF(IJK).GT.0.0D0)) then
               PTILEN(IJK) = PTILEN(IJK)/PTILEF(IJK)
               PTILEP1(IJK) = PTILEP1(IJK)/PTILEF(IJK)
               PTILEP2(IJK) = PTILEP2(IJK)/PTILEF(IJK)
               PTILEP3(IJK) = PTILEP3(IJK)/PTILEF(IJK)
      else
               PTILEN(IJK)  = 0.0D0
               PTILEP1(IJK) = 0.0D0
               PTILEP2(IJK) = 0.0D0
               PTILEP3(IJK) = 0.0D0
      endif
C CONVERT TOTAL FLOW TO DAILY FLOW RATE
             PTILEF(IJK) = PTILEF(IJK)/NDAYS(IJK)
             ETILEFD(IJK) = (ETILEF(IJK)-ETILEF(IJK-1))/NDAYS(IJK)
             RMSETF = RMSETF + (ETILEFD(IJK)-PTILEF(IJK))**2
c calculate yearly tile flow
             ijkyear = int(daytfe(ijk))-int(daytfe(1))+1
             etfyear(ijkyear) = etfyear(ijkyear)+
     +                      etilefd(ijk)*ndays(ijk)
             ptfyear(ijkyear) = ptfyear(ijkyear)+
     +                      ptilef(ijk)*ndays(ijk)
             EACCTF(ijk) =  eacctf(ijk-1)+etilefd(ijk)*ndays(ijk)
             PACCTF(ijk) =  pacctf(ijk-1)+
     +                      ptilef(ijk)*ndays(ijk)
c insert missing N data
      if (etnppm(ijk).lt.0.0d0) then
c          eacctn(ijk)=eacctn(ijk-1)
c          pacctn(ijk)=pacctn(ijk-1)
	    eac1=-99
	    eac2=-99
          do ijkl=ijk-1,1,-1
          if (etnppm(ijkl).ge.0.0d0) then
		    eac1=etnppm(ijkl)
	        goto 1234
	    endif
          enddo
1234   continue
	    do ijkm=ijk+1,ntf
          if (etnppm(ijkm).ge.0.0d0) then
		    eac2=etnppm(ijkm)
	        goto 2134
	    endif
          enddo
2134   continue
          if (eac1.lt.0.0d0) then
          etnppm(ijk)=eac2
	    else if (eac2.lt.0.0d0) then
          etnppm(ijk)=eac1
	    else
          etnppm(ijk)=(eac1 + eac2)/2
	    endif
      endif
          if (etnppm(ijk).lt.0.0d0) etnppm(ijk)=0.0d0
c end of inserting missing N data
c from ppm to kg/ha
      if (etnppm(ijk).ge.0.0d0) then
             etnyear(ijkyear) = etnyear(ijkyear)+
     +                      etilefd(ijk)*ndays(ijk)*etnppm(ijk)/10.0d0
             ptnyear(ijkyear) = ptnyear(ijkyear)+
     +                      ptilef(ijk)*ndays(ijk)*ptilen(ijk)/10.0d0
             EACCTN(ijk) =  eacctn(ijk-1)+
     +                      etilefd(ijk)*ndays(ijk)*etnppm(ijk)/10.0d0
             PACCTN(ijk) =  pacctn(ijk-1)+
     +                      ptilef(ijk)*ndays(ijk)*ptilen(ijk)/10.0d0
      endif
C
C WRITE OUT RMSE FOR N AND PESTICIDES IF BOTH SIMULATED AND PREDICTED
      IF ((PTILEF(IJK).GT.0.0D0).AND.(ETILEFD(IJK).GT.0.0D0)) then
         IF (ETNPPM(IJK).GE.0.0D0) THEN
               NTFN0 = NTFN0 + 1
               RMSETN = RMSETN + (ETNPPM(IJK)-PTILEN(IJK))**2
         ENDIF
         IF (EP1PPM(IJK).GE.0.0D0) THEN
               NTFP1 = NTFP1 + 1
               RMSETP1 = RMSETP1 + (EP1PPM(IJK)-PTILEP1(IJK))**2
         ENDIF
         IF (EP2PPM(IJK).GE.0.0D0) THEN
               NTFP2 = NTFP2 + 1
               RMSETP2 = RMSETP2 + (EP2PPM(IJK)-PTILEP2(IJK))**2
         ENDIF
         IF (EP3PPM(IJK).GE.0.0D0) THEN
               NTFP3 = NTFP3 + 1
               RMSETP3 = RMSETP3 + (EP3PPM(IJK)-PTILEP3(IJK))**2
         ENDIF
      ENDIF
C INITIATIZE FOR THE NEXT EVENT
             IJK = IJK + 1
             PTILEF(IJK) = 0.0D0
             PTILEN(IJK) = 0.0D0
             PTILEP1(IJK) = 0.0D0
             PTILEP2(IJK) = 0.0D0
             PTILEP3(IJK) = 0.0D0
             NDAYS(IJK) = 0
           IF (IJK.GT.NTF) GOTO 65
        ENDIF
C63    CONTINUE
  65   CONTINUE
C
        first=.false.
        END DO   !end while
66      Continue
        
          I=1
          NoDAP=0
      READ (UNIT=11,FMT=1000,err=30,end=37) string
      READ (UNIT=11,FMT=1000,err=30,end=37) string
      READ (UNIT=11,FMT=1000,err=30,end=37) string
          DO WHILE (.true.)
      READ (UNIT=11,FMT=*,err=30,end=37) PLDATE(I),EDAP(i),ADAP(I),
     &      AMDAP(I)
          NoDAP=NoDAP+1
          i=i+1        
          End Do   !end while
37        continue

C  WRITE OUT COMPARED EXPERIMENT AND SIMULATION RESULTS
C
C
      IF (ISDATA(5).EQ.1) THEN
       WRITE (UNIT=6,FMT=121)
c        WRITE (UNIT=4,FMT=772)
        WRITE (UNIT=6,FMT=772)
      DO 71 I=2,NTF
      CALL CDATE(NINT((DAYTFE(I)-INT(DAYTFE(I)))*1000),ID,IM,
     +           INT(DAYTFE(I)))
c      if (daytfe(i).lt.2002) then
c      WRITE (UNIT=4, FMT=261) ID,IM,INT(DAYTFE(I)),
c     +   NINT((DAYTFE(I)-INT(DAYTFE(I)))*1000),ETILEFD(I),
c     +      PTILEF(I),ETNPPM(I), PTILEN(I),EP1PPM(I),PTILEP1(I),
c     +      EP2PPM(I),PTILEP2(I),EP3PPM(I),PTILEP3(I)
c      endif
      WRITE (UNIT=6, FMT=261) ID,IM,INT(DAYTFE(I)),
     +   NINT((DAYTFE(I)-INT(DAYTFE(I)))*1000),ETILEFD(I),
     +      PTILEF(I),ETNPPM(I), PTILEN(I),EP1PPM(I),PTILEP1(I),
     +      EP2PPM(I),PTILEP2(I),EP3PPM(I),PTILEP3(I)
c
   71 CONTINUE
      IF (NTF.GT.0) THEN
      CALL STAT(ETILEFD,PTILEF,NTF,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=574) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=575) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      RMSETF = RMSESTAT
      ELSE
c         WRITE (UNIT=4,FMT=120)
         WRITE (UNIT=6,FMT=120)
      ENDIF
      IF (NTFN0.GT.0) THEN
      CALL STAT(ETNPPM,PTILEN,NTF,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=575) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=575) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      RMSETN = RMSESTAT
      ELSE
c         WRITE (UNIT=4,FMT=120)
         WRITE (UNIT=6,FMT=120)
      ENDIF
      IF (NTFP1.GT.0) THEN
      CALL STAT(EP1PPM,PTILEP1,NTF,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=576) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=576) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      ELSE
c         WRITE (UNIT=4,FMT=120)
         WRITE (UNIT=6,FMT=120)
      ENDIF
      IF (NTFP2.GT.0) THEN
      CALL STAT(EP2PPM,PTILEP2,NTF,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=577) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=577) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      ELSE
c         WRITE (UNIT=4,FMT=120)
         WRITE (UNIT=6,FMT=120)
      ENDIF
      IF (NTFP3.GT.0) THEN
      CALL STAT(EP3PPM,PTILEP3,NTF,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=578) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=578) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      ELSE
c        WRITE (UNIT=4,FMT=120)
         WRITE (UNIT=6,FMT=120)
      ENDIF
      ENDIF
C
      IF (ISDATA(6).EQ.1) THEN
c        WRITE (UNIT=4,FMT=121)
c      WRITE(UNIT=4,FMT=776)
        WRITE (UNIT=6,FMT=121)
      WRITE(UNIT=6,FMT=776)
      DO 78 I=1,NWTB
      IF(DAYWTB(I).GE.DAYB.AND.DAYWTB(I).LE.DAYE) THEN
      CALL CDATE(NINT((DAYWTB(I)-INT(DAYWTB(I)))*1000),ID,IM,
     +           INT(DAYWTB(I)))
c     if (daywtb(i).lt.2002) then
c      WRITE (UNIT=4, FMT=260) ID,IM,INT(DAYWTB(I)),
c     +   NINT((DAYWTB(I)-INT(DAYWTB(I)))*1000),EWTB(I),
c     +      PWTB(I)
c      endif
      WRITE (UNIT=6, FMT=260) ID,IM,INT(DAYWTB(I)),
     +   NINT((DAYWTB(I)-INT(DAYWTB(I)))*1000),EWTB(I),
     +      PWTB(I)

         if (ewtb(i).GT.0.0D0) DiffWT = diffwt + (pwtb(i)-ewtb(i))
      ENDIF
   78 CONTINUE
      CALL STAT(EWTB,PWTB,NWTB,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
C           WRITE (UNIT=4,FMT=174) DSQRT(RMSEWT/NWTB)
      RMSEWT = RMSESTAT
      ENDIF
C
      IF (ISDATA(7).EQ.1) THEN
c        WRITE (UNIT=4,FMT=121)
c      WRITE(UNIT=4,FMT=777)
        WRITE (UNIT=6,FMT=121)
      WRITE(UNIT=6,FMT=777)
      DO 79 I=1,NROF
      IF(DAYROF(I).GE.DAYB.AND.DAYROF(I).LE.DAYE) THEN
      CALL CDATE(NINT((DAYROF(I)-INT(DAYROF(I)))*1000),ID,IM,
     +           INT(DAYROF(I)))
c      if (dayrof(i).lt.2002) then
c      WRITE (UNIT=4, FMT=261) ID,IM,INT(DAYROF(I)),
c     +   NINT((DAYROF(I)-INT(DAYROF(I)))*1000),EROF(I),
c     +      PROF(I),ENROF(I),PNROF(I),EP1ROF(I),PP1ROF(I),
c     +      EP2ROF(I),PP2ROF(I),EP3ROF(I),PP3ROF(I)
c      endif
      WRITE (UNIT=6, FMT=261) ID,IM,INT(DAYROF(I)),
     +   NINT((DAYROF(I)-INT(DAYROF(I)))*1000),EROF(I),
     +      PROF(I),ENROF(I),PNROF(I),EP1ROF(I),PP1ROF(I),
     +      EP2ROF(I),PP2ROF(I),EP3ROF(I),PP3ROF(I)
      ENDIF
   79 CONTINUE
      IF (NROF.GT.0) THEN
      CALL STAT(EROF,PROF,NROF,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=674) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=674) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
C         WRITE (UNIT=4,FMT=674) DSQRT(RMSERO/NROF)
      ELSE
c         WRITE (UNIT=4,FMT=120)
         WRITE (UNIT=6,FMT=120)
      ENDIF
      IF (NROFN0.GT.0) THEN
      CALL STAT(ENROF,PNROF,NROF,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=675) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=675) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
C        WRITE (UNIT=4,FMT=675) DSQRT(RMSENRO/NROFN0)
      ELSE
c         WRITE (UNIT=4,FMT=120)
         WRITE (UNIT=6,FMT=120)
      ENDIF
      IF (NROFP1.GT.0) THEN
      CALL STAT(EP1ROF,PP1ROF,NROF,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=676) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=676) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
C        WRITE (UNIT=4,FMT=676) DSQRT(RMSEP1RO/NROFP1)
      ELSE
c         WRITE (UNIT=4,FMT=120)
         WRITE (UNIT=6,FMT=120)
      ENDIF
      IF (NROFP2.GT.0) THEN
      CALL STAT(EP2ROF,PP2ROF,NROF,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=677) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=677) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      ELSE
c         WRITE (UNIT=4,FMT=120)
         WRITE (UNIT=6,FMT=120)
      ENDIF
      IF (NROFP3.GT.0) THEN
      CALL STAT(EP3ROF,PP3ROF,NROF,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=678) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=678) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      ELSE
c         WRITE (UNIT=4,FMT=120)
         WRITE (UNIT=6,FMT=120)
      ENDIF
      ENDIF
C
      IF (ISDATA(8).EQ.1) THEN
c        WRITE (UNIT=4,FMT=121)
c      WRITE(UNIT=4,FMT=779)
        WRITE (UNIT=6,FMT=121)
      WRITE(UNIT=6,FMT=779)
      DO 88 I=1,NPHT
      IF(DAYPHT(I).GE.DAYB.AND.DAYPHT(I).LE.DAYE) THEN
      CALL CDATE(NINT((DAYPHT(I)-INT(DAYPHT(I)))*1000),ID,IM,
     +           INT(DAYPHT(I)))
c      if (daypht(i).lt.2002) then
c      WRITE (UNIT=4, FMT=260) ID,IM,INT(DAYPHT(I)),
c     +   NINT((DAYPHT(I)-INT(DAYPHT(I)))*1000),EPHT(I),
c     +      PPHT(I)
c      endif
      WRITE (UNIT=6, FMT=260) ID,IM,INT(DAYPHT(I)),
     +   NINT((DAYPHT(I)-INT(DAYPHT(I)))*1000),EPHT(I),
     +      PPHT(I)
      ENDIF
   88 CONTINUE
      CALL STAT(EPHT,PPHT,NPHT,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      RMSEPHT = RMSESTAT
      WRITE (UNIT=6,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
C           WRITE (UNIT=4,FMT=174) DSQRT(RMSEPHT/NPHT)
      ENDIF
C
      IF (ISDATA(9).EQ.1) THEN
c        WRITE (UNIT=4,FMT=121)
c      WRITE(UNIT=4,FMT=780)
        WRITE (UNIT=6,FMT=121)
      WRITE(UNIT=6,FMT=780)
      DO 98 I=1,NET
      CALL CDATE(NINT((DAYETS(I)-INT(DAYETS(I)))*1000),IDS,IMS,
     +           INT(DAYETS(I)))
      CALL CDATE(NINT((DAYETE(I)-INT(DAYETE(I)))*1000),IDE,IME,
     +           INT(DAYETE(I)))
c      if (dayete(i).lt.2002) then
c      WRITE (UNIT=4, FMT=270) IDS,IMS,INT(DAYETS(I)),
c     +                        IDE,IME,INT(DAYETE(I)),
c     +                        EAET(I),PAET(I)
c      endif
      WRITE (UNIT=6, FMT=270) IDS,IMS,INT(DAYETS(I)),
     +                        IDE,IME,INT(DAYETE(I)),
     +                        EAET(I),PAET(I)
   98 CONTINUE
      CALL STAT(EAET,PAET,NET,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
C           WRITE (UNIT=4,FMT=174) DSQRT(RMSEAET/NET)
      ENDIF
C
C
      IF (ISDATA(10).EQ.1) THEN
c        WRITE (UNIT=4,FMT=121)
c      WRITE(UNIT=4,FMT=781)
        WRITE (UNIT=6,FMT=121)
      WRITE(UNIT=6,FMT=781)
      DO 83 I=1,NLAI
      IF(DAYLAI(I).GE.DAYB.AND.DAYLAI(I).LE.DAYE) THEN
      CALL CDATE(NINT((DAYLAI(I)-INT(DAYLAI(I)))*1000),ID,IM,
     +           INT(DAYLAI(I)))
c      if (daylai(i).lt.2002) then
c      WRITE (UNIT=4, FMT=260) ID,IM,INT(DAYLAI(I)),
c     +   NINT((DAYLAI(I)-INT(DAYLAI(I)))*1000),ELAI(I),
c     +      PLAI(I)
c      endif
      WRITE (UNIT=6, FMT=260) ID,IM,INT(DAYLAI(I)),
     +   NINT((DAYLAI(I)-INT(DAYLAI(I)))*1000),ELAI(I),
     +      PLAI(I)
      ENDIF
   83 CONTINUE
      CALL STAT(ELAI,PLAI,NLAI,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      RMSELAI = RMSESTAT
      WRITE (UNIT=6,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
C           WRITE (UNIT=4,FMT=174) DSQRT(RMSELAI/NLAI)
      ENDIF
C
      IF (ISDATA(1).EQ.1) THEN
c        WRITE (UNIT=4,FMT=121)
c      WRITE(UNIT=4,FMT=775)
        WRITE (UNIT=6,FMT=121)
      WRITE(UNIT=6,FMT=775)
      DO 74 I=1,NYIELD
      IF(DAYHAR(I).GE.DAYB.AND.DAYHAR(I).LE.DAYE) THEN
      CALL CDATE(NINT((DAYHAR(I)-INT(DAYHAR(I)))*1000),ID,IM,
     +           INT(DAYHAR(I)))
c      if (dayhar(i).lt.2002) then
c      WRITE (UNIT=4, FMT=260) ID,IM,INT(DAYHAR(I)),
c     +   NINT((DAYHAR(I)-INT(DAYHAR(I)))*1000),EYIELD(I),
c     +      PYIELD(I)
c      endif
      WRITE (UNIT=6, FMT=260) ID,IM,INT(DAYHAR(I)),
     +   NINT((DAYHAR(I)-INT(DAYHAR(I)))*1000),EYIELD(I),
     +      PYIELD(I),IYCROP(I)
c      endif
      ENDIF
   74 CONTINUE
      CALL STAT(EYIELD,PYIELD,NYIELD,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
C           WRITE (UNIT=4,FMT=174) DSQRT(RMSEYD/NYIELD)
      RMSEYD = RMSESTAT
      ENDIF
C
      IF (ISDATA(2).EQ.1) THEN
c        WRITE (UNIT=4,FMT=121)
c      WRITE(UNIT=4,FMT=774)
        WRITE (UNIT=6,FMT=121)
      WRITE(UNIT=6,FMT=774)
      DO 73 I=1,NBIOM
      IF(DAYBIOM(I).GE.DAYB.AND.DAYBIOM(I).LE.DAYE) THEN
       do j=1,nyield
	    if (indbiom(i).eq.(indyield(j))) then
	         ehindex=max(eyield(j)/ebiom(i),0.0d0)
	         phindex=max(pyield(j)/pbiom(i),0.0d0)
	    endif
	 enddo
      CALL CDATE(NINT((DAYBIOM(I)-INT(DAYBIOM(I)))*1000),ID,IM,
     +           INT(DAYBIOM(I)))
c      if (daybiom(i).lt.2002) then
c      WRITE (UNIT=4, FMT=260) ID,IM,INT(DAYBIOM(I)),
c     +   NINT((DAYBIOM(I)-INT(DAYBIOM(I)))*1000),EBIOM(I),
c     +      PBIOM(I)
c      endif
      WRITE (UNIT=6, FMT=260) ID,IM,INT(DAYBIOM(I)),
     +   NINT((DAYBIOM(I)-INT(DAYBIOM(I)))*1000),EBIOM(I),
     +      PBIOM(I),IBCROP(I), ehindex,
     +                phindex
      ENDIF
   73 CONTINUE
      CALL STAT(EBIOM,PBIOM,NBIOM,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
C           WRITE (UNIT=4,FMT=174) DSQRT(RMSEBM/NBIOM)
      RMSEBM = RMSESTAT
      ENDIF
C
      IF (ISDATA(3).EQ.1) THEN
c        WRITE (UNIT=4,FMT=121)
c      WRITE(UNIT=4,FMT=773)
        WRITE (UNIT=6,FMT=121)
      WRITE(UNIT=6,FMT=773)
      DO 72 I=1,NNUP
      IF(DAYNUP(I).GE.DAYB.AND.DAYNUP(I).LE.DAYE) THEN
       do j=1,nbiom
	    if (indnup(i).eq.(indbiom(j))) then
	         ehindex=max(enup(i)/ebiom(j),0.0d0)
               if ((ebiom(j).lt.0.0d0).or.(enup(i).lt.0.0d0)) 
     +                 ehindex=0.0d0
	         phindex=max(pnup(i)/pbiom(j),0.0d0)
	    endif
	 enddo
      CALL CDATE(NINT((DAYNUP(I)-INT(DAYNUP(I)))*1000),ID,IM,
     +           INT(DAYNUP(I)))
c      if (daynup(i).lt.2002) then
c      WRITE (UNIT=4, FMT=260) ID,IM,INT(DAYNUP(I)),
c     +   NINT((DAYNUP(I)-INT(DAYNUP(I)))*1000),ENUP(I),
c     +      PNUP(I)
c      endif
      WRITE (UNIT=6, FMT=260) ID,IM,INT(DAYNUP(I)),
     +   NINT((DAYNUP(I)-INT(DAYNUP(I)))*1000),ENUP(I),
     +      PNUP(I),INUCROP(I),ehindex*100,
     +      phindex*100
      ENDIF
   72 CONTINUE
      CALL STAT(ENUP,PNUP,NNUP,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
C            WRITE (UNIT=4,FMT=174) DSQRT(RMSENB/NNUP)
      RMSENB = RMSESTAT
      ENDIF
C
      IF (ISDATA(4).EQ.1) THEN
c        WRITE (UNIT=4,FMT=121)
c      WRITE(UNIT=4,FMT=778)
        WRITE (UNIT=6,FMT=121)
      WRITE(UNIT=6,FMT=778)
      DO 109 I=1,NGRAINN
      IF(DAYGN(I).GE.DAYB.AND.DAYGN(I).LE.DAYE) THEN
       do j=1,nyield
	    if (indgn(i).eq.(indyield(j))) then
	         ehindex=max(egrainn(i)/eyield(j),0.0d0)
	         phindex=max(pgrainn(i)/pyield(j),0.0d0)
	    endif
	 enddo
      CALL CDATE(NINT((DAYGN(I)-INT(DAYGN(I)))*1000),ID,IM,
     +           INT(DAYGN(I)))
c      if (daygn(i).lt.2002) then
c      WRITE (UNIT=4, FMT=260) ID,IM,INT(DAYGN(I)),
c     +   NINT((DAYGN(I)-INT(DAYGN(I)))*1000),EGRAINN(I),
c     +      PGRAINN(I)
c      endif
      WRITE (UNIT=6, FMT=260) ID,IM,INT(DAYGN(I)),
     +   NINT((DAYGN(I)-INT(DAYGN(I)))*1000),EGRAINN(I),
     +      PGRAINN(I),INGCROP(I),ehindex*100,
     +      phindex*100
      ENDIF
  109 CONTINUE
      CALL STAT(EGRAINN,PGRAINN,NGRAINN,RSQ,SLOPE,SINTER,
     *     RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
C           WRITE (UNIT=4,FMT=174) DSQRT(RMSENG/NGRAINN)
      RMSENG = RMSESTAT
      ENDIF
      
      IF (ISDATA(11).EQ.1) THEN
        WRITE (UNIT=6,FMT=121)
        WRITE (UNIT=6,FMT=782)
        DO I=1,NPHEN
        IF(DAYPHEN(I).GE.DAYB*1000.AND.DAYPHEN(I).LE.DAYE*1000) THEN
          DO J=1,NODAP
              IF (INT(DAYPHEN(I)).EQ.INT(PLDATE(J))) THEN
      CALL CDATE(NINT((DAYPHEN(I)/1000-INT(DAYPHEN(I)/1000))*1000),
     +           ID,IM,INT(DAYPHEN(I)/1000))
      WRITE (UNIT=6, FMT=261) ID,IM,INT(DAYPHEN(I)/1000),
     +    NINT((DAYPHEN(I)/1000-INT(DAYPHEN(I)/1000))*1000),
     +    EEPHEN(I),EDAP(J),
     +    EAPHEN(I),ADAP(J),EMPHEN(I),AMDAP(J)  !,ipcrop(I)
              ENDIF
              ENDDO
          ENDIF
        ENDDO
        ENDIF
              
C
C===================================================================
C   Now READ DATA FROM LAYER.PLT (compare vector data by soil layer)
C===================================================================
C------------- READ THE TILTES ----------------------------
C
        READ (UNIT=1,FMT=100,err=29) TITLE
  100 FORMAT(A80///)
C -------------READ NUMBER OF VARIABLES --------------------
       READ (UNIT=1, FMT=*,err=29) NVAR
C
        READ (UNIT=1,FMT=100,err=29) TITLE
        BACKSPACE (UNIT=1)
C------------READ TITLE OF VARIABLES -----------------------
      DO 10 I=1,2+NVAR
             READ (UNIT=1, FMT=113,err=29) TITLE
             IF (TITLE(1:10).EQ.'SOIL WATER') JNAME(1) = I
             IF (TITLE(1:17).EQ.'SLOW RESIDUE POOL') JNAME(2) = I
             IF (TITLE(1:17).EQ.'FAST RESIDUE POOL') JNAME(3) = I
             IF (TITLE(1:15).EQ.'FAST HUMUS POOL') JNAME(4) = I
             IF (TITLE(1:18).EQ.'INTERMEDIATE HUMUS') JNAME(5) = I
             IF (TITLE(1:15).EQ.'SLOW HUMUS POOL') JNAME(6) = I
             IF (TITLE(1:10).EQ.'NO3-N CONC') JNAME(7) = I
             IF (TITLE(1:10).EQ.'NH4-N CONC') JNAME(8) = I
             IF (TITLE(1:20).EQ.'PEST#1 CONC. ABSORB.') JNAME(9) = I
             IF (TITLE(1:20).EQ.'PEST#1 CONC. IN SOL.') JNAME(10) = I
             IF (TITLE(1:17).EQ.'PEST#1 TOTAL MASS') JNAME(11) = I
             IF (TITLE(1:20).EQ.'PEST#2 CONC. ABSORB.') JNAME(12) = I
             IF (TITLE(1:20).EQ.'PEST#2 CONC. IN SOL.') JNAME(13) = I
             IF (TITLE(1:17).EQ.'PEST#2 TOTAL MASS') JNAME(14) = I
             IF (TITLE(1:20).EQ.'PEST#3 CONC. ABSORB.') JNAME(15) = I
             IF (TITLE(1:20).EQ.'PEST#3 CONC. IN SOL.') JNAME(16) = I
             IF (TITLE(1:17).EQ.'PEST#3 TOTAL MASS') JNAME(17) = I
             IF (TITLE(1:11).EQ.'TEMPERATURE') JNAME(18) = I
             IF (TITLE(1:23).EQ.'FRACTION ORGANIC MATTER') JNAME(19) = I
             IF (TITLE(1:13).EQ.'PRESSURE HEAD') JNAME(20) = I
             IF (TITLE(1:14).EQ.'N2O PRODUCTION') JNAME(21) = I
             IF (TITLE(1:14).EQ.'NxO PRODUCTION') JNAME(22) = I
             IF (TITLE(1:14).EQ.'CH4 PRODUCTION') JNAME(23) = I
             IF (TITLE(1:14).EQ.'CO2 PRODUCTION') JNAME(24) = I
             IF (TITLE(1:17).EQ.'SOIL BULK DENSITY') JNAME(25) = I

  10  CONTINUE
113   FORMAT(A80)
             READ (UNIT=1, FMT=113,err=29) TITLE
C ----------------------CHECK IF SIMULATION AND MEASURED DATA ARE MATCHED ------
      DO 102 I=1,Knumb
             IF ((IDEXP(I).EQ.1) .AND. (JNAME(I).EQ.0)) THEN
             PRINT*, '       '
             PRINT*,'NO SIMULATION RESULTS ARE SELECTED FOR    ',FMAT(I)
             PRINT*, '       '
             STOP
             ENDIF
102   CONTINUE
      if (jname(25).eq.0) then
             PRINT*, '       '
             PRINT*,'NO SIMULATION RESULTS ARE SELECTED FOR soil BD'
             PRINT*, '       '
c             STOP
      endif
      if (jname(1).eq.0) then
             PRINT*, '       '
             PRINT*,'NO SIMULATION RESULTS ARE SELECTED FOR soil water'
             PRINT*, '       '
        if ((itypeNO3.eq.1).or.(itypeNH4.eq.1))  STOP
      endif
C----------- READ DATA -------------------------------------
C
      DO WHILE (.true.)
        DO 400 K=1,NN
           READ (UNIT=1,FMT=*,err=29,end=67) (PDATA(IJ,K),IJ=1,2+NVAR)
400     CONTINUE
C
C---------------------CHECK FOR EXPERIMENTAL DATA -----------------
C--------------------- AND TAKE AVERAGES OF PREDICTED VALUES ------
C -------------------- ACCORDING TO HORIZATION --------------------
C
           DO 222 II=1,Knumb
                 DO 300 I=1,NEDATA(II)
                  DO 304 K=1,MHORZ(II,I)
                    DS(K) = DEP(II,I,K)
                    IF (K.EQ.MHORZ(II,I)) THEN
                      DS(K) = MIN(DEP(II,I,K),TLT(NN))
                    ENDIF
304               CONTINUE
                    IJDAY(II,I)=JULDATE(IDAY(II,I),IMONTH(II,I)
     +                       ,IYEAR(II,I))-JULBDAY+JBDAY
                    IF ((IDEXP(II).EQ.1).AND.
     +                  (NINT(PDATA(1,1)).EQ.IJDAY(II,I)).AND.
     +                  (IJDAY(II,I).GE.JBDAY).AND.
     +                  (IJDAY(II,I).LE.IDAYS)) THEN
                  DO 303 K=1,NN
                    PRED1(K) = PDATA(JNAME(II),K)
                    PRED2(K) = PDATA(2,K)
                    PRED3(K) = PDATA(JNAME(1),K)
303               CONTINUE
c                    CALL dbleMATCH (NN, TLT, PRED1, MHORZ(II,I), DS)
C
                  DO 122 IJ=1,NN
                  if (jname(25).eq.0) then
                  SOILP(ii,i,IJ) = soilpss(ij)
                  else
                  SOILP(ii,i,IJ) = pdata(jname(25),ij)
                  endif
122               CONTINUE
            IF ((II.EQ.11).OR.(II.EQ.14).OR.(II.EQ.17)) THEN
c change data from Kg/ha to ug/g 
            do 2272 j = 1, nn
               IF (J.EQ.1) THEN
      pred1(j)=Pdata(jname(ii),j)/(SOILP(ii,i,j)*pred2(j)*0.1D0)
               else
      pred1(j)=Pdata(jname(ii),j)/(SOILP(ii,i,j)*
     +            (pred2(j)-pred2(j-1))*0.1D0)
               endif
2272         continue
c        else
c                   PRED1(K) = PDATA(JNAME(II),K)
            ENDIF
C transfer concentration to volume based from soil based, because difference
c in soil bulk density
c            if ((ii.eq.7).or.(ii.eq.8).or.(ii.eq.11).or.(ii.eq.14)
c     &         .or.(ii.eq.17).or.(ii.eq.19)) then
            if ((ii.ne.1).and.(ii.ne.10).and.(ii.ne.13).and.(ii.ne.16)
     &         .and.(ii.ne.18).and.(ii.ne.20).and.(ii.ne.21).and.
     &         (ii.ne.22).and.(ii.ne.23).and.(ii.ne.24)) then
              do 123 j =1, nn
                pred1(j) = pred1(j) * soilp(ii,i,j)
123           continue
             endif
            if ((ii.eq.10).or.(ii.eq.13).or.(ii.eq.16)) then
              do j =1, nn
                pred1(j) = pred1(j) * pred3(j)
            enddo
            endif
c
          IF ((II.EQ.21).OR.(II.EQ.22).OR.(II.EQ.23).or.(ii.eq.24)) THEN
c change data to N2O, NxO, CH4 per cm soil layer 
            do j = 1, nn
               IF (J.EQ.1) THEN
               pred1(j)=Pdata(jname(ii),j)/pred2(j)
               else
               pred1(j)=Pdata(jname(ii),j)/(pred2(j)-pred2(j-1))
               endif
           enddo
c        else
c                   PRED1(K) = PDATA(JNAME(II),K)
            ENDIF
                 CALL dbleMATCH (NN, TLT, PRED1, MHORZ(II,I), DS)
C
C
                  DO 124 IJ=1,NN
                  SOILPS(IJ) = soilp(ii,i,ij)
124               CONTINUE
                  CALL dbleMATCH (NN, TLT, SOILPS, MHORZ(II,I), DS)
                  CALL dbleMATCH (NN, TLT, PRED3, MHORZ(II,I), DS)
c
              DO 710 J=1,MHORZ(II,I)
c change back to soil based concentration
c            if ((ii.eq.7).or.(ii.eq.8).or.(ii.eq.11).or.(ii.eq.14)
c     &         .or.(ii.eq.17).or.(ii.eq.19)) then
            if ((ii.ne.1).and.(ii.ne.10).and.(ii.ne.13).and.(ii.ne.16)
     &         .and.(ii.ne.18).and.(ii.ne.20).and.(ii.ne.21).and.
     &         (ii.ne.22).and.(ii.ne.23).and.(ii.ne.24)) then
               AVGS(II,I,J) = PRED1(J)/soilps(j)
             else if ((ii.eq.10).or.(ii.eq.13).or.(ii.eq.16)) then      
               AVGS(II,I,J) = PRED1(J)/pred3(j)
             else IF ((II.EQ.21).OR.(II.EQ.22).OR.(II.EQ.23).
     &                or.(ii.eq.24)) THEN
c change data to N2O, NxO, CH4 per cm soil layer 
               IF (J.EQ.1) THEN
               AVGS(II,I,J)=Pred1(j)*ds(j)
               else
               AVGS(II,I,J)=Pred1(j)*(ds(j)-ds(j-1))
               endif
             else
               AVGS(II,I,J) = PRED1(J)
             endif
C-- PRINT OUT CORRECT H2O, NO3, AND NH4 BASED ON UNIT INPUT (UG/G OR UG/ML)
C CONVERT SIMULATED SWC FROM CM3/CM3 TO G/G
            IF ((II.EQ.1).AND.(ITYPESWC.EQ.1)) THEN
                AVGS(II,I,J) = PRED1(J) / SOILPS(J)
            ENDIF
C CONVERT SIMULATED NO3 FROM UG/G TO UG/ML
            IF ((II.EQ.7).AND.(ITYPENO3.EQ.1)) THEN
c               IF (ITYPESWC.EQ.1) THEN
c                 AVGS(II,I,J) = PRED1(J)/AVGS(1,I,J)
c               ELSE
                 AVGS(II,I,J) = PRED1(J)*SOILPS(J)/pred3(J)
c               ENDIF
            ENDIF
C CONVERT SIMULATED NH4 FROM UG/G TO UG/ML
            IF ((II.EQ.8).AND.(ITYPENH4.EQ.1)) THEN
C               IF (ITYPESWC.EQ.1) THEN
C                 AVGS(II,I,J) = PRED1(J)/AVGS(1,I,J)
C               ELSE
                 AVGS(II,I,J) = PRED1(J)*SOILPS(J)/PRED3(J)
C               ENDIF
            ENDIF
710            CONTINUE
C
                    ENDIF
 300             CONTINUE
 222       CONTINUE
         End DO   !end while
67            Continue
C
C  ---- PRINT OUT PAIRED EXPERIMENTAL AND PREDICTED RESULTS-----
C
      DO 333 II=1,Knumb
C=================================================================
C  SCALE SOIL BULK DENSITY FROM ONE GRID TO ANOTHER
C    USE MMATCH SUBROUTINE
C=================================================================
C
        RMSE(II)=0.0D0
c        DIFF(II)=0.0D0
c        RMSEAVG(II)=0.0D0
c        DIFFAVG(II)=0.0D0
c        RMSETM(II)=0.0D0
c        DIFFTM(II)=0.0D0
        NDATAP = 0
        IF (IDEXP(II).EQ.1) THEN
        WRITE (UNIT=6,FMT=121)
c           WRITE (UNIT=4,FMT=752) FMAT(II),FMAT(II),FMAT(II)
           WRITE (UNIT=6,FMT=752) FMAT(II),FMAT(II),FMAT(II)
           DO 600 I=1,NEDATA(II)
        IF ((IJDAY(II,I).GE.JBDAY).AND.
     +     (IJDAY(II,I).LE.IDAYS)) THEN
           IF (MHORZ(II,I).GT.0) THEN
                  DO 404 K=1,MHORZ(II,I)
                    DS(K) = DEP(II,I,K)
                    IF (K.EQ.MHORZ(II,I)) THEN
                      DS(K) = MIN(DEP(II,I,K),TLT(NN))
                    ENDIF
404               CONTINUE
C
                  DO 12 IJ=1,NN
                  SOILPs(IJ) = soilp(ii,i,ij)
                  SOILPe(IJ) = soilpss(ij)
                  PRED3(IJ) = PDATA(JNAME(1),IJ)
12                CONTINUE
        CALL dbleMATCH (NN, TLT, SOILPs, MHORZ(II,I), DS)
        CALL dbleMATCH (NN, TLT, SOILPe, MHORZ(II,I), DS)
        CALL dbleMATCH (NN, TLT, PRED3, MHORZ(II,I), DS)
       ENDIF
              EAVGHZ(I)=0.0D0
              SAVGHZ(I)=0.0D0
C WRITE OUT EXPERIMENT/SIMULATION RESULTS BY LAYER
              DO 700 J=1,MHORZ(II,I)
c
C CONVERTED FROM GRAMETRIC SOIL WATER CONTENT TO VOLUMETRIC SOIL WATER
C AS REQUESTED BY ROB MALONE
c
C              IF ((II.EQ.1).AND.(ITYPESWC.EQ.1).AND.
C     +            (EXPD(II,I,J).GE.0.0D0)) THEN
C                  EXPD(II,I,J) = EXPD(II,I,J) * SOILPe(J)
C              ENDIF
c              WRITE (UNIT=4,FMT=250) IDAY(II,I),IMONTH(II,I),IYEAR(II,I)
c     +            ,IJDAY(II,I),
c     +               DS(J),SOILPs(J),EXPD(II,I,J),AVGS(II,I,J)
            WRITE (UNIT=6,FMT=250) IDAY(II,I),IMONTH(II,I),IYEAR(II,I)
     +            ,IJDAY(II,I),
     +               DS(J),SOILPs(J),EXPD(II,I,J),AVGS(II,I,J)
c added to avoid -99 values
c
              IF ((EXPD(II,I,J).GE.0.0D0).OR.
     +            ((EXPD(II,I,J).LE.0.0D0).AND.(II.EQ.20))) THEN
              IF (II.EQ.1) THEN
                 IF (J.EQ.1) THEN
                   EAVGHZ(I)=EAVGHZ(I)+EXPD(II,I,J)*DS(J)
                   SAVGHZ(I)=SAVGHZ(I)+AVGS(II,I,J)*DS(J)
                 ELSE
                   EAVGHZ(I)=EAVGHZ(I)+EXPD(II,I,J)*(DS(J)-DS(J-1))
                   SAVGHZ(I)=SAVGHZ(I)+AVGS(II,I,J)*(DS(J)-DS(J-1))
                 ENDIF
C CALCULATE TOTAL NITRATE AND AMMONIUM MASS IN SOIL (in kg/ha)
c              ELSE IF ((II.EQ.7).OR.(II.EQ.8).or.(ii.eq.11).or.
c     +                 (ii.eq.14).or.(ii.eq.17).or.(ii.eq.19)) THEN
        else if ((ii.ne.1).and.(ii.ne.10).and.(ii.ne.13).and.(ii.ne.16)
     &         .and.(ii.ne.18).and.(ii.ne.20).and.(ii.ne.21).and.
     &         (ii.ne.22).and.(ii.ne.23).and.(ii.ne.24)) then
                IF (J.EQ.1) THEN
                   EAVGHZ(I)=EAVGHZ(I)+EXPD(II,I,J)*DS(J)
     +                        *1.0D-1*SOILPe(J)
                   SAVGHZ(I)=SAVGHZ(I)+AVGS(II,I,J)*DS(J)
     +                        *1.0D-1*SOILPs(J)
                 ELSE
                   EAVGHZ(I)=EAVGHZ(I)+EXPD(II,I,J)*(DS(J)-DS(J-1))
     +                        *1.0D-1*SOILPe(J)
                   SAVGHZ(I)=SAVGHZ(I)+AVGS(II,I,J)*(DS(J)-DS(J-1))
     +                        *1.0D-1*SOILPs(J)
                 ENDIF
C CALCULATE TOTAL pesticide MASS IN SOIL solution (in kg/ha)
              ELSE if((ii.eq.10).or.(ii.eq.13).or.(ii.eq.16)) then
c              EAVGHZ(I)=EAVGHZ(I)+EXPD(II,I,J)
c              SAVGHZ(I)=SAVGHZ(I)+AVGS(II,I,J)
              IF (EXPD(1,I,J).EQ.0.0D0) EXPD(1,I,J)=PRED3(J)
                 IF (J.EQ.1) THEN
                   EAVGHZ(I)=EAVGHZ(I)+EXPD(II,I,J)*DS(J)
     +                       *EXPD(1,I,J)*1.0D-1
                   SAVGHZ(I)=SAVGHZ(I)+AVGS(II,I,J)*DS(J)
     +                       *PRED3(J)*1.0D-1
                 ELSE
                   EAVGHZ(I)=EAVGHZ(I)+EXPD(II,I,J)*(DS(J)-DS(J-1))
     +                       *EXPD(1,I,J)*1.0D-1
                   SAVGHZ(I)=SAVGHZ(I)+AVGS(II,I,J)*(DS(J)-DS(J-1))
     +                       *PRED3(J)*1.0D-1
                 ENDIF
            ELSE IF (II.EQ.21.OR. II.EQ.22.or.ii.eq.23.or.ii.eq.24) THEN
                   EAVGHZ(I)=EAVGHZ(I)+EXPD(II,I,J)
                   SAVGHZ(I)=SAVGHZ(I)+AVGS(II,I,J)
C  DO NOT NEED TO ACCUMULATE SOIL TEMPERATURE AND WATER PRESSURE HEAD
C              ELSE IF (II.EQ.18.OR. II.EQ.20) THEN
C                   EAVGHZ(I)=EAVGHZ(I)+EXPD(II,I,J)
C                   SAVGHZ(I)=SAVGHZ(I)+AVGS(II,I,J)
C        ELSE
C	     IF (JNAME(20).GT.0) THEN
C                   EAVGHZ(I)=EAVGHZ(I)+EXPD(II,I,J)
C                   SAVGHZ(I)=SAVGHZ(I)+AVGS(II,I,J)
C           ENDIF
          ENDIF
              RMSE(II)=RMSE(II)+(EXPD(II,I,J)-AVGS(II,I,J))**2
c              DIFF(II)=DIFF(II)+(EXPD(II,I,J)-AVGS(II,I,J))
              ENDIF
700           CONTINUE
c              WRITE (UNIT=4,FMT=120)
              WRITE (UNIT=6,FMT=120)
c              RMSEAVG(II)=RMSEAVG(II)+(EAVGHZ(I)-SAVGHZ(I))**2
c              DIFFAVG(II)=DIFFAVG(II)+(EAVGHZ(I)-SAVGHZ(I))
              NDATAP = NDATAP + MHORZ(II,I)
C
         ENDIF
600        CONTINUE
           if (ii.eq.1)  RMSESWCL = DSQRT(RMSE(II)/NDATAP)
           if (ii.eq. 7) RMSENO3L = DSQRT(RMSE(II)/NDATAP)
c           WRITE (UNIT=4,FMT=174) DSQRT(RMSE(II)/NDATAP)
           WRITE (UNIT=6,FMT=174) DSQRT(RMSE(II)/NDATAP)
C
C WRITE OUT TOTAL MASS IN THE SOIL PROFILE
      if (ii.LT.18.or.ii.eq.21.or.ii.eq.22.or.ii.eq.23.or.ii.eq.24) then
        WRITE (UNIT=6,FMT=121)
c           WRITE (UNIT=4,FMT=751) FMAT(II),FMAT(II),FMAT(II)
           WRITE (UNIT=6,FMT=751) FMAT(II),FMAT(II),FMAT(II)
           DO 750 I=1,NEDATA(II)
        IF ((IJDAY(II,I).GE.JBDAY).AND.
     +     (IJDAY(II,I).LE.IDAYS)) THEN
c              WRITE (UNIT=4,FMT=250) IDAY(II,I),IMONTH(II,I),IYEAR(II,I)
c     +            ,JULDATE(IDAY(II,I),IMONTH(II,I),IYEAR(II,I))-
c     +             JULBDAY+JBDAY, EAVGHZ(I),SAVGHZ(I)
c     +            ,dep(ii,i,mhorz(ii,i))
cc            if ((ii.eq.1).or.(ii.eq.7)) then
              WRITE (UNIT=6,FMT=250) IDAY(II,I),IMONTH(II,I),IYEAR(II,I)
     +            ,JULDATE(IDAY(II,I),IMONTH(II,I),IYEAR(II,I))-
     +             JULBDAY+JBDAY, EAVGHZ(I),SAVGHZ(I)
     +            ,dep(ii,i,mhorz(ii,i))
cc            endif
        ENDIF
750         CONTINUE
C
      CALL STAT(EAVGHZ,SAVGHZ,NEDATA(II),RSQ,SLOPE,
     &        SINTER,RMSESTAT,TTEST,T5)
           if (ii.eq.1) RMSESWC = rmsestat
           if (ii.eq.7) RMSENO3 = rmsestat
c      WRITE (UNIT=4,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
C           WRITE (UNIT=4,FMT=174) DSQRT(RMSEAVG(II)/NEDATA(II))
C
c        WRITE (UNIT=4,FMT=121)
           endif
c        WRITE (UNIT=6,FMT=121)
        ENDIF
333   CONTINUE
C
C write out yearly tile flow
c
      if (ntf.gt.0) then
      ntfy = int(daytfe(ntf))-int(daytfe(1)) + 1
        WRITE (UNIT=6,FMT=121)
c      write (unit=4, fmt=252)
      write (unit=6, fmt=252)
      do 567 i=1,ntfy
c      if (daytfe(i).lt.2002) then
c      write (unit=4,fmt=251) int(daytfe(1))+i-1,etfyear(i),ptfyear(i)
c      endif
      write (unit=6,fmt=251) int(daytfe(1))+i-1,etfyear(i),ptfyear(i)
567   continue
      CALL STAT(ETfyear,PTfyear,NTFy,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=574) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=574) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      endif
c
      if (ntfn0.gt.0) then
      ntfny = int(daytfe(ntf))-int(daytfe(1)) + 1
c      write (unit=4, fmt=253)
      write (unit=6, fmt=253)
      do 568 i=1,ntfy
c     if (daytfe(i).lt.2002) then
c      write (unit=4,fmt=251) int(daytfe(1))+i-1,etnyear(i),ptnyear(i)
c      endif
      write (unit=6,fmt=251) int(daytfe(1))+i-1,etnyear(i),ptnyear(i)
568   continue
      CALL STAT(ETnyear,PTnyear,NTFny,RSQ,
     +           SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=4,FMT=574) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      WRITE (UNIT=6,FMT=574) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
      endif
C write out yield one more time for Rob Malone 
c      IF (ISDATA(1).EQ.1) THEN
c      WRITE (UNIT=4,FMT=121)
c      WRITE(UNIT=4,FMT=775)
c      WRITE (UNIT=6,FMT=121)
c      WRITE(UNIT=6,FMT=775)
c      DO I=1,NYIELD
c      CALL CDATE(NINT((DAYHAR(I)-INT(DAYHAR(I)))*1000),ID,IM,
c     +           INT(DAYHAR(I)))
c      if (INT(DAYHAR(I)).ge.1990) then
c      WRITE (UNIT=4, FMT=260) ID,IM,INT(DAYHAR(I)),
c     +   NINT((DAYHAR(I)-INT(DAYHAR(I)))*1000),EYIELD(I),
c     +      PYIELD(I),IYCROP(I)
c      WRITE (UNIT=6, FMT=260) ID,IM,INT(DAYHAR(I)),
c     +   NINT((DAYHAR(I)-INT(DAYHAR(I)))*1000),EYIELD(I),
c     +      PYIELD(I),IYCROP(I)
c      endif
c      enddo
c      CALL STAT(EYIELD,PYIELD,NYIELD,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
c      WRITE (UNIT=6,FMT=174) RMSESTAT,RSQ,SLOPE,SINTER,TTEST,T5
c      RMSEYD = RMSESTAT
c      ENDIF
c WRITE OUT ACCUMULATIVE TILE FLOW AND ACCUMULATIVE N LOSS IN TILE FLOW
C
      IF (ISDATA(5).EQ.1) THEN
        WRITE (UNIT=6,FMT=121)
c        WRITE (UNIT=4,FMT=733)
        WRITE (UNIT=6,FMT=733)
      DO 711 I=2,NTF
      CALL CDATE(NINT((DAYTFE(I)-INT(DAYTFE(I)))*1000),ID,IM,
     +           INT(DAYTFE(I)))
c      if (daytfe(i).lt.2002) then
c      WRITE (UNIT=4, FMT=261) ID,IM,INT(DAYTFE(I)),
c     +   NINT((DAYTFE(I)-INT(DAYTFE(I)))*1000),EACCTF(I),
c     +      PACCTF(I),EACCTN(I), PACCTN(I)
c      endif
      WRITE (UNIT=6, FMT=261) ID,IM,INT(DAYTFE(I)),
     +   NINT((DAYTFE(I)-INT(DAYTFE(I)))*1000),EACCTF(I),
     +      PACCTF(I),EACCTN(I), PACCTN(I)
C     ,EP1PPM(I),PTILEP1(I),EP2PPM(I),PTILEP2(I),EP3PPM(I),PTILEP3(I)
  711 CONTINUE
      ENDIF
C write out RMSE's for selected variables for the Nashua data
cc      WRITE (UNIT=4, FMT=271) RMSEYD,RMSEBM,RMSEWT,RMSETF,RMSETN,
cc     +                        RMSESWC,RMSENO3,RMSENB,RMSENG
      IF (NTF.GT.0) THEN
      CALL STAT(EACCTF,PACCTF,NTF,RSQ,SLOPE,SINTER,RMSESTAT,TTEST,T5)
      RMSEACCTF=RMSESTAT
      ENDIF
        WRITE (UNIT=6,FMT=121)
      WRITE (UNIT=6, FMT=271) RMSEYD,RMSEBM,RMSELAI,RMSEPHT,RMSESWC,
     +   RMSESWCL,RMSEWT,RMSETF,RMSETN,RMSENO3,RMSENB,RMSENG,RMSEACCTF
      IF (ISDATA(5).EQ.1) THEN
c      write (unit=6, fmt=272) diffwt
c write out monthly tile flow and N in tile flow
        WRITE (UNIT=6,FMT=121)
	 write (unit=6,fmt=389)
       do ijkl=1,iyi
         do ijkk=1,12
         write(unit=6,fmt=390) ijkl+iyrstart-1,ijkk,
     &        ptilefmon(ijkl,ijkk),ptilenmon(ijkl,ijkk)
         enddo
       write (unit=6,fmt=120)
       enddo  
      endif
389   format (/9x,'Year     Month       Monthly_tile       
     &  Monthly_N_tile')
390   format (9x,i4,5x,i3,5x,G15.6,5x,G15.6)	 	       


103   FORMAT ("RZWQM2 Version 2.86 Compare to Experimental Results",/
     &    'Intel Fortran',
     &         15X,A3," ",I2.2,", ",I4,"; ",I2.2,":",I2.2,":",I2.2//)
c     &   '"Let Us not Make the Perfect the Enemy of the Essential"',
c     &   '---President Obama Feb 4, 2009'/)
 1000 FORMAT(A255)
272   format (3x,'pred-obse WT',3x,G15.6)
271   FORMAT (/13X,'ROOT MEAN SQUARE ERRORS FOR',13X,//'  YIELD  
     +   BIOMASS     LAI             Plant_HT         P-SWC       
     +  Layer-SWC       WATER-TAB        TILE-FL          N-TILE      
     + SOIL-NO3-N        N-BIOM          N-GRAIN         ACC-TF',
     +  /2(F9.2,1X),15(G15.6,1X))
260   FORMAT (I3,I4,I6,I7,2G15.6,I5,2G15.6)
261   FORMAT (I3,I4,I6,I7,4G15.6,8G15.6)
270   FORMAT (I3,I4,I6,3X,I3,I4,I6,10G15.6)
250   FORMAT (I3,I4,I6,I7,11(G15.6,1X))
251   format (13x,i7,4(G15.6),2x,i3)
120   FORMAT (/)
121   FORMAT ('=====================================================
     &==============================================================')
252   FORMAT (/'TOTAL YEARLY TILE FLOW (CM)',//
     +          13x, '  YEAR   ',3x,' EXP-',3x,'       PRED-'/)
253   FORMAT (/'TOTAL YEARLY N loss to TILE FLOW (kg-N/ha)',//
     +           13x,'  YEAR   ','    EXP-','     PRED-'/)
751   FORMAT (////'TOTAL AMOUNT OF  ',A8,'IN THE PROFILE (CM FOR SWC AND
     + KG N/HA FOR NITROGEN)',//
     +           '  DD MM YY  JULIAN   ',3x,' EXP-',A5,3x,'  PRED-',A5,
     +          3x, 'depth of soil profile (cm)'/)
752   FORMAT (//'COMPARED CONCENTRATION OF ',A8,'PPM FOR PEST,NO3,NH4
     + Kg N/ha FOR N2O, NxO, CH4)',//
     +    'DD MM YYYY  JULIAN       DEPTH  ',2x,'     BD      ',
     +      8x,  'EXP-',A5,6x,'PRED-',A5/)
174   FORMAT (/25X,'RMSE',11X,'R2',11X,'SLOPE',10X,'INTERCEPT',10X,
     &      'T-SLOPE',5X,'T-5% CONFIDENCE'/,19X,6(1X,G15.6)//)
574   FORMAT (/'TILE FLOW',16X,'RMSE',11X,
     &       'R2',11X,'SLOPE',10X,'INTERCEPT',10X,
     &      'T-SLOPE',4X,'T-5% CONFIDENCE'/,19X,6(1X,G15.6))
575   FORMAT (/'NITROGEN IN TILE FLOW',4X,'RMSE',11X,
     &       'R2',11X,'SLOPE',10X,'INTERCEPT',10X,
     &      'T-SLOPE',4X,'T-5% CONFIDENCE'/,19X,6(1X,G15.6))
576   FORMAT ('PESTICIDE1 IN TILE FLOW',2X,'RMSE',11X,
     &       'R2',11X,'SLOPE',10X,'INTERCEPT',10X,
     &      'T-SLOPE',4X,'T-5% CONFIDENCE'/,19X,6(1X,G15.6))
577   FORMAT ('PESTICIDE2 IN TILE FLOW',2X,'RMSE',11X,
     &       'R2',11X,'SLOPE',10X,'INTERCEPT',10X,
     &      'T-SLOPE',4X,'T-5% CONFIDENCE'/,19X,6(1X,G15.6))
578   FORMAT ('PESTICIDE3 IN TILE FLOW',2X,'RMSE',11X,
     &       'R2',11X,'SLOPE',10X,'INTERCEPT',10X,
     &      'T-SLOPE',4X,'T-5% CONFIDENCE'/,19X,6(1X,G15.6))
674   FORMAT (/'RUNOFF',19X,'RMSE',11X,
     &       'R2',11X,'SLOPE',10X,'INTERCEPT',10X,
     &      'T-SLOPE',4X,'T-5% CONFIDENCE'/,19X,6(1X,G15.6))
675   FORMAT ('NITROGEN IN RUNOFF',7X,'RMSE',11X,
     &       'R2',11X,'SLOPE',10X,'INTERCEPT',10X,
     &      'T-SLOPE',4X,'T-5% CONFIDENCE'/,19X,6(1X,G15.6))
676   FORMAT ('PESTICIDE1 IN RUNOFF',5X,'RMSE',11X,
     &       'R2',11X,'SLOPE',10X,'INTERCEPT',10X,
     &      'T-SLOPE',4X,'T-5% CONFIDENCE'/,19X,6(1X,G15.6))
677   FORMAT ('PESTICIDE2 IN RUNOFF',5X,'RMSE',11X,
     &       'R2',11X,'SLOPE',10X,'INTERCEPT',10X,
     &      'T-SLOPE',4X,'T-5% CONFIDENCE'/,19X,6(1X,G15.6))
678   FORMAT ('PESTICIDE3 IN RUNOFF',5X,'RMSE',11X,
     &       'R2',11X,'SLOPE',10X,'INTERCEPT',10X,
     &      'T-SLOPE',4X,'T-5% CONFIDENCE'/,19X,6(1X,G15.6))
177   FORMAT ('---- AVERAGE EXP-PRED       ',F9.3,' ----'//)
772   FORMAT ('TTILE FLOW (CM/DAY) AND TILE N-PPM',//,
     +           'DD MM YYYY    JULIAN    M-TILE       S-TILE
     +        M-NPPM          S-NPPM         M-P1            S-P1      
     +      M-P2          S-P2          M-P3            S-P3'/)
733   FORMAT (/'TILE FLOW (CM) AND TILE N KG/HA',//,
     +           'DD MM YYYY  JULIAN       M-TILE       S-TILE
     +       M-N LOSS       S-N LOSS'/)
773   FORMAT (//'PLANT N UPTAKE (KG N/HA) ',//,
     +           'DD MM YYYY  JULIAN     M-NAboveG      S-NAboveG   Crop
     +      M-%N      S-%N'/)
774   FORMAT (//'PLANT BIOMASS (KG/HA) ',//,
     +           'DD MM YYYY  JULIAN     M-BIOM         S-BIOM     Crop 
     +    M-Hindex     S-Hindex'/)
775   FORMAT (//'PLANT HARVEST YIELD (KG/HA) ',//,
     +        'DD  MM  YYYY    DOY   M-YIELD        S-YIELD      Crop'/)
776   FORMAT (//'WATER TABLE (CM) ',//,
     +           'DD MM YYYY  JULIAN  M-WATER TABLE   S-WATER TABLE'/)
777   FORMAT (//'RUNOFF (CM/DAY) AND RUNOFF N-PPM',//,
     +           'DD MM YYYY  JULIAN     M-RUNO      S-RUNO
     +      M-NPPM    S-NPPM       M-P1      S-P1       M-P2       S-P2
     +      M-P3        S-P3'/)
778   FORMAT (//'GRAIN N UPTAKE (KG N/HA) ',//,
     +           'DD MM YYYY  JULIAN  M-NGrain   S-NGrain     Crop
     +      M-%N      S-%N'/)
779   FORMAT (//'PLANT HEIGHT (CM) ',//,
     +           'DD MM YYYY  JULIAN     M-HEIGHT       S-HEIGHT'/)
780   FORMAT (//'ET (CM) ',//,
     +         'DD  MM  YYYY   DD  MM  YYYY      M-ET         S-ET'/)
781   FORMAT (//'LAI  ',//,
     +           'DD  MM  YYYY    DOY      M-LAI        S-LAI'/)
782   FORMAT (//'PHENOLOGY DATES (PLANTING DATE + DAY AFTER PLANTING) ',
     +     //   'DD   MM  YYYY    DOY    M-EDAP         S-EDAP        
     + M-ADAP         S-ADAP         M-MDAP         S-MDAP'/)          
C     +Crop'/)
C
      CLOSE (UNIT=1)
      CLOSE (UNIT=3)
c      CLOSE (UNIT=4)
      CLOSE (UNIT=5)
      CLOSE (UNIT=6)
      CLOSE (UNIT=7)
      CLOSE (UNIT=8)
      CLOSE (UNIT=9)
      CLOSE (UNIT=10)
      close (unit=11)
9999  RETURN
13    stop 'cannot open layer.plt file, please select vector variables'
23    stop 'observed data are not available --- model run successful'
27    stop 'check your expdata.dat file (horizon data)'
28    stop 'check your analysis data (*.ana) file'
29    stop 'check your layer.plt file'
30    stop 'check your phenology.out file'
36    stop 'check your expdata.dat file (daily data)'
       END
C ----------------------------------------------------------------------
C --- Subroutine to CALCULATE SUMMARY STATISTICS ---
C --- COMPARING YY(I) WITH YYP(I)  ---
C
      SUBROUTINE STAT(YY,YYP,NOB,RSQ,SLOPE,SINTER,RMSE,TTEST,TVAR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION YY(1500),YYP(1500)
      PARAMETER (NP=2)
      YSUM=0.0
      FSUM=0.0
      YFSUM=0.0
      YSSQ=0.0
      FSSQ=0.0
      YFSSQ=0.0
      NOBREAL = 0
      RESSSQ = 0.0D0
      RSQ = -99.0D0
      SLOPE = -99.0D0
      SINTER = -99.0D0
      RMSE = -99.0D0
      TTEST = -99.0D0
      TVAR = -99.0D0
C
      DO 501 I=1,NOB
        IF (YY(I).GT.0.0D0.AND.YYP(I).GT.0.0D0) THEN
          NOBREAL = NOBREAL + 1
          YSUM=YSUM+YY(I)
          FSUM=FSUM+YYP(I)
          YSSQ=YSSQ+YY(I)*YY(I)
          FSSQ=FSSQ+YYP(I)*YYP(I)
          YFSSQ=YFSSQ+YY(I)*YYP(I)
          RESSSQ=RESSSQ + (YY(I)-YYP(I))**2
        ENDIF
501   CONTINUE
C      REGSSQ=YSSQ-RESSSQ
C      RESDF=NOBREAL-NP
C      RESMSQ=RESSSQ/RESDF
      IF (NOBREAL.EQ.0) RETURN
      RMSE=SQRT(RESSSQ/NOBREAL)
C   Sxx
      SXX=YSSQ-YSUM*YSUM/NOBREAL
C   Syy
      SYY=FSSQ-FSUM*FSUM/NOBREAL
C   Sxy
      SXY=YFSSQ-YSUM*FSUM/NOBREAL
      YBAR=YSUM/NOBREAL
      FBAR=FSUM/NOBREAL
      IF (SXX.EQ.0.0D0) RETURN
      SLOPE = SXY/SXX
      SINTER = FBAR - SLOPE * YBAR
      IF (SYY.EQ.0.0D0) RETURN
      RSQ = SXY**2/(SXX*SYY)
C ALFA = 0.05
      IF (NOBREAL.EQ.NP) RETURN
      Z=1.0D0/(NOBREAL-NP)
      TVAR=1.96D0+Z*(2.3779D0+Z*(2.7135D0+Z*(3.187936D0+2.4666D0*Z**2)))
C
C TEST SLOPE = 1.0
C Syx
c  need to check
      SYX = DSQRT(abs(SYY-SLOPE*SXY)/(NOBREAL-NP))
      TTEST = (SLOPE-1.0D0)/(SYX/DSQRT(SXX))
C      DO 600 I=1,NOB
C         YMSE=RESMSQ*(1/NOB+(YY(I)-YBAR)*(YY(I)-YBAR)/YCSSQ)
C         UCLM(I)=YYP(I)+TVAR*SQRT(YMSE)
C         LCLM(I)=YYP(I)-TVAR*SQRT(YMSE)
C         UCLI(I)=YYP(I)+TVAR*SQRT(YMSE+RESMSQ)
C         LCLI(I)=YYP(I)-TVAR*SQRT(YMSE+RESMSQ)
C600   CONTINUE
      RETURN
      END
