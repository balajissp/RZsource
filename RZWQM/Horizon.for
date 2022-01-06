       SUBROUTINE Horizon(NN,JBDAY,JULBDAY,JULEDAY,TLT,DRSPAC,soilpss,
     &            ds,NHOR,itypeswc,itypeno3,itypenh4)
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
       parameter (mxnod=300,maxhor=25,MXANA=135)
       DIMENSION PDATA(25,mxnod)
       Dimension AVGS(25,maxhor)
       DIMENSION IDEXP(25),NEDATA(25)
       DIMENSION JNAME(26), TLT(mxnod),PRED1(mxnod)
       DIMENSION DS(mxnod), SOILP(mxnod),pred2(mxnod),
     +           soilps(mxnod),soilpss(mxnod),soilpe(mxnod),PRED3(MXNOD)
       CHARACTER*80 TITLE
       CHARACTER*8 FMAT(25),ctime
       CHARACTER*255 SCENNAME,string
       character*3 mon(12)
       character*4 cyr
       character*2 cmon,cday
       character*10 day_mon_yr
       integer DATE_TIME(8),IIdex(25),III
       logical :: first=.true. !RM - f95 way to declare variable w/value
c       COMMON SCENNAME
       DATA FMAT/'SWC','SRP','FRP','FHP','IHP','SHP',
     +         'NO3N','NH4N','P1AD','P1SLN','P1TOT',
     +         'P2AD','P2SLN','P2TOT','P3AD','P3SLN','P3TOT',
     +         'ST','SOC','SP','N2O','NxO','CH4','CO2',"SON"/
C
       DATA IDEXP/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
       DATA IIDEX/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
       DATA JNAME/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
       DATA MON /'Jan','Feb','Mar','Apr','May','Jun','Jul'
     &         ,'Aug','Sep','Oct','Nov','Dec'/
C
        OPEN (UNIT=1,FILE='LAYER.PLT',STATUS='OLD',err=13)
        OPEN (UNIT=3,FILE='EXPDATA.DAT',STATUS='OLD',err=23)
        OPEN (UNIT=6,FILE='HORIZON.OUT',STATUS='unknown')
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
C  read the new addition to expdata.dat for horizon.out depth
c      CALL ECHO(3)
      read (3,1000,err=27) string
      read (3,1000,err=27) string
          IF ((string(5:15).EQ.'Horizon.out') .or.
     +       (string(5:15).EQ.'Horizon.OUT') .or.
     +       (string(5:15).EQ.'Horizon.Out') .or.
     +       (string(5:15).EQ.'HORIZON.out') .or.
     +       (string(5:15).EQ.'HORIZON.OUT') .or.
     +       (string(5:15).EQ.'HORIZON.Out')) then
              read (3,1000,err=27) string
              read (3,1000,err=27) string
                 do i=1,nhor
                     ds(i)=0.0d0
                 enddo
c
                 NHOR=inumb(string)
                 backspace (3)
               READ (UNIT=3,FMT=*,err=27) (DS(I),I=1,NHOR)
          else
             backspace (3)
             backspace (3)
             backspace (3)
             backspace (3)
          endif
C
      CALL ECHO(3)
      read (3,1000,err=27) string
        knumb=inumb(string)
      backspace (3)
      READ (UNIT=3,FMT=*,err=27) (IDEXP(I),I=1,knumb)
      III=0
      DO II=1,KNUMB
                    IF (IDEXP(II).EQ.1) THEN
                        III=III+1
                        IIDEX(III)=II
                    ENDIF
      ENDDO
        WRITE (UNIT=6,FMT=121)
        WRITE (UNIT=6,FMT=752) (FMAT(IIDEX(II)),II=1,III)
c
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
             IF (TITLE(1:23).EQ.'FRACTION ORGANIC CARBON') JNAME(19) = I
             IF (TITLE(1:13).EQ.'PRESSURE HEAD') JNAME(20) = I
             IF (TITLE(1:14).EQ.'N2O PRODUCTION') JNAME(21) = I
             IF (TITLE(1:14).EQ.'NxO PRODUCTION') JNAME(22) = I
             IF (TITLE(1:14).EQ.'CH4 PRODUCTION') JNAME(23) = I
             IF (TITLE(1:14).EQ.'CO2 PRODUCTION') JNAME(24) = I
             IF (TITLE(1:25).EQ.'FRACTION ORGANIC NITROGEN') JNAME(25)=I
             IF (TITLE(1:17).EQ.'SOIL BULK DENSITY') JNAME(26) = I

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
      if (jname(26).eq.0) then
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
          CALL CJULDATE(int(JULBDAY+pdata(1,1)-jbday),IDB1,IMB1,IYYYB1)
c          CALL CJULDATE(JULEDAY,IDE,IME,IYYYE)
C
C---------------------CHECK FOR EXPERIMENTAL DATA -----------------
C--------------------- AND TAKE AVERAGES OF PREDICTED VALUES ------
C -------------------- ACCORDING TO HORIZATION --------------------
C
           DO 222 II=1,Knumb
C                 DO 300 I=1,NEDATA(II)
C                  DO 304 K=1,NHOR
C                    DS(K) = DEP(II,I,K)
C                    IF (K.EQ.MHORZ(II,I)) THEN
C                      DS(K) = MIN(DEP(II,I,K),TLT(NN))
C                    ENDIF
C304               CONTINUE
C                    IJDAY(II,I)=JULDATE(IDAY(II,I),IMONTH(II,I)
C     +                       ,IYEAR(II,I))-JULBDAY+JBDAY
                    IF (IDEXP(II).EQ.1) THEN
                  DO 303 K=1,NN
                    PRED1(K) = PDATA(JNAME(II),K)
                    PRED2(K) = PDATA(2,K)
                    PRED3(K) = PDATA(JNAME(1),K)
303               CONTINUE
c                    CALL dbleMATCH (NN, TLT, PRED1, MHORZ(II,I), DS)
C
                  DO 122 IJ=1,NN
                  if (jname(26).eq.0) then
                  SOILP(IJ) = soilpss(ij)
                  else
                  SOILP(IJ) = pdata(jname(26),ij)
                  endif
122               CONTINUE
            IF ((II.EQ.11).OR.(II.EQ.14).OR.(II.EQ.17)) THEN
c change data from Kg/ha to ug/g 
            do 2272 j = 1, nn
               IF (J.EQ.1) THEN
      pred1(j)=Pdata(jname(ii),j)/(SOILP(j)*pred2(j)*0.1D0)
               else
      pred1(j)=Pdata(jname(ii),j)/(SOILP(j)*
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
                pred1(j) = pred1(j) * soilp(j)
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
                 CALL dbleMATCH (NN, TLT, PRED1, NHOR, DS)
C
C
                  DO 124 IJ=1,NN
                  SOILPS(IJ) = soilp(ij)
124               CONTINUE
                  CALL dbleMATCH (NN, TLT, SOILPS, NHOR, DS)
                  CALL dbleMATCH (NN, TLT, PRED3, NHOR, DS)
c
              DO 710 J=1,NHOR
c change back to soil based concentration
c            if ((ii.eq.7).or.(ii.eq.8).or.(ii.eq.11).or.(ii.eq.14)
c     &         .or.(ii.eq.17).or.(ii.eq.19)) then
            if ((ii.ne.1).and.(ii.ne.10).and.(ii.ne.13).and.(ii.ne.16)
     &         .and.(ii.ne.18).and.(ii.ne.20).and.(ii.ne.21).and.
     &         (ii.ne.22).and.(ii.ne.23).and.(ii.ne.24)) then
               AVGS(II,J) = PRED1(J)/soilps(j)
             else if ((ii.eq.10).or.(ii.eq.13).or.(ii.eq.16)) then      
               AVGS(II,J) = PRED1(J)/pred3(j)
             else IF ((II.EQ.21).OR.(II.EQ.22).OR.(II.EQ.23).
     &                or.(ii.eq.24)) THEN
c change data to N2O, NxO, CH4 per cm soil layer 
               IF (J.EQ.1) THEN
               AVGS(II,J)=Pred1(j)*ds(j)
               else
               AVGS(II,J)=Pred1(j)*(ds(j)-ds(j-1))
               endif
             else
               AVGS(II,J) = PRED1(J)
             endif
C-- PRINT OUT CORRECT H2O, NO3, AND NH4 BASED ON UNIT INPUT (UG/G OR UG/ML)
C CONVERT SIMULATED SWC FROM CM3/CM3 TO G/G
            IF ((II.EQ.1).AND.(ITYPESWC.EQ.1)) THEN
                AVGS(II,J) = PRED1(J) / SOILPS(J)
            ENDIF
C CONVERT SIMULATED SOM to SOC using 0.58 as conversion factor
C            IF (II.EQ.19) THEN
C                AVGS(II,J) = PRED1(J) * 0.58d0
C            ENDIF
C CONVERT SIMULATED NO3 FROM UG/G TO UG/ML
            IF ((II.EQ.7).AND.(ITYPENO3.EQ.1)) THEN
c               IF (ITYPESWC.EQ.1) THEN
c                 AVGS(II,I,J) = PRED1(J)/AVGS(1,I,J)
c               ELSE
                 AVGS(II,J) = PRED1(J)*SOILPS(J)/pred3(J)
c               ENDIF
            ENDIF
C CONVERT SIMULATED NH4 FROM UG/G TO UG/ML
            IF ((II.EQ.8).AND.(ITYPENH4.EQ.1)) THEN
C               IF (ITYPESWC.EQ.1) THEN
C                 AVGS(II,I,J) = PRED1(J)/AVGS(1,I,J)
C               ELSE
                 AVGS(II,J) = PRED1(J)*SOILPS(J)/PRED3(J)
C               ENDIF
            ENDIF
710            CONTINUE
C
                    ENDIF
C 300             CONTINUE
 222       CONTINUE
c         End DO   !end while
c67            Continue
C
C  ---- PRINT OUT PAIRED EXPERIMENTAL AND PREDICTED RESULTS-----
C
C      DO 333 II=1,Knumb
C=================================================================
C  SCALE SOIL BULK DENSITY FROM ONE GRID TO ANOTHER
C    USE MMATCH SUBROUTINE
C=================================================================
C
c        IJDAYB1=JULDATE(IDB1,IMB1,IYYYB1)-JULBDAY+JBDAY
        IJDAYB1=JULDATE(IDB1,IMB1,IYYYB1)-JULDATE(31,12,IYYYB1-1)
C
                  DO 12 IJ=1,NN
                  SOILPs(IJ) = soilp(ij)
                  SOILPe(IJ) = soilpss(ij)
                  PRED3(IJ) = PDATA(JNAME(1),IJ)
12                CONTINUE
        CALL dbleMATCH (NN, TLT, SOILPs, NHOR, DS)
        CALL dbleMATCH (NN, TLT, SOILPe, NHOR, DS)
        CALL dbleMATCH (NN, TLT, PRED3, NHOR, DS)
c       ENDIF
C WRITE OUT EXPERIMENT/SIMULATION RESULTS BY LAYER
              DO 700 J=1,NHOR
c
            WRITE (UNIT=6,FMT=250) IDB1,IMB1,IYYYB1
     +            ,IJDAYB1,
     +               DS(J),SOILPs(J),(AVGS(IIDEX(II),J),II=1,III)

700           CONTINUE
c              WRITE (UNIT=4,FMT=120)
C
        End DO   !end while
C333   CONTINUE

67            Continue
C
        WRITE (UNIT=6,FMT=121)
        WRITE (UNIT=6,FMT=177)
390   format (9x,i4,5x,i3,9(5x,G15.6))	 	       
103   FORMAT ("Daily Simulation Results for COMP2EXP.OUT",/
     &    'RZWQM2 Version 4.2 Intel Fortran (2.22.2021)',
     &         15X,A3," ",I2.2,", ",I4,"; ",I2.2,":",I2.2,":",I2.2/)
c     &   '"Let Us not Make the Perfect the Enemy of the Essential"',
c     &   '---President Obama Feb 4, 2009'/)
 1000 FORMAT(A255)
260   FORMAT (I3,I4,I6,I7,2G15.6,I5,2G15.6)
261   FORMAT (I3,I4,I6,I7,4G15.6,14G15.6)
270   FORMAT (I3,I4,I6,3X,I3,I4,I6,10G15.6)
250   FORMAT (I3,I4,I6,I7,26(G15.6,1X))
251   format (13x,i7,8(G15.6),2x,i3)
120   FORMAT (/)
121   FORMAT (415('='))
752   FORMAT ('  DD MM YYYY     DOY     DEPTH    ',4x,'     BD     ',
     +      8x,24(A5,11x)/)
177   FORMAT ('DAY (DD MM YYY DOY)                                 ',/
     = 'DEPTH (cm)                                                 ',/
     = 'SWC, SOIL WATER CONTENT (cm3/cm3 or g/g)                   ',/
     = 'SRP, SLOW RESIDUE POOL (ug-C/g)                             ',/
     = 'FRP, FAST RESIDUE POOL (ug-C/g)                             ',/
     = 'FHP, FAST HUMUS POOL (ug-C/g)                               ',/
     = 'IHP, INTERMEDIATE HUMUS POOL(ug-C/g)                        ',/
     = 'SHP, SLOW HUMUS POOL (ug-C/g)                               ',/
     = 'NO3, NO3-N CONC. (ug/g)                                    ',/
     = 'NH4, NH4-N CONC. (ug/g)                                    ',/
     = 'P1S, PEST#1 CONC. ABSORB. (ug/g)                           ',/
     = 'P1W, PEST#1 CONC. IN SOL. (mg/ml)                          ',/
     = 'P1T, PEST#1 TOTAL MASS (ug/g of soil)                      ',/
     = 'P2S, PEST#2 CONC. ABSORB. (ug/g)                           ',/
     = 'P2W, PEST#2 CONC. IN SOL. (ug/ml)                          ',/
     = 'P2T, PEST#2 TOTAL MASS (ug/g of soil)                      ',/
     = 'P3S, PEST#3 CONC. ABSORB (ug/g)                            ',/
     = 'P3W, PEST#3 CONC. IN SOL. (mg/ml)                          ',/
     = 'P3T, PEST#3 TOTAL MASS (ug/g of soil)                      ',/
     = 'SOILT, SOIL TEMPERATURE (oC)                               ',/
     = 'SOC, TOTAL SOIL ORGANIC C [Expressed as a fraction] (g-C/g)',/
     = 'SP,  SOIL PRESSURE HEAD (cm)                               ',/
     = 'N2O, N2O PRODUCTION (KG N/HA)                              ',/
     = 'NxO, NxO PRODUCTION (KG N/HA)                              ',/
     = 'CH4, CH4 PRODUCTION (KG N/HA)                              ',/)

C
      CLOSE (UNIT=1)
      CLOSE (UNIT=3)
      CLOSE (UNIT=6)
9999  RETURN
13    stop 'cannot open layer.plt file, please select vector variables'
23    stop 'observed data are not available --- model run successful'
27    stop 'check your expdata.dat file (horizon data)'
28    stop 'check your analysis data (*.ana) file'
29    stop 'check your layer.plt file'
30    stop 'check your phenology.out file'
36    stop 'check your expdata.dat file (daily data)'
       END
