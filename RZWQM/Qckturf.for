C $Id: Qckturf.for,v 1.1 2002/08/28 00:00:48 rojas Exp $
C
      SUBROUTINE QUICKTURF(FIRST2,JDAY,NN,CN,RDF,HEIGHT,LAI,TLAI,PLTSLV,
     +    PWRTS,DMDNIT,RM,RCN,RPOOL,PLDEN,JGS,TLT,TL,ISDP1,TADRT,TADRTC,
     +    iyyy,OMSEA,TM,RCO2,resage)
C     JDAY:   DAYS FROM THE BEGINNING OF THE YEAR
C     TNLOSS: TOTAL N LOSS FROM PLANT UPTAKE ON JDAY
C     RMASS:  AMOUNT OF RESIDUE ADDED (KG/HA)
C     RCN:        CN RATIO OF SURFACE RESIDUE
C     CN(1):  CN RATIO OF RPOOL (I,1)
C     CN(2):  CN RATIO OF RPOOL (I,2)
C     FCR:        FRACTION OF CARBON IN ADDED RESIDUE
C     TM:     MEAN AIR TEMPERATURE (C)
C     T3:     3-DAY MOVING AVERAGE TEMP (C) USED TO TRIGGER DORMANCY
C     T10:    10-DAY MOVING AVERAGE TEMP (C) USED TO RELEASE DORMANCY
C     DORM:   FLAG SIGNIFIES DORMANT OVER-WINTER CONDITION
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (INP2 = 42,MAXCUT = 20)
      PARAMETER (CKG2G = 1.0D-5,FCR = 0.4D0)
      PARAMETER (MXNOD=300,MXANA=135)
      LOGICAL FIRST2, DORM
C
      INTEGER JGS(2), JDAY, ICUTD(MAXCUT), DDAY
      DOUBLE PRECISION RPOOL(MXNOD,2), CN(9), RDF(MXNOD), 
     +    LAI, TLAI, TOTNUP(MAXCUT), THEIGHT(MAXCUT), TOTLAI(MAXCUT), 
     +    RMASS, FODDER(MAXCUT), ROOTSHT, FCN(MAXCUT),GSD, TLT(NN),
     +    TL(NN),TADRT(2),TADRTC(2),OMSEA(MXANA),TEMP5(5),TEMP10(10),
     +    rootmass(maxcut),rootcn(maxcut)
C
      SAVE GSD, TOTNUP, THEIGHT, TOTLAI, RMASS, FODDER, STARTDAY
      save rootmass
      DATA ICUTD /MAXCUT * 367/,nocut/0/
C
C==============================================================
C     SET INITIAL PARAMETERS FOR QUICKPLANT SUBROUTINE
C=============================================================
      IF (FIRST2) THEN
        FIRST2 = .FALSE.
        OPEN (INP2,FILE='QCKTURF.DAT',STATUS='OLD')
        CALL ECHO(INP2)
        READ (INP2,*) CUTHIT, CUTHITB, CUTLAI, DDAY, RDFDEP
        CALL ECHO(INP2)
        READ (INP2,*) NUMCUT
        CALL ECHO(INP2)
        DO 10 IP = 1, NUMCUT
          READ (INP2,*) ICUTD(IP), TOTNUP(IP), THEIGHT(IP), TOTLAI(IP),
     +        FODDER(IP), FCN(IP),rootmass(ip),rootcn(ip)
   10   CONTINUE
        CLOSE (INP2)
C
        IF (CUTHITB.GE.THEIGHT(1)) THEN
            CUTHITB=CUTHIT
            CALL CDATE(JDAY,ID,IM,IYYY)
            WRITE(70,1100)
            WRITE(70,1300) ID,IM,IYYY,JDAY
            WRITE(70,1100)
        ENDIF
C       .. DETERMINE WHICH LAYER THE ROOT DEPTH STOPS IN
C       .. FIGURE TOTAL DEPTH (CM) THAT THE ROOTS EXTEND
        IBOT = 1        
        ROOTBOT = 0.0D0 
        ISDP=1
        DO 32 I = 1,NN
          IF (TLT(I) .LE. RDFDEP) THEN
            IBOT = I
            ROOTBOT = TLT(I)
          ENDIF
          IF(TLT(I).LE.ISDP1) ISDP=I
32      CONTINUE
C
        XRT = 2.0D0 / ROOTBOT  !total area for root mass
C       
C       FIND AREA UP TO THE PLANTING DEPTH
        DL = ROOTBOT * TLT(ISDP)
        RDF(1) = TLT(1) * TLT(1) / DL / TL(1)
        Y2 = RDF(1) * TL(1)
        DO 30 I = 2, ISDP
          Y1 = TLT(I) * TLT(I) / DL
          RDF(I) = (Y1 - Y2) / TL(I)
          Y2 = Y1
   30   CONTINUE
C
C       .. BELOW THE ROOTING DEPTH
        DDL = ROOTBOT * (ROOTBOT - TLT(ISDP))
        DO 35 I = ISDP+1, IBOT
          Y1 = 1.0D0 - (ROOTBOT-TLT(I))**2.0D0 / DDL
          RDF(I) = (Y1 - Y2 ) / TL(I)
          Y2 = Y1
   35   CONTINUE
C        
C       .. NORMALIZE THE ROOT DISTRIBUTION == 1
        TRDF = 0.0D0
        DO 38 I =1,IBOT
          TRDF = TRDF + RDF(I)
   38   CONTINUE
        DO 39 I = 1,IBOT
          RDF(I) = RDF(I) / TRDF
   39   CONTINUE

C=============================================================
C       INITIALIZE PLANT GROWTH NUMBERS
C=============================================================
        NC = NUMCUT + 1
        TOTNUP(NC) = TOTNUP(1)
        THEIGHT(NC) = THEIGHT(1)
        CUTLAIB = CUTHITB / THEIGHT(1) * TOTLAI(1)
        TOTLAI(NC) = CUTLAIB
        FODDER(NC) = FODDER(1)
        FCN(NC) = FCN(1)
C
        PLTSLV = PLDEN * 1.0D3
        PWRTS = PLDEN * 1.0D3
        JGS(1) = 1
        DO 50 I = 1, NUMCUT
          IF (JDAY.LE.ICUTD(I)) GO TO 60
   50   CONTINUE
        I = NUMCUT + 1
   60   ICUT = I
        IF (ICUT .EQ. 1) THEN
          STARTN = ((CUTHITB-CUTHIT)/(THEIGHT(1)-CUTHIT)) * TOTNUP(1)
          LAI = CUTLAIB
        ELSE
          STARTN = 0.0D0
          LAI = CUTLAI
        ENDIF
        HEIGHT = CUTHITB
        if (icut.ne.nc) then
        GSD = DBLE(ICUTD(ICUT) - JDAY)
        else 
        GSD = DBLE(ICUTD(ICUT) - JDAY + icutd(1))
        nocut=1                                      !no cut the first year
        endif
        IDAY = 1
        IGDAY = 0
        DORM = .FALSE.
        IF (JDAY.LE.DDAY) DORM = .TRUE.
        DO 82 I=1,5
          TEMP5(I)=0.0D0
   82   CONTINUE
        DO 92 I=1,10
          TEMP10(I)=0.0D0
   92   CONTINUE
      END IF
C     
C=============================================================
C     DETERMINE DORMANCY FOR OVER WINTER CROPS
C============================================================= 
        T5=AVEMOV(TEMP5,5,TM,IDAY)
        T10=AVEMOV(TEMP10,10,TM,IDAY)
C        IF((T10.LT.0.0D0).AND..NOT.DORM) THEN
      IF (JDAY.LT.DDAY.AND..NOT.DORM) THEN
        DORM = .TRUE.
        DMDNIT = 0.0D0
      END IF
c      IF ((JDAY.GT.DDAY.OR.T10.GT.0.0).AND.DORM) THEN
      IF (JDAY.GE.DDAY.AND.DORM) THEN
        DORM = .FALSE.
        GSD = DBLE(ICUTD(1)+IGDAY-DDAY)
C        IGDAY = 0
      END IF
C=============================================================
C     .. IF NOT IN A DORMANT STATE, GROW THE SUCKER
C=============================================================
      IF (.NOT.DORM .AND. GSD.NE.0.0D0) THEN
C=============================================================
C       CALCULATE TOTAL LOSSES OF NITROGEN FROM THE SOIL PROFILE (KG N/HA)
C       FROM PLANT UPTAKE OR LEACHING
C       USE A TRIANGULAR PLANT UPTAKE MODEL
C=============================================================
        DO 70 I = 1, NUMCUT
          IF (JDAY.LE.ICUTD(I)) GO TO 80
   70   CONTINUE
        I = NUMCUT + 1
   80   ICUT = I
        IF (ICUT.EQ.2) STARTN = 0.0D0
        FACT = 2.0D0 / (GSD*GSD)
        IGDAY = IGDAY + 1
C       
C=============================================================
C       DETERMINE NITROGEN UPTAKE FOR TODAY (G/PLANT)
C=============================================================
        IF (ICUT.EQ.NUMCUT+1) THEN
C          DMDNIT = FACT * IGDAY * TOTNUP(ICUT) / PWRTS * 1.0D3 * CUTHITB
C     +        / THEIGHT(1)
          if ((nocut.ne.1))  !.and.(numcut.eq.1))
     +          GSD = DBLE(ICUTD(ICUT) - ICUTD(ICUT-1) + ICUTD(1))  !AVERAGE UPTAKE RATE
          DMDNIT = TOTNUP(ICUT)/GSD/ PWRTS * 1.0D3  
          STARTN = STARTN + DMDNIT * PWRTS * 1.0D-3    !NEED TO BE CHANGED BECAUSE DMDNIT MAY NOT BE MET
        ELSE
          DMDNIT = (TOTNUP(ICUT)-STARTN)/GSD / PWRTS * 1.0D3
        END IF
C=============================================================
C       DETERMINE PLANT LAI AND HEIGHT FOR TODAY
C=============================================================
        IF (ICUT.EQ.1) THEN
          LAI = (TOTLAI(ICUT)-CUTLAIB) / GSD * IGDAY + CUTLAIB
        ELSE
          LAI = (TOTLAI(ICUT)-CUTLAI) / GSD * IGDAY + CUTLAI
        END IF
        LAI = MIN(LAI,TOTLAI(ICUT))
        TLAI = LAI
        IF (ICUT.EQ.1) THEN
          HEIGHT = (THEIGHT(ICUT)-CUTHITB) / GSD * IGDAY + CUTHITB
        ELSE
          HEIGHT = (THEIGHT(ICUT)-CUTHIT) / GSD * IGDAY + CUTHIT
        END IF
        HEIGHT = MIN(HEIGHT,THEIGHT(ICUT))
C       
C=================================================================
C       CHECK FOR HARVEST TIME AND RESIDUE INPUT AND PARTITION INTO 
C       RESIDUE SURFACE AND SUBSURFACE POOLS
C=================================================================
        IF (JDAY.EQ.ICUTD(ICUT)) THEN
C          GSD = DBLE(ICUTD(ICUT+1) - ICUTD(ICUT))
C        LAI = MIN(LAI,TOTLAI(ICUT))
C        HEIGHT = MIN(HEIGHT,THEIGHT(ICUT))
          STARTN=0.0D0
          GSD = DBLE(ICUTD(ICUT+1) - ICUTD(ICUT))
          IDAY = 1
          IGDAY = 0
          JGS(2) = 1
          nocut=0
C     ..ADJUST RESIDUE AGE (AGE IS RESET IN HARVST ROUTINE)
      RESAGE=0.0d0
C          IF (FODDER(ICUT).GT.0.0D0.AND.FCN(ICUT).GT.0.0D0) THEN
          IF(FCN(ICUT).GT.0.0D0)  OMSEA(22)=fodder(ICUT)*FCR/FCN(ICUT)
          IF(ROOTCN(ICUT).GT.0.0D0) 
     &        OMSEA(23)=rootmass(ICUT)*FCR/RootCN(ICUT)
            OMSEA(41)=fodder(ICUT)
            OMSEA(42)=rootmass(ICUT)
            RCN = CNEW(RM,FODDER(ICUT),RCN,FCN(ICUT),0)
            RM = RM + FODDER(ICUT)
C           .. ASSUME ROOT:SHOOT RATIO = 1:8
c            ROOTSHT = 0.125D0
C           ..ADD TO SOIL RESIDUE POOLS
            DO 90 I = 1, NN
c              RMASS = FODDER(ICUT) * ROOTSHT * RDF(I)
              RMASS = ROOTmass(icut) * RDF(I)
              IF (rootCN(ICUT).LE.CN(1)) THEN
                S = 0.0D0
              ELSE IF (rootCN(ICUT).GE.CN(2)) THEN
                S = 1.0D0
              ELSE
                S = (1.0D0/rootCN(ICUT)-1.0D0/CN(1))/(1.0D0/CN(2)-1.0D0/
     +              CN(1))
              END IF
c              RMASS=0.0D0   !ASSUME NO ROOT DEATH, 4-4-2009
              RPOOL(I,1) = RPOOL(I,1) + RMASS * (1.0D0-S) * CKG2G * FCR
              RPOOL(I,2) = RPOOL(I,2) + RMASS * S * CKG2G * FCR
c need to check if qckplnt and qckturf need this mass balance or not. Liwang Ma, 5-30-2007
          TADRT(1) = TADRT(1)+RMASS*(1.0D0-S)*1.0D5*CKG2G* FCR/CN(1)
          TADRT(2) = TADRT(2) + RMASS *S*1.0D5*CKG2G* FCR/CN(2)
          TADRTC(1) = TADRTC(1)+RMASS*(1.0D0-S)*1.0D5*CKG2G* FCR
          TADRTC(2) = TADRTC(2) + RMASS *S*1.0D5*CKG2G* FCR
              IF (rootCN(ICUT).LE.CN(1)) THEN
          RCO2=RCO2+RMASS*FCR-(RMASS*FCR*(1.0D0-S)/ROOTCN(ICUT))*CN(1)
              ELSE IF (rootCN(ICUT).GE.CN(2)) THEN
          RCO2=RCO2+RMASS*FCR-(RMASS*FCR*S/ROOTCN(ICUT))*CN(2)
              endif
c
   90       CONTINUE
C          END IF
      CALL CDATE(JDAY,ID,IM,IYYY)
      WRITE(70,1100)
      WRITE(70,1200) ID,IM,IYYY,JDAY
      WRITE(70,1100)
        END IF
      END IF
C=============================================================
C       OUTPUT STUFF
C=============================================================
        WRITE (*,1000) (DMDNIT*PWRTS*1.0D-3), LAI, HEIGHT
        OMSEA(62)=HEIGHT
        OMSEA(63)=RDFDEP
        OMSEA(43)=LAI
C       
      IDAY = IDAY + 1
C     
      RETURN
 1000 FORMAT (' QCKTURF==> NITROGEN DEMAND: ',G10.5,2X,
     +    'LEAF AREA INDEX: ',F5.2,2X,'HEIGHT: ',F8.2)
 1100 FORMAT(70('*'))
 1200 FORMAT(/'Quick Turf Cut On ',
     +      4('-'),I3,'/',I2,'/',I4,' -----',I6,'  ----',
     +'AS SCHEDULED'/)
 1300 FORMAT(/'Quick Turf Cut On ',
     +      4('-'),I3,'/',I2,'/',I4,' -----',I6,'  ----',
     +'INITIAL HEIGHT IS TOO HIGH'/)
      END
