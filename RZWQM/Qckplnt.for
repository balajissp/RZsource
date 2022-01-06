C $Id: Qckplnt.for,v 1.1 2002/08/28 00:00:48 rojas Exp $
C
      SUBROUTINE QUICKPLANT(FIRST1,JDAY,NN,CN,RDF,HEIGHT,LAI,TLAI,
     +    PLTSLV,PWRTS,DMDNIT,RM,RCN,RPOOL,PLDEN,JGS,TM,PLNAME,TLT,TL,
     +    ISDP1,TADRT,TADRTC,iyyy,omsea,RCO2,rwl,rootn,resage)
C     JDAY:	DAYS FROM THE BEGINNING OF THE YEAR
C     TNLOSS:	TOTAL N LOSS FROM PLANT UPTAKE ON JDAY
C     RMASS:	AMOUNT OF RESIDUE ADDED (KG/HA)
C     RCN:		CN RATIO OF SURFACE RESIDUE
C     CN(1):	CN RATIO OF RPOOL (I,1)
C     CN(2):	CN RATIO OF RPOOL (I,2)
C     FCR:		FRACTION OF CARBON IN ADDED RESIDUE
C     TM:     MEAN AIR TEMPERATURE (C)
C     T3:     3-DAY MOVING AVERAGE TEMP (C) USED TO TRIGGER DORMANCY
C     T10:    10-DAY MOVING AVERAGE TEMP (C) USED TO RELEASE DORMANCY
C     DORM:   FLAG SIGNIFIES DORMANT OVER-WINTER CONDITION
C     ISDP:   SEED PLANTING DEPTH (cm)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(INP2=42)
      PARAMETER(CKG2G=1.0D-5,FCR=0.4D0) !,REPRO=0.7D0)
      PARAMETER(MXNOD=300,MXANA=135)
      LOGICAL FIRST1,DORM
      CHARACTER PLNAME*30,PLDES*30,UPCASE*1
C
      INTEGER PLNTDAY,GSD,GSD2,JGS(2),JDAY,igdaypeak
      DOUBLE PRECISION PROP(366),RPOOL(MXNOD,2),CN(9),RDF(MXNOD),LAI,
     +    TLAI,TOTNUP,THEIGHT,TOTLAI,RMASS,FODDER,ROOTSHT,TEMP5(5),
     +    TEMP10(10),TL(NN),TLT(NN),TADRT(2),TADRTC(2),omsea(MXANA),
     +    lai0,lai1,height0,rwl(NN),rootn(NN)
C
      SAVE PROP,PLNTDAY,GSD,TOTNUP,THEIGHT,TOTLAI,RMASS,FODDER
      DATA GSD/0.0/,TOTNUP/0.0/,THEIGHT/0.0/,TOTLAI/0.0/,FODDER/0.0/,
     +     FCN/0.0/,DDAY/0.0/,RDFDEP/0.0/,ROOTCN/0.0/ROOTMASS/0.0/
C
C==============================================================
C     SET INITIAL PARAMETERS FOR QUICKPLANT SUBROUTINE
C=============================================================
      IF(FIRST1) THEN
        FIRST1=.FALSE.
        OPEN(INP2,FILE='QCKPLNT.DAT',STATUS='OLD')
        IDX=0
        CALL ECHO(INP2)
        READ(INP2,*) NPL
        DO 10 IP=1,NPL
          READ(INP2,'(A30)') PLDES
        DO I = 1, LEN(PLDES)
         PLDES(I:I) = UPCASE(PLDES(I:I))
        END DO
          
          IF(INDEX(PLDES,PLNAME(1:ITRIM(PLNAME))).GT.0) IDX=IP
   10   CONTINUE
        CALL ECHO(INP2)
        DO 20 IP=1,NPL
          READ(INP2,*) IT1,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13
          IF(IP.EQ.IDX) THEN
            GSD=IT1
            TOTNUP=T1
            THEIGHT=T2
            TOTLAI=T3
            FODDER=T4
            FCN=T5
            DDAY=T6
            RDFDEP=T7
            ROOTMASS=T8
            ROOTCN=T9
            lai0=T10
            lai1=T11
            height0=T12
            REPRO=T13
            RTDEP=ISDP1
            if (repro.eq.0.0d0) repro=0.70d0
          ENDIF
   20   CONTINUE
        CLOSE(INP2)
C       
C=============================================================
C       INITIALIZE PLANT GROWTH NUMBERS
C=============================================================
        PLTSLV=PLDEN*1.0D3
        PWRTS=PLDEN*1.0D3
        PLNTDAY=JDAY
        JGS(1)=1
        STARTN=0.0D0
        DO 80 I=1,5
          TEMP5(I)=0.0D0
   80   CONTINUE
        DO 90 I=1,10
          TEMP10(I)=0.0D0
   90   CONTINUE
C=============================================================
C       CALCULATE TOTAL LOSSES OF NITROGEN FROM THE SOIL PROFILE (KG N/HA)
C       FROM PLANT UPTAKE OR LEACHING
C       USE A TRIANGULAR PLANT UPTAKE MODEL
C=============================================================
        RUN=NINT(GSD*REPRO)   !ASSUME 70% OF GSD TO BE REPRODUCTIVE STAGE PER GREY MCMASTER SUGGESTION
        IF(PLNTDAY+GSD.LT.366) THEN
          VOLUME=TOTNUP*0.5D0*GSD*REPRO
          FACT=TOTNUP/GSD/VOLUME
          DO 100 I=1,366
            IF(I.LE.RUN) THEN
              PROP(I)=I*FACT
              PROP(GSD-I)=PROP(I)
            ENDIF
  100     CONTINUE
        ENDIF
        IDAY=1
        IGDAY=0
        DORM=.FALSE.
         LAI=LAI0
         height=height0
         igdaypeak=GSD
      ENDIF
C     
C=============================================================
C     DETERMINE DORMANCY FOR OVER WINTER CROPS
C=============================================================
      IF(PLNTDAY+GSD.GT.366) THEN
C       .. WINTER BASED CROP
        T5=AVEMOV(TEMP5,5,TM,IDAY)
        T10=AVEMOV(TEMP10,10,TM,IDAY)
        IF(JDAY.GE.PLNTDAY.AND..NOT.DORM) THEN
          FACT=TOTNUP/GSD/(TOTNUP*0.5D0*GSD*REPRO)
c          PROP(IGDAY+1)=FACT*IDAY
          IF(T5.LT.0.0D0) DORM=.TRUE.
        ENDIF
c  Liwang Ma Dec 30, 2004
        IF(JDAY.eq.PLNTDAY) THEN
          j=0
          DO 111 I=1+IGDAY,366
            IF(I.LE.RUN+IGDAY) THEN
              J=J+1
              PROP(I)=J*FACT
              PROP(GSD-J+IGDAY)=PROP(I)
            ENDIF
  111     CONTINUE
	endif
c Liwang Ma
        IF((JDAY.LT.PLNTDAY.AND.JDAY.GE.INT(DDAY)).AND.T10.GE.0.0D0.AND.
     +      DORM) THEN
          DORM=.FALSE.
          LAI=LAI0
          height=height0
          GSD2=GSD-IDAY
          RUN=NINT(GSD2*REPRO)
          VOLUME=(TOTNUP-STARTN)*0.5D0*GSD2*REPRO
          FACT=(TOTNUP-STARTN)/GSD2/VOLUME
          J=0
          DO 110 I=1+IGDAY,366
            IF(I.LE.RUN+IGDAY) THEN
              J=J+1
              PROP(I)=J*FACT
              PROP(GSD2-J+IGDAY)=PROP(I)
            ENDIF
  110     CONTINUE
        ENDIF
        DMDNIT=0.0D0
      ELSE
        DORM=.FALSE.
      ENDIF
C=============================================================
C     .. IF NOT IN A DORMANT STATE, GROW THE SUCKER
C=============================================================
      IF(.NOT.DORM) THEN
        IGDAY=IGDAY+1
C       
C=============================================================
C       DETERMINE NITROGEN UPTAKE FOR TODAY (G/PLANT)
C=============================================================
        DMDNIT=PROP(IGDAY)*TOTNUP/PWRTS*1.0D3
        STARTN=STARTN+DMDNIT*PWRTS*1.0D-3  !NEED TO BE CHANGED BECAUSE DMDNIT MAY NOT BE MET
C=============================================================
C       DETERMINE PLANT LAI AND HEIGHT FOR TODAY
C=============================================================
c Liwang Ma, October 5,2005
c        LAI=TOTLAI/RUN*IGDAY
c        HEIGHT=THEIGHT/RUN*IGDAY
        LAI=LAI+prop(IGday)*totlai/REPRO
        if (LAI.gt.totlai) then
	    igdaypeak=igday-1
        endif
        if (igday.gt.igdaypeak) then
          LAI=totLAI-(totlai-LAI1)/(GSD-igdaypeak)*(igday-igdaypeak)
        endif
        LAI=MIN(LAI,TOTLAI)
        TLAI=LAI
        HEIGHT=HEIGHT+prop(igday)*theight/REPRO
        HEIGHT=MIN(HEIGHT,THEIGHT)
        Droot=Droot+prop(igday)*rootmass/REPRO
        Droot=MIN(Droot,Rootmass)
c Liwang Ma
C       
C       .. DETERMINE WHICH LAYER THE ROOT DEPTH STOPS IN
C       .. FIGURE TOTAL DEPTH (CM) THAT THE ROOTS EXTEND
        RTDEP=RTDEP+PROP(IGDAY)*(RDFDEP-ISDP1)/REPRO
        RTDEP=MIN(RTDEP,RDFDEP)
        IBOT=1
        ROOTBOT=0.0D0
        ISDP=1
        DO 30 I=1,NN
          IF(TLT(I).LE.RTDEP) THEN
            IBOT=I
            ROOTBOT=TLT(I)
          ENDIF
          IF(TLT(I).LE.ISDP1) ISDP=I
   30   CONTINUE
C       
        XRT=2.0D0/ROOTBOT       !total area for root mass
C       
C       FIND AREA UP TO THE PLANTING DEPTH
        DL=ROOTBOT*TLT(ISDP)
        RDF(1)=TLT(1)*TLT(1)/DL/TL(1)
        Y2=RDF(1)*TL(1)
        DO 40 I=2,ISDP
          Y1=TLT(I)*TLT(I)/DL
          RDF(I)=(Y1-Y2)/TL(I)
          Y2=Y1
   40   CONTINUE
C       
C       .. BELOW THE PLANTING DEPTH
        DDL=ROOTBOT*(ROOTBOT-TLT(ISDP))
        DO 50 I=ISDP+1,IBOT
          Y1=1.0D0-(ROOTBOT-TLT(I))**2.0D0/DDL
          RDF(I)=(Y1-Y2)/TL(I)
          Y2=Y1
   50   CONTINUE
C       
C       .. NORMALIZE THE ROOT DISTRIBUTION == 1
        TRDF=0.0D0
        DO 60 I=1,IBOT
          TRDF=TRDF+RDF(I)
   60   CONTINUE
        DO 70 I=1,IBOT
          RDF(I)=RDF(I)/TRDF
          RWL(I)=Droot*RDF(I)
          RootN(i)=RWL(I)*FCR/RootCN
   70   CONTINUE
C       
C=================================================================
C       CHECK FOR HARVEST TIME AND RESIDUE INPUT AND PARTITION INTO 
C       RESIDUE SURFACE AND SUBSURFACE POOLS
C=================================================================
        IF(IDAY.GE.GSD) THEN
          FIRST1=.TRUE.
          JGS(2)=1
C     ..ADJUST RESIDUE AGE (AGE IS RESET IN HARVST ROUTINE)
      RESAGE=0.0d0
          IF(FCN.GT.0.0D0)  OMSEA(22)=fodder*FCR/FCN
          IF(ROOTCN.GT.0.0D0) OMSEA(23)=rootmass*FCR/RootCN
            OMSEA(41)=fodder
            OMSEA(42)=rootmass
            RCN=CNEW(RM,FODDER,RCN,FCN,0)
            RM=RM+FODDER
C           .. ASSUME ROOT:SHOOT RATIO = 1:4
            ROOTSHT=0.25D0
C           ..ADD TO SOIL RESIDUE POOLS
            DO 120 I=1,NN
              IF (ROOTMASS.EQ.0.0D0) THEN
                 RMASS=FODDER*ROOTSHT*RDF(I)
              ELSE 
                 RMASS=ROOTMASS*RDF(I)
              ENDIF
              IF(ROOTCN.LE.CN(1)) THEN
                S=0.0D0
              ELSEIF(ROOTCN.GE.CN(2)) THEN
                S=1.0D0
              ELSE
                S=(1.0D0/ROOTCN-1.0D0/CN(1))/(1.0D0/CN(2)-1.0D0/CN(1))
              ENDIF
              RPOOL(I,1)=RPOOL(I,1)+RMASS*(1.0D0-S)*CKG2G*FCR
              RPOOL(I,2)=RPOOL(I,2)+RMASS*S*CKG2G*FCR
c need to check if qckplnt and qckturf need this mass balance or not. Liwang Ma, 5-30-2007
          TADRT(1) = TADRT(1)+RMASS*(1.0D0-S)*1.0D5*CKG2G*FCR/CN(1)
          TADRT(2) = TADRT(2) + RMASS*S*1.0D5*CKG2G*FCR/CN(2)
          TADRTC(1) = TADRTC(1)+RMASS*(1.0D0-S)*1.0D5*CKG2G*FCR
          TADRTC(2) = TADRTC(2) + RMASS *S*1.0D5*CKG2G*FCR
              IF(ROOTCN.LE.CN(1)) THEN
          RCO2=RCO2+RMASS*FCR-(RMASS*FCR*(1.0D0-S)/ROOTCN)*CN(1)
              ELSEIF(ROOTCN.GE.CN(2)) THEN
          RCO2=RCO2+RMASS*FCR-(RMASS*FCR*S/ROOTCN)*CN(2)
              endif
c
  120       CONTINUE
      CALL CDATE(JDAY,ID,IM,IYYY)
      WRITE(70,1100)
      WRITE(70,1200) ID,IM,IYYY,JDAY
      WRITE(70,1100)
c
        ENDIF
      ENDIF
C=============================================================
C       OUTPUT STUFF
C=============================================================
        WRITE(*,1000) JDAY,(DMDNIT*PWRTS*1.0D-3),LAI,HEIGHT
C       
        OMSEA(62)=HEIGHT
        OMSEA(63)=RTDEP
        OMSEA(43)=LAI
        OMSEA(45)=DBLE(IDAY/GSD)
        IDAY=IDAY+1
C     
      RETURN
 1000 FORMAT(' DAY',I4,'  NITROGEN DEMAND: ',E10.5,2X,
     +    'LEAF AREA INDEX: ',F5.2,2X,'HEIGHT: ',F8.2)
 1100 FORMAT(70('*'))
 1200 FORMAT(/'Quick Plant Harvested On ',
     +      4('-'),I3,'/',I2,'/',I4,' -----',I6,'  ----'/)
      END
