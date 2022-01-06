C***********************************************************************
C
      SUBROUTINE LEAFT(NC,NS,ITER,TC,TCDT,TLC,TLCDT,TLCLWR,VAPC,VAPCDT,
     + WCAN,WCANDT,PCAN,PCANDT,MAT,MATDT,TRNSP,TPOTNL,XTRACT,SWCAN,
     + LWCAN,TDIFFC,DIFKL,HEATC,ETLYR,DETLYR,DHEATC,DTLDTC,
     + WT,WDT,DT,DCHAR,DRYCAN,CANLAI,TOTLAI,NPLANT,ITYPE,IEVAP,RSTOM0,
     + RSTEXP,PLEAF0,RLEAF0,TOTROT,RLEAF,RROOT,ROOTDN,
     + CANMA,CANMB,WINDC,ZC,ATRANS,ISTRESS,TLCSHAW,TLEAFU,TLEAFL,HOUR)
C
C     THIS SUBROUTINE COMPUTES LEAF TEMPERATURE OF EACH CANOPY TYPE AND
C     THE TOTAL HEAT AND WATER TRANSFERRED FROM CANOPY TO SURROUNDING
C     AIR SPACE IN EACH CANOPY LAYER
c
CnT   THIS SUBROUTINE HAS BEEN MODIFIED TO USE TRANSPIRATION SPECIFIED
CnT   FROM OUTSIDE GOSHAW.  LINES THAT ARE NO LONGER NEEDED HAVE BEEN
CnT   COMMENTED OUT BY "CnT" 
C
C***********************************************************************
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXNOD=300, nodcan=11, MXSPEC=10, CL=4.2D3, CR=1.9D3,
     +          STEFAN=5.6697D-08)
      common /shawp/ pplastic,emitc,emitr(10),emitsp,emits,emitf
     &      ,palbedo,ptransm
      INTEGER HOUR
C
      DOUBLE PRECISION DCHAR(MXSPEC,NODCAN),DRYCAN(MXSPEC,NODCAN),
     + CANLAI(MXSPEC,NODCAN),TOTLAI(MXSPEC),RSTOM0(MXSPEC),
     + RSTEXP(MXSPEC),PLEAF0(MXSPEC),RLEAF0(MXSPEC),
     + TOTROT(MXSPEC),RLEAF(MXSPEC,NODCAN),RROOT(MXSPEC,MXNOD),
     + ROOTDN(MXSPEC,MXNOD),WINDC(NODCAN),TPOTNL(MXSPEC)
C     >>>STATE VARIABLES
      DOUBLE PRECISION TC(NODCAN),TCDT(NODCAN),TLC(MXSPEC,NODCAN),
     + TLCDT(MXSPEC,NODCAN),TLCLWR(MXSPEC,NODCAN),A1LWR(MXSPEC,NODCAN),
     + VAPC(NODCAN),VAPCDT(NODCAN),WCAN(NODCAN),WCANDT(NODCAN),
     + PCAN(MXSPEC),PCANDT(MXSPEC),MAT(MXNOD),MATDT(MXNOD),
     + TRNSP(MXSPEC),
     + XTRACT(MXNOD),SWCAN(MXSPEC,NODCAN),LWCAN(MXSPEC,NODCAN),
     + TDIFFC(NODCAN),DIFKL(MXSPEC+1,NODCAN),
     + HEATC(NODCAN),ETLYR(NODCAN),DETLYR(NODCAN),DHEATC(NODCAN),
     + DTLDTC(NODCAN),zc(nodcan)
C    + HEATC(NODCAN),ETLAY(NODCAN),RETLAY(NODCAN)

C     >>>LOCAL VARIABLES
      DOUBLE PRECISION AVGTMP(NODCAN),AVGVAP(NODCAN),RSTOM(NODCAN),
     + PLEAF(NODCAN),PEVAP(NODCAN),PXYLEM(MXSPEC),RHCAN(MXSPEC,NODCAN),
     + ETCAN(MXSPEC,NODCAN),AVGMAT(MXNOD),LV
      INTEGER INIT(MXSPEC),IEVAP(MXSPEC),ITYPE(MXSPEC),istress
C
      DOUBLE PRECISION F1(NODCAN),DF1DP(NODCAN),DF1DT(NODCAN),
     + DF1DX(NODCAN),DP(NODCAN),AA1(NODCAN),CC1(NODCAN),F2(NODCAN),
     + DF2DP(NODCAN),DF2DT(NODCAN),DF2DX(NODCAN),DTLC(NODCAN),  
     + DF3DP(NODCAN),HUMID(NODCAN),VTSLOP(NODCAN)
C
      PARAMETER (LV=2.5D06, RHOL=1.0D03, RHOA=1.25D0, CA=1006.D0)
      SAVE INIT,PXYLEM,RHCAN,ETCAN    !,TLC, GERRALD: CANNOT SAVE TLC HERE
C
C
C     INITIALIZE ROOT EXTRACTION AND COMPUTE AVERAGE MATRIC POTENTIAL
      DO 5 I=1,NS
         XTRACT(I)=0.0d0
       IF (ISTRESS.GE.3) THEN   
           AVGMAT(I)=WT*MAT(I)+WDT*MATDT(I)
           IF(AVGMAT(I).gt.0.0D0) AVGMAT(I)=0.0D0
       ENDIF
    5 CONTINUE
C
C     INITIALIZE THE TOTAL TRANSP. FROM THE ENTIRE TRANSPIRING CANOPY
      TRNSP(NPLANT+1)=0.0d0
C
C     INITIALIZE CANOPY TEMP, VAPOR DENSITY, AND HEAT AND WATER FLUXES.
C     HEATC(I) AND ETLYR ARE HEAT AND WATER FLUXES; DHEATC AND DETLYR
C     ARE DERIVATIVES OF FLUX TERMS.
      DO 10 I=1,NC
         AVGTMP(I)=WT*TC(I) + WDT*TCDT(I)
         IF (ISTRESS.GE.3) 
     &       AVGVAP(I)=WT*VAPC(I) + WDT*VAPCDT(I)
         HEATC(I)=0.0d0
         DHEATC(I)=0.0d0
         IF (ISTRESS.GE.3) ETLYR(I)=0.0d0
         DETLYR(I)=0.0d0
         IF (ISTRESS.GE.3) DTLDTC(I)=0.0d0
C        INITIALIZE RESISTANCE TO TRANSPORT FROM CANOPY LEAVES
C        (NOT NECESSARY IF ALREADY CALCULATED THIS TIME STEP)
         DO 8 J=1,NPLANT
          A1LWR(J,I)=8.D0*(1.D0-TDIFFC(I))*DIFKL(J,I)/DIFKL(NPLANT+1,I)
     >                    *EMITC*STEFAN*((tlclwr(j,i)+273.16D0)**3.0D0)
C           RH NOT NECESSARY IF ALREADY CALCULATED THIS TIME STEP
            IF (ITER .EQ. 1) then
               RHCAN(J,I)=307.d0*SQRT(DCHAR(J,I)/WINDC(I))
               INIT(J)=1
            END IF
    8       CONTINUE
   10 CONTINUE
C
      DO 60 J=1,NPLANT
         IF (ISTRESS.GE.3) THEN
          SUMET=0.0d0
          SUMEV = 0.0d0
         ENDIF
         FRACTN = 1.0d0
CnT      SET TRNSP AND TPOTNL TO ZERO SINCE THEY ARE NO LONGER COMPUTED
         IF (ISTRESS.LT.3) THEN  
         TRNSP(J)=0.0d0
         TPOTNL(J)=0.0d0
         ENDIF
C
         IF (TOTLAI(J) .EQ. 0.0d0) THEN
C           NO LEAF AREA FOR THIS PLANT(PERHAPS DORMANT OR SNOW-COVERED)
            TRNSP(J)=0.0d0
            GO TO 60
         END IF
C
            IF (ISTRESS.LT.3) THEN
            IF (ITYPE(J) .NE. 0) THEN
C
C********   TRANSPIRING PLANT - CHECK IF CONDITIONS ARE SUCH THAT PLANT
C           WILL TRANSPIRE
            PCANDT(J)=0.0d0
C********      PLANT IS TRANSPIRING              
C      USE TRANSIPIRATION PROVIDED OUTSIDE GOSHAW AND COMPUTE 
C              LEAF TEMPERATURE DIRECTLY
               VAPDEF=0.0d0
               RHAVG=0.0d0
               DO 43 I=1,NC
CnT              THIS STATEMENT ASSUMES ONLY ONE PLANT SPECIES !!!!!
                 ETLYR(I)=ATRANS*CANLAI(J,I)/TOTLAI(J)
                 TLCDT(J,I)=tlclwr(j,i)+(SWCAN(J,I)+LWCAN(J,I)
     >           -LV*ETLYR(I)
     >           -RHOA*CA*CANLAI(J,I)*(tlclwr(j,i)-AVGTMP(I))/RHCAN(J,I)
     >           -DRYCAN(J,I)*CR*(tlclwr(j,i)-TLC(J,I))/DT)
     >                     /(RHOA*CA*CANLAI(J,I)/RHCAN(J,I)
     >                      + A1LWR(J,I) + DRYCAN(J,I)*CR/DT)
C                TLC(J,I)=AVGTMP(I) + RHCAN(J,I)*
C    >               (SWCAN(J,I)+LWCAN(J,I)-ETLYR(I))
C    >                /(CANLAI(J,I)*RHOA*CA)
               IF (CANLAI(J,I) .GT. 0.0d0) THEN
               CALL VSLOPE (VTSLOP(I),SATV,AVGTMP(I))
               VAPDEF = CANLAI(J,I)*(SATV-AVGVAP(I))
               RHAVG = RHAVG + CANLAI(J,I)/RHCAN(J,I)
             IF ((HOUR.EQ.14).and. (I.eq.1)) THEN
c              TLEAFU=AVGTMP(I) + (RHCAN(J,I))*(SWCAN(J,I)+LWCAN(J,I))
c     >         /CANLAI(J,I)/(RHOA*CA)
c              TLEAFL=AVGTMP(I) + (RSTOM0(J)+RHCAN(J,I))
c     >         *(SWCAN(J,I)+LWCAN(J,I)-LV*VAPDEF/(RSTOM0(J)+RHCAN(J,I)))
c     >         /CANLAI(J,I)/(LV*VTSLOP(I)+RHOA*CA)

C Leaf tempearture at non-transpiring condition
               TLEAFU=tlclwr(j,i)+(SWCAN(J,I)+LWCAN(J,I)
     >           -RHOA*CA*CANLAI(J,I)*(tlclwr(j,i)-AVGTMP(I))/RHCAN(J,I)
     >           -DRYCAN(J,I)*CR*(tlclwr(j,i)-TLC(J,I))/DT)
     >                     /(RHOA*CA*CANLAI(J,I)/RHCAN(J,I)
     >                      + A1LWR(J,I) + DRYCAN(J,I)*CR/DT)
C Leaf temperature at fully transpiring condition
c               TLEAFL=tlclwr(j,i)+(SWCAN(J,I)+LWCAN(J,I)
c     >               -LV*VAPDEF/RHCAN(J,I)
c     >           -RHOA*CA*CANLAI(J,I)*(tlclwr(j,i)-AVGTMP(I))
c     >               /(RHCAN(J,I)+RSTOM0(J))
c     >           -DRYCAN(J,I)*CR*(tlclwr(j,i)-TLC(J,I))/DT)
c     >                     /(RHOA*CA*CANLAI(J,I)/RHCAN(J,I)
c     >                + A1LWR(J,I) + DRYCAN(J,I)*CR/DT
c     >                + lv*canlai(j,i)*vtslop(i)/(RSTOM0(J)+rhcan(j,i)))
    
              TLEAFL=tlclwr(j,i)+(SWCAN(J,I)+LWCAN(J,I)
     >               -LV*VAPDEF/(RHCAN(J,I)+RSTOM0(J))
     >          -RHOA*CA*CANLAI(J,I)*(tlclwr(j,i)-AVGTMP(I))/RHCAN(J,I)
     >          -DRYCAN(J,I)*CR*(tlclwr(j,i)-TLC(J,I))/DT)
     >                /(RHOA*CA*CANLAI(J,I)/RHCAN(J,I)
     >                + A1LWR(J,I) + DRYCAN(J,I)*CR/DT
     >                + lv*canlai(j,i)*vtslop(i)/(rhcan(j,i)+RSTOM0(J)))
    
              TLCSHAW = TLCDT(1,1)
              
              ENDIF
               endif
  43          CONTINUE
C back calculate stomatal resistance based on SHAW transpiration
               VAPDEF=VAPDEF/TOTLAI(J)
               IF (VAPDEF .LT. 0.0d0) VAPDEF=0.0d0
               RHAVG=TOTLAI(J)/RHAVG
               IF (ATRANS.LE.0.0D0) THEN
                 RSTOM1=0.0D0
               ELSE
                 RSTOM1=TOTLAI(J)*VAPDEF/ATRANS-RHAVG
               ENDIF
          ELSE
C********   DEAD PLANT MATERIAL -- COMPUTE WATER CONTENT AND TEMPERATURE
            DO 46 I=1,NC
C              NO EVAPORATION FROM STANDING DEAD PROVIDED FROM OUTSIDE
C              GOSHAW  !
C!!!!!         FOR NOW ASSUME NO EVAPORATION FROM STANDING DEAD RESDIUE 
C!!!!!         AND COMPUTE LEAF TEMPERATURE DIRECTLY
               TLCDT(J,I)=tlclwr(j,i)+(SWCAN(J,I)+LWCAN(J,I)
     >           -RHOA*CA*CANLAI(J,I)*(tlclwr(j,i)-AVGTMP(I))/RHCAN(J,I)
     >           -DRYCAN(J,I)*(CR+WCAN(I)*CL)*(tlclwr(j,i)-TLC(J,I))/DT)
     >                     /(RHOA*CA*CANLAI(J,I)/RHCAN(J,I)
     >                      + A1LWR(J,I) + DRYCAN(J,I)*CR/DT)
   46       CONTINUE
            INIT(J) = 2
          END IF
         ELSE   !istress=3 or istress=4
         IF (ITYPE(J) .NE. 0) THEN
C   USE SHAW TRASPIRATION
C********   TRANSPIRING PLANT - CHECK IF CONDITIONS ARE SUCH THAT PLANT
C           WILL TRANSPIRE
            PCANDT(J)=0.0d0
C********      PLANT IS TRANSPIRING
              IF (IEVAP(J) .EQ. 0.d0 .OR. PCAN(J) .GT. 0.0d0) THEN
C*****         PLANT ISN'T TRANSPIRING - PERHAPS NO SUNLIGHT, TOO COLD,
C              OR INTERCEPTED PRECIP AVAILABLE ON PLANT LEAVES
C              "FRACTN" IS FRACTION OF TIME STEP PLANTS WILL TRANSPIRE
               FRACTN=0.0d0
C              CALCULATE LEAF TEMPERATURE
               DO 12 I=1,NC
C                 CHECK IF PLANT HAS ANY LEAF AREA IN LAYER
                  IF (CANLAI(J,I) .LE. 0.0d0) GO TO 12
                  RSTOM(I)=0.0d0
                  ETCAN(J,I)=0.0d0
                  IF (PCAN(J) .GT. 0.0d0) THEN
C***                 INTERCEPTED PRECIP AVAILABLE FOR EVAPORATION
                     HUMID(I)=1.0d0
c                     CALL VSLOPE (VTSLOP(I),SATV,AVGTMP(I))
                     CALL VSLOPE (VTSLOP(I),SATV,TLCLWR(J,I))
                     VAPDEF = CANLAI(J,I)*(SATV-AVGVAP(I))
                     TLCDT(J,I)=tlclwr(j,i)+(SWCAN(J,I)+LWCAN(J,I)
     >                     -LV*VAPDEF/RHCAN(J,I)
     >           -RHOA*CA*CANLAI(J,I)*(tlclwr(j,i)-AVGTMP(I))/RHCAN(J,I)
     >                     -DRYCAN(J,I)*CR*(tlclwr(j,i)-TLC(J,I))/DT)
     >                     /(RHOA*CA*CANLAI(J,I)/RHCAN(J,I)
     >                      + A1LWR(J,I) + DRYCAN(J,I)*CR/DT 
     >                      +lv*canlai(j,i)*vtslop(i)/rhcan(j,i))
C                    DETERMINE AMOUNT OF INTERCEPTED PRECIP THAT
C                    EVAPORATED FROM PLANT SURFACES
                     PEVAP(I)=(CANLAI(J,I)*VTSLOP(I)
     >                      *(TLCdt(J,I)-tlclwr(j,I))+VAPDEF)/RHCAN(J,I)
                     SUMEV=SUMEV+PEVAP(I)
                    ELSE
C***                 NO WATER AVAILABLE FOR EVAPORATION
                     HUMID(I)=0.0d0
                     VTSLOP(I)=0.0d0
                     PEVAP(I)=0.0d0
                     SUMEV=0.0d0
                     TLCDT(J,I)=tlclwr(j,i)+(SWCAN(J,I)+LWCAN(J,I)
     >           -RHOA*CA*CANLAI(J,I)*(tlclwr(j,i)-AVGTMP(I))/RHCAN(J,I)
     >                     -DRYCAN(J,I)*CR*(tlclwr(j,i)-TLC(J,I))/DT)
     >                     /(RHOA*CA*CANLAI(J,I)/RHCAN(J,I)
     >                     +A1LWR(J,I) + DRYCAN(J,I)*CR/DT)
                  END IF
C  THIS IS FROM QIANG YU, LIWNAG MA, 8-24-2009
c*********************************************************************
C                 CALL SUBROUTINE TO GET SOIL AND PLANT RESPIRATION
cc                  CALL PHOTOSYNTHESIS (I,CANLAI(J,I),AVGTMP(I),WINDAY,
cc     >			AVGVAP(I)/SATV*100,GSTOM(I),CFLUX(I),VM(I),
cc     >			VPDA(I),PN(I),PFD(I),CO2I(I))
c*******************************************************************
   12          CONTINUE
               IF (PCAN(J) .GT. 0.0d0) THEN
C*****            CALCULATE WATER ON PLANTS AT END OF TIME STEP
                  PCANDT(J)=PCAN(J)-DT*SUMEV/RHOL
                  IF (PCANDT(J) .LT. 0.0d0) THEN
C                    NO WATER REMAINING ON PLANTS - COMPUTE FRACTION OF
C                    TIME STEP PLANTS WILL BE TRANSPIRING AND ADJUST
C                    AMOUNT EVAPORATED
                     PCANDT(J)=0.0d0
                     FRACTN= 1.d0 - (RHOL*PCAN(J)/DT)/SUMEV
                     SUMEV = (1.d0-FRACTN)*SUMEV
                     DO 13 I=1,NC
                        PEVAP(I)=PEVAP(I)*(1.d0-FRACTN)
   13                CONTINUE
                  END IF
               END IF
            END IF
C
            IF (PCANDT(J) .LE. 0.0d0 .AND. IEVAP(J) .NE. 0d0) THEN
C********      PLANT IS TRANSPIRING
C
C              FIND THE EXTREMES OF MATRIC POTENTIAL SEEN BY ROOTS
C              TAKING INTO ACCOUNT ROOT RESISTANCE FOR EACH LAYER
               MAX=0
               DO 14 I=1,NS
                  IF (ROOTDN(J,I) .GT. 0.0d0) THEN
                    IF (MAX .EQ. 0) THEN
                       MAX=I
                       MIN=I
                     ELSE
                       IF (AVGMAT(I).LT.AVGMAT(MIN)) MIN=I
                       IF (AVGMAT(I)/RROOT(J,I) .GT.
     >                     AVGMAT(MAX)/RROOT(J,MAX)) MAX=I
                    END IF
                  END IF
   14          CONTINUE
C   
C              DETERMINE LEAF POTENTIAL AND TEMP
               VAPDEF=0.0d0
               RHAVG=0.0d0
C              INITIZE VARIABLES FOR CANOPY
               DO 15 I=1,NC
C                 CHECK IF PLANT HAS ANY LEAF AREA IN THIS LAYER
                  IF (CANLAI(J,I) .GT. 0.0d0) THEN
                     IF (FRACTN .GT. 0.999d0) PEVAP(I)=0.0d0
C                     TLC(J,I) = AVGTMP(I)
                     HUMID(I)=1.0d0
C                    CALCULATE AVERAGE CONDITIONS IN CANOPY FOR AN 
C                    INTIIAL APPROXIMATION TO PLEAF AND PXYLEM
                     CALL VSLOPE (VTSLOP(I),SATV,TLCDT(J,I))
                     VAPDEF = VAPDEF + CANLAI(J,I)*(SATV-AVGVAP(I))
                     RHAVG = RHAVG + CANLAI(J,I)/RHCAN(J,I)
                  END IF
   15          CONTINUE
               VAPDEF=VAPDEF/TOTLAI(J)
               IF (VAPDEF .LT. 0.0d0) VAPDEF=0.0d0
               RHAVG=TOTLAI(J)/RHAVG
C
C*****         BEGIN ITERATION TO FIND INITIAL ESTIMATE FOR PXYLEM
               ITER0=0
C              CALCULATE INITIAL GUESS FOR PXYLEM IF THIS IS FIRST TIME
C              FOR THIS TIME STEP
   18          IF (INIT(J) .EQ. 1) PXYLEM(J)=2.d0*AVGMAT(MAX)
C***           COMPUTE SUM OF ROOT CONDUCTANCE TIMES MATRIC POTENTIAL
   19          RSOIL=0.0d0
               SRROOT=0.0d0
               IROOT=0
               DO 20 I=1,NS
C                 DO NOT CONSIDER IF NO PLANT J ROOTS IN SOIL LAYER
                  IF (ROOTDN(J,I).GT. 0.0d0) THEN
C                    DO NOT INCLUDE SOIL LAYERS DRYER THAN PLANT XYLEM
                     IF (AVGMAT(I) .GE. PXYLEM(J)) THEN
                        RSOIL=RSOIL + RROOT(J,I)*AVGMAT(I)
                        SRROOT= SRROOT + RROOT(J,I)
                       ELSE
                        IROOT=1
                     END IF
                  END IF
   20          CONTINUE
               IF (INIT(J) .GT. 1) THEN
C                 USE VALUES FOR PXYLEM, ETCAN AND TLC FROM PREVIOUS
C                 CALCULATIONS FOR THIS TIME STEP IF AVAILABLE
C                 (CALCULATE SUMET AND GO DIRECTLY TO ITERATIVE SCHEME)
                  SUMET=RSOIL - PXYLEM(J)*SRROOT
                  IF (SUMET*SRROOT .LE. 0.0d0) THEN
C                    PREVIOUS VALUE OF PXYLEM WILL NOT WORK FOR UPDATED
C                    END-OF-TIME-STEP CONDITIONS
                     INIT(J) = 1
                     GO TO 18
                  END IF
                  GO TO 24
               END IF
C***           CALC. EFFECTIVE MATRIC POT. AND TOTAL RESISTANCE OF PLANT
               IF (SRROOT .EQ. 0.0d0) THEN
                  RSOIL=RROOT(J,MAX)*AVGMAT(MAX)
                  SRROOT=RROOT(J,MAX)
               END IF
               SOIMAT=RSOIL/SRROOT
               RESIST= 1.d0/SRROOT + 1.d0/RLEAF0(J)
               IF (ITER0 .EQ. 0) THEN
C                 ESTIMATE STARTING POINT FOR ITERATION
                  PLEAF1=PXYLEM(J)
                  IF (PLEAF1/PLEAF0(J) .GT. 40.d0) THEN
C                    LIKELIHOOD OF ARITHMETIC OVERFLOW -- PROCEED WITH
C                    CAUTION BY TAKING LOGARITHM
                     RSLOG=DLOG10(RSTOM0(J))
     >                     + RSTEXP(J)*DLOG10(PLEAF1/PLEAF0(J))
                     IF (RSLOG .GT. 20.d0) THEN
C                       LOG OF STOMATAL RESISTANCE EXTREMELY LARGE --
C                       TRANSP IS ESSENTIALLY ZERO - CALC LEAF TEMP
                        SUMET=0.0d0
                        DO 21 I=1,NC
                           IF (CANLAI(J,I) .LE. 0.0D0) GO TO 21
                           HUMID(I)=0.0D0
                           RSTOM(I)=1.0D20
                           ETCAN(J,I)=0.0D0
                     TLCDT(J,I)=tlclwr(j,i)+(SWCAN(J,I)+LWCAN(J,I)
     >           -RHOA*CA*CANLAI(J,I)*(tlclwr(j,i)-AVGTMP(I))/RHCAN(J,I)
     >                     -DRYCAN(J,I)*CR*(tlclwr(j,i)-TLC(J,I))/DT)
     >                     /(RHOA*CA*CANLAI(J,I)/RHCAN(J,I)
     >                     + A1LWR(J,I) + DRYCAN(J,I)*CR/DT)
   21                   CONTINUE
                        GO TO 48
                     END IF
                  END IF
C FROM QIANG YU, LIWANG MA, 8-24-2009
cc**********                 Estimate an average stomatal conductance
cc                  GSAVG=0
cc                  DO I=1,NC
cc                    CALL VSLOPE (VTSLOP(I),SATV,avgtmp(I))
cc                    CALL PHOTOSYNTHESIS (I,CANLAI(J,I),AVGTMP(I),WINDAY,
cc     >		           AVGVAP(I)/SATV*100,GSTOM(I),CFLUX(I),VM(I),
cc     >			       VPDA(I),PN(I),PFD(I),CO2I(I))
cc                    GSAVG=GSAVG+GSTOM(I)
cc                  ENDDO
cc                GSAVG=GSAVG/NC
cc			  GSMAX=0.4
C*******************************************************************************CCCCCC

                  RSTOM1=RSTOM0(J)*(1.d0+ (PLEAF1/PLEAF0(J))**RSTEXP(J))
                  SUMET=TOTLAI(J)*VAPDEF/(RSTOM1+RHAVG)
                  PLEAF1=SOIMAT-SUMET*RESIST/2.d0
               END IF
C***           UPDATE STOMATAL RESISTANCE AND TRANSPIRATION
   22          RSTOM1=RSTOM0(J)*(1.d0 + (PLEAF1/PLEAF0(J))**RSTEXP(J))
               SUMET=TOTLAI(J)*VAPDEF/(RSTOM1+RHAVG)
C              CALCULATE ERROR IN ET ESTIMATE, DERIVATIVE WITH RESPECT
C              TO LEAF POTENTIAL, AND NEW APPROX. TO LEAF POTENTIAL
               ERROR = (SOIMAT-PLEAF1)/RESIST - SUMET
               DERIV = -1.d0/RESIST + SUMET*RSTOM0(J)*RSTEXP(J)
     >                            *(PLEAF1/PLEAF0(J))**(RSTEXP(J)-1.d0)
     >                            /PLEAF0(J)/(RSTOM1+RHAVG)
               DELTA=ERROR/DERIV
C***           DEPENDING ON MAGNITUDE OF RSTEXP, A DRASTIC POINT OF
C              INFLECTION OCCURS IN THE ERROR FUNCTION AT PLEAF0. IF
C              UPDATED PLEAF1 CROSSES THIS POINT, CUT DELTA IN HALF
               ANEG=(PLEAF1-PLEAF0(J))*(PLEAF1-DELTA-PLEAF0(J))
               IF (ANEG .LT. 0.0d0) THEN
                  PLEAF1=PLEAF1-DELTA/2.d0
                 ELSE
                  PLEAF1=PLEAF1-DELTA
               END IF
C              CALCULATE UPDATED ET AND XYLEM POTENTIAL
               SUMET = (SOIMAT-PLEAF1)/RESIST
               PXYLEM(J)=(RSOIL-SUMET)/SRROOT
               IF (ABS(PLEAF1) .LT. 1.0d0) THEN
C                 AVOID DIVISION BY ZERO
                  COMPAR = 1.0d0
                 ELSE
                  COMPAR = PLEAF1
               END IF                 
               IF (ABS(DELTA/COMPAR).GT.0.01d0 .AND.ITER0.LE.20.d0) THEN
C***              PLEAF AND PXYLEM NOT CLOSE ENOUGH
                  ITER0=ITER0+1
C                 IF PXYLEM > MINIMUM SOIL POTENTIAL, RECALCULATE
C                 RSOIL, SRROOT AND PXYLEM TO EXCLUDE DRY LAYERS
                  IF (PXYLEM(J).GT.AVGMAT(MIN) .OR. IROOT.GT.0) GO TO 19
                  GO TO 22
               END IF
C*****         ESTIMATE TRANSP (ETCAN) WITHIN EACH LAYER TO BEGIN ITER
               DO 23 I=1,NC
                  ETCAN(J,I)=CANLAI(J,I)*SUMET/TOTLAI(J)
   23          CONTINUE
               INIT(J) = 2
C
C*****         BEGIN ITERATION TO FIND LEAF TEMPERATURE, LEAF POTENTIAL
C              AND TRANSPIRATION FROM EACH CANOPY LAYER FOR PLANT
   24          ITER1 = 0
               DO 25 I=1,NC
C                 INTIAL ESTIMATE OF LEAF POTENTIAL IN EACH LAYER
C                 AND DEFINE NEWTON-RAPHSON COEFF. THAT ARE CONSTANT
                  IF (CANLAI(J,I) .GT. 0.0d0) THEN
                     PLEAF(I)=PXYLEM(J) - ETCAN(J,I)/RLEAF(J,I)
                     DF1DX(I) = -RLEAF(J,I)
                     DF2DT(I) = -CANLAI(J,I)*RHOA*CA/RHCAN(J,I)
     >                          -A1LWR(J,I) - DRYCAN(J,I)*CR/DT
                  END IF
   25          CONTINUE
C
   26          IFLAG= 0
               SUMET=0.0d0
               DF3DX=0.0d0
C              SET UP COEFFICIENTS FOR NEWTON RAPHSON SOLUTION
               DO 30 I=1,NC
                  IF (CANLAI(J,I) .GT. 0.0d0) THEN
                  CALL VSLOPE (VTSLOP(I),SATV,TLCDT(J,I))
C***********************************************************************
C     QIANG YU: A NEW STOMOTAL MODEL CONDERING SOLAR RADIATION, VPD, CO2
cc                  CALL PHOTOSYNTHESIS (I,CANLAI(J,I),AVGTMP(I),WINDAY,
cc    >			AVGVAP(I)/SATV*100,GSTOM(I),CFLUX(I),VM(I),
cc     >			VPDA(I),PN(I),PFD(I),CO2I(I))
cc				GSMAX=0.4
cc				RSTOM(I)=RSTOM0(J)/GSTOM(I)*GSMAX*(1. +
cc     >            (PLEAF(I)/PLEAF0(J))**RSTEXP(J))
C***********************************************************************
                  RSTOM(I)=RSTOM0(J)*(1.d0 +
     >                               (PLEAF(I)/PLEAF0(J))**RSTEXP(J))
                  F1(I) =CANLAI(J,I)*(SATV-AVGVAP(I))
     >                    /(RSTOM(I)+RHCAN(J,I))
                  IF (F1(I) .LT. 0.0d0) THEN
C***                 NO TRANSPIRATION
                     VTSLOP(I)=0.0d0
                     ETCAN(J,I)=0.0d0
                     DF1DP(I) = RLEAF(J,I)
                     DF1DT(I) = 0.0d0
                     DF2DP(I) = 0.0d0
                     DF2DX(I) = 0.0d0
                     DF3DP(I) = 0.0d0
C                    FORCE LEAF POTENTIAL EQUAL TO PXYLEM POTENTIAL,
C                    I.E. FORCE TRANSPIRATION IN LAYER TO ZERO
                     F1(I) = 0.0d0
                     PLEAF(I) = PXYLEM(J)
                    ELSE
C***                 CALCULATE TRANPIRATION IN EACH LAYER AND SET UP 
C***                 MATRIX FOR NEWTON-RAPHSON APPROX. OF UPDATED XYLEM 
C                    POTENTIAL, LEAF TEMPERATURE AND LEAF POTENTIAL
                     ETCAN(J,I)=RLEAF(J,I)*(PXYLEM(J)-PLEAF(I))
                     SUMET = SUMET +ETCAN(J,I)
                     DF1DP(I) = RLEAF(J,I) - F1(I)*RSTOM0(J)*RSTEXP(J)
     >                           *(PLEAF(I)/PLEAF0(J))**(RSTEXP(J)-1.d0)
     >                           /PLEAF0(J)/(RSTOM(I)+RHCAN(J,I))
                     DF1DT(I) = CANLAI(J,I)*VTSLOP(I)
     >                          /(RSTOM(I)+RHCAN(J,I))
                     DF2DP(I) = LV*RLEAF(J,I)
                     DF2DX(I) = -DF2DP(I)
                     DF3DP(I) = RLEAF(J,I)
                     DF3DX = DF3DX - RLEAF(J,I)
                     F1(I) = F1(I) - ETCAN(J,I)
                  END IF
                  F2(I)= SWCAN(J,I) + LWCAN(J,I) - LV*ETCAN(J,I) 
     >            -A1LWR(J,I)*(tlcdt(j,i)-tlclwr(j,i))
     >            -CANLAI(J,I)*RHOA*CA*(TLCDT(J,I)-AVGTMP(I))/RHCAN(J,I)
     >            -DRYCAN(J,I)*CR*(TLCDT(J,I)-TLC(J,I))/DT
                  END IF
   30          CONTINUE
               F3 = RSOIL - PXYLEM(J)*SRROOT - SUMET
               DF3DX = DF3DX - SRROOT
               CC3=DF3DX
C
               IF (SUMET .LE. 0.0d0) THEN
C                 SET TRANPIRATION TO ZERO AND CALCULATE LEAF TEMP
                  SUMET=0.0d0
                  DO 31 I=1,NC
                     IF (CANLAI(J,I) .GT. 0.0D0) THEN
                     HUMID(I)=0.0D0
                     RSTOM(I)=1.0D20
                     ETCAN(J,I)=0.0D0
                     TLCDT(J,I)=tlclwr(j,i)+(SWCAN(J,I)+LWCAN(J,I)
     >           -RHOA*CA*CANLAI(J,I)*(tlclwr(j,i)-AVGTMP(I))/RHCAN(J,I)
     >           -DRYCAN(J,I)*CR*(tlclwr(j,i)-TLC(J,I))/DT) /(-DF2DT(I))
                     INIT(J)=1
                     END IF
   31             CONTINUE
                  GO TO 48
               END IF
C
C              SOLVE MATRIX FOR CHANGE IN PXYLEM(J) 
               DO 32 I=1,NC  
                  IF (CANLAI(J,I) .GT. 0.0d0) THEN
                  AA1(I)=DF1DP(I)-(DF1DT(I)/DF2DT(I))*DF2DP(I)
                  CC1(I)=DF1DX(I)-(DF1DT(I)/DF2DT(I))*DF2DX(I)
                  F1(I) =  F1(I) -(DF1DT(I)/DF2DT(I))*F1(I)
                  CC3=CC3-(DF3DP(I)/AA1(I))*CC1(I)
                  F3 = F3-(DF3DP(I)/AA1(I))*F1(I)
                  END IF
   32          CONTINUE
               DX=F3/CC3
               PXYLEM(J)=PXYLEM(J)-DX
C
C              SOLVE MATRIX FOR CHANGE IN PLEAF(I) AND TLCDT(J,I) 
               DO 33 I=1,NC
                  IF (CANLAI(J,I) .GT. 0.0d0) THEN
                  DP(I)=(F1(I)-CC1(I)*DX)/AA1(I)
                  DTLC(I)=(F2(I)-DF2DX(I)*DX-DF2DP(I)*DP(I))/DF2DT(I)
C                 ADJUST LEAF TEMP & POTENTIAL
                  PLEAF(I)=PLEAF(I)-DP(I)
C                 Add small amount to PXYLEM when checking to avoid 
c                 error related to inability of COMPAQ compiler to  
c                 correctly assess the non-equality.  Otherwise it 
c                 sometimes thinks that PLEAF(I) is > PXYLEM when they 
c                 are in fact equal.
                  IF (PLEAF(I) .GT. PXYLEM(J)+1.E-5)
     >                PLEAF(I)=(PLEAF(I)+DP(I)+PXYLEM(J))/2.
                  TLCDT(J,I)=TLCDT(J,I)-DTLC(I)
C                 CHECK IF TEMPERATURE CHANGE IS WITHIN 0.01 C
                  IF (ABS(DTLC(I)) .GT. 0.01d0) IFLAG=IFLAG+1
                  END IF
   33          CONTINUE
C
C*****         CHECK IF TOLERANCES HAVE BEEN MET
               IF (IFLAG .GT. 0) THEN
                  ITER1 = ITER1 + 1
                  IF (ITER1 .LT. 20) GO TO 26
               END IF
C
C*****         SOLUTION HAS BEEN FOUND FOR LEAF TEMP AND TRANPIRATION
C              FIND FINAL XYLEM POTENTIAL FOR USE IN ROOT EXTRACTION
               IF (SRROOT*SUMET .NE. 0.0d0) THEN
                  PXYLEM(J)=(RSOIL-SUMET)/SRROOT
                  IF (PXYLEM(J).GT.AVGMAT(MIN) .OR. IROOT.GT.0) THEN
                     RSOIL=0.0d0
                     SRROOT=0.0d0
                     DO 34 I=1,NS
                     IF (ROOTDN(J,I).GT. 0.0d0) THEN
C                       DON'T INCLUDE SOIL LAYERS DRYER THAN PLANT XYLEM
                        IF (AVGMAT(I) .GE. PXYLEM(J)) THEN
                           RSOIL=RSOIL + RROOT(J,I)*AVGMAT(I)
                           SRROOT= SRROOT + RROOT(J,I)
                        END IF
                     END IF
   34                CONTINUE
C                    RECALCULATE WATER POTENTIAL IN XYLEM
                     PXYLEM(J)=(RSOIL-SUMET)/SRROOT
                  END IF
C
C                 CALCULATE ROOT EXTRACTION FROM EACH SOIL LAYER
                  DO 35 I=1,NS
                     IF (AVGMAT(I) .GT. PXYLEM(J)) XTRACT(I) = XTRACT(I)
     >               + FRACTN*TOTROT(J)*RROOT(J,I)*(AVGMAT(I)-PXYLEM(J))
   35             CONTINUE
               ELSE
C                 SOIL TOO DRY FOR PLANTS TO EXTACT WATER
                  SUMET=0.0d0
               END IF
C
C              SUM TRANSPIRATION FROM ENTIRE TRANSPIRING CANOPY
               TRNSP(NPLANT+1)=TRNSP(NPLANT+1)+FRACTN*SUMET
            END IF
C
          ELSE
C********   DEAD PLANT MATERIAL -- COMPUTE WATER CONTENT AND TEMPERATURE
            DO 45 I=1,NC
               IF (CANLAI(J,I) .LE. 0.0d0) GO TO 45
C
               PEVAP(I)=0.0d0
               RSTOM(I)=0.0d0
               B1LV = DRYCAN(J,I)/DT
               A1 = -A1LWR(J,I) - CANLAI(J,I)*RHOA*CA/RHCAN(J,I)
     >              -DRYCAN(J,I)*(CR+WCAN(I)*CL)/DT
               B1=LV*B1LV
C              CALCULATE WATER CONTENT BASED ON VAPOR DENSITY OF AIR
               CALL VSLOPE (DUMMY,SATV,TLCDT(J,I))
               HUMCAN=AVGVAP(I)/SATV
               CALL CANHUM (2,HUMCAN,DUMMY,WCHUM,TCDT(I),CANMA,CANMB)
C
               IF (INIT(J) .EQ. 1) THEN
C                 INITIALIZE VARIABLES FOR THIS TIME STEP
                  ETCAN(J,I)= -B1LV*(WCANDT(I)-WCAN(I))
                  IF (ETCAN(J,I) .EQ. 0.0D0) THEN
CXXX                 TLC(J,I) = AVGTMP(I)
C                    COMPUTE HUMIDITY IN PLANT MATERIAL
                     CALL CANHUM (1,HUMID(I),DUMMY,WCANDT(I),TCDT(I),
     >                            CANMA,CANMB)
                     CALL VSLOPE (DUMMY,SATV,TLCDT(J,I))
                     ETCAN(J,I)=CANLAI(J,I)*
     >                          (HUMID(I)*SATV-AVGVAP(I))/RHCAN(J,I)
                     WCANDT(I) = WCAN(I) - ETCAN(J,I)/B1LV
C                    CHECK IF WATER CONTENT IS REASONABLE --
                     IF ((WCANDT(I)-WCHUM)*(WCAN(I)-WCHUM).LT.0.0d0)THEN
C                       WATER CONTENT WENT BEYOND EQUILIBRUIM WITH AIR
                        ETCAN(J,I)= -B1LV*(WCHUM-WCAN(I))
                        HUMID(I)=(AVGVAP(I) +
     >                       ETCAN(J,I)*RHCAN(J,I)/CANLAI(J,I))/SATV
                        CALL CANHUM (2,HUMID(I),DUMMY,WCANDT(I),TCDT(I),
     >                               CANMA,CANMB)
                        ETCAN(J,I)= -B1LV*(WCANDT(I)-WCAN(I))
                     END IF
                  END IF
                  TLCDT(J,I)=-(SWCAN(J,I)+LWCAN(J,I)-LV*ETCAN(J,I)
     >                    +RHOA*CA*CANLAI(J,I)*AVGTMP(I)/RHCAN(J,I)
     >                    +DRYCAN(J,I)*(CR+WCAN(I)*CL)*TLC(J,I)/DT) /A1
               END IF
C
               CALL VSLOPE (VTSLOP(I),SATV,TLCDT(J,I))
C
C*****         BEGIN ITERATIONS TO FIND LEAF TEMP AND WATER CONTENT
               ITER1 = 0
   40          ETCAN(J,I)= -B1LV*(WCANDT(I)-WCAN(I))
C              COMPUTE HUMIDITY IN PLANT MATERIAL AT END OF TIME STEP
               CALL CANHUM(1,HUMID(I),HWSLOP,WCANDT(I),TCDT(I),
     >                     CANMA,CANMB)
C***           SET UP AND SOLVE 2X2 MATRIC FOR NEWTON-RAPHSON APPROX.
C              FOR LEAF TEMP AND WATER CONTENT
               A2 = CANLAI(J,I)*HUMID(I)*VTSLOP(I)/RHCAN(J,I)
               B2 = CANLAI(J,I)*SATV*HWSLOP/RHCAN(J,I) + B1LV
               FF1 = SWCAN(J,I) + LWCAN(J,I) - LV*ETCAN(J,I)
     >            -A1LWR(J,I)*(TLCDT(J,I)-TLCLWR(J,I))  
     >            -CANLAI(J,I)*RHOA*CA*(TLCDT(J,I)-AVGTMP(I))/RHCAN(J,I)
     >            -DRYCAN(J,I)*(CR+WCAN(I)*CL)*(TLCDT(J,I)-TLC(J,I))/DT
               FF2=CANLAI(J,I)*(HUMID(I)*SATV-AVGVAP(I))/RHCAN(J,I)
     >                        - ETCAN(J,I)
               DET = A1*B2 - A2*B1
               D1 = (FF1*B2 - FF2*B1)/DET
               D2 = (A1*FF2 - A2*FF1)/DET
C
C***           UPDATE VALUES
               TLCDT(J,I)=TLCDT(J,I) - D1
               WCANDT(I) = WCANDT(I) - D2
C              CHECK IF WATER CONTENT IS REASONABLE --
               CALL VSLOPE (VTSLOP(I),SATV,TLCDT(J,I))
               HUMCAN=AVGVAP(I)/SATV
               CALL CANHUM (2,HUMCAN,DUMMY,WCHUM,TCDT(I),CANMA,CANMB)
               DELMAX=WCAN(I)-WCHUM
               DELTA=WCANDT(I)-WCHUM
CCCCC          IF(DELTA*DELMAX.LT.0.0 .OR.ABS(DELMAX).LT.ABS(DELTA))THEN
               IF (DELTA*DELMAX .LT. 0.0d0) THEN
C                 WATER CONTENT WENT BEYOND EQUILIBRUIM WITH HUMIDITY
                  ETCAN(J,I)= -B1LV*(WCHUM-WCAN(I))
                  CALL CANHUM (1,HUM,DUMMY,WCAN(I),TCDT(I),CANMA,CANMB)
                  ETMAX=CANLAI(J,I)*(HUM*SATV-AVGVAP(I))/RHCAN(J,I)
                  IF (ABS(ETCAN(J,I)) .GT. ABS(ETMAX)) ETCAN(J,I)=ETMAX
                  HUMID(I)=(AVGVAP(I) +
     >                       ETCAN(J,I)*RHCAN(J,I)/CANLAI(J,I))/SATV
                  CALL CANHUM (2,HUMID(I),DUMMY,WCANDT(I),TCDT(I),
     >                         CANMA,CANMB)
               END IF
               IF (ABS(D1) .GT. 0.01d0) THEN
                  ITER1 = ITER1 +1
                  IF (ITER1 .LT. 10) GO TO 40
               END IF
C              CALCULATE EVAPORATION FROM THIS LAYER
               ETCAN(J,I)= -B1LV*(WCANDT(I)-WCAN(I))
               SUMET=SUMET+ETCAN(J,I)
   45       CONTINUE
            INIT(J) = 2
         END IF
C
C********STORE EVAPORATION/TRANSPIRATION FROM PLANT SPECIES
   48    TRNSP(J)=FRACTN*SUMET+SUMEV
C        COMPUTE POTENTIAL TRANSPIRATION
         IF (IEVAP(J) .NE. 0) THEN
            TPOTNL(J) = SUMEV
            DO 49 I=1,NC
              CALL VSLOPE (VTSLOP(I),SATV,AVGTMP(I))
              VAPDEF = CANLAI(J,I)*(SATV-AVGVAP(I))
              IF ((HOUR.EQ.14).and.(i.eq.1)) THEN
C Leaf tempearture at non-transpiring condition
c              TLEAFU=AVGTMP(I) + (RHCAN(J,I))*(SWCAN(J,I)+LWCAN(J,I))
c     >         /CANLAI(J,I)/(RHOA*CA)
C Leaf temperature at fully transpiring condition
c              TLEAFL=AVGTMP(I) + (RSTOM0(J)+RHCAN(J,I))
c     >         *(SWCAN(J,I)+LWCAN(J,I)-LV*VAPDEF/(RSTOM0(J)+RHCAN(J,I)))
c     >         /CANLAI(J,I)/(LV*VTSLOP(I)+RHOA*CA)
C Leaf tempearture at non-transpiring condition
               TLEAFU=tlclwr(j,i)+(SWCAN(J,I)+LWCAN(J,I)
     >           -RHOA*CA*CANLAI(J,I)*(tlclwr(j,i)-AVGTMP(I))/RHCAN(J,I)
     >           -DRYCAN(J,I)*CR*(tlclwr(j,i)-TLC(J,I))/DT)
     >                     /(RHOA*CA*CANLAI(J,I)/RHCAN(J,I)
     >                      + A1LWR(J,I) + DRYCAN(J,I)*CR/DT)
C Leaf temperature at fully transpiring condition
c               TLEAFL=tlclwr(j,i)+(SWCAN(J,I)+LWCAN(J,I)
c     >               -LV*VAPDEF/RHCAN(J,I)
c     >           -RHOA*CA*CANLAI(J,I)*(tlclwr(j,i)-AVGTMP(I))
c     >               /(RHCAN(J,I)+RSTOM0(J))
c     >           -DRYCAN(J,I)*CR*(tlclwr(j,i)-TLC(J,I))/DT)
c     >                     /(RHOA*CA*CANLAI(J,I)/RHCAN(J,I)
c     >                      + A1LWR(J,I) + DRYCAN(J,I)*CR/DT
c     >            + lv*canlai(j,i)*vtslop(i)/(RSTOM0(J)+rhcan(j,i)))
              TLEAFL=tlclwr(j,i)+(SWCAN(J,I)+LWCAN(J,I)
     >               -LV*VAPDEF/(RHCAN(J,I)+RSTOM0(J))
     >          -RHOA*CA*CANLAI(J,I)*(tlclwr(j,i)-AVGTMP(I))/RHCAN(J,I)
     >          -DRYCAN(J,I)*CR*(tlclwr(j,i)-TLC(J,I))/DT)
     >                /(RHOA*CA*CANLAI(J,I)/RHCAN(J,I)
     >                + A1LWR(J,I) + DRYCAN(J,I)*CR/DT
     >                + lv*canlai(j,i)*vtslop(i)/(rhcan(j,i)+RSTOM0(J)))
               TLCSHAW = TLCDT(1,1)
              ENDIF
C
              TLEAF=AVGTMP(I) + (RSTOM0(J)+RHCAN(J,I))
     >         *(SWCAN(J,I)+LWCAN(J,I)-LV*VAPDEF/(RSTOM0(J)+RHCAN(J,I)))
     >         /CANLAI(J,I)/(LV*VTSLOP(I)+RHOA*CA)
C
              TPOTNL(J)= TPOTNL(J) + FRACTN*(CANLAI(J,I)*VTSLOP(I)
     >         *(TLEAF-AVGTMP(I))+VAPDEF)/(RSTOM0(J)+RHCAN(J,I))
   49       CONTINUE
            IF (TPOTNL(J) .LT. TRNSP(J)) TPOTNL(J)=TRNSP(J)
           ELSE
            TPOTNL(J) = 0.0D0
         END IF
C             
         ENDIF
C
C        SUM HEAT AND WATER TRANSFER IN EACH LAYER FROM ALL CANOPY TYPES
         DO 50 I=1,NC
            IF (CANLAI(J,I) .LE. 0.0d0) GO TO 50
            HEATC(I)=HEATC(I)
     >           + CANLAI(J,I)*RHOA*CA*(TLCDT(J,I)-AVGTMP(I))/RHCAN(J,I)
         IF (ISTRESS.GE.3) 
     >     ETLYR(I)=ETLYR(I) + FRACTN*ETCAN(J,I) + PEVAP(I)
            DHEATC(I)=DHEATC(I)
     >             + CANLAI(J,I)*RHOA*CA/RHCAN(J,I)
         IF (ISTRESS.GE.3) THEN
             DETLYR(I)=DETLYR(I) + CANLAI(J,I)/(RSTOM(I)+RHCAN(J,I))
             DTLDTC(I)=DTLDTC(I) + LV*HUMID(I)*VTSLOP(I)*
     >                            CANLAI(J,I)/(RSTOM(I)+RHCAN(J,I))
         ELSE
            DTLDTC(I)=0.0d0
         ENDIF
   50    CONTINUE
   60 CONTINUE
C
c
C     CALCULATE CHANGE OF LEAF TEMP WITH RESPECT TO CANOPY TEMP
      DO 70 I=1,NC
         DTLDTC(I)=DHEATC(I)/(DHEATC(I)+DTLDTC(I))
   70 CONTINUE
      RETURN
      END
