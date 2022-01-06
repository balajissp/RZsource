!**********************************************************************  
      subroutine HERMES_SUCROS(FIRST1,TSOIL,TEMP,WG,W,WMIN,GSAT,        
     + RDF,C1,TP,PE,RAD,SUND,LAI,pltpop,plden1,                         
     + OBMAS,WUMAS,DZ,TLTR,WORG,GORG,DGORG,WDORG,JGS,TLAI,    
     + FIXN,LAT,CO2KONZ,ADATE,MDATE,GEHOB,WUGEH,DAUERKULT,EWP,          
     + PESUM,RTDEP,TAG,ISTAGE,IPL,NN,DMDNIT,SUMPE,CROPR,LUKRIT,
     + RM,CN,OMSEA,RPOOL,TADRT,TADRTC,RCO2,FHARV,LAIFKT,KOMP,
     + RCN,PLNAME,ihtyper,iharvr,HPC,hdate,ZEIT,resage,NRKOM,ad,
     + lured,PLHGHT,PLALFA,plden,sdead,sdcn,stubht,HEIGHT,wumalt,obalt,
     + WORG2MIN,WORG3MIN,imagic,ilast,EDATE,GS)
      implicit none
!**********************************************************************  
!RMaruqez - Note: changed array "SUM" to SUMTEMP
!SUMTEMP is the sum of temperature C days
! all depths are in decimeters, i.e., 10 cm, need to convert from cm to dm
!**********************************************************************  
!INPUT Section:
      LOGICAL FIRST1,FHARV,FIRSTNFIX
      integer :: ihtyper,iharvr,hdate(3),yrsimr,JGS(2),MXNOD,MXANA,INFIX
      double precision HPC,resage,plden1,RTDEP,PLHGHT,PLALFA,ckg2g,fcr
      DOUBLE PRECISION C2BM,RFIXN,GS
      PARAMETER (MXANA=135,MXNOD=300)
      PARAMETER(CKG2G=1.0D-5,FCR=0.4D0)                      !,REPRO=0.7D0)
      parameter (c2bm=2.5d0,RFIXN=2.830d0)
      double precision :: SUMTEMP(10)                        !Cumulative Deg Temp day (RANGE = PHASE #, SUM IS PER PHASE)     
      double precision Tsoil(MXNOD)                          !soil temperature Tsoil(0,Z) begining of the day, Tsoil(1,Z) end of the day, only use Tsoil(Z) now.
      double precision :: TEMP                               !average air temperature, TM=(TMAX+TMIN)/2 IN RZWQM
      double precision :: WG(MXNOD)                          !what is WG? water content (cm^3/cm^3) WG(0,Z) begin of time step, WG(1,Z) end of time step, only WG(Z) now. THETA()
      double precision :: W(MXNOD)                           !field capacity (cm^3/cm^3) in layer Z, CGDUL(I)
      double precision :: WMIN(MXNOD)                        !wilting point (cm^3/cm^3) in layer Z, CGDLL(I)
      double precision :: GSAT(MXNOD)                        !SATURATED SOIL WATER CONTENT (cm^3/cm^3) in layer Z, GSAT(I)
c      double precision,intent(inout) :: NFOS(:)             !organic N pool of easily (fast) decomposable fraction (fos = fresh organic substance)
c      double precision,intent(inout) :: NAOS(:)             !organic N pool of slowly (fast) decomposable fraction (aos = old (alt) organic substance)
      double precision :: WRAD(MXNOD)                        !root radius (cm)
      double precision :: C1(MXNOD)                          !soil mineral NO3-N content in layer z (kg N/ha)
      double precision :: TP(MXNOD)                          !water uptake of roots in layer z
      double precision :: PE (MXNOD)                         !N uptake by roots (kg N/ha)
      double precision :: RAD                                !radiation (in fact it is already tranferred to PAR (MJ/m^2)
      double precision :: SUND                               !sunshine duration (hours)
      double precision :: WUDICH(MXNOD)                      !Initi to 0 Root length density (cm root per cm^3 soil)
      double precision :: FL(MXNOD)                          !root surface (cm^2/cm^3 soil)
                                                             !leaf and stem only
      double precision :: LAI,TLAI                           !Initial and current LAI  (LET LAI = (WOrg(2))*LAIFKT(ISTAGE), crop initialization)     
      double precision :: DT=1.0d0                           !daily time step
      double precision :: EWP                                !actual transpiration/potential transpiration, EWP IN RZWQM
      double precision :: LURED                              !threshold for oxygen limitation
      INTEGER,intent(inout) :: ZEIT                          !counter for time (days since 1st January 1900)
      double precision :: P2                                 !this is only used for timing of n fertilization and represents the day of 16 hours length
      double precision :: OBMAS                              !Above ground biomass 
      double precision :: TENDSUM                            !TENDSUM + TSUM(i) (sum of temperature days across phases
      double precision :: REDWU                              !not used anymore
      double precision :: WUMAS                              !Initial and current weight roots (Crop initialization)
      double precision :: OPTI                               !optimum N concentration in above ground biomass
!      double precision,intent(inout) :: WURZMAX             !maximum effective rooting depth (from soil file) 
      double precision :: DZ(MXNOD)                          !thickness of soil compartments z = 1 to N (standard 10 cm), TL(:) IN RZWQM2, changed to decimeters
      double precision :: TLTR(MXNOD)                        !SOIL DEPTH OF EACH LAYER, FROM RZWQM2, changed to decimeters
      double precision :: AD                                 !soil specific factor to calculate apparent diffusion coefficient depending on water content
      double precision :: MASSUM                             !sum of N into roots by mass flow
      double precision :: DIFFSUM                            !sum of N to roots by diffusion
c      double precision,intent(inout) :: Schnorr             !is related to the amount of C required by nodules for n2 fixation (may not active anymore)
      double precision :: fixNsum                            !cumulative sum of fixed nitrogen
      double precision :: lat                                !latitude (DEGREES?)

      double precision :: co2konz                            !CO2 concentration (ppm unit?)
      double precision :: WULAEN                             !(WUMAS*100000*100./7.)/(.015^2.*PI) total root length over profile (Crop initialization)
      INTEGER :: DOUBLER                                     !only 0 if non perennial (From crop file) day of double ridge stage
      INTEGER :: ASIP                                        !only 0 if non perennial (From crop file) day of flowering
      INTEGER, intent(inout) :: EDATE                        !only 0 if non perennial (From crop file) day of Emergence
      INTEGER, intent(inout) :: ADATE                        !only 0 if non perennial (From crop file) day of flowering
      INTEGER, intent(inout) :: MDATE                        !only 0 if non perennial (From crop file) day of maturity
C      double precision,intent(inout) :: ENDPRO              !only 0 if non perennial (From crop file) final grain protein content (target)
      double precision :: PHYLLO                             !only 0 if non perennial (From crop file), Non 0 if perennial cross year? Phyllo is the sum of the modified temperature sums
      double precision :: VERNTAGE                           !only 0 if non perennial (From crop file) accumulated vernalization days
      double precision :: trootsum                           !only 0 if perennial (From crop file)     accumulated soil temperature sum at current rooting depth
      double precision :: ASPOO                              !0 from Crop initialization             temporary assimilation pool
      double precision :: PESUM                              !Initialized based on croptype ()       accumulated n uptake crop (sum of PE per time step)
      
      integer :: wurz                                        !actual rooting depth (unit? decimeters)
      integer, intent(inout)         :: TAG                  !DAY OF THE YEAR, JDAY IN RZWQM2
      integer, intent(inout)         :: ISTAGE               !Development stage?

      integer, intent(inout)         :: IPL                  !running crop number (increases by 1 at harvest to look for the next crops sowing and harvest date), IPL IN RZWQM2
      integer, intent(inout)         :: NN                   !Figure 20 layers for WULAE/WULAE2/MASS?
C      integer, intent(inout)         :: GRW                 !groundwater level (decimeter)
      integer :: CO2METH                                     !selects the method for CO2 effect on photosynthesis (3 methods implemented)
      
!LOCALS      
      double precision GORG(5)                               !Daily organ live weight (growth)
      double precision :: DGORG(5)                           !Daily organ death weight (death)
      double precision :: WULAE(MXNOD)                       
      double precision :: WULAE2(MXNOD)
      double precision :: MASS(MXNOD)
      double precision :: FKc
      double precision :: DLE
      double precision :: DLP
      double precision :: GPHOT
      double precision :: MAINT
      double precision :: GTW
      double precision :: EZIEL
      double precision :: PROZIEL
      double precision :: FV
      double precision :: FP
      double precision :: daycrit
      double precision :: maxdaylength
      double precision :: NPROG
      double precision :: REDUK
      double precision :: WPROG
      double precision :: DEVPROG
      double precision :: SUMAE
      double precision :: RELINT
      double precision :: GEHMIN
      double precision :: GEHMAX
      double precision :: DVKOR
      double precision :: GEARMAX
      double precision :: WUX
      double precision :: GEHALT
      double precision :: MININ
      double precision :: AUX
      double precision :: OBALT
      double precision :: WUMALT
      double precision :: WGM
      double precision :: DTGESN
      double precision :: DTOPTN
      double precision :: WUMM
      double precision :: WURM
      double precision :: QREZ
      double precision :: TIEFE
      double precision :: maxup
      double precision :: NMINSUM
      double precision :: TRNSUM
      double precision :: SUMDIFF
      double precision :: SUMPE
      double precision :: FIXN
      integer :: I,imagic,ilast
      integer :: nrkob
      double precision :: DCOEFNO3(MXNOD)             !apparent D(I) diffusion coefficient of NO3 depending of water content
      double precision :: DIFF(MXNOD)                 !diffusion to roots
      
! common
      integer NUMPHASES,NUMORGANS,ipet
      PARAMETER (NUMPHASES=10,NUMORGANS=5)
      double precision :: Kc (10)                     !kc factor for evapotranspiration at end of phase (range = 1 to NRENTW) (From crop file)
      integer :: GSPhase (10)                         !GS at end of phase (range = 1 to NRENTW) (From crop file)
      double precision :: TSUM(10)                    !required development phase temperature sums (range = 1 to NRENTW) (From crop file)
      double precision :: WORG(5)                     !(initial and current) weight kg d.m./ha per ORGAN (range = 1  to NumCompartment) (From crop file)
      double precision :: WDORG(5)                    !weight dead organ (kg d.m./ha) per organ (range = 1 to NumCompartment)
      character(len=2) :: CROPR                       !RMarquez -> Note this is an array of 3char values.(Crop abbreviations)       
      double precision :: VSCHWELL(10)                !Vernalisation requirements (days) (range = 1 to NRENTW)(From crop file)
      double precision :: DAYL(10)                    !day length requirements (hours) (range = 1 to NRENTW)(From crop file)
      double precision :: DLBAS(10)                   !Base day length in phase 1(hours)(range = 1 to NRENTW)(From crop file)
      double precision :: DRYswell(10)                !drought stress below ETA/ETP-quotient (range = 1 to NRENTW)(From crop file)      
      double precision :: PRO(10,5)                   !Phase N Partitioning array, (x,y) where X = phase, Y = crop organ # (From crop file)
      double precision :: DEAD(10,5)                  !Phase Death rate array, (x,y) where X = phase, Y = crop organ #   
      double precision :: LAIFKT(10)                  !specific leave area (area per mass) (m2/m2/kg TM) (range = 1 to NRENTW)(From crop file)
      double precision :: BAS(10)                     !Base temperature in phase (range = 1 to NRENTW)(From crop file)
      double precision :: WGMAX(10)                   !N-content root end of phase (range = 1 to NRENTW)(From crop file)         
      double precision :: MAIRT(5)                    !Maintainance rates of organs (From crop file)      
      double precision :: Kcini                       !initial kc factor for evapotranspiration (From crop file)
      double precision :: GEHOB                       !Start and current concentration N in ab. gr. biomass (% i. d.m.) (From crop file)
      double precision :: WUGEH                       !Start and current concentration N in roots (% i. d.m.) (From crop file)
      double precision :: WUMAXPF                     !maximum effective rooting depth (From crop file)      
      double precision :: MINTMP                      !Minimum temperature crop growth    (Grad C) (From crop file)
      double precision :: maxamax                     !AMAX: Max. CO2 assimilation rate (kg CO2/ha leave/h) (From crop file)
      double precision :: LUKRIT(10)
      DOUBLE PRECISION :: ENDPRO
      character(len=30) :: PLNAME
      double precision :: yifak
      integer :: KOMP (5)                             !Rmarquez new-> array contains comparment number of organs above ground (REPLACES PROGIP1)to identfy which organs are summarized to above ground biomass, not include seeds
      integer          :: yorgan
      integer          :: NGEFKT                      !crop N-content function no. (critical and max. N-contents) (From crop file)
      integer          :: NRKOM                       !Number of compartments for plant (From crop file)
      integer          :: NRENTW                      !number of development phases (From crop file)
      integer          :: TEMPTYP                     !Type of temperature dependency (C3 = 1/ C4 = 2) (From crop file)      
      integer          :: IEDATE
      integer          :: IADATE
      integer          :: IMDATE
      character(len=1) :: DAUERKULT                   !Perennial crop (From crop file)
      character(len=1) :: LEGUM                       !Legume crop (From crop file)
! RZWQM PARAMETERS
      DOUBLE PRECISION LFWT,LFWTS,LFWTSD,RTWT,RTWTS,RTWTSD,STWT,STWTS,
     &               STWTSD,RM,RCN,TOPRES,SDWT,SDWTS,SDWTSD,plden,
     &               TUWT,TUWTS,TUWTSD,WTNRES,CNEW,TOTRTRES,FCN,
     &               TOTWTNRRS,RTRES(MXNOD),WTNRRS(MXNOD),OMSEA(MXANA),
     &               PLTPOP,PCN,RMASS,CN(9),S,RPOOL(MXNOD,2),TADRT(2),
     &               TADRTC(2),RCO2,RMASS1,RDF(MXNOD),DMDNIT,TNITUP,
     &               sumtemproot,MaxNup,sdead,sdcn,stubht,HEIGHT,SDMN
      double precision CNOD,DWNOD,DWNODA,NDTH,NFIXN,NODGR,WTNFX,CTONODR,
     +  SENNOD(300),DNOD,CTONOD,CTONODS,WNDOT,WTNOO,NNOFF,WTNNO,PRONOD
      double precision ACTEVP,PET,ACTTRN,PER,PES,TOTPUP,PTRANSDAY
     &    ,RET_day_T,RET_day_S
      common/HERMES/Kc,TSUM,VSCHWELL,DAYL,DLBAS,DRYswell,PRO,    !Array
     + DEAD,WGMAX,MAIRT,Kcini,WUMAXPF,MAXAMAX,
     + YIFAK,MINTMP,TEMPTYP,YORGAN,TENDSUM
      COMMON/NFIX1/CNOD,DWNOD,DWNODA,NDTH,NFIXN,NODGR,WTNFX,
     +  SENNOD,DNOD,CTONOD,CTONODS,WNDOT,WTNOO,NNOFF,WTNNO,PRONOD
      COMMON /CROPH2O/ ACTEVP,PET,ACTTRN,PER,PES,TOTPUP,PTRANSDAY
     &    ,RET_day_T,RET_day_S,ipet
C     
!Parameters
      double precision PI,WORG2MIN,WORG3MIN
      data PI/3.141593D0/
!functions:
      double precision :: VERN
      IF(FIRST1) THEN
        FIRST1=.FALSE.
        FIRSTNFIX=.TRUE.
        MDATE=0
        ADATE=0
        EDATE=0
        do i=1,5
        worg(i)=0.0d0
        enddo
        JGS(1)=1
        istage=1
C        AD=0.005d0    
C        lured=0.5d0   
        wurz=1
        WULAEN=0.0D0
        PHYLLO=0.0D0
        TROOTSUM=0.0D0
        ASPOO=0.0D0
        FKc = Kcini
        VERNTAGE=0.0d0
c
      CALL hermes_setupcrop(WULAEN,GEHOB,WUGEH,DAUERKULT,INFIX,
     + PHYLLO,VERNTAGE,trootsum,ASPOO,komp,PLNAME,IPL,WORG,MaxNup,
     + NRKOM,CO2METH,LEGUM,NGEFKT,NRENTW,LAIFKT,LUKRIT,PLHGHT,PLALFA,
     + pltpop,WORG2MIN,WORG3MIN,BAS,GSPhase)
!
       DO I=1,NRENTW
          IF (GSPHASE(I).EQ.1) IEDATE=I
          IF (GSPHASE(I).EQ.2) IADATE=I
          IF (GSPHASE(I).EQ.3) IMDATE=I
       ENDDO
        do i = 1,NRENTW
           SUMTEMP(i) = 0.0d0
        enddo
!
       IF (IEDATE.EQ.0) IEDATE=1
       IF (IADATE.EQ.0) IADATE=NRENTW-1
       IF (IMDATE.EQ.0) IMDATE=NRENTW
       
              nrkob = size(KOMP)
              do i = 1, nrkob
                  !RMarquez - accumulate above ground organ mass using aboveground organ #'s
                    !komp= VAL(PROGIP1$(i:i)) !original
                    !OBMAS=OBMAS + WORG(komp) !original
                    if (KOMP(I).NE.0) OBMAS=OBMAS + WORG(KOMP(I))
                    enddo
             wumas=worg(1)
             LAI = (WOrg(2))*LAIFKT(1)
             TLAI=LAI
             WULAEN=(WUMAS*100000.D0*100.D0/7.D0)/(.015D0**2.D0*PI)
             ASPOO = 0.0D0          !.24 * WORG(5)
!           ! Including beet/tuber mass in calculation of N-content for sugar beets and potatoes 
           IF (CROPR == "BS" .or. CROPR == "PT") then
                PESUM = (OBMAS+WORG(4))*gehob+WUMAS*WUGEH
           ELSE
                PESUM =(OBMAS*gehob+WUMAS*WUGEH)    !*.007
           END IF
           DO I = 1, NN
                 wudich(i) = 0
           ENDDO
             
	  if (hdate(1).gt.0) CALL Y2K_DOY(HDATE(1))        
       ENDIF
      IF((IMAGIC.EQ.-8).or.(LAI.le.0.0d0)) THEN
        EWP=1.0D0
      else       
       IF (IPET.EQ.1) THEN
          EWP=ACTTRN/(RET_day_T*(1.0D0-DEXP(-0.594D0*LAI)))
          ELSE IF (IPET.EQ.2) THEN
          EWP=ACTTRN/(RET_day_S*(1.0D0-DEXP(-0.594D0*LAI)))
       ENDIF
       endif
       
       CALL Y2K_DOY(YRSIMR)
!**********************************************************************   
 ! temperature accumulation
 !**********************************************************************
      If(Tsoil(wurz) > 0.0D0) then
          trootsum = trootsum + TSOIL(wurz)    !soil temperature for root growth. Wurz, actural rooting depth, 0 begining of the day, 1 is end of day temperature.
        endif
        IF(ISTAGE == 1) then                   !ISTAGE: deveopmental stage 1-10, germination, depend on soil water and temperature. 
           IF (TEMP > BAS(ISTAGE)) THEN        !average air temperature (TEMP) and base temperature (BAS)
              IF (WG(1) > (0.3D0*(W(1)-WMIN(1))+WMIN(1))) then      ! vorher 0.3 nFK
                   SUMTEMP(ISTAGE)=SUMTEMP(ISTAGE)+
     +         (TEMP-BAS(ISTAGE))*DT           !* red
             else
                SUMTEMP(ISTAGE) = SUMTEMP(ISTAGE)
     +           +(TEMP-BAS(ISTAGE))
     +           *(WG(1)/(0.3D0*(W(1)-WMIN(1))+WMIN(1)))*DT    !* red
             ENDIF
           ENDIF
             FKc = Kcini + (Kc(ISTAGE)-Kcini)*SUMTEMP(1)/TSUM(1)
        ENDIF
        IF(SUMTEMP(1) >= TSUM(1)) THEN
           IF(SUMTEMP(ISTAGE) >= TSUM(ISTAGE)) then  !sum=teperature sum, Tsum=sum defined in the parameter file.
              IF(ISTAGE < NRENTW) then
              ! Improvement 1: Consideration of oversum for next stage
                SUMTEMP(ISTAGE+1) = SUMTEMP(ISTAGE)-Tsum(ISTAGE) 
                   ISTAGE = ISTAGE +1
              ENDIF
           ENDIF
           ! ***  LAI von Blattgewicht ***
           IF(LAI <= 0.0d0) THEN
                LAI = 0.001D0
                TLAI = 0.001D0
           END IF
           CALL RADIA(DLE,DLP,GPHOT,MAINT,temp,rad,sund,lat,tag,
     + maxamax,co2konz,mintmp,dryswell,CROPR,EWP,lai,dt,
     + co2meth,temptyp,lured,ISTAGE,IPL,NRKOM,
     + WORG,WDORG,MAIRT)                   !calculation of photosynthesis from Penning De Vries, 1982. 
           
           GTW = (GPHOT-MAINT+ASPOO)*.7D0  !ASPOO, not converted to biomass, 
           ASPOO = 0.0D0                   !Aspoo*0.2
           ! Beruecksichtigung des Proteinziels im Korn f?r Duengerbedarfsanalyse
           IF(CROPR == "WH") then
                EZIEL = 95.D0
                proziel = 0.0224D0
           ELSEIF(CROPR == "WR") then
                proziel = 0.0175D0
                EZIEL = 80.D0
           ELSEIF(CROPR == "WG") then
                proziel = 0.0192D0
                EZIEL = 80.D0
           END IF
           IF(VSCHWELL(ISTAGE) == 0) then
             FV = 1
           ELSE
             FV = MIN(1.0D0,VERN(TEMP,VSCHWELL,DT,TAG,ISTAGE,VERNTAGE))
           END IF
           ! **** Calculating daylength effect *** DLP = photeperiod active daylength (h)
           IF((DAYL(ISTAGE)-DLBAS(ISTAGE)).NE.0.0d0.and.
     &          DAYL(ISTAGE)>0.0D0) then                                ! --DLBAS(ISTAGE) to avoid dividing by zero
              FP = (DLP-DLBAS(ISTAGE))/(DAYL(ISTAGE)-DLBAS(ISTAGE))     ! /20 oder -7/13 
           Elseif(DayL(ISTAGE) < 0.D0) then           
             IF(DLP <= ABS(DAYL(ISTAGE))) then
               FP = 1.d0
             Else
                Daycrit = ABS(Dayl(ISTAGE))
                Maxdaylength = ABS(DLBAS(ISTAGE))
                FP = (DLP - Maxdaylength)/(Daycrit - Maxdaylength) 
             Endif
           ELSE
                FP = 1.0d0
           ENDIF
           IF(FP > 1.d0) FP = 1.d0
           IF(FP < 0.d0) FP = 0.d0
           ! Advancement of development for crops (not sugar beet and potatoes) by water and N stress
           IF((CROPR == "BS").or.(CROPR == "MZ")) then      !  
                Nprog = 1.d0       ! keine Beschleunigung durch N-Stress eingebaut am 9.1.2008
           ELSE IF (LEGUM == "L") then
                NPROG = 1.0D0
           ELSE
                Nprog = 1.d0+(1.d0-Reduk)**2.d0
           END IF
           ! **** EWP = Ta/Tp, LURED is 1 when oxygen shortage reduces transpiration. ***
           IF(EWP < DRYswell(ISTAGE)) then
              IF(LURED < 1.d0) then
                   WPROG = 1.d0
              ELSE
                   WPROG = 1.d0+(1.d0-EWP)**2.d0
              ENDIF
           ELSE
                   wprog = 1.d0
           ENDIF
           devprog = MAX(NPROG,WPROG)
           IF(TEMP >= BAS(ISTAGE)) THEN
                SUMTEMP(ISTAGE) = SUMTEMP(ISTAGE)+(TEMP-   
     +                         BAS(ISTAGE))*FV*FP*devprog*DT
                PHYLLO = PHYLLO + (TEMP-             
     +                   BAS(ISTAGE))*fv*fp*devprog*dt
              IF(SUMTEMP(2) >= TSUM(2)) then
                 IF(DOUBLER == 0) then
                    !  Datum des Doppelringstadiums
                      DOUBLER = ZEIT
                    !CALL KALENDER (ZEIT,DOPP$)  !zeit is counted from 1900.
                 ENDIF
              ENDIF
              IF(ZEIT > P2+10) THEN
                   SUMAE = SUMAE +(TEMP-BAS(ISTAGE))*fv*FP*DT
                 IF((SUMAE > 130.d0).AND.(ASIP == 0)) THEN
                      ASIP = ZEIT
                    !CALL KALENDER (ASIP,AEHR$)
                 ENDIF
              ELSE
                   SUMAE = 0.d0
              ENDIF
C              IF((SUMTEMP(1)>=TSUM(1)).and.(EDATE==0))THEN      ! gedndert von ISTAGE 3 nach 4 am 22.5.2009
              IF((SUMTEMP(IEDATE)>=TSUM(IEDATE)).and.(EDATE==0))THEN      ! gedndert von ISTAGE 3 nach 4 am 22.5.2009
                   EDATE = ZEIT
              ENDIF
C              IF((SUMTEMP(NRENTW-1)>=TSUM(NRENTW-1)).and.(ADATE==0))THEN      ! gedndert von ISTAGE 3 nach 4 am 22.5.2009
              IF((SUMTEMP(IADATE)>=TSUM(IADATE)).and.(ADATE==0))THEN      ! gedndert von ISTAGE 3 nach 4 am 22.5.2009
                   ADATE = ZEIT
                 !CALL KALENDER(ADATE,BLUEH$)
              ENDIF
              IF((SUMTEMP(IMDATE) >= TSUM(IMDATE)).and.(MDATE == 0))THEN
                   MDATE = ZEIT
                 !CALL KALENDER(MDATE,MDATEE$)
              ENDIF
           ENDIF
           IF(ISTAGE < 2) then
                relint=SUMTEMP(ISTAGE)/TSUM(ISTAGE)
              IF(relint > 1.d0) relint=1.d0
                FKc = Kcini + (Kc(ISTAGE)-Kcini)*relint                !SUM(ISTAGE)/TSUM(ISTAGE)
           ELSE
                relint=SUMTEMP(ISTAGE)/TSUM(ISTAGE)
              IF(relint > 1.d0) relint=1.d0
                FKc = Kc(ISTAGE-1)+(Kc(ISTAGE)-Kc(ISTAGE-1))*relint     !repSUM(ISTAGE)/TSUM(ISTAGE)
           ENDIF            
           ! +++++++++++++++++++  N-content functions  +++++++++++++++++++++++++
           IF(NGEFKT == 1) then
              IF(PHYLLO < 200.d0) THEN
                   gehmin = .0415d0
                   GEHMAX = .06d0
              ELSE
                 IF((CROPR=="WR").or.(CROPR=="BA")) then
                      GEHMIN = 5.1d0 * EXP(-.00165d0*PHYLLO)/100.d0
                      GEHMAX = 8.0d0 * EXP(-.0017d0*PHYLLO)/100.d0
                 ELSE
                      GEHMIN = 5.5d0 * EXP(-.0014d0*PHYLLO)/100.d0
                      GEHMAX = 8.1d0 * EXP(-.00147d0*PHYLLO)/100.d0
                 ENDIF
              ENDIF
           ELSEIF(NGEFKT == 2) then
              IF(PHYLLO < 263.d0) then
                   gehmin = 0.035d0
              ELSE
                 gehmin = 0.035d0-0.024645d0*(1-EXP(-(phyllo-152.30391d0
     +             *log(1-2**(0.5d0)/2)-438.63545d0)/152.30391d0))**2.d0
              END IF
              IF(PHYLLO < 142.d0) then     ! 198
                   GEHMAX = 0.049d0
              ELSE
                   gehmax = 0.049d0 - 0.037883841d0*(1-EXP(-(phyllo   
     +                    -201.50354d0*log(1-2**(0.5d0)/2)-385.8318d0)/
     +                     201.50354d0))**2
              END IF
           ELSEIF(NGEFKT == 3) then
              IF(OBMAS < 1000.d0) then
                   gehmax = 0.06d0
                   gehmin = 0.045d0
              ELSE
                   gehmax =  0.06d0 * (obmas/1000)**(-0.25d0)
                   gehmin = 0.045d0 *(obmas/1000)**(-0.25d0)
              END IF  
           ELSEIF(NGEFKT == 4) then
              IF((OBMAS+WORG(4)) < 1000.d0) then
              gehmax = 0.06d0
              gehmin = 0.045d0
              ELSE
              gehmax=0.0285d0+0.0403d0*EXP(-0.26d0*(obmas+WORG(4))/1.d3)
              gehmin=0.0135d0+0.0403d0*EXP(-0.26d0*(obmas+WORG(4))/1.d3)
              END IF              
           ELSEIF(NGEFKT == 5) then
              IF((OBMAS+WORG(4)) < 1100.d0) then
                   gehmax = 0.06d0
                   gehmin = 0.045d0
              ELSE
                   gehmax = 0.06d0 *((obmas+WORG(4))/1000.d0)**0.5294d0
                   gehmin = 0.046694d0 *((obmas+WORG(4))/1000)**0.5294d0
              END IF             
           ELSEIF(NGEFKT == 6) then
              IF(PHYLLO < 400.d0) THEN
                   gehmin = .0415d0
                   GEHMAX = .06d0
              ELSE
                   GEHMIN = 5.5d0 * EXP(-.0007d0*PHYLLO)/100
                   GEHMAX = 8.1d0 * EXP(-.0007d0*PHYLLO)/100
              END IF              
           ELSEIF(NGEFKT == 7) then
              IF(OBMAS < 1000.d0) then
                   gehmax = 0.0615d0
                   gehmin = 0.0448d0
              ELSE
                   gehmax =  0.0615d0 * (obmas/1000)**(-0.25d0)
                   gehmin = 0.0448d0 *(obmas/1000)**(-0.25d0)
              ENDIF              
           ELSEIF(NGEFKT == 8) then
              IF(PHYLLO < (200.d0*Tendsum/1260.d0)) THEN     ! vorher = 200
                   gehmin = .0415d0
                   GEHMAX = .06d0
              ELSE
                dvkor = 1.d0/((Tendsum-200.d0)/(1260.d0-200.d0)) 
                 IF((CROPR=="WR").or.(CROPR=="BA")) then
                    GEHMIN = 5.1d0 * EXP(-.00165d0*dvkor*PHYLLO)/100.d0
                    GEHMAX = 8.0d0 * EXP(-.0017d0*dvkor*PHYLLO)/100.d0
                 ELSE
                    GEHMIN = 5.5d0 * EXP(-.0014d0*dvkor*PHYLLO)/100.d0
                    GEHMAX = 8.1d0 * EXP(-.00147d0*dvkor*PHYLLO)/100.d0
                 END IF
              END IF
           END IF
           GEARMAX = MAX(PROZIEL,GEHMAX)
           !      -------------------------------------------------------
           !      -     Trockenmassenproduktion
           !      -------------------------------------------------------
             GEHALT = GEHOB
           IF(ISTAGE > 0) THEN
              IF(WUGEH < 0.01d0) THEN
                 IF(WUGEH <= .005d0) THEN
                      REDWU = 0.0d0
                 ELSE
                      WUX =(WUGEH-.005d0)/.005d0
                      REDWU = 1.d0-SQRT(1.d0 - WUX*WUX)
                 END IF
              ELSE
                   REDWU = 1.0d0
              END IF
              IF(GEHOB < GEHMIN) THEN
                 IF(NGEFKT == 1) then
                      MININ = 0.005d0
                 ELSE
                      MININ = 0.004d0
                 END IF
                 IF(GEHOB <= MININ) THEN
                      REDUK = 0.0d0
                 ELSE
                      AUX = (GEHOB-MININ)/(GEHMIN-MININ)
                      REDUK = (1.d0-EXP(1.d0+1.d0/(AUX-1.d0)))**2.d0           ! Reduk is N stress
                 END IF
              ELSE
                   REDUK = 1.d0
              END IF              
              do I = 1, NRKOM
                  IF((SUMTEMP(ISTAGE)/TSUM(ISTAGE)) > 1.d0) then
                       GORG(I) = 0.0d0
                  ELSE
                       GORG(I) = GTW*((PRO(ISTAGE-1,I)+(PRO(ISTAGE,I)
     +                          -PRO(ISTAGE-1,I))*SUMTEMP(ISTAGE)
     +                  /TSUM(ISTAGE))) * REDUK
                       DGORG(I) = WORG(I)*(DEAD(ISTAGE-1,I)
     +                             +(DEAD(ISTAGE,I)-DEAD(ISTAGE-1,I))
     +                     *(Min(1.d0,SUMTEMP(ISTAGE)/TSUM(ISTAGE))))
                  ENDIF
                  !
                  IF(I < 4) then
                     IF((WORG(I)+(Gorg(i)-DGORG(I))*dt) > 0.0d0) then
                         WORG(I) = WORG(I) + GORG(I)*dt - DGORG(I) *DT
                     else 
                       DGORG(I) = WORG(I)/dt + GORG(I)
                       WORG(i) = 0.1d0
                     endif
                  ELSE
                     !   WORG(I) = WORG(I) + GORG(I)*dt - DGORG(I) *DT + DGORG(i-1)*dt +DGORG(I-2)*dt + DGORG(I-3)*dt
                     IF(ISTAGE < NRENTW) then
                          WORG(I) = WORG(I) + GORG(I)*dt - DGORG(I) *DT
     +                             + 0.3d0*(DGORG(i-1)*dt +DGORG(I-2)*dt
     +                             + DGORG(I-3)*dt)
                     ELSE
                          WORG(I) = WORG(I) + GORG(I)*dt - DGORG(I) *DT
                     END IF
                     IF(WORG(I) < 0.d0) then 
                        DGORG(I) = DGORG(I) + WORG(I)/dt
                        WORG(I) = 0.d0
                     end if
                  END IF
! ++++++++++++++++ gedndert am 18.12.2011 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                  IF(I == 2) then
                       LAI = LAI + GORG(I)* (LAIFKT(ISTAGE-1)          
     +              + (SUMTEMP(ISTAGE)/Tsum(ISTAGE)*(LAIFKT(ISTAGE)
     +              - LAIFKT(ISTAGE-1))))*dt-DGORG(I)*LAIFKT(1)*dt
                       TLAI = TLAI + GORG(I)* (LAIFKT(ISTAGE-1)          
     +              + (SUMTEMP(ISTAGE)/Tsum(ISTAGE)*(LAIFKT(ISTAGE)
     +              - LAIFKT(ISTAGE-1))))*dt
                  endif
                  LAI=MAX(0.0D0,LAI)
                  ! RMarquez- checked this code, it is correct, it does nothing.
                  IF(I == 1) then

                  elseif(I < 4) then
c                       NFOS(1)=NFOS(1)+0.7d0*0.8d0*DGORG(I)*GEHalt*dt
c                       NAOS(1)=NAOS(1)+0.7d0*0.2d0*DGORG(I)*GEHalt*dt
                       PESUM = PESUM - 0.7d0*DGORG(I) * GEHalt *dt  !WHAT IS PESUM?
                  ENDIF
                    WDORG(I) = WDORG(I) + DGORG(I)*DT
                  IF (WORG(I)-WDORG(I) < 0.0d0) THEN
                       WDORG(I) = WORG(I)
                  END IF
              enddo              
              ASPOO = ASPOO+GTW * (1.d0-REDUK)               !*.1  !reduk N stress, 1=no stress
              IF((CROPR == "BS").or.(CROPR == "PT")) then
                   OBALT = OBMAS+worg(4)                     !for tuber crop, above ground biomass only worg(2)+worg(3)
              ELSE
                   Obalt = Obmas
              END IF
              ! Definieren Oberirdische Masse
              OBMAS = 0.0d0
              nrkob = size(KOMP)
              do i = 1, nrkob
                  !RMarquez - accumulate above ground organ mass using aboveground organ #'s
                    !komp= VAL(PROGIP1$(i:i)) !original
                    !OBMAS=OBMAS + WORG(komp) !original
                    if (KOMP(I).NE.0) OBMAS=OBMAS + WORG(KOMP(I))
              enddo
c           IF (CROPR == "BS" .or. CROPR == "PT") then
c                PESUM = (OBMAS+WORG(4))*gehob+WUMAS*WUGEH
c           ELSE
c                PESUM =(OBMAS*gehob+WUMAS*WUGEH)    !*.007
c           END IF
              PESUM =(OBALT*gehob+WUMAS*WUGEH)    !*.007
              WUMALT = WUMAS
              WUMAS = WORG(1)

              ! ++++++++++++++++ Einschub Gras automatischer Wiederaustrieb ++++++++++++++++++++++
              !the following section needs to be modified for harvest of perennials. It should be deleted.
              IF((DAUERKULT == "D").and.(ISTAGE > 4)) then
                  MDATE=ZEIT
                  GOTO 60
                 IF((OBMAS <= OBALT).and.(WORG(2) < 100)) then
                      ISTAGE = 1    !
                      SUMTEMP(1) = 5.d0     !TSUM(1)+1
                    do i = 2,NRENTW
                          SUMTEMP(i) = 0.0d0
                    enddo
                      Phyllo = 0.0d0
C C                     NAOS(1) = NAOS(1) + (WORG(3)+WORG(4)) * GEHOB *dt
                      Pesum= Pesum - ((Worg(3)+Worg(4)) * gehob)
                    do I = 1,NRKOM
                        IF(i < 3) then
                             WORG(I) = WORG(I)
                        ELSE
                             WORG(I) = 0.0d0
                        END IF
                          wdorg(i) = 0.0d0
                    enddo
                    !RMarquez -- Assumed the following commented line was supposed to assign Worg(2) to both variables.
                    OBMAS = Worg(2)
                    OBALT = Worg(2)
                 END IF
              END IF              
60            CONTINUE            
              IF(ISTAGE > 1) then
                   WGM = WGMAX(ISTAGE-1)-(WGMAX(ISTAGE-1)        
     +                  -WGMAX(ISTAGE))*SUMTEMP(ISTAGE)/TSUM(ISTAGE)
              ELSE
                   WGM = WGmax(ISTAGE)
              END IF
              IF((CROPR == "BS").or.(CROPR == "PT")) then
                   DTGESN =(GEHMAX*(OBMAS+Worg(4))+(WUMAS*WGM)-PESUM)*DT
                   DTOPTN =((GEHMAX-(gehmax-gehmin)*.15d0)           
     +                      *(OBMAS+WOrg(4))+(WUMAS*WGM)-PESUM)*DT
              ELSE
                   DTGESN = (GEHMAX*OBMAS+WUMAS*WGMAX(ISTAGE)-PESUM)*DT
                   DTOPTN = ((GEHMAX-(gehmax-gehmin)*.15d0)*OBMAS    
     +                       +WUMAS*WGMAX(ISTAGE)-PESUM)*DT
                 !  Einschub Duengerbedarf
                 IF((CROPR=="WR").or.(CROPR=="WH")) then
                   IF(PHYLLO < 1284.d0) then
                   IF(PHYLLO <= 200.d0) then
                         opti = 0.d0
                   ELSEIF((PHYLLO > 200.d0).and.(PHYLLO < 450.d0)) then
                   opti = -.25d0+.25d0*((PHYLLO-200.d0)/(450.d0-200.d0))
                   ELSE
                   opti = 2.1d0*(PHYLLO-450.d0)/(800.d0-450.d0)
                          IF((GEHMAX-(gehmax-gehmin)*opti)<0.005d0) then
                               opti = gehmax/(Gehmax-gehmin)-0.005d0
                          ENDIF
                       ENDIF
                    ENDIF
                 ENDIF
                 !  Ende Einschub
              ENDIF
              !                dtgesn = dtoptn
              !IF grt = 0 and Gst = 0 and GEA = 0 then   dtgesn = 0
           ENDIF
!Rmarquez this endif is for a HUGE if statement.        
        END IF  !END IF(SUMTEMP(1) >= TSUM(1)) THEN

c        IF(DTGESN > (6.0d0*DT)) DTGESN = 6.0d0*DT
        IF(DTGESN > (MaxNup*DT)) DTGESN = MaxNup*DT
        IF(DTGESN < 0.d0) THEN
             DTGESN = 0.0d0
      END IF
!  daily root death? Liwang 
        IF(WUMAS < WUMALT) THEN
             WUMM=(WUMALT-WUMAS)*WUGEH
        ELSE
             WUMM = 0.0d0
        END IF

        ! ---------------------------------------------------------------
        ! ------ Berechnung der Wurzeldichte (-laenge/Dichte Boden) -----
        ! Calculation of root density ( length / density floor )
        ! ---------------------------------------------------------------
!Rmarquez - changed from "round" to nint, double check intent.        
        !wurm = ROUND(wurzmax * (WUMAXPF/11))
c        wurm = NINT(wurzmax * (WUMAXPF/11.d0))  for now, soil is not a factor, use maximum rooting depth from crop file (WUMAXPF)
        wurm = NINT(WUMAXPF)  !for now, soil is not a factor, use maximum rooting depth from crop file (WUMAXPF)
        sumtemproot=sumtemp(1)  !/2.0d0  !+sumtemp(2)
        IF(WURM > tltr(NN)) WURM = tltr(NN)
        IF((CROPR == "OS").or.(CROPR == "OF").or.
     +      (CROPR == "OR").or.(CROPR == "BS")) then
          Qrez = (0.081476d0+exp(-.004*(phyllo+SUMTEMProot)))**1.8d0
        ELSEIF((CROPR == "MZ").or.(CROPR == "PT")) then
          Qrez = (0.081476d0+exp(-.0035d0*(phyllo+SUMTEMProot)))**1.8d0
        ELSEIF((CROPR == "GR").and.(IPL > 2)) then
          Qrez=(0.081476d0+exp(-.002787d0*(Max(phyllo+
     +          SUMTEMProot,1500.d0))))**1.8d0
        ELSEIF((CROPR == "AA").and.(IPL == 2)) then
          Qrez=(0.081476+exp(-.0045d0*((phyllo+
     +          SUMTEMProot))))**1.8d0
        ELSEIF((CROPR == "AA").and.(IPL > 2)) then
c          Qrez=(0.081476+exp(-.002787d0*(Max(phyllo+
c     +          SUMTEMProot,1500.d0))))**1.8d0
          Qrez=(0.081476+exp(-.003d0*(Max(phyllo+     !Per Kersebaum, Dec 1, 2017
     +          SUMTEMProot,1500.d0))))**1.8d0
        ELSEIF((CROPR == "WR").and.(IPL > 2)) then                  !WHY IPL>2?
           !  Qrez = (0.081476+exp(-.008*(Trootsum)))**1.8
           !  Qrez = (0.081476+exp(-.008*(phyllo+SUM(1))))**1.8
          Qrez = (0.081476+exp(-.0095*(phyllo+SUMTEMProot)))**1.8d0
        ELSEIF((CROPR == "WT").and.(IPL > 2)) then
           !  Qrez = (0.081476+exp(-.008*(Trootsum)))**1.8
          Qrez=(0.081476d0+exp(-.0095d0*(phyllo+143.D0)))**1.8d0
        ELSE
           Qrez=(0.081476d0+exp(-.002787d0*(phyllo+SUMTEMProot)))**1.8d0
        END IF        
        

c        IF(QREZ > 0.35d0) QREZ = .35d0
c        IF(QREZ < 4.5d0/WURM) QREZ = 4.5d0/WURM  !where is the 4.5 coming from?
          RTDEP = min(2.3d0/QREZ,wumaxpf)   !COMMENTED OUT BY LIWANG
          DO I=1,NN
              IF (TLTR(I).LE.rtdep) THEN
                  WURZ=I
              ENDIF
          ENDDO
C
C         WURZ = NINT(4.5d0/QREZ)   !COMMENTED OUT BY LIWANG
c          WURZ = max(1,INT(WURM))
        ! Annahme: Wurzelradius nimmt mit der Tiefe ab
        do I = 1, wurz         !10
            IF((CROPR == "BS").or.(CROPR == "PT")) then
                 WRAD(I) = 0.01d0  !- I*.001
            ELSE
                 WRAD(I) = max(0.02d0  - tltr(I)*0.001d0,0.01d0)  !COMMENTED OUT BY LIWANG TO VOID ZERO WHEN I=20
            END IF
        enddo        
        
        do I = 1, WURZ
            Tiefe = TLTR(I)    !I * dz
           WULAE(I) = (WUMAS*(1-EXP(-QREZ*TIEFE))/100000.d0*100.d0/7.d0)
            IF(I > 1) THEN
           WULAE2(I) = ABS(WULAE(I)-WULAE(I-1))/(WRAD(I)**2.d0*PI)/DZ(I)
            ELSE
              WULAE2(I) = ABS(WULAE(I))/(WRAD(I)**2.d0*PI)/DZ(I)
            ENDIF
            !                Wulae(i) = Wurzellaenge von 0 bis Tiefe i
            ! -------------------------------------------------------------
            ! ------ Wurzeldichte /Volumen Boden -(cm/cm^3) ---------------
            ! Root density / volume bottom - ( cm / cm ^ 3 )
            ! -------------------------------------------------------------
              WUDICH(I)=WULAE2(I)
            ! ------------------------------------------------------------ 
            ! ---------- Wurzelflaeche / dzmitt(i) * cm^3 Boden ----------
            !  the following is not used, but should it be WRAD(I)^2?
            ! ------------------------------------------------------------ 
              FL(I)=WUDICH(I)*WRAD(I)**2*PI
        enddo        
        
        WULAEN = 0
        do I = 1, WURZ
            ! ---------------  WURZELLNGE in cm/cm^2 -----------------------  
              WULAEN = WULAEN+WULAE2(I)*DZ(I)
            enddo
c        do I = 1, 3
c              NFOS(I) = NFOS(I)+0.5d0*WUMM /3.0d0  !* WULAE2(I)*10/WULAEN
c              NAOS(I) = NAOS(I)+0.5d0*WUMM /3.0d0  ! * WULAE2(I)*10/WULAEN
c        enddo
!  Root Density Function to be used in RZWQM2, RDF
            do i=1,WURZ
                RDF(I)=WULAE2(I)*DZ(I)/WULAEN
      ENDDO
!
        !! Limitieren der maximalen N-Aufnahme auf 26-13*10^-14 mol/cm W./sec
        IF((CROPR == "OF").or.(CROPR == "OR").or.
     +      (CROPR == "MT")) then
             maxup = .09145d0 - .015725d0*(phyllo/1300.d0)
        ELSEIF(CROPR == "MZ") then
             maxup = .074d0 - .01d0*(phyllo/TENDSUM)
        ELSEIF(CROPR ==  "BS") then
             maxup = .05645d0 - .01d0*(phyllo/TENDSUM)
        ELSE
             maxup = .03145d0 - .015725d0*(phyllo/1300.d0)
        END IF
        IF(DTGESN > (WULAEN*MAXUP*DT)) THEN    !   .036134*DT THEN
           !Rmarquez - LEGUM = flag? maybe just 0/1 or T/F
           IF(LEGUM == "L") then
                dtgesn = dtgesn   !for legume, N is not limited 
           ELSE
                DTGESN = WULAEN*MAXUP*DT
           END IF
        END IF        
        
        NMINSUM = 0.0d0
        TRNSUM = 0.0d0
        SUMDIFF = 0.0d0
        do I = 1, WURZ
C            IF(I < 11) THEN                                             !why 11 layers?
               NMINSUM = NMINSUM + (C1(I)-.75d0)
               MASS(I) = TP(I)*(C1(I)/(WG(I)*DZ(I)))*dt
               TRNSUM = TRNSUM + TP(I)*(C1(I)/(WG(I)*DZ(I)))*dt             !mass flow from transpiration
               DCOEFNO3(I) = 2.14d0 * (AD*EXP(WG(I)*10.d0))/WG(I)           !diffusion coefficient for NO3, no NH4 is simulated
               DIFF(I) = (DCOEFNO3(I)*WG(i)*2.d0*PI*WRAD(I)*(C1(I)/1.d3     !diffusion part of N uptake
     +                      /WG(I)-.000014d0)*sqrt(pi*wudich(i)))   
     +                      *wudich(i)*1000.d0*dt  
               SUMDIFF=SUMDIFF + DIFF(I)
C            ENDIF
        enddo
        SUMPE = 0.d0
        
        do I = 1, WURZ
            IF(dtgesn > 0.0d0) then
               IF(TRNSUM >= DTGESN) THEN
                    PE(I) = DTGESN * MASS(I)/TRNSUM
               ELSE
                  IF(DTGESN-TRNSUM < SUMDIFF) THEN
                       PE(I)=MASS(I)+(DTGESN - TRNSUM)*DIFF(I)/SUMDIFF
                  ELSE
                       PE(I) = MASS(I) + DIFF(I)
                  ENDIF
               ENDIF
                 MASSUM = MASSUM + MASS(I)
                 DIFFSUM = DIFFSUM + DIFF(I)
               IF(PE(I) > (C1(I)-0.5d0)) PE(I) = C1(I)-0.5d0
               IF(PE(I) < 0.0d0) PE(I) = 0.0d0
            ELSE
                 PE(I) = 0.0d0
            END IF
              SUMPE = SUMPE + PE(I)
        enddo
! ADJUST C1 AFTER UPTAKEN
        DO I=1,WURZ
            C1(I)=MAX(C1(I)-PE(I),0.0D0)
      ENDDO
! END OF C1 ADJUST
        IF(Legum == "L") then
        IF (INFIX.EQ.1) THEN
C ADDED BY LIWANG MA TO INCOPORATE N-FIXATION FROM CROPGRO
C        infix=1
         IF (dtgesn-sumpe > 0.1D-5) THEN
        CTONOD = MAX(0.0d0, (Dtgesn-sumpe)/10.0d0
     &           *RFIXN/0.16d0)
        IF (CTONODS.GT.CTONOD) THEN
            CTONODS=CTONODS-CTONOD
            CTONODR=0.0d0
        ELSE
            CTONODR=CTONOD-CTONODS
            CTONODS=0.0d0
        ENDIF
              WUMAS=WUMAS-CTONODR*10.0D0
        IF (WUMAS-WUMALT.LT.0.0D0) THEN
            CTONOD=(WORG(1)-WUMALT)/10.0D0
            WUMAS=WUMALT
            WORG(1)=WUMALT
        ENDIF
C
      CALL DSSATNFIX(PLTPOP,GSAT,TSOIL,WG, 
     +     EWP,DZ*10.d0,NN,firstNFIX) 
c      FIXN=max(0.0d0,NFIXN*10.0d0)                      !FROM G/M2 TO KG/HA
      FIXN   = max(0.0d0,(NFIXN+(NODGR * 0.16d0 * PRONOD)) * 10.0d0)   !kg N/ha/d
c      DO I=1,NN
      DGORG(1)=DGORG(1)+NDTH*10.0D0                     !NEED TO CHECK, IT DOES NOT MAKE SENSE
     &       *(1.0D0/WUGEH/2.5D0)*(C2BM*0.16D0*PRONOD)
c      ENDDO
      ELSE
       FIXN=0.0D0
      ENDIF
          else            
           IF((dtgesN - Sumpe) > (0.74d0 * dtgesn)) then      !0.74
                fixN = max(0.0d0,0.74d0*dtgesn)  !assume N fixation can supply 74% demand
           ELSE
                fixN = max(0.0d0,dtgesn-sumpe)
           ENDIF
           ENDIF
        else
             fixN = 0.0d0
        END IF
c        Schnorr = fixN
        fixNsum = fixNsum + fixN
        IF(WUMAS > WUMALT) THEN
           IF((CROPR == "BS").or.(CROPR == "PT")) then
                WUGEH = (WUMALT*WUGEH + ((WUMAS-WUMALT)              
     +                   /(OBMAS+WORG(4)-OBALT+WUMAS-WUMALT)*SUMPE)) 
     +                   /WUMAS
           ELSE
                WUGEH = (wumalt*wugeh + (WUMAS-WUMALT)               
     +                   /(OBMAS-OBALT+WUMAS-WUMALT)*(SUMPE+fixN))   
     +                   /WUMAS
           END IF
             WUGEH = MIN(WUGEH,WGMAX(ISTAGE))
           IF(Wugeh < 0.005d0) wugeh = 0.005d0
        END IF
           IF((CROPR == "BS").or.(CROPR == "PT")) then
                GEHOB = (PESUM+SUMPE-WUMAS*WUGEH)/(OBMAS+WORG(4))
              IF((GEHOB*(OBMAS+WORG(4))) < (OBALT*GEHALT)) THEN
                   WUGEH = (PESUM+SUMPE-(OBMAS+Worg(4))*GEHOB)/(WUMAS)
              END IF
           ELSE
                   GEHOB = (PESUM+SUMPE+FIXN-WUMAS*WUGEH)/OBMAS
           END IF   
! output to RZWQM
! total weight of the organs
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
! CHECK FOR FINAL HARVEST OF CROP
      IF ((ZEIT.EQ.MDATE.AND.HDATE(1).LE.0).OR.(ZEIT.EQ.HDATE(1))) THEN
C     ..ADJUST RESIDUE AGE (AGE IS RESET IN HARVST ROUTINE)
          RESAGE=0.0d0
          FHARV=.TRUE.
           IF (Dauerkult.eq."D".and. IPL > 1) then
                ISTAGE = 2
                DO I=1,NRENTW
                VSCHWELL(I)=0.0D0
                ENDDO
                sumtemp(1) = TSUM(1)+1
                Sumtemp(2) = Min(Phyllo,TSUM(2)*0.5)
                Phyllo = Min(Phyllo,TSUM(2)*0.5)
                DO I=3,NUMPHASES
                  SUMTEMP(I)=0.0d0
                ENDDO
               TROOTSUM=0.0D0
c               Phyllo = SUMTEMP(1)+tsum(2)*0.5d0
c                obmas = 0
c           WUMAS=WORG(1)
c           WULAEN=(WUMAS*100000d0*100.d0/7.d0)/(.015**2.d0*PI)
           ASPOO = 0.0D0
           ELSE
                ISTAGE = 1
!                OBMAS = 0
! return nodules to root biomass
          RTWTSD=RTWTSD+DWNOD*10.0D0                        !NEED TO CHECK, IT DOES NOT MAKE SENSE
     &       *(1.0D0/WUGEH/2.5D0)*(C2BM*0.16D0*PRONOD)
          DWNOD=0.0D0
           END IF     
          
! HARVEST RETURN ALL RESIDUE AND ROOT TO RESIDUE POOLS
         IF ((DAUERKULT .EQ. "N").OR.(DAUERKULT .EQ. "D".and.
     +      plden.eq.0.0D0.and.plden1.gt.0.0d0).or.ilast.eq.0.or.
     +      plden1.gt.0.0d0) THEN
          JGS(2)=1
          FIRST1=.TRUE.
         IF (iharvr.EQ.3) TOPRES = LFWT+STWT+SDWT*(1.0d0-HPC)        !seed
         IF (iharvr.EQ.4) TOPRES = (LFWT+STWT)*(1.0d0-HPC)      !aboveground
         IF (iharvr.EQ.5) TOPRES = LFWT+STWT+SDWT                    !root
         IF (iharvr.EQ.6) TOPRES = (LFWT+STWT+SDWT)*(1.0d0-HPC)      !above and belowground
         SDMN=(LFWT+STWT)*STUBHT/HEIGHT
         WTNRES = TOPRES*GEHOB
         TotRTRES = 0.0
         TotWTNRRS = 0.0
      DO I=1,NN
        IF (iharvr.EQ.5.or.iharvr.eq.6) RTRES(I)=RTWT*RDF(I)*(1.0d0-HPC)
        IF (iharvr.EQ.4.OR.IHARVR.EQ.3) RTRES(I)=RTWT*RDF(I)
        WTNRRS(I) = RTRES(I)*WUGEH   !IS WUGEH FOR DEAD OR LIVE ROOT?
        TotRTRES = TotRTRES + RTRES(I)
        TotWTNRRS = TotWTNRRS + WTNRRS(I)
      ENDDO
! RESET ALL THE CROP VARIABLES 
CC      DO I=1,NUMORGANS
CC      WORG(I)=0.0D0
CC      WDORG(I)=0.0D0
CC      ENDDO
!
CC      LAI=0.0D0
CC      TLAI=0.0d0
CC      GEHOB=0.0D0
CC      WUGEH=0.0D0
CC      ISTAGE=1                                !=0 AFTER HARVEST?
CC      wumas=0.0d0
CC      obmas=0.0d0
! HARVEST PARTIALLY EACH YEAR FOR PERNNIALS       
      ELSEIF (DAUERKULT .EQ. "D".and.plden1.eq.0.0d0) THEN
c         IF (iharvr.EQ.3) TOPRES = LFWT+STWT+SDWT*HPC/100.D0
c         IF (iharvr.EQ.4) TOPRES = (LFWT+STWT+SDWT)*HPC/100.D0
c         IF (iharvr.EQ.5) TOPRES = LFWT+STWT+SDWT
C           RTWT=WORG(1)
C           LFWT=WORG(2)
C           STWT=WORG(3)
C           SDWT=WORG(4)
C           TUWT=WORG(5)
C RESET VARIABLES IN RZMAN.FOR 
CC           DO I=2,NUMORGANS
CC             WORG(I)=WORG(I)*(1.0D0-HPC)
CC             WDORG(I)=0.0D0
CC           ENDDO
CC             LAI = (WOrg(2))*LAIFKT(3)
CC             TLAI = (WOrg(2))*LAIFKT(3)
CC              do i = 1, nrkob
CC                if (KOMP(I).NE.0) OBMAS=OBMAS + WORG(KOMP(I))
CC              enddo
CC             OBALT = obmas
CC             ISTAGE=3                            !HOW TO SET THE GROWTH STAGE AFTER HARVEST A PERRENIAL?
C         TOPRES = (LFWT+STWT+SDWT)*HPC/100.D0
C         WTNRES = TOPRES*GEHOB
      DO  I=1,NN
         RTRES(I) = RTWTSD*RDF(I)
         WTNRRS(I) = RTWTSD*RDF(I)*WUGEH   !IS WUGEH FOR DEAD OR LIVE ROOT?
      ENDDO
         ENDIF
!!ONLY DAILY RETURN OF DEAD RESIDUES AND ROOT         
      ELSE                  
!! return daily residues to soil
c return dead residue and root to the soil system each day
         TOPRES = LFWTSD+STWTSD+SDWTSD
         WTNRES = (LFWTSD+STWTSD+SDWTSD)*GEHalt
         TotRTRES = 0.0
         TotWTNRRS = 0.0
      DO  I=1,NN
         RTRES(I) = RTWTSD*RDF(I)
         WTNRRS(I) = RTWTSD*RDF(I)*WUGEH   !IS WUGEH FOR DEAD OR LIVE ROOT?
         TotRTRES = TotRTRES + RTRES(I)
         TotWTNRRS = TotWTNRRS + WTNRRS(I)
      ENDDO
      ENDIF
!
         if (WTNRES.ne.0.0d0) then
	   FCN = TOPRES/2.5d0/WTNRES
         else
         FCN=0.0d0
         endif
         if (fcn.gt.0.0d0 .and.topres.gt.0.0d0) then
        SDCN=CNEW(SDEAD,SDMN,SDCN,FCN,0)
        SDEAD=SDEAD+SDMN
         RCN = CNEW(RM,TOPRES,RCN,FCN,0)
         RM = RM + TOPRES
         endif
C     ..PARTITION DEAD ROOT CARBON   :::USE DWL FOR LAYER DIST
C      PCN = DBLE((RTWT/2.5)/(PCNRTR*RTWT))
C         TotRTRES = 0.0
C         TotWTNRRS = 0.0
C      DO 94 I=1,NN
C         RTRES(I) = RTWTSD*RDF(I)
C         WTNRRS(I) = RTWTSD*RDF(I)*WUGEH   !IS WUGEH FOR DEAD OR LIVE ROOT?
C         TotRTRES = TotRTRES + RTRES(I)
C         TotWTNRRS = TotWTNRRS + WTNRRS(I)
C94    CONTINUE
         OMSEA(69)=TOPRES/PLTPOP/10.0D0
         OMSEA(71)=TOTRTRES/PLTPOP/10.0D0
C
c limit to NLAYRI DS(20)      DO 90 I = 1, NNR
      DO 95 I = 1, NN
         if (WTNRRS(I).ne.0.0d0) then
         PCN = (RTRES(I)/2.5d0)/WTNRRS(I)
         else
         PCN=0.0d0
         endif
c 1.0d-5 is coverting kg/ha to g/cm3
c        if (i.eq.1) then
        RMASS = RTRES(I)  !*1.0d-5/2.5d0*tltr(i)*10.0d0
c        else
c        RMASS = RTRES(I)*1.0d-5/2.5d0*(tltr(i)-tltr(i-1))*10.0d0
c        endif
        IF (PCN.GT.0.0D0) THEN
          IF (PCN .LE. CN(1)) THEN
            S = 0.0D0
          ELSE IF (PCN .GE. CN(2)) THEN
            S = 1.0D0
          ELSE
            S = (1.0D0/PCN-1.0D0/CN(1)) / (1.0D0/CN(2)-1.0D0/CN(1))
          ENDIF
          RPOOL(I,1)=RPOOL(I,1) + RMASS*(1.0D0-S)*CKG2G*FCR
          RPOOL(I,2)=RPOOL(I,2) + RMASS*S*CKG2G*FCR
C
C         .. SAVE FOR MASS BALANCE CALCULATIONS (CONVERT TO KG/HA)
c need to check if qckplnt and qckturf need this mass balance or not. Liwang Ma, 5-30-2007
          TADRT(1) = TADRT(1)+RMASS*(1.0D0-S)*1.0D5*CKG2G*FCR/CN(1)
          TADRT(2) = TADRT(2) + RMASS *S*1.0D5*CKG2G*FCR/CN(2)
          TADRTC(1) = TADRTC(1)+RMASS*(1.0D0-S)*1.0D5*CKG2G*FCR
          TADRTC(2) = TADRTC(2) + RMASS*S*1.0D5*CKG2G*FCR
          IF (PCN .LE. CN(1)) THEN
              RCO2=RCO2+RMASS*FCR-(RMASS*FCR*(1.0D0-S)/PCN)*CN(1)
          ELSE IF (PCN .GE. CN(2)) THEN
              RCO2=RCO2+RMASS*FCR-(RMASS*FCR*S/PCN)*CN(2)
          endif
c
        END IF
   95 CONTINUE
C           DO I=1, NRENTW
           GS=SUM(SUMTEMP)/SUM(TSUM)
C           ENDDO
      RETURN
!**********************************************************************  
      end subroutine HERMES_SUCROS
!**********************************************************************         
        

!**********************************************************************     
! Vernalization Function
!**********************************************************************  
      double precision function vern(TEMP,VSCHWELL,DT,TAG,ISTAGE,
     &                          VERNTAGE)
      implicit none
      double precision, intent(inout) :: TEMP
      double precision :: VSCHWELL(10)
      
      
      double precision, intent(inout) :: DT
      
      integer, intent(inout) :: TAG
      integer, intent(inout) :: ISTAGE
      double precision :: VEFF
      double precision :: FV 
      double precision :: VERNTAGE
      double precision :: Verschwell
        IF((TEMP < 0.d0).and.(temp > -4.d0)) THEN
             veff = (Temp+4.d0)/4.d0
        ELSEIF(temp < -4.d0) then
             VEFF = 0.d0
        ELSEIF((TEMP > 3.d0).AND.(TEMP < 7.d0)) THEN
             VEFF = 1.d0 - .2d0 * (TEMP-3.d0)/4.d0
        ELSEIF((TEMP > 7.d0).AND.(TEMP < 9.d0)) THEN
             VEFF = .8d0 - .4d0*(TEMP-7.d0)/2.d0
        ELSEIF((TEMP > 9.d0).AND.(TEMP < 18.d0)) THEN
             VEFF = .4d0 - .4d0*(TEMP-9.d0)/9.d0
        ELSEIF((TEMP < -4.d0).OR.(TEMP > 18.d0)) THEN
             VEFF = 0.0d0
        ELSE
             VEFF = 1.0d0
        END IF
        
        VERNTAGE = VERNTAGE + VEFF*DT
        Verschwell = MIN(VSCHWELL(ISTAGE),9.d0)-1.d0
        IF(VERSCHWELL >= 1.d0) then
             FV = (VERNTAGE-verschwell)/(VSCHWELL(ISTAGE)-verschwell)
           IF(FV < 0.0d0) THEN
                FV = 0.0d0
           END IF
        ELSE
             FV = 1.d0
        END IF
        vern = fv
        RETURN
      end function vern
!**********************************************************************