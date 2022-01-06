      subroutine RADIA(DLE,DLP,GPHOT,MAINT,temp,rad,sund,lat,tag,
     + maxamax,co2konz,mintmp,dryswell,CROPR,EWP, lai,dt,
     + co2meth,temptyp,lured,ISTAGE,IPL,NRKOM,
     + WORG,WDORG,MAIRT)
      implicit none
      !output
      double precision, intent(inout) :: DLP          ! photoperiod active daylength
      double precision, intent(inout) :: DLE          ! effective daylength
      double precision, intent(inout) :: GPHOT        ! gross photosynthsis
      double precision, intent(inout) :: MAINT        ! maintainance respiration of biomass 
      double precision, intent(inout) :: SUND         ! sunshine duration
!input
      double precision, intent(in) :: Temp            !average air temperature, oC
      double precision, intent(in) :: RAD             ! radiation (PAR (Mj/m^2)
      double precision, intent(in) :: WORG(5)         ! mass of organ() kg /ha
      double precision, intent(in) :: WDORG(5)        ! dead mass of organ() 
      double precision, intent(in) :: MAIRT(5)        ! maintainace rate of single organs at standard temperature (from crop file)
      
      double precision, intent(in) :: LAT             !LATITUDE, INPUT, oC
      integer :: TAG                                  !DAY OF THE YEAR, JDAY IN RZWQM
      double precision, intent(in) :: MAXAMAX         !AMAX: Max. CO2 assimilation rate (kg CO2/ha leave/h) (From crop file)
      double precision, intent(in) :: CO2Konz         !CO2 concentration unit? CO2R in RZWQM?
      double precision, intent(in) :: mintmp          !Minimum temperature crop growth    (Grad C) (From crop file)
      double precision, intent(in) :: DRYSWELL(10)    ! threshold for water stress (Ta/Tpot) specified for each stage (from crop file)
      character(len=2) :: CROPR                       !RMarquez -> Note this is an array of 3char values.(Crop abbreviations)
      double precision, intent(in) :: EWP             !what is this variable? (Tact/Tpot)
      double precision, intent(in) :: LAI             ! leaf area index
      double precision, intent(in) :: dt              ! time step (1 day)

      integer, intent(in) :: CO2Meth                  !3 methods to consider CO2 effect on photosynthesis
      integer, intent(in) :: TEMPtyp                  ! temperature dependence C3 or C4 crops
      DOUBLE PRECISION, intent(in) :: LURED                    ! threshold for oxygen stress (cm^3/cm^3)
      integer, intent(in) :: ISTAGE                   !development stage, 1-10, can we make it 1-7 as in RZWQM generic model?
      integer, intent(in) :: IPL                      ! running crop no.
      integer, intent(in) :: NRKOM                    !number of compartments in the plant
!params
      double precision PI
      data PI/3.141593D0/
!local
      double precision :: dec
      double precision :: sinld
      double precision :: cosld
      double precision :: dl
      double precision :: RDN
      double precision :: DRC
      double precision :: DRO
      double precision :: EFF0
      double precision :: COcomp
      double precision :: EFF
      double precision :: KTvmax
      double precision :: Ktkc
      double precision :: kTko
      double precision :: Fakamax
      double precision :: vcmax
      double precision :: Mko
      double precision :: MKC
      double precision :: Oi
      double precision :: Ci
      double precision :: amax
      double precision :: KCo1
      double precision :: Coco
      double precision :: sc
      double precision :: ext
      double precision :: glob
      double precision :: KCO2
      double precision :: REFLC
      double precision :: EFFE
      double precision :: SSLAE
      double precision :: X
      double precision :: PHCH1
      double precision :: Y
      double precision :: PHCH2
      double precision :: PHCH
      double precision :: PHC3
      double precision :: PHC4
      double precision :: MIPHC
      double precision :: MAPHC
      double precision :: PHCL
      double precision :: Z
      double precision :: PHOH1
      double precision :: PHOH
      double precision :: PHO3
      double precision :: MIPHO
      double precision :: MAPHO
      double precision :: PHOL
      double precision :: DGAC
      double precision :: DGAO
      double precision :: DTGA
!      double precision :: MONAT
      double precision :: KOREK
      double precision :: FOV
      double precision :: RADSUM
      double precision :: vswell
      double precision :: TEFF
      double precision :: MAINTS
      
      integer :: I
!functions


        !  EWP = 1
        !     -------- BERECHNUNG VON TAGLAENGE UND EINSTRAHLUNG -----
        !CALCULATION OF daylength AND RADIATION -
        !  LAT = 51.45
        !     ----------------------- DECLINATION -----------------------
          DEC = -23.4d0*COS(2.d0*PI*(TAG+10.d0)/365.d0)
          SINLD = SIN(DEC*PI/180.d0)*SIN(LAT*PI/180.d0)
          COSLD = COS(DEC*PI/180.d0)*COS(LAT*PI/180.d0)
        !     -------------------- ASTRONOMIC Daylength ------------

          DL = 12.d0*(PI+2.d0* ASIN(SINLD/COSLD))/PI    !* DAUKO
        !     -------------------- EFFECTIVE Daylength ----------------
          DLE = 12.0d0 * (PI+ 2.d0*ASIN((-SIN(8.d0*PI/180.d0)+SINLD)
     +               /COSLD))/PI  !* DAUKO
        ! photoperiod active daylength (including dawn before sunrise and after sunset, -6 degrees under horizon)
          DLP = 12.0d0 * (PI+2.d0*ASIN((-SIN(-6.d0*PI/180.d0)+SINLD)  !* DAUKO
     +               /COSLD))/PI
        !     ----- average PHOTOSYNTHETIC active radiation ------ 
          RDN = 3600.d0*(SINLD*DL+24.d0/PI*COSLD*
     +          SQRT(1.d0-(SINLD/COSLD)**2.d0))
        !     ------------radiation clear day (in Joule/m^2)------------
          DRC = 0.5d0*1300.d0*RDN*EXP(-.14d0/(RDN/(DL*3600.d0)))
        !     --------------radiation overcast day ----------------------
          DRO = 0.2d0*DRC
          EFF0 = 0.5d0
        ! ++++++++++++++  Selection of method for CO2 effect on photosynthsis +++++++++++++++
        
        
        IF(CO2Meth == 1) then
             COcomp = 17.5d0*2.d0**((Temp-10.d0)/10.d0)
             EFF = (CO2Konz- COcomp)/(CO2Konz+2.d0*COcomp)*EFF0
        ELSEIF(CO2Meth == 3) then
           ! ********* equations of Long 1991 and Mitchel et al. 1995 **************************
             KTvmax=EXP(68800.d0*((Temp+273.d0)-298.d0)        
     +                   /(298.d0*(Temp+273.d0)*8.314d0))
             Ktkc=EXP(65800.d0*((Temp+273.d0)-298.d0)         
     +                 /(298.d0*(Temp+273.d0)*8.314d0))
             kTko=EXP(1400.d0*((Temp+273.d0)-298.d0)           
     +                 /(298.d0*(Temp+273.d0)*8.314d0))
           ! ****Berechnung des Transformationsfaktors für pflanzenspez. AMAX bei 25 grad *********
             Fakamax= MAXAMAX/34.695d0
             vcmax = 98.d0*FAKAMAX * KTVMAX
           ! **************************************************************************************
             MKC = 460.d0 * Ktkc
             Mko = 210.d0 * Ktko
             Oi = 210.d0+(0.047d0-0.0013087d0*Temp+0.000025603d0
     +          *Temp**2.d0-0.00000021441d0*Temp**3.d0)
     +        /0.026934d0
             Ci=CO2Konz*0.7d0*(1.674d0-0.061294d0*Temp+0.0011688d0
     +          *Temp**2.d0-0.0000088741d0*Temp**3.d0)/0.73547d0
             !Rmarquez - I am assuming that this version of BASIC used was not case sensitive. Thats right!
             cocomp = 0.5d0*0.21d0*vcmax*Oi/(vcmax*Mko)
             Amax = (ci-cocomp)*vcmax/(Ci + Mkc*(1.d0+Oi/Mko))*1.656d0
           IF(Temp < Mintmp) Amax = 0.d0
              EFF = Eff0        !(Ci-cocomp)/(4.5*Ci+10.5*cocomp)
        ELSE
             EFF = Eff0
        END IF
        IF(TEMPtyp == 1) THEN
           IF(CO2meth /= 3) then
              IF(temp < mintmp) then
                   amax = 0.0d0
              ELSEIF(temp < 10.d0) then
                   amax = MAXAMAX * temp/10.d0*.4d0
              ELSEIF(temp < 15.d0) then
                   amax = MAXAMAX * (.4d0+(temp-10.d0)/5.d0*.5d0)
              ELSEIF(temp < 25.d0) then
                   amax = MAXAMAX * (.9d0+(temp-15.d0)/10.d0*.1d0)
              ELSEIF(temp < 35.d0) then
                   amax = MAXAMAX * (1.d0-(temp-25.d0)/10.d0)
              ELSE
                   amax = 0.0d0
              END IF
           END IF
           IF(CO2Meth == 1) then
                amax = Amax*(CO2Konz- Cocomp)/(350.d0- Cocomp)
           ELSEIF(CO2Meth == 2) then
              IF(RAD > 0.d0) then
                   KCo1=220.d0+0.158d0*RAD*20.d0  ! Einheiten der Strahlung pruefen
                   Coco = 80.d0-0.0036d0*Rad*20.d0
              ELSE
                   SC = 1367.d0 *(1.d0+0.033d0*COS(2.d0*Pi*TAG/365.d0))
                   EXT = SC * RDN/10000.d0
                   Glob = EXT * (0.19d0 + 0.55d0*SUND/DL)
                   KCo1=220.d0+0.158d0*GLOB    ! Einheiten der Strahlung pruefen
                   Coco = 80.d0-0.0036d0*GLOB
              ENDIF
                KCO2 = ((CO2Konz-coco)/(kco1+CO2Konz-coco))/
     +                 ((350.d0-coco)/(kco1+350.d0-coco))
                Amax = AMAX * kco2
           ENDIF
        ELSE
           IF(temp < mintmp) then
                amax = 0.d0
           ELSEIF(temp < 9.d0) then
                amax = MAXAMAX * temp/10.d0*.0555d0
           ELSEIF(temp < 16.d0) then
                amax = MAXAMAX * (.05d0+(temp-9.d0)/7.d0*.75d0)
           ELSEIF(temp < 18.d0) then
                amax = MAXAMAX * (.8d0+(temp-16.d0)*.07d0)
           ELSEIF(temp < 20.d0) then
                amax = MAXAMAX * (.94+(temp-18.d0)*.03d0)
           ELSEIF((temp >= 20.d0).and.(temp <= 30.d0)) then
                amax = MAXAMAX
           ELSEIF(temp < 36.d0) then
                amax = MAXAMAX * (1.d0-(temp-30.d0)*.0083d0)
           ELSEIF(temp < 42.d0) then
                amax = MAXAMAX * (1.d0-(temp-36.d0)*.0065d0)
           ELSE
                amax = 0.d0
           END IF
        END IF
        IF(AMAX < .1d0) AMAX = .1d0
          REFLC=.08d0
          EFFE = (1.d0-REFLC)*EFF
          SSLAE = SIN((90.d0+DEC-LAT)*PI/180.d0)
          X = LOG(1.d0+ .45d0*DRC/(DLE*3600.d0)*EFFE/(SSLAE*AMAX))
          PHCH1 = SSLAE*AMAX*DLE*X/(1.d0+X)
        ! Žnderung nach P.d. Vries am 25.5.93
          Y = LOG(1.d0+.55d0*DRC/(DLE*3600.d0)*EFFE/((5.d0-SSLAE)*AMAX))
          PHCH2=(5.d0-SSLAE)*AMAX*DLE*Y/(1.d0+Y)
          PHCH=0.95d0*(PHCH1+PHCH2)+20.5d0
          PHC3=PHCH*(1.d0-EXP(-.8d0*LAI))
          PHC4 = DL*LAI*AMAX
        IF(PHC3 < PHC4) THEN
             MIPHC = PHC3
             MAPHC = PHC4
        ELSE
             MIPHC = PHC4
             MAPHC = PHC3
        END IF
        If(MIPHC == 0.d0) MIPHC = 0.000001d0
          PHCL = MIPHC*(1.d0-EXP(-MAPHC/MIPHC))
          Z = DRO/(DLE*3600.d0)*EFFE/(5.0d0*AMAX)
          PHOH1 = 5.0d0*AMAX*DLE*Z/(1.0d0+Z)
          PHOH=0.9935d0*PHOH1+1.1d0
        ! Žnderung nach P.d. Vries am 25.5.93
          PHO3=PHOH*(1.d0-EXP(-.8d0*LAI))
        IF(PHO3 < PHC4) THEN
             MIPHO = PHO3
             MAPHO = PHC4
        ELSE
             MIPHO = PHC4
             MAPHO = PHO3
        END IF
        If( Mipho == 0.d0) MIPHO = 0.000001d0
          PHOL = MIPHO*(1.d0-EXP(-MAPHO/MIPHO))
        IF((LAI-5.0d0) < 0.0d0) THEN
             DGAC = PHCL
             DGAO = PHOL
        ELSE
             DGAC = PHCH
             DGAO = PHOH
        END IF
        

        !----------- Using sunshine duration if radiation is not provided -------
        IF(RAD == 0.d0) THEN
           IF(SUND > DLE) THEN
                SUND = DLE
           END IF
             DTGA = SUND/DLE*DGAC+(1.0d0-SUND/DLE)*DGAO
        ELSE
!RMarquez - KALENDER can be replaced with JDAY or replace AKTUELL With jday?        
!           CALL KALENDER(INT(ZEIT),AKTUELL)  !use jday from RZWQM
!             MONAT = VAL(AKTUELL$(cpos(AKTUELL$,".")+
!     +               1:CPOSR(AKTUELL$,".")-1))
             KOREK = 1.0d0
             RADSUM = RADSUM + RAD *dt * KOREK
             FOV = (DRC - 1000000.d0 * RAD*KOREK)/(.8d0*DRC)
           IF(FOV > 1.0d0) FOV = 1.0d0  !fraction of overcast
           IF(FOV < 0.d0) FOV = 0.d0
             DTGA = FOV * DGAO+(1.0d0-FOV)*DGAC  !add photo from both overcast and clear sky radiation. 
        END IF
        !     ------- PHOTOSYNTHESERATE IN KG GLUCOSE/HA leaf/day------
          GPHOT = DTGA*30.D0/44.D0
        IF(LURED == 1.d0) then
             vswell = DRYSWELL(ISTAGE)
        ELSE
           IF((CROPR == "MZ").or.(CROPR == "PT").or.
     +         (CROPR == "WR").or.(CROPR == "BA").or.
     +         (CROPR == "WH").or.(CROPR == "WG")) then
                vswell = 1.0D0
           ELSE
                vswell = 0.8D0
           END IF
        END IF
        IF(EWP < vswell) THEN  !EWP=TA/TP
             GPHOT = GPHOT * EWP
        END IF
        ! ----------- MAINTENANCE IN ABH. VON TEMPERATUR -----------
          TEFF = 2.0d0**(0.1d0*TEMP-2.5D0)
          MAINTS = 0.d0
        do I = 1, NRKOM  !nrkom: number of compartments
              MAINTS = MAINTS + (WORG(I)-WDORG(I))*MAIRT(I)  !only the living materials, worg: weight of the organ, wdorg: weight of the dead organ
        enddo
        ! this is to avoid negative growth
        IF(GPHOT < (MAINTS*TEFF)) THEN
             MAINT = GPHOT
        ELSE
             MAINT = MAINTS*TEFF
        END IF
        IF(TEMP < MINtmp) THEN
             GPHOT = MAINT
        END IF
      end subroutine RADIA