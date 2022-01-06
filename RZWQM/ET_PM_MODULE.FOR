C   !LIBRARY WASSUBA ( Simulation Wasserhaushalt fÅr N-Modell )
C   ! Einbau von Luftmangel fuer Pflanzen am 16.12.98
C   MODULE WASSUBA FROM HERMES MODEL, CONTRIBUTED BY CHRISTIAN KERSEBAUM
C   DECLARE DEF ACOS
      SUBroutine ET_PM(DOY,RAD,Wind,Tmax,Tmin,TEMP,SUND,EVmax,Tramax)
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C=======================================================================

        DIMENSION WUEFF(21),VERDU(366),VAR(21),TRRED(21)
        DIMENSION WG()
        TPAKT = 0.0D0
        IF (ZEIT.GT.BEGINN) THEN
           DO I = 1 to N
               WG(0,I) = WG(1,I)
               q1(I) = 0.0D0
           ENDDO
           WG(0,N+1)=WG(1,N)
        END IF
C
        WOB = WG(0,1)+REGEN(TAG)/DZ                     !-(WG(0,1)+REGEN(TAG)/DZ - WG(0,2))/4
        IF (WOB.LT.WMIN(1)/3.0D0) WOB = WMIN(1)/3.0D0   ! geÑndert 22.3.93
        PROZ = (WOB-WMIN(1)/3.0D0)/(W(1)-WMIN(1)/3.0D0)
        IF (PROZ.GT.1.0D0) PROZ = 1.0D0
        IF (PROZ.GT.0.33D0) THEN
           REDEV = 1.0D0 - (0.1D0 * (1.0D0-PROZ)/(1.0D0-.33D0))
        ELSE IF (PROZ.GT.0.22D0) THEN
           REDEV = .9D0 - (.625D0 * (.9D0-PROZ)/(.9D0-.22D0))
        ELSE IF (PROZ.GT.0.2D0) THEN
           REDEV = .275D0 - (.225D0 * (.22D0-PROZ)/(.22D0-.2D0))
        ELSE
           REDEV = .05D0 - (.05D0 * (.2D0-PROZ)/.2D0)
        END IF
        TRPOS = 0.D0
        DO I = 1 TO N            !WURZ
            NFK(I) = (WG(0,I)-WMIN(I))/(WNOR(I)-WMIN(I))
            NFK(1) = (WG(0,1)+REGEN(TAG)/DZ-WMIN(1))/(WNOR(1)-WMIN(1))
            IF (NFK(i).LT. 0.D0) NFK(I) = 0.D0
        ENDDO
        IF (ZEIT.EQ.SAAT(AKF)) then
           CALL VERDUN
           RDTSUM = 1.0D0
           REDSUM = 1.0D0
        END IF
        IF ((ZEIT.GT.SAAT(AKF).AND.(ZEIT.LT.ERNTE(AKF))
     +     .AND.(INTWICK.GT.1.0D0)) THEN
           VRED = 1.0D0           !REDSUM/RDTSUM
        ENDIF


              ! ----------------------- Calculation of reference evapotranspiration for grass acc. to Penman-Monteith  (cm/day) --------------
              ! -- Required inputs:   Temp (temperature daily average, C∞)
              !                       Tmin (temperature daily minimum, C∞)
              !                       Tmax (temperature daily maximum, C∞)
              !                       RAD  (radiation daily sum,  MJ/m^2)
              !                       RH   (relative humidity, %)
              !                       DOY  (julian day)
              !                       LAT  (latitude)
              !                       Height of wind speed measurement, m)
              !                       Alti (station altitude a.s.l.)
              !                       Wind (average windspeed 2m height m/s)
              !                       RADn (net radiation, MJ/m^2/d)
              ! -- predefined constants: Rstom0 (standard stomata resistance FAO 56)
              !                          Albedo (albedo of grass vegetation)
              !                          Bolz (Bolzmann constant 4.9 * 10^-9 MJ/(m^2 + K^4)
              Rstom0 = 100.D0
              Albedo = 0.23D0
              Bolz = 4.9D-9    

              !     -------- Calculation of daylength and radiation -----
              !     ----------------------- DECLINATION -----------------------
              DEC = 0.409*sin(2*PI/365*DOY-1.39)*180/PI     ! formula of FAO paper
              SINLD = SIN(DEC*PI/180.)*SIN(LAT*PI/180.)
              COSLD = COS(DEC*PI/180.)*COS(LAT*PI/180.)

              !     -------------------- ASTRONOMIC DAYLENGTH ------------
              DL = 12.*(PI+2.* ASIN(SINLD/COSLD))/PI

              !  -------- Calculation of extraterrestrial radiation RS0 (MJ/m^2) ---------
              SC = 24*60/PI*8.20 *(1+0.033*COS(2*Pi*DOY/365))
              SHA = ACOS(-Tan(lat*PI/180)*TAN(dec*PI/180))
              EXT = SC * (SHA*SINLD + COSLD*SIN(SHA))/100
              RS0 = (0.75 + 0.00002*ALTI)*EXT
              ! ---- Calculation atmospheric pressure in kPa
              ATMPress=101.3d0*((293.d0-(6.5d-3*Alti))/293.d0)^5.26d0
              ! ---- Calculation Psychrometer-constant
              Psych = 0.000665d0 * ATMPress
              ! ---- Calculation saturation vapour pressure at Tmax
              SatPmax = 0.6108d0*exp(((17.27d0*Tmax)/(237.3d0+Tmax)))
              ! ---- Calculation saturation vapour pressure at Tmin
              SatPmin = 0.6108d0*exp(((17.27d0*Tmin)/(237.3d0+Tmin)))
              ! ---- Calculation average saturation vapour pressure
              SatP = (SatPmin + SatPmax)/2.0d0
              ! ---- Calculation actual vapour pressure
              Vapres = 0.6108d0*exp(((17.27d0*Tmin)/(237.3d0+Tmin)))  
              ! ---- Calculation vapour pressure deficit (satdef)
              Satdef = SatP - Vapres    !* (1 - RH(DOY)/100)
              ! Deltsat = slope of saturation pressure-temperature relation
              Deltsat = (4098.d0*(0.6108d0*Exp((17.27d0*Temp)/
     +                  (Temp+237.3d0))))/((TEMP+237.3d0)^2.d0)
              ! ----------------------- Call submodule for dynamic calculation of stomata resistance ---------------------------------
              Call stomat(satdef)
              ! ----------------------------------------------------------------------------------------------------------------------
              ! ----- if wind speed measurement different from 2 meter transfer to 2 meter height -------
              IF (Windhi.NE.2) then
               WIND=Wind*(4.87D0/(log(67.8D0*Windhi-5.42D0)))
              END IF
              ! ----- Calculation of aerodynamic resistance ra (Raero)
              If (Wind.EQ.0.D0) wind = 0.00001D0
               Raero =  208.D0 / WIND
              ! ----- Calculation of surface resistance rs with standard FAO stomata resistance 100 s/m (Rsurf0)
               Rsurf0 = Rstom0/1.44D0
              ! ----- Calculation of surface resistance rs with dynamic stomata resistance from TSOMAT submodule (Rsurf)
               Rsurf =  Rstom/1.44D0
              !-------------------- Calculation of global radiation from sunshine duration, if measurements are not available ----------------
              IF (RAD.GT. 0.D0) then
                 ! Calculation of net radiation from measured global radiation 
                 RADRatio = Rad/RS0
                 If (RADRatio.GT.1.0D0) Radratio = 1
                 RADn = (1-Albedo)*RAD-Bolz*((Tmin+273.16D0)^4.d0
     +              +(Tmax+273.16d0)^4.0d0)/2.d0*
     +              (1.35d0*Radratio-0.35d0)*(0.34d0-0.14d0*SQR(Vapres))
              ELSE
                 Glob = EXT * (0.19d0 + 0.55d0*SUND/DL)     ! Calculation of Global radiation (MJ/m^2) from sunshine duration
                 RADRatio = Glob/RS0
                 If (RADRatio.GT.1.0D0) Radratio = 1.d0
                 RADn = (1-Albedo)*GLOB-Bolz*((Tmin(DOY)+273.16d0)^4.d0
     +             +(Tmax(DOY)+273.16D0)^4.d0)/2.d0* 
     +             (1.35D0*Radratio-0.35D0)*(0.34D0-0.14D0*SQR(Vapres))
              END IF

              ! ------------------------ Calculation of reference evapotranspiration ET0 --------------------
              ! CTRANS = 0 represents the calculation of standard FAO 56 ET0, CTRANS = 1 use the dynamic stomata resistance calculated in SUB STOMAT
              IF (CTRANS.eq.0.d0) then
                ! standard FAO ET0
                ET0 = ( (0.408D0*Deltsat*Radn)+(Psych*(900.D0 /
     +             (Temp+273.D0))*WIND*Satdef))/ 
     +             (Deltsat+Psych*(1.d0+(Rsurf0/208.D0)*WIND))
              else
                ! dynamic ET0
                ET0 = ( (0.408D0*Deltsat*Radn)+(Psych*(900.d0/ 
     +                (Temp+273.d0))*WIND*Satdef))/
     +                (Deltsat+Psych*(1.0d0+(Rsurf/208.d0)*WIND))
              end if
             IF (ET0.LT.0) ET0 = 0.d0
             ! Transfer of reference ET= to crop specific ET using FKC (kc factor)
              VERDU = ET0 * FKC * 0.1d0    !FKC = kc
!------------------------------------------------------------------------------------------------------------
           ! Limitation to maximum value for Germany
           IF (verdu.gt.0.65d0) verdu = 0.65d0
           ! Distribution of potential ET based on LAI to evaporation (EVMAX) and transpiration (TRAMAX)
           EVMAX = VERDU*EXP(-.5d0*LAI)   
           TRAMAX = VERDU - EVMAX
           END SUB

           SUBroutine stomat (Satdef)
           ! -------------------------------------------------------------------------
           ! Input:  Agross (gross photsynthesis of grass (mol CO2/m^2/sec)
           !         CO2Konz (CO2 concentration atmosphere, ppm)
           !
           ! Constants:  alpha = 40
           !              beta  = 2.5
           ! -------------------------------------------------------------------------
           RSTOM = 1/(alph * Agross/ (CO2Konz*(1+satdef/satbeta)))   !Original satbeta = 3.5
          end sub
