c     ASCE STANDARDIZED REFERENCE EVAPOTRANSPIRATION EQUATION.  2005.  ALLEN, R.G., I.A. WALTER, R.L. ELLIOTT, T.A. HOWELL, D. ITENFISU,
C M.E. JENSEN, AND R.L. SNYDER. (EDS), AM. SOC. CIV. ENGRS., 216 P. ISBN 078440805X  AVAILABLE AT: 
C
C	HTTP://WWW.ASCE.ORG/BOOKSTORE/BOOK.CFM?BOOK-5430
C
C     HOURLY ET CALCULATION, ASCE STANDARDIZED
C
C    sub ASCE_hr
C    REFERENCE ET WAS BASED ON RICHARD ALLEN, THE ASCE STANTARDIZED EVAPORTANSPIRAITON EQUATION
C

C# INITIALIZE CALCULATION VARIABLES
!======================================================================
      subroutine REF_ET(I,tmin,tmax,hrts1,hrh1,hru1,elev,lat,long,jday,
     +      ETr_hr_T,ETr_hr_S,trat,xw,ihourly,PA)
!======================================================================
!
!       PURPOSE:
!
!       REF:
!
!       VARIABLE DEFINITIONS:
!       TYP  VARIABLE    USE   DESCRIPTION
!       ---  --------   -----  -----------
!       DBL  AVP_hr     LOCAL
!       DBL  B          LOCAL         
!       DBL  BETA       LOCAL
!       DBL  CD         LOCAL
!       DBL  CN         LOCAL
!       DBL  Delta      LOCAL
!       DBL  dr         LOCAL
!       DBL  ELEV       INPUT       
!       DBL  ET_DEN     LOCAL
!       DBL  ET_HR      LOCAL
!       DBL  ET_NUM     LOCAL
!       DBL  ETR_HR_S   OUTPT 
!       DBL  ETR_HR_T   OUTPT
!       DBL  FCD        INOUT        
!       DBL  FCD_SAVE   INOUT
!       DBL  G          LOCAL        
!       DBL  Gamma      LOCAL
!       DBL  hrh1       INPUT
!       DBL  tmin       INPUT
!       DBL  tmax       INPUT
!       DBL  hrts1      INPUT
!       DBL  hru1       INPUT
!       INT  I          INPUT
!       INT  IHR        LOCAL
!       INT  JDAY       INPUT
!       DBL  LAT        INPUT
!       DBL  LAT_R      LOCAL
!       DBL  LON_R      LOCAL
!       DBL  LONG       INPUT
!       DBL  P          LOCAL
!       DBL  PI         PARAM
!       DBL  RA_HR      LOCAL
!       DBL  RELSOL     LOCAL
!       DBL  RH_HR      LOCAL
!       DBL  RN_HR      LOCAL
!       DBL  RNL_HR     LOCAL
!       DBL  Rns_hr     LOCAL
!       DBL  Rs_hr      LOCAL
!       DBL  RSO_HR     LOCAL
!       DBL  SC         LOCAL
!       DBL  SD         LOCAL
!       DBL  SVP_hr     LOCAL
!       DBL  t          LOCAL
!       DBL  T_HR       LOCAL
!       DBL  T1         LOCAL
!       DBL  THETA_HR   LOCAL
!       DBL  TK_hr      LOCAL        
!       DBL  TRAT       INPUT         
!       DBL  u2_HR      LOCAL
!       DBL  W          LOCAL
!       DBL  W1         LOCAL
!       DBL  W2         LOCAL
!       DBL  WS         LOCAL
!       DBL  XW         INPUT  HEIGHT OF MEASURED WINDSPEED (m)     
!
!       COMMENTS: REFERENCE HOURLY ET BASED ON RICHARD ALLEN, THE ASCE STANTARDIZED EVAPORTANSPIRAITON EQUATION
!           
!       EXTERNAL REFERENCES: NONE
!       CALLED FROM:  PHYSCL, RZMAIN
!
!       PROGRAMMER:
!
!       VERSION:      x.x
!
!======================================================================
       IMPLICIT NONE
        DOUBLE PRECISION TK_hr,Rs_hr,t,PA,Gamma,Delta,SVP_hr,AVP_hr,
     +  Rns_hr, dr,SD,W,W1,W2,WS,B,LAT_R,LON_R,FCD,RELSOL,G,ET_NUM,
     +  ET_DEN,ET_HR,RH_HR,u2_HR,ELEV,LAT,LONG,PI,FCD_SAVE,THETA_HR,
     +  SC,T1,RA_HR,RSO_HR,RNL_HR,RN_HR,CN,ETR_HR_T,T_HR,
     +  BETA,CD,ETR_HR_S,hrts1,hrh1,hru1,trat,xw,period,
     +  tmin, tmax, svpmin, svpmax, tavg, TK_day
        INTEGER I, IHR ,JDAY,ihourly
c	character *8, refcrop
      PARAMETER (PI = 3.1416D0)   !,lat=40.4487,elev=1431.6)
      DATA FCD_SAVE/0.55d0/
      save fcd_save
C      long=104.6383
        IHR=I-1
        TK_hr   = 0.0D0
        TK_day  = 0.0D0
        t       = 0.0D0
        PA      = 0.0D0
        Gamma   = 0.0D0
        Delta   = 0.0D0
        SVP_hr  = 0.0D0
        AVP_hr  = 0.0D0
        Rns_hr  = 0.0D0
        dr      = 0.0D0
        sd      = 0.0D0
        w       = 0.0D0
        w1      = 0.0D0
        w2      = 0.0D0
        ws      = 0.0D0
        b       = 0.0D0
        Lat_r   = 0.0D0
        Lon_r   = 0.0D0
        fcd     = 0.0D0
        relsol  = 0.0D0
        G       = 0.0D0
        ET_num  = 0.0D0
        ET_den  = 0.0D0
        ET_hr   = 0.0D0
        svpmin  = 0.0D0
        svpmax  = 0.0D0
        tavg    = 0.0D0
        T_hr=tmax
	  Rs_hr=hrts1
	  rh_hr=hrh1/100.0d0
	  U2_hr=hru1*4.87d0/log(67.8d0*xw-5.42d0)
C
c        p=84.689
c	  T_hr=-3.686
c	  Rs_hr=0.0d0
c	  hr=24.0d0
c	  Rh_hr=0.651
c	  U2_hr=2.253
c        doy=1
c        refcrop='tallref'
c       read (1, *) p,T_hr, Rs_hr,hr,Rh_hr,doy
c       Arcsin(X) = Atn(X / Sqr(-X * X + 1))       
c       Arccos(X) = Atn(-X / Sqr(-X * X + 1)) + 2 * Atn(1)
C# DEFINE CALC. CONSTANTS
C      Stefan-Boltzmann constant in MJ K^-4 m^-2 h^-1
C       theta_hr = 0.0000000002042
C    # UNIT CONVERSION, kJ m^-2 min^-1 to MJ m^-2 h^-1
c      Rs_hr = Rs_hr * 0.001D0 * 60.0D0
C    # UNIT CONVERSION, W m^-2 to MJ m^-2 h^-1
c      CALL ECONST(DEO,EA,ED,ELEV,PA,PSY,RH,RHOA,TMIN,TMAX,XLH,TA)
c
      IF(ihourly.eq.1) THEN
       theta_hr = 2.042D-10
       Rs_hr = Rs_hr*3.6d3/1.0d6 !RTS IN MJ/M2/HR?
      ELSE
       theta_hr = 4.901D-9
       Rs_hr = Rs_hr ! RTS in MJ/M^2/DAY?
       tavg = (tmin+tmax)/2.0d0 ! T_hr = avg temp daily
       T_hr = tavg
      ENDIF

C    # UNIT CONVERSION, DEGREES CELSIUS TO KELVIN
      TK_hr = 273.16D0 + T_hr


      !Rs_hr = Rs_hr*3.6d3/1.0d6

      Lat_r = (PI * Lat) / 180.D0
      Lon_r = (PI * Long) / 180.D0

C    # CALCULATE MID-HOUR TIME STEP
      IF (ihourly.eq.1) then
          IF (IHR.EQ.0) THEN
	    T=24.0D0-0.5D0
	    ELSE
          t = Ihr - 0.5D0
          ENDIF
      ENDIF
      

C    # ATMOSPHERIC PRESSURE IN kPa (EQN. 34)
      PA = 101.3D0 * ((293.0D0 - 0.0065D0 * Elev) / 293.0D0)**5.26D0

C     # PSYCHROMETRIC CONSTANT, GAMMA (EQN. 35)
      Gamma = 0.000665D0 * PA

C         #NOTE: Assumes latent heat of vaporization (lambda) is equal
C         #         to 2.45 MJ kg^-1 and density of water is 1.0 Mg m^-3

C    # SLOPE OF THE SAT. VAPOR PRESS.-TEMP. CURVE IN kPa C^-1 (EQN. 36)
      Delta = (2503.0d0 * dexp((17.27d0 * T_hr) / (T_hr + 237.3d0))) 
     +    / (T_hr + 237.3d0)**2.0d0



      if (ihourly.eq.1) then
          !    # SAT.VAPOR PRESSURE IN kPa (Tetens)
          SVP_hr = 0.6108d0 * dexp((17.27d0*T_hr) / (T_hr+237.3d0))
          !    # ACT. VAPORT PRESSURE IN kPa (FROM AVG. AIR TEMP. & RH, EQN. 41)
          AVP_hr = SVP_hr * RH_hr
      else ! daily
          svpmax = 0.6108d0 * dexp((17.27D0*tmax)/(tmax+237.3D0))
          svpmin = 0.6108d0 * dexp((17.27D0*tmin)/(tmin+237.3D0))
          !    # SAT.VAPOR PRESSURE IN kPa (Tetens) (EQN. 6)
          SVP_hr = (svpmin+svpmax)/2.0d0
          !    # ACT. VAPORT PRESSURE IN kPa (EQN. 14)
          AVP_hr = RH_hr*(0.6108d0*dexp((17.27D0*tavg)/(tavg+237.3D0)))
      endif


      

C    # INVERSE DISTANCE FACTOR FOR EARTH-SUN (EQN. 50)
      dr = 1.0d0 + 0.033d0 * dcos(0.0172d0 * jday)

C    # SOLAR DECLINATION IN RADIANS (EQN. 51)
      sd = 0.409d0 * dsin((0.0172d0 * jday) - 1.39d0)

C    # "b" A FACTOR USED IN CALC. OF SEASONAL CORRECTION FOR SOLAR TIME (EQN. 58)
      b = (2.0d0 * PI * (jday - 81)) / 364.0d0

C    # SEASONAL CORRECTION FOR SOLAR TIME (EQN. 57)
      Sc = 0.1645d0*dsin(2.0d0*b)-0.1255d0*dcos(b)-0.025d0*dsin(b)

C    # SOLAR TIME ANGLE AT THE MIDPOINT OF THE PERIOD (EQN. 55)
      w  = (PI/12.0d0)*((t + 0.06667d0*(105.0d0 - Long)+Sc)-12.0d0)

C    # SOLAR TIME ANGLE AT BEGINNING OF EACH PERIOD IN RADIANS (EQN. 53)
       t1=1.0d0   !for hourly
c	 t1=0.5   !for 30 min
      w1 = w - ((PI * t1) / 24.0d0)

C    # SOLAR TIME ANGE AT END OF EACH PERIOD IN RADIANS (EQN. 36)
      w2 = w + ((PI * t1) / 24.0d0)

C    # SUNSET HOUR ANGLE (EQN. 59)
      ws = acos(-tan(Lat_r) * tan(sd))

C    # DEFINE INTEGRATION LIMITS TO EXTRATERRESTRIAL RADIATION (EQN. 56)

       IF (w1 .LT. -ws)  w1 = -ws
       IF (w2 .LT. -ws)  w2 = -ws
       IF (w1 .GT. ws)   w1 = ws
       IF (w2 .GT. ws)    w2 = ws
       IF (w1 .GT. w2)   w1 = w2
C    # EXTRATERRESTRIAL RADIATION FOR HOURLY PERIODS IN MJ m^-2 h^-1 (EQN. 48)
      if(ihourly.eq.1) then
        period = 12.0d0/PI
        Ra_hr = period*4.92d0*dr * ((w2 - w1)*dsin(Lat_r)*dsin(sd)
     +  + dcos(Lat_r) * dcos(sd) * (dsin(w2) - dsin(w1)))
      else
        period = 24.0d0/PI
        Ra_hr = period*4.92d0*dr * ( ws *dsin(Lat_r)*dsin(sd)
     +  + dcos(Lat_r) * dcos(sd) * dsin(ws) )
      endif


C    # CLEAR-SKY SOLAR RADIATION IN MJ m^-2 h^-1 (EQN. 47)
      Rso_hr = (0.75d0 + 0.00002d0 * Elev) * Ra_hr


c       my ($Rso_hr, $relsol);
       IF (Rso_hr .LE.1.0d-10) THEN
        relsol = 0.0d0
	  else
C    # RELATIVE SOLAR RADIATION
        relsol = Rs_hr / Rso_hr
       ENDIF
C   # ANGLE OF SUN ABOVE THE HORIZON AT THE MIDPOINT OF THE PERIOD (EQN. 62)
      beta = asin(dsin(Lat_r)*dsin(sd)+dcos(Lat_r)*dcos(sd)*dcos(w))

C       # DEFINE INTEGRATION LIMITS TO CLOUDINESS FUNCTION
c       my $relsol;
       IF (relsol .LT. 0.30d0) relsol = 0.30d0
       IF (relsol .GT. 1.000d0) relsol = 1.0d0
C   # CALCULATE THE CLOUDINESS FUNCTION (EQN. 45)
       if (beta.gt.0.0d0) then
	 fcd = 1.35d0 * relsol - 0.35d0
       fcd_save=fcd
       else
	 fcd=fcd_save   !saved from previous day
	 endif

C   # NET SHORT-WAVE RADIATION IN MJ m^-2 h^-1 (EQN. 43)
      Rns_hr = 0.77d0 * Rs_hr


      IF (ihourly.eq.1) then
!   # NET OUTGOING LONG-WAVE RADIATION IN MJ m^-2 h^-1 (EQN. 44)
          Rnl_hr = theta_hr*fcd*(0.34d0-0.14d0*sqrt(AVP_hr))*TK_hr**4.d0
      ELSE
          TK_day = ((tmax+273.16D0)**4.d0 + (tmin+273.16D0)**4.0d0)/2.d0
          ! (EQN. 19) Calculate each day
          fcd = 1.35d0 * relsol - 0.35d0

!   # NET OUTGOING LONG-WAVE RADIATION IN MJ m^-2 h^-1 (EQN. 17)    
          Rnl_hr = theta_hr*fcd*(0.34d0-0.14d0*sqrt(AVP_hr))*TK_day
      ENDIF

C   # CALCULATE NET RADIATION IN MJ m^-2 h^-1 (EQN. 42)
      Rn_hr = Rns_hr - Rnl_hr

C  # SOIL HEAT FLUX DENSITY (EQN. 65 & 66) & INTEGRATION LIMITS TO ETr/ETo
c      my ($CD, $CN, $G);

c      IF (REFCROP .eq. 'TALLREF') THEN
C      if ($refType eq 'TALLREF') {
        CN = 66.0d0
        if (ihourly.eq.1) then
         IF (Rn_hr .GT. 1.0d-4) THEN
            CD = 0.25d0
            G = 0.04d0 * Rn_hr
         else 
            CD = 1.7d0
            G = 0.2d0 * Rn_hr
         ENDIF
        else
            CN = 1600.0D0
            CD = 0.38D0
            G = 0.0d0
        endif
C    # CALCULATE ETr (EQN. 1)
       ET_num=0.408d0*Delta*(Rn_hr - G) + Gamma * (CN/(T_hr+273.16D0))
     +   * U2_hr * (SVP_hr - AVP_hr)
       ET_den = Delta + Gamma * (1.0d0 + CD * U2_hr*trat)
       ETr_hr_t = ET_num / ET_den

c      ENDIF
c      IF (REFCROP .eq. 'SHORTREF') THEN
        CN = 37.0d0
       if (ihourly.eq.1) then
        IF (Rn_hr .GT. 1.0d-4) THEN
            CD = 0.24d0
            G = 0.1d0 * Rn_hr
         else 
            CD = 0.96d0
            G = 0.5d0 * Rn_hr
        ENDIF
       else
            CN = 900.0D0
            CD = 0.34D0
            G = 0.0d0
       endif
c      ENDIF
C    # CALCULATE ETr (EQN. 1)
       ET_num=0.408d0*Delta*(Rn_hr - G) + Gamma * (CN/(T_hr+273.16D0))
     +   * U2_hr * (SVP_hr - AVP_hr)
       ET_den = Delta + Gamma * (1 + CD * U2_hr*trat)
       ETr_hr_s = ET_num / ET_den

C      print*, IHR,ETr_hr_T,ETr_hr_S
c       return(ETr_hr)
c }
      end
