C $Id: Rzpet.for,v 1.1 2002/08/28 00:00:48 rojas Exp $
C
      FUNCTION ALBSWS(A0,AW,WC13,WC15,THETA,ICRUST,RR)
C
C======================================================================
C
C       PURPOSE: CALCULATE ALBEDO WEIGHTED FOR WATER CONTENT
C
C       REF: DECOURSEY
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       A0         I  ALBEDO DRY SOIL
C       A0T        L
C       ALBSWS     O  ALBEDO WEIGHTED BY WATER CONTENT
C       AW         I  ALBEDO WET SOIL
C       AWT        L
C       ICRUST     I  FLAG INDICATING IF A CRUST EXISTS (=1;TRUE)
C       RR         I  RANDOM ROUGHNESS (CM)
C       RR8        P  8% CHANGE IN ALBEDO PER 1CM OF RR
C       RRMOD  L
C       SLOPE  L
C       THETA  I  SOIL MOISTURE CONTENT
C       TRCP   L
C       WC13   I  1/3 BAR WATER CONTENT
C       WC15   I  15 BAR WATER CONTENT
C
C       CALLED FROM:  POTEVP
C
C       PROGRAMMER:   KEN ROJAS
C
C       VERSION       1.0
C
C======================================================================
C
C     ..DECLARE VARIABLES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(RR8=0.08D0)
C
      AWT=AW
      A0T=A0
C
C     ..DOUBLE ALBEDO AFTER CRUSTING (REF: SHAFFER & LARSON, NTRM)
      IF(ICRUST.NE.0) A0T=2.0D0*A0T
C
C     ..ADJUST FOR SURFACE ROUGHNESS
      RRMOD=RR*RR8
      IF(RRMOD.GT.0.0D0) THEN
        AWT=AWT*(1.0D0-RRMOD)
        A0T=A0T*(1.0D0-RRMOD)
      ENDIF
C
C     ..CALCULATE ALBEDO AS FN OF WATER CONTENT
      SLOPE=(AWT-A0T)/(WC13-WC15)
      TRCP=AWT-(SLOPE*WC13)
      ALBSWS=SLOPE*THETA+TRCP
C
C     ..CHECK BOUNDS
      IF(THETA.GE.WC13) THEN
        ALBSWS=AWT
      ELSEIF(THETA.LE.WC15) THEN
        ALBSWS=A0T
      ENDIF
C
      RETURN
      END
C
      FUNCTION CSRAD(T,C1,C2,B)
C
C======================================================================
C
C       PURPOSE: CALCULATE INSTANTANEOUS CLEAR-SKY DIRECT RADIATION
C              ON EARTH SURFACE. [RCSI]
C
C       REF: EAGLESON, P.S., DYNAMIC HYDROLOGY, MCGRAW-HILL, P.34, 1970;
C          SCHAFFER & LARSON, NTRM PROGRAM.
C          SWIFT, L.R., WRR, 12(1):108-112,1976.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       A1         L
C       B      I  ATMOSPHERIC TURBIDITY.
C       C1         I  COMBINATION CONSTANT (=SIND*SINP).
C       C2         I  CONBINATION CONSTANT (=COND*COSP).
C       CSRAD  O  INSTANTANEOUS, TOTAL OR NET CLEAR-SKY DIRECT
C                 RADIATION ON EARTH SURFACE [MJ/M^2/DAY]
C       H0            AIR MASS REL OPTICAL THICKNESS.
C       RCSI      INSTANTANEOUS, TOTAL OR NET CLEAR-SKY DIR RAD
C                 ON EARTH SURFACE [MJ/M^2/HR].
C       SINA      SIN OF SOLAR ALTITUDE (A).
C       T      I  SOLAR HOUR ANGLE [R].
C
C
C       COMMENTS: THIS ROUTINE IS APPROPRIATE FOR HORIZONTAL OR SLOPE
C             SURFACES, DEPENDING ON VALUES OF LATITUDE (ACTUAL
C             OR EQUIVALENT) USED TO EVALUATE COMBINATION CONSTANTS
C             C1 & C2 (SEE SWIFT).
C
C       EXTERNAL REFERENCES:
C                 NONE
C
C       CALLED FROM:  GAUSS
C
C       PROGRAMMER:   CHARLES HEBSON AND KEN ROJAS
C
C       VERSION:      1.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     ..A=SOLAR ALTITUDE ANGLE.
      SINA=C1+C2*DCOS(T)
C
      if (sina.gt.4.2622d-3) then  !this is to make sure sina and a1 are greater than zero
C     ..CALCULATE AIR MASS RELATIVE OPTICAL THICKNESS.
      H0=1.D0/SINA
C
C     ..CALCULATE AIR MOLECULAR SCATTERING COEF, A1.
      A1=0.128D0-0.054D0*DLOG10(H0)
C
C     ..CSRAD HERE IS RELATIVE TO ADJUSTED SOLAR CONSTANT. THEREFORE
C     MUST BE MULTIPLIED BY SOLAR CONST IN A HIGHER CALLING ROUTINE.
C
      CSRAD=SINA*DEXP(-B*A1*H0)
      else
	csrad=0.0d0
	endif
C
      RETURN
      END
C
      SUBROUTINE ECONST(DEO,EA,ED,ELEV,PA,PSY,RH,RHOA,TMIN,TMAX,XLH,TA)
C
C======================================================================
C
C       PURPOSE: CALCULATE AERODYNAMIC TRANSFER COEFFICIENTS
C
C       REF:
C          VAN BAVEL, WRR, 2(3):455-467, 1966
C          SAT VAPOR CURVE EQUATION: BOSEN, 1960, MONTHLY WEATHER
C                            REVIEW, 88(8):257-276.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       CP         P  SPECIFIC HEAT OF MOIST AIR AT CONST PRES (MJ/KG C)
C       DEO        O  SLOPE OF THE SATURATION VAPOR CURVE (KPA/C)
C       EA         O  AVERAGE SATURATION VAPOR PRESSURE (KPA)
C       ED         O  SATURATED VAPOR PRESSURE AT DEW TEMP (KPA)
C       ELEV   I  ELEVATION AT SITE (M)
C       ETTA   P  ADIABATIC LAPSE RATE FOR DRY AIR (K/M)
C       GRAV   P  GRAVITY ACC. (M/S**2)
C       K      P  VON KARMAN CONSTANT ( =.41)
C       P0         P  STP, ATM PRESSURE (KPA)
C       PA         O  ATMOSPHERIC PRESSURE (KPA)
C       PSY        O  PSYCHROMETRIC CONSTANT (KPA/C)
C       R      P  GAS CONSTANT (J/KG/K)
C       RH         I  RELATIVE HUMIDITY
C       RHOA   O  DENSITY OF AIR (KPA)
C       T0         P  STP, TEMPERATURE (K)
C       TA         L  TEMPERATURE AT AV. SAT VP (C)
C       TMAX   I  MAXIMUM AIR TEMPERATURE (C)
C       TMIN   I  MINIMUM AIR TEMPERATURE (C)
C       XLH        O  LATENT HEAT OF VAPORIZATION (MJ/KG)
C       Z0         P  STP, ELEVATION (M)
C
C       COMMENTS: SAT VAP CURVE EQUATION ACCURATE FOR TEMPERATURE VALUES
C             RANGING FROM  -51C < T < 51C.
C
C
C       EXTERNAL REFERENCES:
C                 NONE
C
C       CALLED FROM:  POTEVP
C
C       PROGRAMMER:   KEN ROJAS
C
C       VERSION:      1.0
C
C======================================================================
C
C ... DECLARE VARIABLES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(CP=1.013D-3,P0=101.3D0,T0=288.0D0,Z0=0.0D0,GRAV=9.8D0,
     +    ETTA=0.01D0,R=286.9D0)
C
C ... STATEMENT FUNCTION FOR SATURATED VAPOR PRESSURE CURVE
      EO(T)=EXP((16.78D0*T-116.9D0)/(T+237.3D0))
C
C     ---------------------------------
C     AVERAGE SATURATION VAPOR PRESSURE (KPA)
C     ---------------------------------
      EA=(EO(TMAX)+EO(TMIN))*0.5D0
C
C     ----------------------------------
C     TEMPERATURE FOR AVERAGE SAT VP (C)
C     ----------------------------------
      TA=(237.3D0*LOG(EA)+116.9D0)/(16.78D0-LOG(EA))
C
C     -------------------------------------------------
C     VAPOR PRESSURE AT TA (KPA)
C     -------------------------------------------------
      ED=RH*EA*1.0D-2
C
C     -----------------------------------
C     SLOPE OF THE SATURATION VAPOR CURVE (KPA/C)
C     -----------------------------------
      DEO=4098.0D0*EO(TA)/(TA+237.3D0)**2.0D0
C
C     -------------------------------------------
C     ATMOSPHERIC PRESSURE CORRECTED FOR ALTITUDE (KPA)
C     -------------------------------------------
      PA=P0*((T0-ETTA*(ELEV-Z0))/T0)**(GRAV/ETTA/R)
C
C     -------------------
C     VIRTUAL TEMPERATURE (DEG K)
C     -------------------
      TV=(TA+273.2D0)/(1.0D0-0.378D0*ED/PA)
C
C     --------------
C     DENSITY OF AIR (KG/M^3)
C     --------------
      RHOA=1.0D3*PA/(TV*R)
C
C     ---------------------------
C     LATENT HEAT OF VAPORIZATION (MJ/KG)
C     ---------------------------
      XLH=2.501D0-2.361D-3*TA
C
C     ----------------------
C     PSYCHROMETRIC CONSTANT (KPA/C)
C     ----------------------
      PSY=CP*PA/(6.22D-1*XLH)
C
      RETURN
      END
C
C
      FUNCTION GAUSS(A,B,NGP,FUNC,C1,C2,C3)
C
C======================================================================
C
C       PURPOSE: INTEGRATE A FUNCTION BY GAUSS-LEGENDRE QUADRATURE.
C              THE FUNCTION MAY CONTAIN UP TO 5 CONSTANT PARAMETERS.
C
C       REF: ABRAMOWITZ & STEGUN, HANDBOOK OF MATH FUNCTIONS,
C          PP.916-919, DOVER, NY, 1972.
C          CARNAHAN ET AL, APPLIED NUMERICAL METHODS, PP.106-110,
C          WILEY, NY, 1969.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       A      I  LOWER INTEGRATION LIMIT.
C       B      I  UPPER INTEGRATION LIMIT.
C       C1         I  PARAMETERS OF FUNCTION.
C       C2         I      "      "    "
C       C3         I      "      "    "
C       C4         I      "      "    "
C       C5         I      "      "    "
C       D      L  SOLAR DECLINATION [R]
C       FN         L  VALUE OF FUNCTION
C       FN1        L
C       FN2        L
C       FUNC   I  DOUBLE PRECISION FUNCTION TO BE INTEGRATED
C       GAUSS  O  VALUE OF INTEGRAL FROM FUNCTION (FUNC)
C       I      L  INDEX VARIABLE
C       INP        L
C       J      L  INDEX VARIABLE
C       JE         L
C       JS         L
C       KEY        L
C       MG         P
C       MG1        P
C       NP         L
C       NGP        I  NUMBER OF SPECIFIED GAUSS POINTS.
C       S      L  SLOPE OF FIELD [RAD]
C       WG         L
C       XC         L  CONC. VECTOR FROM MOST RECENT SUB-STEP
C                 RUNGE-KUTTA SOLUTION FOR NEW TIME STEP
C       XG         L
C       XX         L
C
C       COMMENTS: FUNCTION MUST BE DECLARED EXTERNAL IN CALLING
C             ROUTINE.
C
C       EXTERNAL REFERENCES:
C                 FUNC
C
C       CALLED FROM:  MAXSW
C
C       PROGRAMMER:   CHARLES HEBSON
C
C       VERSION:      2.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MG=7,MG1=8)
      EXTERNAL FUNC
      INTEGER NP(MG),KEY(MG1),I,INP,J,JS,JE
      DIMENSION XG(24),WG(24)
      DATA NP /2,3,4,5,6,10,15/,KEY /1,2,4,6,9,12,17,25/
      DATA XG /         0.577350269D0,0.0D0,        0.774596669D0,
     +    0.339981046D0,0.861136312D0,0.0D0,        0.538469310D0,
     +    0.906179846D0,0.238619186D0,0.661209387D0,0.932469514D0,
     +    0.148874339D0,0.433395394D0,0.679409568D0,0.865063367D0,
     +    0.973906529D0,0.0D0,        0.201194094D0,0.394151347D0,
     +    0.570972173D0,0.724417731D0,0.848206583D0,0.937273392D0,
     +    0.987992518D0/
C
      DATA WG /         1.0D0,        0.888888889D0,0.555555556D0,
     +    0.652145155D0,0.347854845D0,0.568888889D0,0.478628671D0,
     +    0.236926885D0,0.467913935D0,0.360761573D0,0.171324493D0,
     +    0.295524225D0,0.269266719D0,0.219086363D0,0.149451349D0,
     +    0.066671344D0,0.202578242D0,0.198431485D0,0.186161000D0,
     +    0.166269206D0,0.139570678D0,0.107159221D0,0.070366047D0,
     +    0.030753242D0/
C
C======================================================================
C     FIND INDEX FOR SPECIFIED NO. OF GAUSS POINTS, NGP.
C======================================================================
C
      INP=0
      IF(NGP.LT.NP(1)) THEN
        INP=1
      ELSEIF(NGP.GT.NP(MG)) THEN
        INP=MG
      ELSE
        DO 10 I=1,MG
          IF(NGP.EQ.NP(I)) THEN
            INP=I
            GOTO 30
          ENDIF
   10   CONTINUE
      ENDIF
C
C     ..NGP NOT CONTAINED IN NP--FIND BRACKETING NP VALUES AND USE
C     LARGEST.
      IF(INP.EQ.0) THEN
        DO 20 I=2,MG
          IF(NP(I-1).LT.NGP.AND.NGP.LT.NP(I)) THEN
            INP=I
            GOTO 30
          ENDIF
   20   CONTINUE
      ENDIF
C
C     ..SET UP START, END INDICES FOR WEIGHT AND GAUSS POINT VECTORS.
   30 JS=KEY(INP)
      JE=KEY(INP+1)-1
      C=(B-A)/2.D0
      D=(B+A)/2.D0
C
C     ..ACCUMULATE THE SUM IN THE NGP-POINT FORMULA.
      S=0.D0
      DO 40 J=JS,JE
        IF(XG(J).EQ.0.0) THEN
          FN=FUNC(D,C1,C2,C3)
          S=S+WG(J)*FN
        ELSE
          XC=XG(J)*C
          XX=XC+D
          FN1=FUNC(XX,C1,C2,C3)
          XX=-XC+D
          FN2=FUNC(XX,C1,C2,C3)
          S=S+WG(J)*(FN1+FN2)
        ENDIF
   40 CONTINUE
C
C     ..MAKE INTERVAL CORRECTION
      GAUSS=C*S
C
      RETURN
      END
      FUNCTION GAUSS_OLD(A,B,NGP,FUNC,C1,C2,C3)
C
C======================================================================
C
C       PURPOSE: INTEGRATE A FUNCTION BY GAUSS-LEGENDRE QUADRATURE.
C              THE FUNCTION MAY CONTAIN UP TO 5 CONSTANT PARAMETERS.
C
C       REF: ABRAMOWITZ & STEGUN, HANDBOOK OF MATH FUNCTIONS,
C          PP.916-919, DOVER, NY, 1972.
C          CARNAHAN ET AL, APPLIED NUMERICAL METHODS, PP.106-110,
C          WILEY, NY, 1969.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       A      I  LOWER INTEGRATION LIMIT.
C       B      I  UPPER INTEGRATION LIMIT.
C       C1         I  PARAMETERS OF FUNCTION.
C       C2         I      "      "    "
C       C3         I      "      "    "
C       C4         I      "      "    "
C       C5         I      "      "    "
C       D      L  SOLAR DECLINATION [R]
C       FN         L  VALUE OF FUNCTION
C       FN1        L
C       FN2        L
C       FUNC   I  DOUBLE PRECISION FUNCTION TO BE INTEGRATED
C       GAUSS  O  VALUE OF INTEGRAL FROM FUNCTION (FUNC)
C       I      L  INDEX VARIABLE
C       INP        L
C       J      L  INDEX VARIABLE
C       JE         L
C       JS         L
C       KEY        L
C       MG         P
C       MG1        P
C       NP         L
C       NGP        I  NUMBER OF SPECIFIED GAUSS POINTS.
C       S      L  SLOPE OF FIELD [RAD]
C       WG         L
C       XC         L  CONC. VECTOR FROM MOST RECENT SUB-STEP
C                 RUNGE-KUTTA SOLUTION FOR NEW TIME STEP
C       XG         L
C       XX         L
C
C       COMMENTS: FUNCTION MUST BE DECLARED EXTERNAL IN CALLING
C             ROUTINE.
C
C       EXTERNAL REFERENCES:
C                 FUNC
C
C       CALLED FROM:  MAXSW
C
C       PROGRAMMER:   CHARLES HEBSON
C
C       VERSION:      2.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MG=7,MG1=8)
      EXTERNAL FUNC
      INTEGER NP(MG),KEY(MG1),I,INP,J,JS,JE
      DIMENSION XG(56),WG(56)
      DATA NP /4,6,10,16,20,24,32/,KEY /1,3,6,11,19,29,41,57/
      DATA XG /0.339981043584856D0,0.861136311594053D0,
     +    0.238619186083197D0,0.661209386466265D0,0.932469514203152D0,
     +    0.148874338981631D0,0.433395394129247D0,0.679409568299024D0,
     +    0.865063366688985D0,0.973906528517172D0,0.095012509837637D0,
     +    0.281603550779259D0,0.458016777657227D0,0.617876244402644D0,
     +    0.755404408355003D0,0.865631202387832D0,0.944575023073233D0,
     +    0.989400934991650D0,0.076526521133497D0,0.227785851141645D0,
     +    0.373706088715420D0,0.510867001950827D0,0.636053680726515D0,
     +    0.746331906460151D0,0.839116971822219D0,0.912234428251329D0,
     +    0.963971927277914D0,0.993128599185095D0,0.064056892862606D0,
     +    0.191118867473616D0,0.315042679696163D0,0.433793507626045D0,
     +    0.545421471388840D0,0.648093651936976D0,0.740124191578554D0,
     +    0.820001985973903D0,0.886415527004401D0,0.938274552992733D0,
     +    0.974728555971309D0,0.995187219997021D0,0.048307665787738D0,
     +    0.144471961582796D0,0.239287362252137D0,0.331868602282128D0,
     +    0.421351276130635D0,0.506899908932229D0,0.587715757240762D0,
     +    0.663044266930215D0,0.732182118740289D0,0.794483795967942D0,
     +    0.849367613732569D0,0.896321155766052D0,0.934906075937739D0,
     +    0.964762255587506D0,0.985611511545268D0,0.997263861849481D0/
C
      DATA WG /0.652145154862546D0,0.347854845137454D0,
     +    0.467913934572691D0,0.360761573048139D0,0.171324492379170D0,
     +    0.295524224714753D0,0.269266719309996D0,0.219086362515982D0,
     +    0.149451349150581D0,0.066671344308688D0,0.189450610455068D0,
     +    0.182603415044924D0,0.169156519395003D0,0.149595988816577D0,
     +    0.124628971255534D0,0.095158511682493D0,0.062253523938648D0,
     +    0.027152459411754D0,0.152753387130726D0,0.149172986472604D0,
     +    0.142096109318382D0,0.131688638449177D0,0.118194531961518D0,
     +    0.101930119817240D0,0.083276741576705D0,0.062672048224109D0,
     +    0.040601429800287D0,0.017614007139152D0,0.127938195346752D0,
     +    0.125837456346828D0,0.121670472927803D0,0.115505668053726D0,
     +    0.107444270115966D0,0.097618652104113D0,0.086190161531953D0,
     +    0.073346481411080D0,0.059298584915437D0,0.044277438817420D0,
     +    0.028531388628934D0,0.012341229799987D0,0.006540088841428D0,
     +    0.094638720079275D0,0.093844399080805D0,0.091173878695763D0,
     +    0.087652093004404D0,0.083311924226947D0,0.078193895787070D0,
     +    0.072345794108848D0,0.065822222776362D0,0.058684093478536D0,
     +    0.050998059262376D0,0.042835898022227D0,0.034273862913021D0,
     +    0.025392065309262D0,0.016274394730906D0,0.007018610009470D0/
C
C======================================================================
C     FIND INDEX FOR SPECIFIED NO. OF GAUSS POINTS, NGP.
C======================================================================
C
      INP=0
      IF(NGP.LT.NP(1)) THEN
        INP=1
      ELSEIF(NGP.GT.NP(MG)) THEN
        INP=MG
      ELSE
        DO 10 I=1,MG
          IF(NGP.EQ.NP(I)) THEN
            INP=I
            GOTO 30
          ENDIF
   10   CONTINUE
      ENDIF
C
C     ..NGP NOT CONTAINED IN NP--FIND BRACKETING NP VALUES AND USE
C     LARGEST.
      IF(INP.EQ.0) THEN
        DO 20 I=2,MG
          IF(NP(I-1).LT.NGP.AND.NGP.LT.NP(I)) THEN
            INP=I
            GOTO 30
          ENDIF
   20   CONTINUE
      ENDIF
C
C     ..SET UP START, END INDICES FOR WEIGHT AND GAUSS POINT VECTORS.
   30 JS=KEY(INP)
      JE=KEY(INP+1)-1
      C=(B-A)/2.D0
      D=(B+A)/2.D0
C
C     ..ACCUMULATE THE SUM IN THE NGP-POINT FORMULA.
      S=0.D0
      DO 40 J=JS,JE
        IF(XG(J).EQ.0.0) THEN
          FN=FUNC(D,C1,C2,C3)
          S=S+WG(J)*FN
        ELSE
          XC=XG(J)*C
          XX=XC+D
          FN1=FUNC(XX,C1,C2,C3)
          XX=-XC+D
          FN2=FUNC(XX,C1,C2,C3)
          S=S+WG(J)*(FN1+FN2)
        ENDIF
   40 CONTINUE
C
C     ..MAKE INTERVAL CORRECTION
      GAUSS_OLD=C*S
C
      RETURN
      END
C
      SUBROUTINE MAXSW(A,J,P,RCHD,RN,RCH,S,PP,RCS,RCSHR,RCHHR)
C
C======================================================================
C
C       PURPOSE: CALCULATES MAXIMUM POSSIBLE S-W RADIATION FOR TRANS-
C              PARENT ATM. ON A SLOPING SURFACE OVER A 24-HR PERIOD.
C
C       REF: SWIFT, L.R., WRR, 12(1):108-112, 1976.
C          SCHAFFER & LARSON, NTRM PROG & DOC, USDA-ARS, 1982.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O  DESCRIPTION
C       --------  ---  -----------
C       A      I   ASPECT ANGLE, CLOCKWISE FROM NORTH  [R]
C       B      P   ATM. TURBIDITY
C       C1            COMBINATION VARIABLE
C       C2            COMBINATION VARIABLE
C       D      L  SOLAR DECLINATION [R]
C       E      L  ECCENTRICITY OF EARTH'S ORBIT [RAD]
C       J      I  INDEX VARIABLE
C       NGP        P  NUMBER POINTS IN GAUSS QUADRATURE
C       P      I  LATITUDE [R]
C       PE         L  LATITUDE OF EQUIVALENT SLOPE [RAD]
C       PI         P  MATHEMATICAL CONSTANT = 3.141592654...
C       PP        I/O PHOTOPERIOD [HR]
C       PPCNST     P
C       R1         L
C       R2         L
C       RCH       I/O DAILY TOTAL CLEAR-SKY DIRECT AND DIFFUSE RAD ON
C                 HOR  EARTH SURFACE [MJ/M^2/DAY]
C       RCHD  I/O DAILY TOTAL CLEAR-SKY DIRECT RAD ON HOR EARTH
C                 SURFACE [MJ/M^2/DAY]]
C       RCS       I/O DAILY TOTAL CLEAR-SKY DIRECT AND DIFFUSE RAD ON
C                 SLOPING EARTH SURF [MJ/M^2/DAY]
C       RCS1   L
C       RCS2   L
C       RCSD   L  DAILY TOTAL CLEAR-SKY DIRECT RAD OF SLOPING
C                 EARTH SURFACE [MJ/M^2/DAY]
C       RDAY   I
C       RDIF   L  Daily DIFFUSION RATE [CM^2/HR]
C       RN        I/O DAILY NET CLEAR-SKY DIRECT RAD ON HOR EARTH
C                 SURFACE [MJ/M^2/DAY]
C       RP         L  RADIUS OF CYLINDRICAL MACROPORES IN HORIZON [CM]
C       RPH        L  DAILY MAX (POTENTIAL) S-W RAD OF A HOR SURFACE
C                 [MJ/M^2/DAY]
C       S      I  SLOPE OF FIELD [RAD]
C       T0         L  STP, TEMPERATURE (K)
C       TSHIFT     L  TIME SHIFT FOR SLOPING SURFACE
C       TSR        L  SUNRISE ON ACTUAL SLOPE [R]
C       TSR2   L  SUNRISE ON SECOND SUNRISE [R]
C       TSRE   L  SUNRISE ON EQUIVALENT SLOPE [R]
C       TSRH   L  SUNRISE ON HORIZONTAL [R]
C       TSS        L  SUNSET ON ACTUAL SLOPE [R]
C       TSS2   L  SUNSET ON SECOND SUNSET [R]
C       TSSE   L  SUNSET ON EQUIVALENT SLOPE [R]
C       TSSH   L  SUNSET ON HORIZONTAL [R].
C       W      P  HOURLY SOLAR CONSTANT = 5.016 [MJ/M^2/HR]
C       WE         L  HOURLY SOLAR CONST ADJUSTED FOR ORBIT
C                 ECCENTRICITY  [MJ/M^2/HR]
C       X      L  I: PREVIOUS SOLUTION VECTOR, O: REPLACED WITH XNEW
C       XJ         L
C
C       EXTERNAL REFERENCES:
C                 CSRAD
C                 GAUSS
C
C       CALLED FROM:  POTEVP
C
C       PROGRAMMER:   CHARLES HEBSON & KEN ROJAS
C
C       VERSION:      2.0
C
C======================================================================
C
C
C     ..DECLARE VARIABLES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER J,NGP
      DIMENSION PP(2),rcs1hr(24),rcshr(24),rcs2hr(24),rchhr(24),rphr(24)
     +        ,rdifhr(24)
C
C     ..SET VALUES OF PI & SOLAR CONSTANT W [MJ/M^2/DAY].
C
      PARAMETER(W=4.9212D0,PI=3.141592654D0,NGP=10,PPCNST=12.0D0/PI,B=
     +    3.5D0)
      EXTERNAL CSRAD
C
C     .. STATEMENT FUNCTION FOR DAILY RAD @ TOP OF ATM
      RDAY(CC1,CC2,SHIFT,SS,SR)=PPCNST*(CC1*(SS-SR)+CC2*(
     +    DSIN(SS+SHIFT)-DSIN(SR+SHIFT)))
C
C=======================================================================
C
C     .. ADJUST SOLAR CONSTANT FOR ORBIT ECCENTRICITY.
C
      XJ=DBLE(J)
      E=1.D0-0.0167D0*DCOS(0.0172D0*(XJ-3.D0))
      WE=W/(E*E)
C
C     .. CALCULATE DECLINATION
C
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     gnf Liwang Ma, RZ-SHAW, why did Joe change to a different D formular??
c     jak    
      D=DASIN(0.39785D0*DSIN(4.868961D0+0.017203D0*XJ+0.033446D0*
     +    DSIN(6.224111D0+0.017202D0*XJ)))
C      D=0.4102D0*DSIN(2.D0*3.14159D0*(XJ-80.D0)/365.D0)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee

C
C======================================================================
C     CALCULATE POTENTIAL RAD ON HORIZONTAL SURFACE, RP.
C======================================================================
C
C     ..CALCULATE SUNRISE(SR), SUNSET(SS), IN RADIANS
C     NOTE: NOON = 0[R]
C
C     TSSH = DACOS(-DTAN(P) * DTAN(D))
      TSSH1=-DTAN(P)
      TSSH2=DTAN(D)
      TSSH=DACOS(TSSH1*TSSH2)
      TSRH=-TSSH
C
C     ..CALCULATE COMBINATION TERMS.
C
      C1H=DSIN(D)*DSIN(P)
      C2H=DCOS(D)*DCOS(P)
C
C     ..CALCULATE RP [MJ/M^2/DAY].
C
      RP=WE*RDAY(C1H,C2H,0.D0,TSSH,TSRH)
C
C======================================================================
C     CALCULATE POT RAD ON SLOPING SURFACE.
C======================================================================
C     ..STORE MAX RAD ON HORIZONTAL.
      RPH=RP
C
C     ..CALCULATE EFFECTS OF SLOPE ASPECT & INCLINATION.
      IF(S.NE.0.D0) THEN
C       ..CALCULATE EQUIVALENT LATITUDE
C
        PE=DASIN(DCOS(S)*DSIN(P)+DSIN(S)*DCOS(P)*DCOS(A))
C
C       ..CALCULATE COMBINATION COEFFICIENTS FOR SLOPE RAD EQN
C
        C1=DSIN(D)*DSIN(PE)
        C2=DCOS(D)*DCOS(PE)
C
C       ..CALCULATE TIME SHIFT.
C
        X=DCOS(S)*DCOS(P)-DSIN(S)*DSIN(P)*DCOS(A)
        IF(X.GT.0.D0) THEN
          TSHIFT=DATAN(DSIN(S)*DSIN(A)/X)
        ELSE
          TSHIFT=0.0D0
        ENDIF
C
C       .. CALCULATE SR, SS TIMES (RAD) ON EQUIV SLOPE.
C
        Ta0=(-DTAN(D)*DTAN(PE))
        if ((Ta0.ge.1.0d0).or.(Ta0.le.-1.0d0)) then   !when Ta0 is >1 or <-1, there is no sunrise and sunset. direct sunlight is zero. Per Gerald Flerchinger
        T0=0.0d0
        else
        T0=DACOS(-DTAN(D)*DTAN(PE))
        endif
        TSRE=-T0-TSHIFT
        TSSE=T0-TSHIFT
C
C       .. DETERMINE ACTUAL SUNRISE, SUNSET ON SLOPE
C
        TSS=MIN(TSSH,TSSE)
        TSR=MAX(TSRH,TSRE)
C
C       .. CALC DAILY RAD ON SLOPE, ACCOUNTING FOR POSSIBITLTY OF
C       .. MULTIPLE RAD PDS OVER A SINGLE DAY
C
        IF(TSS.LE.TSR) THEN
          TSR=0.D0
          TSS=0.D0
          R1=0.D0
        ELSE
          R1=RDAY(C1,C2,TSHIFT,TSS,TSR)
        ENDIF
C
        TSRE=TSRE+2.D0*PI
        IF(TSRE.LT.TSSH) THEN
          TSR2=TSRE
          TSS2=TSSH
          R2=RDAY(C1,C2,TSHIFT,TSS2,TSR2)
        ELSE
          TSSE=TSSE-2.D0*PI
          IF(TSSE.GT.TSRH) THEN
            TSR2=TSRH
            TSS2=TSSE
            R2=RDAY(C1,C2,TSHIFT,TSS2,TSR2)
          ELSE
            TSR2=0.D0
            TSS2=0.D0
            R2=0.D0
          ENDIF
        ENDIF
        PP(1)=12.0D0+TSR/0.2618D0
        PP(2)=12.0D0+TSS/0.2618D0
C
C       ..SUM RADIATIONS FROM MULTIPLE PERIODS
C
        RP=WE*(R1+R2)
C
C       ..CALCULATE DAILY CLEAR-SKY DIR RAD OF SLOPING EARTH SURF.
C
        RCS2=0.D0
C
C       INTEGRATE INSTAN. TOTAL CLEAR-SKY DIR RAD (SLOPE SURFACE)
C       OVER DAYLIGHT HOURS.
C
c        IF(R1.GT.0.D0) RCS1=GAUSS(TSR,TSS,NGP,CSRAD,C1,C2,B)
        IF(R1.GT.0.D0) then
              RCS1=GAUSS(TSR,TSS,NGP,CSRAD,C1,C2,B)
C             sumrcs=0.0d0
              do i=1,24
              tsr1=(dble(i-1)-12.0d0)*0.2618d0
              tss1=(dble(i)-12.0d0)*0.2618d0
              if (tsr1.lt.tsr.and.tss1.gt.tsr.and.
     +            (tss1-tsr).gt.0.05d0) then 
               RCS1Hr(i)=GAUSS(TSR,TSs1,2,CSRAD,C1,C2,B)
              else if (tsr1.ge.tsr.and.tss1.le.tss) then
               RCS1Hr(i)=GAUSS(TSR1,TSS1,4,CSRAD,C1,C2,B)
              else if (tsr1.lt.tss.and.tss1.gt.tss.and.
     +           (tss-tsr1).gt.0.05d0) then
               RCS1Hr(i)=GAUSS(TSR1,TSS,2,CSRAD,C1,C2,B)
             else
               rcs1hr(i)=0.0d0
             endif
C             sumrcs=sumrcs+rcs1hr(i)
             enddo
        endif
C
        IF(R2.GT.0.D0) then
	 	 RCS2=GAUSS(TSR2,TSS2,NGP,CSRAD,C1,C2,B)
c             sumrcs=0.0d0
              do i=1,24
              tsr1=(dble(i-1)-12.0d0)*0.2618d0
              tss1=(dble(i)-12.0d0)*0.2618d0
              if (tsr1.lt.tsr2.and.tss1.gt.tsr2.and. 
     +            (tss1-tsr2).gt.0.05d0) then 
               RCS2Hr(i)=GAUSS(TSR2,TSs1,2,CSRAD,C1,C2,B)
              else if (tsr1.ge.tsr2.and.tss1.le.tss2) then
               RCS2Hr(i)=GAUSS(TSR1,TSS1,4,CSRAD,C1,C2,B)
              else if (tsr1.lt.tss2.and.tss1.gt.tss2.and.
     +           (tss2-tsr1).gt.0.05d0) then
               RCS2Hr(i)=GAUSS(TSR1,TSS2,2,CSRAD,C1,C2,B)
             else
               rcs2hr(i)=0.0d0
             endif
c             sumrcs=sumrcs+rcs1hr(i)
             enddo
        endif
C
        RCSD=WE*(RCS1+RCS2)*PPCNST
C        SUMRCS=0.0D0
        do i=1,24
	    rcshr(i)=WE*(RCS1hr(i)+RCS2hr(i))*PPCNST
C        SUMRCS=SUMRCS+RCSHR(I)
        enddo
      ENDIF
C
      if (s.eq.0.0d0) then
        PP(1)=12.0D0+TSRH/0.2618D0
        PP(2)=12.0D0+TSSH/0.2618D0
      endif
C======================================================================
C     CALCULATE DAILY CLEAR-SKY DIRECT RADIATION ON HOR EARTH
C     SURFACE, RCHD
C======================================================================
C
C     ..INTEGRATE INSTAN. TOTAL CLEAR-SKY DIR RAD (HOR SURFACE)
C     OVER DAYLIGHT HOURS.
C
      RCHD=GAUSS(TSRH,TSSH,NGP,CSRAD,C1H,C2H,B)
C            SUMRCS=0.0D0
              do i=1,24
              tsr1=(dble(i-1)-12.0d0)*0.2618d0
              tss1=(dble(i)-12.0d0)*0.2618d0
              if (tsr1.lt.tsrh.and.tss1.gt.tsrh.and.
     +            (tss1-tsrh).gt.0.05d0) then 
               RChHr(i)=GAUSS(TSRh,TSs1,2,CSRAD,C1h,C2h,B)
              else if (tsr1.ge.tsrh.and.tss1.le.tssh) then
               RChHr(i)=GAUSS(TSR1,TSS1,4,CSRAD,C1h,C2h,B)
              else if (tsr1.lt.tssh.and.tss1.gt.tssh.and.
     +           (tssh-tsr1).gt.0.05d0) then
               RChHr(i)=GAUSS(TSR1,TSSh,2,CSRAD,C1h,C2h,B)
             else
               rcHhr(i)=0.0d0
             endif
C           SUMRCS=SUMRCS+RCHHR(I)
             enddo
C
      RCHD=WE*RCHD*PPCNST
C
        do i=1,24
	    rchhr(i)=WE*RChhr(i)*PPCNST
        enddo
C======================================================================
C     CALCULATE DAILY NET DIRECT CLEAR-SKY RADIATION ON HOR
C     EARTH SURFACE.
C======================================================================
C
C     ..INTEGRATE INSTAN NET CLEAR-SKY DIR RAD ON HOR EARTH SURF.
C
      RN=GAUSS(TSRH,TSSH,NGP,CSRAD,C1H,C2H,B)
      RN=WE*RN*PPCNST
C
C     ..CALC DAILY DIFFUSE S-W RADIATION.
C          SUMRCS=0.0D0
              do i=1,24
              tsr1=(dble(i-1)-12.0d0)*0.2618d0
              tss1=(dble(i)-12.0d0)*0.2618d0
              if (tsr1.lt.tsrh.and.tss1.gt.tsrh.and.
     +            (tss1-tsrh).gt.0.05d0) then 
               RPhr(i)=WE*RDAY(C1H,C2H,0.D0,TSS1,TSRH)
              else if (tsr1.ge.tsrh.and.tss1.le.tssh) then
               RPhr(i)=WE*RDAY(C1H,C2H,0.D0,TSS1,TSR1)
              else if (tsr1.lt.tssh.and.tss1.gt.tssh.and.
     +           (tssh-tsr1).gt.0.05d0) then
               RPhr(i)=WE*RDAY(C1H,C2H,0.D0,TSSh,TSR1)
             else
               rphr(i)=0.0d0
             endif
             RDIFhr(i)=max(0.0d0,(0.91D0*RPHr(i)-RCHhr(i))*0.5D0)
C          SUMRCS=SUMRCS+RDIFHR(I)
             enddo
C
      RDIF=(0.91D0*RPH-RCHD)*0.5D0
      IF(RDIF.LT.0.D0) RDIF=0.D0
C
C     ..CALC DAILY CLEAR-SKY TOTAL S-W RAD ON HOR EARTH SURFACE
C
      RCH=RDIF+RCHD
C
C     ..CALC DAILY CLEAR-SKY TOTAL S-W RAD ON SLOPING EARTH SURFACE
C
      IF(S.NE.0) THEN
        RCS=RDIF+RCSD
      ELSE
        RCS=RCH
      ENDIF
C     WRITE(22,*)J,RCS
      sumrcs=0.0d0
      do i=1,24
      IF(S.NE.0) THEN
        RCShr(i)=RDIFhr(i)+RCShr(i)
      ELSE
        RCShr(i)=RDIFhr(i)+RCHhr(i)
      ENDIF
       sumrcs=sumrcs+rcshr(i)
	enddo
C
       if ((sumrcs-rcs)/rcs.gt.0.05) then
	 write (*,*) (sumrcs-rcs)/rcs
	 stop 'RADIATION DIFF BETWEEN DAILY AND HOURLY STEP IS >5%'
	 endif
      RETURN
      END
C
c      SUBROUTINE NETRAD(AC,AR,AS,CCL,CO,CR,CS,RN,RNR,RNS,RTS,RNL,TTR,TTS)
C     +  LAI,TLAI)
C
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak/ Liwang Ma, RZ-Penflux, June 2008
      SUBROUTINE NETRAD(AC,AR,AS,CCL,CO,CR,CS,RN,RNR,RNS,RTS,RNL,TTR,
     * TTS,rsns,rsnr,rsnc,ishaw)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C======================================================================
C
C       PURPOSE: CALCULATE THE NET RADIATIONS AT THE LAYERS
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       AC         I  ALBEDO OF PLANT CANOPY
C       AR         I  ALBEDO OF RESIDUE
C       AS         I  ALBEDO OF SOIL
C       CCL        I  (1-C0) FRACTION AREA COVERED BY CROP
C       CO         I  AREA NOT COVERED BY CROP
C       CR         I  (1-CS) AREA COVERED BY RESIDUE
C       CS         I  EXPOSED SOIL, NOT COVERED BY RESIDUE
C       RN         O  NET SOLAR RADIATION OVER THE FIELD
C       RNL        I  LONGWAVE RADIATION
C       RNR        O  NET SOLAR RADIATION AT RESIDUE SURFACE
C       RNS        O  NET SOLAR RADIATION AT SOIL SURFACE
C       RTS        I  DAILY ACTUAL TOTAL S-W RAD AT CANOPY
C                 [MJ/M^2/DAY]
C       TTR        O  RESIDUE PARTITION OF SOLAR ENERGY
C       TTS        O  SOIL PARTITION OF SOLAR ENERGY
C       RSNS          NET SHORTWAVE IRRADIANCE FOR SURFACE SOIL LAYER W m-2
C       RSNR          NET SHORTWAVE IRRADIANCE FOR FLAT RESIDUE LAYER W m-2
C       RSNC          NET SHORTWAVE IRRADIANCE FOR CROP CANOPY LAYER W m-2
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                 NONE
C
C       CALLED FROM:  POTEVP
C
C       PROGRAMMER:   KEN ROJAS
C
C       VERSION:      1.0
C
C======================================================================
C
C ... DECLARE VARIABLES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C      DOUBLE PRECISION LAI
      common /shawp/ pplastic,emitc,emitr(10),emitsp,emits,emitf
     &      ,palbedo,ptransm
C
      TC=1.0D0-AC
      TP=1.0D0-PALBEDO-PTRANSM
      TR=1.0D0-AR
      TS=1.0D0-AS

csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak/ Liwang Ma, RZ-Penflux, June 2008.
C     SET UP TRANSMISSION FACTORS FOR CANOPY AND RESIDUE LAYERS        
      TMC = (1.D0-CCL)*RTS
      TMP = (1.0D0-PPLASTIC)*TMC+PTRANSM*PPLASTIC*TMC
      TMR = (1.D0-CR)*TMP
C
C     COMPUTE NET ABSORBED RADIATION AT CANOPY, RESIDUE AND SOIL SURFACES
C      RSNC = ((1.0D0 - AC)*RTS + AR*TMC + TR*AS*TMR)*CCL
C      RSNR = ((1.0D0 - AR)*TMC + AS*TMR)*CR
C      RSNS = (1.0D0 - AS)*TMR
      RSNC = ((1.0D0 - AC)*RTS + PALBEDO*TMC + 
     +        AR*TMP*(1.0D0-PPLASTIC+PPLASTIC*PTRANSM) + 
     +        TR*AS*TMR*(1.0D0-PPLASTIC+PPLASTIC*PTRANSM))*CCL
      RSNP = ((1.0D0-PALBEDO-PTRANSM)*TMC+AR*TMP*(1.0d0-PTRANSM)+
     +        TR*AS*TMR*(1.0D0-PTRANSM))*PPLASTIC
      RSNR = ((1.0D0 - AR)*TMP + AS*TMR)*CR
      RSNS = (1.0D0 - AS)*TMR
C
C     .. NET RADIATION AT CANOPY, SOIL AND RESIDUE SURFACE (MJ/M^2/DAY)
C
C      TCAN=CCL*TC+CO*CR*TR+CO*CS*TS
      TCAN=CCL*TC+CO*PPLASTIC*TP+
     +     CO*CR*TR*(1.0D0-PPLASTIC+PTRANSM*PPLASTIC)+
     +     CO*CS*TS*(1.0D0-PPLASTIC+PTRANSM*PPLASTIC)
      RN=TCAN*RTS+RNL   !*(1.0d0-PPLASTIC)
      IF(RN.LT.0.0D0) THEN
        RN=TCAN*RTS/3.0D0
      ENDIF
      AAC=(0.5D0+0.44D0*CCL)
      TAC=1.0D0-AAC
C      TTR=CR*TR*(CO+TAC*CCL*TC)
C      TTS=CS*TS*(CO+TAC*CCL*TC)
      TTR=CR*TR*(1.0D0-PPLASTIC+PTRANSM*PPLASTIC)*(CO+TAC*CCL*TC)
      TTS=CS*TS*(1.0D0-PPLASTIC+PTRANSM*PPLASTIC)*(CO+TAC*CCL*TC)
C
      RNR=TTR*RTS+CO*CR*RNL*(1.0d0-PPLASTIC)
      RNS=TTS*RTS+CO*CS*RNL*(1.0d0-PPLASTIC)
C this was commented out by JAK Maybe due to RNS and RNR can be zero
c   at night  Liwang Ma, 3-22-2009
c      if (ishaw.ne.1) then
      IF(RNS.LT.0.0D0.OR.RNR.LT.0.0D0) THEN
        RNS=RN*CO*CS  !*(1.0D0-PPLASTIC+PTRANSM*PPLASTIC)
        RNR=RN*CO*CR  !*(1.0D0-PPLASTIC+PTRANSM*PPLASTIC)
      ENDIF
c      endif
C this was commented out by JAK
C     RNS = RN * DEXP(-0.594D0 * (LAI+TLAI)*0.5D0)
C
      RETURN
      END
C
      SUBROUTINE POTEVP(ASPECT,CS,ELEV,EPAN,FT,FTR,JDAY,PET,PER,PES,RR,
     +    RTS,S,THETA,TMIN,TMAX,U,WRES,LAI,XLAT,PP,ICRUST,RH,WC13,WC15,
     +    IPL,HEIGHT,AS,ESN,RCS,PKTEMP,IRTYPE,TLAI,IPR,SDEAD_HEIGHT,
     +    SDEAD,IHOURLY,tdew,itime,iwzone,hrts,co2r,trat,RTH,HRTH)
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
C       A0         I  ALBEDO DRY SOIL
C       AC         I  ALBEDO OF PLANT CANOPY
C       AR         L  RESIDUE ALBEDO AS FUNCTION OF AGE
C       ARI        I  ALBEDO OF FRESH RESIDUE
C       AS         L  ALBEDO OF SOIL
C       ASPECT     I  SLOPE ASPECT RELATIVE TO TRUE NORTH (R)
C       AW         I  ALBEDO WET SOIL
C       CCL        L  (1-C0) FRACTION AREA COVERED BY CROP
C       CCC        L  SHUTTLEWORTH EVAP CONSTANTS CANOPY
C       CCS        L  SHUTTLEWORTH EVAP CONSTANTS SUBSTRATE/RESIDUE
C       CO         L  AREA NOT COVERED BY CROP [0..1]
C       CP         P  SPECIFIC HEAT OF MOIST AIR AT CONST PRES (MJ/KG C)
C       CR         L  (1-CS) AREA COVERED BY RESIDUE
C       CRA        L  COEFF FOR SHUTTLEWORTH EVAP CONSTANTS CANOPY
C       CRC        L  COEFF FOR SHUTTLEWORTH EVAP CONSTANTS RESIDUE
C       CRS        L  COEFF FOR SHUTTLEWORTH EVAP CONSTANTS SOIL
C       CS         I  EXPOSED SOIL, NOT COVERED BY RESIDUE
C       DEO        L  SLOPE OF THE SATURATION VAPOR CURVE (KPA/C)
C       DO         L  VAPOR PRESSURE DEFICIT
C       EA         L  AVERAGE SATURATION VAPOR PRESSURE (KPA)
C       ED         L  SATURATED VAPOR PRESSURE AT DEW TEMP (KPA)
C       ELEV   I  ELEVATION AT SITE (M)
C       ESN       I/O POTENTIAL ATMOSPHERIC EVAPORATIVE POTENTIAL (CM)
C       FSUN   I  SUNSHINE FRACTION
C       G      L  SOIL HEAT FLUX [MJ/M^2]
C       ICRUST     I  FLAG INDICATING IF A CRUST EXISTS (=1;TRUE)
C       IPL        I  PLANT CURRENTLY MODELING [1..MXSPEC]
C       JDAY   I  JULIAN DAY    [1..366]
C       K      P  VON KARMAN CONSTANT ( =.41)
C       LAI        I  LEAF AREA INDEX
C       PEN123     P  ROUGHNESS LENGTH FOR MOMENTUM TRANFER CONSTANT
C       PER       I/O RESIDUE EVAPORATION (CM)
C       PES       I/O SUBSTRATE EVAPORATION (CM)
C       PET       I/O TRANSPIRATION (CM)
C       PKTEMP     I  SNOWPACK TEMPERATURE
C       PMC        L  P-M CLOSED CANOPY TRANSPIRATION
C       PMS        L  P-M BARE SUBSTRATE EVAPORATION
C       PP         I  PHOTOPERIOD
C       PPOP   I  NUMBER OF PLANT PER AREA  [#/HA]
C       PSY        L  PSYCHROMETRIC CONSTANT (KPA/C)
C       RAA        L  AERODYNAMIC RESISTANCE BETWEEN SUBSTATE AND CANOPY
C       RAC        L    @ CANOPY (S/M)
C       RAS        L    @ SUBSTRATE (S/M)
C       RCH        L  DAILY TOTAL CLEAR-SKY RAD (DIR & DIFFUSE) ON
C                 A HORIZONTAL SURFACE [MJ/M^2/DAY].
C       RCS        L  DAILY TOTAL CLEAR-SKY RAD (DIR & DIFFUSE) ON
C                 SLOPING SURFACE [MJ/M^2/DAY].
C       RDIA   I  RESIDUE DIAMETER (CM)
C       RESAGE     I  AGE OF THE RESIDUE (DAYS)
C       RH         I  RELATIVE HUMIDITY
C       RHOA   L  DENSITY OF AIR (KPA)
C       RHOR   I  DENSITY OF RESIDUE (G/CM^3)
C       RM         I  MASS OF RESIDUE (T/HA)
C       RN         L  NET SOLAR RADIATION OVER THE FIELD
C       RNL        L  LONGWAVE RADIATION
C       RNR        L  NET SOLAR RADIATION AT RESIDUE SURFACE
C       RNS        L  NET SOLAR RADIATION AT SOIL SURFACE
C       RR         I  RANDOM ROUGHNESS (CM)
C       RSC        L    @ CANOPY (S/M)
C       RSS        L    @ SUBSTRATE (S/M)
C       RTS       I/O DAILY ACTUAL TOTAL S-W RAD AT CANOPY
C                 [MJ/M^2/DAY]
C       S      I  SLOPE OF FIELD (R)
C       SIGMA  P  STEFAN-BOLTZMAN CONSTANT (MJ/M**2/DAY/K**4)
C       SWET   L  SHUTTLEWORTH VAPOR FLUX DENSITY (MJ/(M^2.T))
C       THETA  I  SOIL MOISTURE CONTENT
C       TM         L  MEAN DAILY AIR TEMPERATURE (C)
C       TMAX   I  MAXIMUM AIR TEMPERATURE (C)
C       TMIN   I  MINIMUM AIR TEMPERATURE (C)
C       TTR        L  RESIDUE PARTITION OF SOLAR ENERGY
C       TTS        L  SOIL PARTITION OF SOLAR ENERGY
C       TWOTRD     P  2/3 CONSTANT
C       U      I  WIND RUN FOR THE DAY (KM/DAY)
C       UNEW   L  WIND ADJUSTED FOR HEIGHT
C       WC13   I  1/3 BAR WATER CONTENT
C       WC15   I  15 BAR WATER CONTENT
C       WNDADJ     L  WIND MEASUREMENT HEIGHT ADJUSTMENT
C       WRES   I  MOISTURE CONTENT OF RESIDUE
C       XLAT   I  LATITUDE ON EARTH (R)
C       XW         I  WIND MEASUREMENT HEIGHT (M)
C
C
C
C       COMMENTS: HOURLY ET CALCULATION WAS FROM RICHARD ALLEN, FAO IRRIGAITON AND DRAINAGE PAPER NO 56
C                 CROP EVAPOTRANSPIRATION (GUIDELINES FOR COMPUTING CROP WATER REQUIREMENTS) 
C
C       EXTERNAL REFERENCES:
C                 ALBSWS
C                 CTRANS
C                 ECONST
C                 MAXSW
C                 NETRAD
C                 RESIST
C                 SWSUN
C
C       CALLED FROM:  PHYSCL
C
C       PROGRAMMER:   KEN ROJAS
C
C       VERSION:      3.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION LAI,K,TLAI,al(3),bl(3)
      PARAMETER(CP=1.013D-3,TWOTRD=2.0D0/3.0D0,PEN123=
     +    0.123D0,MXSPEC=10,SLODGE=1.0D0-1.0D0/300.0D0)
C
C     .. SPACE PERTAINING TO POTENTIAL EVAPORATION
      COMMON /IPOTEV/ A0,AW,AC,ARI,XW,FSUN,COEPAN,rss,RST(MXSPEC)
     +    ,RXANGLE(MXSPEC),RTCCRIT(MXSPEC),RPLEAF0(MXSPEC),
     +      RRSTEXP(MXSPEC),RRLEAF0(MXSPEC)
C
      COMMON /RESID/ RM,RESAGE,CRES,HR,RCN,NADLY,NSDLY,NAREC,NSREC,WORM
     +               ,SAI,HSTUBL,WSTUBL
C
      DIMENSION PP(2),IRTYPE(MXSPEC),rcshr(24),hrts(24),RCHHR(24)
     +         ,hrth(24)
      SAVE WNDADJ
      DATA WNDADJ /1.0D0/,al/1.2d0,1.1d0,1.0d0/,bl/-0.2d0,-0.1d0,0.0d0/
C
C ... STATEMENT FUNCTION FOR SATURATED VAPOR PRESSURE CURVE
      EO(T)=EXP((16.78D0*T-116.9D0)/(T+237.3D0))
C     ..SET UP CONSTANTS ACCORDING TO TIME STEP
      IF (IHOURLY .EQ. 1) THEN
        K = 3600.0D0
        SIGMA = 4.903D-9/24.0D0
C     ..ADJUST RESIDUE AGE (AGE IS RESET IN HARVST ROUTINE)
c        RESAGE=RESAGE+1.0d0/24.d0
       ELSE
        K = 86400.0D0
        SIGMA = 4.903D-9
C     ..ADJUST RESIDUE AGE (AGE IS RESET IN HARVST ROUTINE)
c        RESAGE=RESAGE+1
      ENDIF
C
      IF(RM+SDEAD.GT.0.0D0) THEN
        RSDEAD=SDEAD/(RM+SDEAD)
      ELSE
	  RSDEAD=0.0D0
      ENDIF
      REPHEIGHT=MAX(RSDEAD*SDEAD_HEIGHT,HEIGHT)/100.0D0
C     ..GET WIND HEIGHT AND SPEED ADJUSTMENT FACTORS (HAMID, 9/10/93)
      IF(XW.GE.10.0D0) THEN
C
C       ..DATA IS ASSUMED FROM AN AIRPORT SETTING WITH 40 CM OF COVER.
        IF(REPHEIGHT.GT.0.0D0) THEN
          XWNEW=1.330D0+TWOTRD*REPHEIGHT
          W1=LOG((XWNEW-TWOTRD*REPHEIGHT)/(PEN123*REPHEIGHT))
          W2=LOG((XW-0.27D0)/0.05D0)
        ELSEIF(REPHEIGHT.LE.0.0D0) THEN
          XWNEW=1.60D0
          W1=LOG(XWNEW/0.01D0)
          W2=LOG((XW-0.27D0)/0.05D0)
        ENDIF
      ELSE
C
C       ..DATA IS FROM A MICROTOWER WITH 10 CM OF COVER.
        IF(REPHEIGHT.GT.0.0D0) THEN
          XWNEW=1.930D0+TWOTRD*REPHEIGHT
          W1=LOG((XWNEW-TWOTRD*REPHEIGHT)/(PEN123*REPHEIGHT))
          W2=LOG((XW-0.07D0)/0.0123D0)
        ELSEIF(REPHEIGHT.LE.0.0D0) THEN
          XWNEW=2.0D0
          W1=LOG(XWNEW/0.01D0)
          W2=LOG((XW-0.07D0)/0.0123D0)
        ENDIF
      ENDIF
      WNDADJ=W1/W2
C
C     ..ADJUST WIND (KM/DAY)
      UNEW=U*WNDADJ
C
C
C
C     ..ENERGY CONSTANTS
      CALL ECONST(DEO,EA,ED,ELEV,PA,PSY,RH,RHOA,TMIN,TMAX,XLH,TA)
C
C     ..MAXIMUM SHORT WAVE RADIATION
C
      ORTS=RTS
      ORTH=RTH
C
      CALL MAXSW(ASPECT,JDAY,XLAT,RCHD,RN,RCH,S,PP,RCS,RCSHR,RCHHR)
c  use SHAW to partition measured solar radiation to direct and diffuse for each hour to take accout aspect and slope effects
C MOVED TO RZMAIN.FOR AFTER READING METEOLOGICAL DATA
C      RTS=0.0D0
C      NHRPDT=1
C      do i=1,24
C          SUNHOR = HRTS(i)
C      CALL SOLAR_SHAW(DIRECT,DIFFUS,SUNSLP,ALTITU,SUNHOR,
C     >                  I,XLAT,S,ASPECT,jday,NHRPDT)
C       HRTS(I)=(DIRECT+DIFFUS)
C       RTS=RTS+HRTS(I)*3.6d3/1.d6
C      enddo
c==== Use SHAW to partition 
      IF(RTS.EQ.0.0D0.AND.IHOURLY.NE.1) THEN
        RTS=SWSUN(FSUN,RCS)
        RTH=SWSUN(FSUN,RCH)
      ELSE
        RTS=ORTS
        RTH=ORTH
      ENDIF
C
C     ..GET SOIL ALBEDO WEIGHTED BY MOISTURE CONTENT
      AS=ALBSWS(A0,AW,WC13,WC15,THETA,ICRUST,RR)
C
C     .. CALCULATE RESIDUE ALBEDO AS FUNCTION OF AGE
      AR=A0*(1.06D0+((ARI/A0)-1.06D0)*DEXP(-0.0255D0*RESAGE))
      AR=MAX(AR,0.0D0)
      IF(WRES.GT.0.D0) AR=.75D0*AR
C
C     ..RESISTANCE TERMS FOR SHUTTLEWORTH
C     ..RESISTANCE TERMS FOR SHUTTLEWORTH  RZ-Penflux
       hstubl = min(hstubl,sdead_height)
       wstubl = min(wstubl,sdead)
      CALL RESISThr(XWNEW,UNEW,LAI,RM,RAA,RAC,RAS,RSC,RSR,RSS,CS,IPL,
     +    MXSPEC,ta,HEIGHT,HR,IRTYPE,CRES,RST,TLAI,IPR,sai,hstubl,
     +    wstubl,stemai,zstubl,stublw,rbr,rcr,rhorb,hrm,theta,wcsat,
     +    wc13,starsoil,ihourly,ipenflux,ishaw,co2r,ea,ed,trat,
     +    rts*1.D6/3.6D3)
c
      if (rts*1.D6/3.6D3.lt.10.d0) rsc=rsc*10.0d0    !nighttime canopy resistance is 10 times higher assumed.
c      CALL RESIST(XWNEW,UNEW,LAI,RM,RAA,RAC,RAS,RSC,RSR,RSS,CS,IPL,
c     +    MXSPEC,TA,HEIGHT,HR,IRTYPE,CRES,RST,TLAI,IPR,ihourly,co2r,
c     +    ea,ed,trat)
      CR=1.0D0-CS
      FTR=CS
C
C     ..LIGHT TRANMISSION FRACTION THRU THE CANOPY
      IF(TLAI.NE.0.0D0) THEN
        CO=DEXP(-0.594D0*TLAI)
      ELSE
        CO=1.0D0
      ENDIF
      CCL=1.0D0-CO
      FT=CO
C
C     ..ESTIMATE NET L-W RADIATION RNL
c      AL=1.1D0
c      BL=-0.1D0
      TL=((TMAX+273.15D0)**4.0D0+(TMIN+273.15D0)**4.0D0)*0.5D0
      RB0=(0.39D0-0.158D0*SQRT(ED))*SIGMA*TL
      IF(RCS.LT.RTS) RCS=RTS
      IF(RCH.LT.RTH) RCH=RTH
      IF (IHOURLY.EQ.1.AND.RCHHR(ITIME).NE.0.0D0) THEN
          RSRATIO=HRTH(itime)*3.6D3/1.D6/RCHHR(ITIME)   !converts HRTH to MJ/M2 before deivided by RCHHR.
	ELSE IF (IHOURLY.EQ.0.AND.RCH.NE.0.0D0) THEN
          RSRATIO=RTH/RCH
      ELSE
      RSRATIO=HRTH(INT(PP(2)-2.0D0))*3.6D3/1.D6/RCHHR(INT(PP(2)-2.0D0))
c      ELSE IF (IWZONE.EQ.1) THEN
c	    RSRATIO=0.75D0
c	ELSE
c	    RSRATIO=0.50D0
	ENDIF
      RSRATIO=Max(RSRATIO,0.0D0)
      RSRATIO=Min(RSRATIO,1.0D0)
      RNL=-(AL(iwzone)*RSRATIO+BL(iwzone))*RB0
C
C     ..CALCULATE EFFECTIVE NET RADIATIONS AT SOIL, RESIDUE, & CANOPY
      CALL NETRAD(AC,AR,AS,CCL,CO,CR,CS,RN,RNR,RNS,RTS,RNL,TTR,TTS
     &           ,rsns,rsnr,rsnc,ishaw)
C     ,LAI,TLAI)
C
C     ..IF USE ENERGY APPROACH OR PAN EVAPORATION (cm/day)...
      IF(EPAN.GT.0.0D0) THEN
        PANET=COEPAN*EPAN
        IF(CCL.GT.0.0D0) THEN
          PET=MAX(CCL,1.0D-1)*PANET
        ELSE
          PET=0.0D0
        ENDIF
        IF(RM.GT.0.0D0) THEN
C         ... SUBSTRATE EVAPORATION IS PARTITIONED ACCORDING TO AREA
C         COVERED. HOWEVER, RATE OF RESIDUE EVAP IS ASSUMED
C         HALF THE BARE SOIL...   BY: HAMID J. FARAHANI
C
          PER=(PANET-PET)/2.0D0*CR
          PES=(PANET-PET)/2.0D0-PER
          PES=MAX(PES,0.0D0)
        ELSE
          PER=0.0D0
          PES=PANET-PET
        ENDIF
C
      ELSE
C
        DEO=4098.0D0*EO(TA)/(TA+237.3D0)**2.0D0
        RNSUB=RNS+RNR
C     ..SOIL HEAT FLUX [MJ/M^2/DAY]
      if (ihourly.eq.0) then
      G=0.0D0
      else if (itime.ge.pp(1).and.itime.le.pp(2)) then
	G=0.1*Rn
	else
	G=0.5*Rn
      endif
        C1=DEO*(RN-G)
        C2=K*RHOA*CP
C
        IF(LAI.GT.0.0D0.AND.HEIGHT.GT.0.0D0) THEN
          C3=C1+(C2*(EA-ED)-DEO*RAC*(RNSUB-G))/(RAA+RAC)
          C4=DEO+PSY*(1.0D0+RSC/(RAA+RAC))
          PMC=MAX(C3/C4,0.0D0)
        ELSE
          PMC=0.D0
        ENDIF
C
        IF(CS.GT.0.0D0) THEN
          C5=C1+(C2*(EA-ED)-DEO*RAS*(RN-RNSUB))/(RAA+RAS)
          C6=DEO+PSY*(1.0D0+RSS/(RAA+RAS))
          PMS=MAX(C5/C6,0.0D0)
        ELSE
          PMS=0.0D0
        ENDIF
C       PRINT *,'RN-RNS =',RN-RNS,'EA-ED =',EA-ED
C
        IF(RM.GT.0.0D0) THEN
          C7=C1+(C2*(EA-ED)-DEO*RAS*(RN-RNSUB))/(RAA+RAS)
          C8=DEO+PSY*(1.0D0+(RSS+RSR)/(RAA+RAS))
          PMR=MAX(C7/C8,0.0D0)
        ELSE
          PMR=0.0D0
        ENDIF
C
C       ..GET COEFFICIENTS FOR SHUTTLEWORTH EVAP
        IF(RM.GT.0.0D0) THEN
          CRA=(DEO+PSY)*RAA
          CRS=(DEO+PSY)*RAS+PSY*RSS
          CRC=(DEO+PSY)*RAC+PSY*RSC
          CRR=(DEO+PSY)*RAS+PSY*(RSS+RSR)
          DENOM=CRC*CRS*CRR+CRA*CRS*CRR+CRA*CRC*CRR*CS+CRA*CRC*CRS*CR
          CCC=CRS*CRR*(CRC+CRA)/DENOM
          CCS=CRC*CRR*(CRS+CRA)*CS/DENOM
          CCR=CRC*CRS*(CRR+CRA)*CR/DENOM
        ELSE
          CRA=(DEO+PSY)*RAA
          CRS=(DEO+PSY)*RAS+PSY*RSS
          CRC=(DEO+PSY)*RAC+PSY*RSC
          IF(CRS.NE.0.0D0) THEN
            CCC=1.0D0/(1.0D0+CRC*CRA/(CRS*(CRC+CRA)))
          ELSE
            CCC=1.0D0
          ENDIF
          IF(CRC.NE.0.0D0) THEN
            CCS=1.0D0/(1.0D0+CRS*CRA/(CRC*(CRS+CRA)))
          ELSE
            CCS=1.0D0
          ENDIF
          CCR=0.0D0
        ENDIF
C
C       ..VAPOR FLUX DENSITY (MJ/(M^2.DAY))
C
        SWET=CCC*PMC+CCS*PMS+CCR*PMR
C
C       ..VAPOR PRESSURE DEFICIT AT CANOPY SOURCE HEIGHT
        C7=RAA/C2
        DOP=(EA-ED)+(C1-(DEO+PSY)*SWET)*C7
C
C       ....P O T E N T I A L    T R A N S P I R A T I O N....
C
        IF(LAI.GT.0.0D0.AND.HEIGHT.GT.0.0D0) THEN
          C8=DEO*(RN-RNSUB)+C2*DOP/RAC
          C9=DEO+PSY*(1.0D0+RSC/RAC)
          PET=C8/C9
        ELSE
          PET=0.0D0
        ENDIF
C
C       .. P O T E N T I A L  SOIL   E V A P O R A T I O N
        C10=DEO*(RNSUB-G)+C2*DOP/RAS
        C11=DEO+PSY*(1.0D0+RSS/RAS)
        PES=(C10/C11)*CS
C
C       .. P O T E N T I A L  RESIDUE   E V A P O R A T I O N
        C12=DEO*(RNSUB-G)+C2*DOP/RAS
        C13=DEO+PSY*(1.0D0+(RSS+RSR)/RAS)
        PER=(C12/C13)*CR
C
C       ..CONVERT UNITS FROM MJ/M^2/DAY ==> CM/DAY
C       ..DIVIDE BY HEAT OF VAPORIZATION (MJ/KG)
        PET=MAX(PET/(XLH*10.0D0),0.0D0)
        PER=MAX(PER/(XLH*10.0D0),0.0D0)
        PES=MAX(PES/(XLH*10.0D0),0.0D0)
C
C       ..DETERMINE SNOW PACK SUBLIMATION POTENTIAL
        ESN=SNOWQE(UNEW,TMAX,TMIN,PKTEMP,XLH,RH)
        ESN=MIN(ESN,PES)
        ESN=0.011D0
C     PRINT*,ESN,PES
C
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE RESIST(XWNEW,UNEW,LAI,RM,RAA,RAC,RAS,RSC,RSR,RSS,CS,
     +    IPL,MXSPEC,TA,HEIGHT,HR,IRTYPE,CRES,RST,TLAI,IPR,ihourly,co2r
     +    ,ea,ed,trat)
C
C======================================================================
C
C       PURPOSE: CALCULATE ALL THE RESISTANCE TERMS FOR SHUTTLEWORTH
C
C       REF: effect of CO2 and VPD on RSC is from Claudio Stockle, 1992, Ag Systems 38:225-238
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       CONVW  P  CONVERT WIND RUN (KM/DAY) ==> M/S
C       CRES   P  COEFF ACCOUNTS FOR RANDOMNESS OF RESIDUE.
C       CS        I/O EXPOSED SOIL, NOT COVERED BY RESIDUE
C       CST           BULK STOMATAL CONDUCTANCE (M/S)
C       D      L  ZERO PLANE DISPLACEMENT (M)
C       DP         L  PREFERED VALUE OF CROP DISPLACEMENT HEIGHT (M)
C       HEIGHT     O  CANOPY HEIGHT (CM)
C       HGHT      CANOPY HEIGHT (M)
C       HR         L  HEIGHT OF RESIDUE LAYER (cm)
C       HRM        L  HEIGHT OF RESIDUE LAYER (m)
C       K      P  VON KARMAN CONSTANT ( =.41)
C       LAI        I  CURRENT LEAF AREA INDEX
C       N      P  EDDY DIFFUSIVITY DECAY CONSTANT
C                 AERODYNAMIC RESISTANCE BETWEEN CANOPY AND
C                 MEASUREMENT HEIGHT
C       RAA       I/O   @ ABOVE CANOPY (S/M)
C       RAC       I/O   @ CANOPY (S/M)
C       RAS       I/O   @ SUBSTRATE (S/M)
C       RB         P  LEAF BOUNDARY LAYER RESISTANCE (S/M)
C       RDIA   I  MEAN RESIDUE DIAMETER (CM)
C       RHORS  I  SPECIFIC DENSITY OF RESIDUE (G/CM^3)
C       RHORB  L  BULK DENSITY OF RESIDUE (G/CM^3)
C       RM         I  MASS OF RESIDUE (T/HA)
C       RSS        O  SOIL SURFACE RESISTANCE (S/M)
C       RSR        O  RESIDUE SURFACE RESISTANCE (S/M)
C       RSC        O  CANOPY SURFACE (STOMATAL) RESISTANCE (S/M)
C       RST        P  MEAN STOMATAL RESISTANCE (S/M)
C       RTS2      SOLAR EADIATION (W/M^2)
C       UNEW   I  TOTAL WIND RUN (KM/DAY) AT MEASUREMENT HEIGHT
C       URES      WIND SPEED WITHIN RESIDUE (M/S)
C       US            WIND SPEED AT MEASUREMENT HEIGHT (M/S)
C       XW         I  WIND MEASUREMENT HEIGHT (M)
C       Z0         L  ROUGHNESS LENGTH FOR MOMENTUM TRANSFER (M)
C       ZP            PREFFERED VALUE OF CROP ROUGHNESS LENGTH (M)
C       Z0P        P  EFFECTIVE ROUGHNESS LENGTH FOR BARE SOIL (M)
C       Z0R        L  EFFECTIVE ROUGHNESS LENGTH FOR RESIDUE (M)
C
C       COMMENTS:  RHOR AND RDIA ARE IN CROP ORDER:
C               1) CORN
C               2) SOYBEAN
C               3) WHEAT
C
C       EXTERNAL REFERENCES:
C                 NONE
C
C       CALLED FROM:  POTEVP
C
C       PROGRAMMER:   KEN ROJAS
C
C       VERSION:      1.0
C
C======================================================================
C
C ... DECLARE VARIABLES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION K,LAI,N
      DIMENSION IRTYPE(MXSPEC),RST(MXSPEC)
      PARAMETER(K=4.1D-1,Z0P=1.0D-2,N=2.5D0,CD=0.07D0,MXRES=3,CONVW=
     +    1.0D3/(24.0D0*60.0D0*60.D0),RB=10.0D0)
C
C     .. INITIALIZE CROP DEPENDENT RESIDUE VALUES
      DOUBLE PRECISION RHORS(MXRES),RDIA(MXRES)
      SAVE RHORS,RDIA
      DATA RHORS/0.15,0.17,0.18/,
     +     RDIA /1.0,0.5,0.25/
C
C     ..CONVERT WIND RUN (KM/DAY) ==> M/S
      US=UNEW*CONVW
C     SET A CAP ON WIND SPEED.
C     IF(US.GT.3.50D0) THEN
C     US=3.50D0
C     ELSEIF (US.LT.0.50D0) THEN
C     US = 0.50D0
C     ENDIF
      DK=1.0D0/(K**2.0D0*US)
C
C     CONVERT PLANT HEIGHT FROM cm TO m.
      PLHT=MAX(HEIGHT/100.0D0,0.05D0)
      XLAI=MAX((LAI+TLAI)*0.5D0,0.05D0)
C
C     ===::> C A N O P Y
C
C     .. BULK CANOPY LAYER RESISTANCE OF THE CANOPY
C
      IF(LAI.GT.0.0D0.AND.PLHT.GT.0.0D0) THEN
C
C       ..GET CROP CHARACTERISTICS
        DP=6.3D-1*PLHT
        ZP=1.3D-1*PLHT
        X=CD*XLAI
        D=1.10D0*PLHT*LOG(1.0D0+X**0.250D0)
C
        IF(X.GT.0.0D0.AND.X.LE.0.2D0) THEN
          Z0=Z0P+0.30D0*PLHT*(X**0.50D0)
        ELSEIF(X.GT.0.20D0) THEN
          Z0=0.30D0*PLHT*(1.0D0-D/PLHT)
        ENDIF
C
        RAC=RB/(2.0D0*XLAI)
C
C       ..BULK STOMATAL RESISTANCE OF THE CANOPY (MONTEITH)
C       (FARAHANI & BAUSCH, 1994)
C
        IF(XLAI.LT.2.0D0) THEN  !tested by Liwang Ma, 2-22-2020
c        IF(LAI.LT.2.0D0) THEN
c          RSC=RST(IPL)/(2.0D0*LAI)/(1.4d0-0.4d0/330.0d0*co2r)  !from Claudio Stockle
          RSC=RST(IPL)/(2.0D0*LAI)*trat   !from DSSAT by Jagtap and Jones
        ELSEIF(XLAI.GT.3.0D0) THEN
c          RSC=RST(IPL)/3.0D0/(1.4d0-0.4d0/330.0d0*co2r)  !from Claudio Stockle
          RSC=RST(IPL)/3.0D0*trat   !from DSSAT by Jagtap and Jones
        ELSE
c          RSC=RST(IPL)/LAI/(1.4d0-0.4d0/330.0d0*co2r)  !from Claudio Stockle
          RSC=RST(IPL)/LAI*trat   !from DSSAT by Jagtap and Jones
        ENDIF
C
C       ..BULK STOMATAL RESISTANCE OF THE CANOPY (CERES-MAIZE)
C       HRT = -100.D0
C       A1 = (1.4D0+5.0D0*(-HRT/15000.0D0))*100.0D0
C       A2 = 1.0D0+0.02D0*(STMPOT-48.6D0)*(-HRT/15000.0D0)
C       RSC = A1 / A2
C
C       C0 = 1.0D-3
C       C1 = 6.0D-5
C       C2 = 4.0D-3
C       RTS2 = RTS * 1.0D6/(24.0D0*3600.0D0)
C       CST = C0*XLAI+C1/(C2*0.60D0)*LOG((1.0D0+C2*0.6D0*RTS2)/
C       +   (1.0D0+C2*0.60d0*RTS2*EXP(-0.6D0*XLAI)))
C
C       RSC = 1.0D0/CST
C
C
C       @   CANOPY ==> RAS
C
        C1=LOG((XWNEW-D)/Z0)*DK
        C2=PLHT/(N*(PLHT-D))*EXP(N)*EXP(-N*Z0P/PLHT)
        C3=PLHT/(N*(PLHT-D))*EXP(N)*EXP(-N*(ZP+DP)/PLHT)
        IF(C2.LE.C3) THEN
          RAS=LOG(XWNEW/Z0)*LOG((XWNEW/2.0D0)/Z0)*DK
        ELSE
          RAS=C1*(C2-C3)
        ENDIF
C
C       @   CANOPY ==> RAA
C
        C1=LOG((XWNEW-D)/Z0)*DK
        C2=LOG((XWNEW-D)/(PLHT-D))
        C3=PLHT/(N*(PLHT-D))
        C4=(EXP(N*(1.0D0-(DP+ZP)/PLHT))-1.0D0)
        RAA=C1*(C2+C3*C4)
      ELSEIF(LAI.LE.0.0D0.OR.PLHT.LE.0.0D0) THEN
        RSC=1.0D30
        RAC=1.0D30
      ENDIF
C
C     ===::> R E S I D U E
C
C     ..CALCULATE RESIDUE CHARACTERISTICS
      IF(RM.GT.1.0D-6 .AND. IPR.NE.0) THEN
C
C       ..CONVERT FROM KG/HA ==> T/HA
        TRM=RM*1.0D-3
        IF(CRES.LE.0.0D0) THEN
          CRES=1.320D0
        ENDIF
C
C       ... REVISED ALGORITHM TO ESTIMATE FRACTION OF SOIL COVERED
C       RHORB = RESIDUE BULK DENSITY
C
c        NDXR=IRTYPE(IPR)
        CS=EXP(-CRES*1.270D-2*TRM/(RDIA(IPR)*RHORS(IPR)))
C
C       ...       IF NO REISEDUE THICKNESS IS GIVEN BY USER, OR IF
C       ... THERE IS NO RESIDUE AT THE BEGINNING OF A CROP SEASON
C       ... AND THUS HR IS SET TO ZERO, THEN AT HARVEST, TRM BECOMES
C       ... A POSITIVE VALUE AND THUS HR AND RHOBR NEED TO BE
C       ... ESTIMATED.    THIS IS DONE BY ASSUMING A MEAN RESIDUE POROSITY
C       ... VALUE OF 0.8 FROM THE LITERATURE.
C       ... A RESIDUE POROSITY OF 0.8 MEANS RHORB = 0.2 * RHORS.
C
c        IF(HR.EQ.0.0D0) THEN
          RHORB=0.20D0*RHORS(IPR)
          HR=TRM*1.0D-2/((1.0D0-CS)*RHORB)
c        ELSE
c          RHORB=TRM*1.0D-2/((1.0D0-CS)*HR)
c        ENDIF
C       ...    CONVERT RESIDUE THICKNESS FROM cm TO m ....
C
        HRM=HR/100.0D0
        Z0R=1.97D-1*HRM
C
C       .. BULK EVAPORATIVE RESISTANCE OF THE RESIDUE
C       U2 = WIND SPEED AT 2 M ABOVE RESIDE LAYER (m/s)
C       RSR  = RESISTANCE TO EVAPORATION THROUGH RESIDUE (s/m)
C       RESP = RESIDUE POROSITY
C
        RESP=1.0D0-RHORB/RHORS(IPR)
        IF(RESP.LE.0.5D0.OR.RESP.GT.0.95D0) THEN
          RESP=0.8D0
        ENDIF
C
        U2=US*LOG(2.0D0/Z0R)/(LOG(XWNEW/Z0R))
        RSR=1.10D0*HRM/(2.120D-5*(1.D0+0.007D0*TA)*(1.0D0+(1.25D-3*RHORB
     +      **(-1.79D0))*U2)*RESP)
      ELSE
        RSR=0.0D0
        Z0R=0.0D0
        CS=1.0D0
      ENDIF
C
C     .. EVAPORATIVE RESISTANCE OF SOIL SURFACE
C     From Jackson (1973), for saturated soil.
C
c      RSS=37.0D0
c      RSS=200.0D0
C
C     ===::> BARE SOIL W/O RESIDUE (NO CROP)
C
      IF(LAI.EQ.0.0D0.OR.PLHT.EQ.0.0D0) THEN
        Z0=MAX(Z0P,Z0R)
        RAS=LOG(XWNEW/Z0)*LOG((XWNEW/2.0D0)/Z0)*DK
        RAA=(LOG(XWNEW/Z0))**2.0D0*DK-RAS
      ENDIF
C
      RETURN
      END
c
      SUBROUTINE POTEVPHR(ASPECT,CS,ELEV,EPAN,FT,FTR,JDAY,PET,PER,PES,
     +    RR,RTS,S,THETA,TMIN,TMAX,U,WRES,LAI,XLAT,PP,ICRUST,RH,
     +    WC13,WC15,IPL,HEIGHT,AS,ESN,RCS,PKTEMP,IRTYPE,TLAI,IPR,
     +    SDEAD_HEIGHT,SDEAD,tl,cshslab,thermk,delt,pfirst,h,ipenflux,
     +    aevap,tair,tz1,tzlb,znlb,daytim,clouds,gflux,ishaw,iyyy,wcsat,
     +    ihflag,ihourly,tm1,tm2,hkbar,nn,rdf,qs,atrans,hroot,pup,trts,
     +    ppop,stemai,zstubl,stublw,pa,xlh,rhorb,hrm,cr,ar,unew,
     +    nsp,nr,rhosp,zsp,orts,delz,cor,itime,iwzone,hrts,co2r,trat,
     +    rth,hrth,jpenflux,tm4,rtstot)
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
C       A0         I  ALBEDO DRY SOIL
C       AC         I  ALBEDO OF PLANT CANOPY
C       AR         L  RESIDUE ALBEDO AS FUNCTION OF AGE
C       ARI        I  ALBEDO OF FRESH RESIDUE
C       AS         L  ALBEDO OF SOIL
C       ASPECT     I  SLOPE ASPECT RELATIVE TO TRUE NORTH (R)
C       AW         I  ALBEDO WET SOIL
C       CCL        L  (1-C0) FRACTION AREA COVERED BY CROP
C       CCC        L  SHUTTLEWORTH EVAP CONSTANTS CANOPY
C       CCS        L  SHUTTLEWORTH EVAP CONSTANTS SUBSTRATE/RESIDUE
C       CO         L  AREA NOT COVERED BY CROP [0..1]
C       CP         P  SPECIFIC HEAT OF MOIST AIR AT CONST PRES (MJ/KG C)
C       CR         L  (1-CS) AREA COVERED BY RESIDUE
C       CRA        L  COEFF FOR SHUTTLEWORTH EVAP CONSTANTS CANOPY
C       CRC        L  COEFF FOR SHUTTLEWORTH EVAP CONSTANTS RESIDUE
C       CRS        L  COEFF FOR SHUTTLEWORTH EVAP CONSTANTS SOIL
C       CS         I  EXPOSED SOIL, NOT COVERED BY RESIDUE
C       DEO        L  SLOPE OF THE SATURATION VAPOR CURVE (KPA/C)
C       DO         L  VAPOR PRESSURE DEFICIT
C       EA         L  AVERAGE SATURATION VAPOR PRESSURE (KPA)
C       ED         L  SATURATED VAPOR PRESSURE AT DEW TEMP (KPA)
C       ELEV   I  ELEVATION AT SITE (M)
C       ESN       I/O POTENTIAL ATMOSPHERIC EVAPORATIVE POTENTIAL (CM)
C       FSUN   I  SUNSHINE FRACTION
C       G      L  SOIL HEAT FLUX [MJ/M^2]
C       ICRUST     I  FLAG INDICATING IF A CRUST EXISTS (=1;TRUE)
C       IPL        I  PLANT CURRENTLY MODELING [1..MXSPEC]
C       JDAY   I  JULIAN DAY    [1..366]
C       K      P  VON KARMAN CONSTANT ( =.41)
C       LAI        I  LEAF AREA INDEX
C       PEN123     P  ROUGHNESS LENGTH FOR MOMENTUM TRANFER CONSTANT
C       PER       I/O RESIDUE EVAPORATION (CM)
C       PES       I/O SUBSTRATE EVAPORATION (CM)
C       PET       I/O TRANSPIRATION (CM)
C       PKTEMP     I  SNOWPACK TEMPERATURE
C       PMC        L  P-M CLOSED CANOPY TRANSPIRATION
C       PMS        L  P-M BARE SUBSTRATE EVAPORATION
C       PP         I  PHOTOPERIOD
C       PPOP   I  NUMBER OF PLANT PER AREA  [#/HA]
C       PSY        L  PSYCHROMETRIC CONSTANT (KPA/C)
C       RAA        L  AERODYNAMIC RESISTANCE BETWEEN SUBSTATE AND CANOPY
C       RAC        L    @ CANOPY (S/M)
C       RAS        L    @ SUBSTRATE (S/M)
C       RCH        L  DAILY TOTAL CLEAR-SKY RAD (DIR & DIFFUSE) ON
C                 A HORIZONTAL SURFACE [MJ/M^2/DAY].
C       RCS        L  DAILY TOTAL CLEAR-SKY RAD (DIR & DIFFUSE) ON
C                 SLOPING SURFACE [MJ/M^2/DAY].
C       RDIA   I  RESIDUE DIAMETER (CM)
C       RESAGE     I  AGE OF THE RESIDUE (DAYS)
C       RH         I  RELATIVE HUMIDITY
C       RHOA   L  DENSITY OF AIR (KPA)
C       RHOR   I  DENSITY OF RESIDUE (G/CM^3)
C       RM         I  MASS OF RESIDUE (T/HA)
C       RN         L  NET SOLAR RADIATION OVER THE FIELD
C       RNL        L  LONGWAVE RADIATION
C       RNR        L  NET SOLAR RADIATION AT RESIDUE SURFACE
C       RNS        L  NET SOLAR RADIATION AT SOIL SURFACE
C       RR         I  RANDOM ROUGHNESS (CM)
C       RSC        L    @ CANOPY (S/M)
C       RSS        L    @ SUBSTRATE (S/M)
C       RTS       I/O DAILY/HOURLY ACTUAL TOTAL S-W RAD AT CANOPY
C                 [MJ/M^2/DAY]
C       RTSTOT     total daily S-W radiation [MJ/M^2/DAY]
C       S      I  SLOPE OF FIELD (R)
C       SIGMA  P  STEFAN-BOLTZMAN CONSTANT (MJ/M**2/DAY/K**4)
C       SWET   L  SHUTTLEWORTH VAPOR FLUX DENSITY (MJ/(M^2.T))
C       THETA  I  SOIL MOISTURE CONTENT
C       TM         L  MEAN DAILY AIR TEMPERATURE (C)
C       TMAX   I  MAXIMUM AIR TEMPERATURE (C)
C       TMIN   I  MINIMUM AIR TEMPERATURE (C)
C       TTR        L  RESIDUE PARTITION OF SOLAR ENERGY
C       TTS        L  SOIL PARTITION OF SOLAR ENERGY
C       TWOTRD     P  2/3 CONSTANT
C       U      I  WIND RUN FOR THE DAY (KM/DAY)
C       UNEW   L  WIND ADJUSTED FOR HEIGHT
C       WC13   I  1/3 BAR WATER CONTENT
C       WC15   I  15 BAR WATER CONTENT
C       WNDADJ     L  WIND MEASUREMENT HEIGHT ADJUSTMENT
C       WRES   I  MOISTURE CONTENT OF RESIDUE
C       XLAT   I  LATITUDE ON EARTH (R)
C       XW         I  WIND MEASUREMENT HEIGHT (M)
C
C
C
C       COMMENTS: HOURLY ET CALCULATION WAS FROM RICHARD ALLEN, FAO IRRIGAITON AND DRAINAGE PAPER NO 56
C                 CROP EVAPOTRANSPIRATION (GUIDELINES FOR COMPUTING CROP WATER REQUIREMENTS) 
C
C       EXTERNAL REFERENCES:
C                 ALBSWS
C                 CTRANS
C                 ECONST
C                 MAXSW
C                 NETRAD
C                 RESIST
C                 SWSUN
C
C       CALLED FROM:  PHYSCL
C
C       PROGRAMMER:   KEN ROJAS
C
C       VERSION:      3.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION LAI,K,TLAI,al(3),bl(3)
      PARAMETER(CP=1.013D-3,TWOTRD=2.0D0/3.0D0,PEN123=
     +    0.123D0,MXSPEC=10,SLODGE=1.0D0-1.0D0/300.0D0)
      parameter (mxnod=300,nodsno=100)
	logical pfirst
C
C     .. SPACE PERTAINING TO POTENTIAL EVAPORATION
      COMMON /IPOTEV/ A0,AW,AC,ARI,XW,FSUN,COEPAN,rss,RST(MXSPEC)
     +    ,RXANGLE(MXSPEC),RTCCRIT(MXSPEC),RPLEAF0(MXSPEC),
     +      RRSTEXP(MXSPEC),RRLEAF0(MXSPEC)
C
      COMMON /RESID/ RM,RESAGE,CRES,HR,RCN,NADLY,NSDLY,NAREC,NSREC,WORM,
     +               SAI,HSTUBL,WSTUBL
C
       LOGICAL START, FIRST,ihflag
      DIMENSION ZSP(NODSNO), RHOSP(NODSNO), DELZ(NN), H(NN),
     + HKBAR(NN), QS(NN), RDF(NN),rcshr(24),hrts(24),hrth(24),rchhr(24)
      DIMENSION PP(2),IRTYPE(MXSPEC),tl(mxnod),eflux(3),htflux(3),tm(5)
      SAVE WNDADJ,penfluxE, penfluxT
      DATA WNDADJ /1.0D0/,al/1.2d0,1.1d0,1.0d0/,bl/-0.2d0,-0.1d0,0.0d0/
C
C ... STATEMENT FUNCTION FOR SATURATED VAPOR PRESSURE CURVE
      EO(T)=EXP((16.78D0*T-116.9D0)/(T+237.3D0))
C
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
C     ..SET UP CONSTANTS ACCORDING TO TIME STEP
      IF (IHOURLY .EQ. 1.or.jpenflux.eq.1) THEN
c      IF (Ipenflux .EQ. 1 .or.Ishaw.eq.1) THEN   !hourly is used when penflux is used.
        K = 3600.0D0
        SIGMA = 4.903D-9/24.0D0
C     ..ADJUST RESIDUE AGE (AGE IS RESET IN HARVST ROUTINE)
c        RESAGE=RESAGE+1.0d0/24.d0
       ELSE
        K = 86400.0D0
        SIGMA = 4.903D-9
C     ..ADJUST RESIDUE AGE (AGE IS RESET IN HARVST ROUTINE)
c        RESAGE=RESAGE+1
      ENDIF
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
 
      IF(RM+SDEAD.GT.0.0D0) THEN
        RSDEAD=SDEAD/(RM+SDEAD)
      ELSE
	  RSDEAD=0.0D0
      ENDIF
      REPHEIGHT=MAX(RSDEAD*SDEAD_HEIGHT,HEIGHT)/100.0D0
C     ..GET WIND HEIGHT AND SPEED ADJUSTMENT FACTORS (HAMID, 9/10/93)
      IF(XW.GE.10.0D0) THEN
C
C       ..DATA IS ASSUMED FROM AN AIRPORT SETTING WITH 40 CM OF COVER.
        IF(REPHEIGHT.GT.0.0D0) THEN
          XWNEW=1.330D0+TWOTRD*REPHEIGHT
          W1=LOG((XWNEW-TWOTRD*REPHEIGHT)/(PEN123*REPHEIGHT))
          W2=LOG((XW-0.27D0)/0.05D0)
        ELSEIF(REPHEIGHT.LE.0.0D0) THEN
          XWNEW=1.60D0
          W1=LOG(XWNEW/0.01D0)
          W2=LOG((XW-0.27D0)/0.05D0)
        ENDIF
      ELSE
C
C       ..DATA IS FROM A MICROTOWER WITH 10 CM OF COVER.
        IF(REPHEIGHT.GT.0.0D0) THEN
          XWNEW=1.930D0+TWOTRD*REPHEIGHT
          W1=LOG((XWNEW-TWOTRD*REPHEIGHT)/(PEN123*REPHEIGHT))
          W2=LOG((XW-0.07D0)/0.0123D0)
        ELSEIF(REPHEIGHT.LE.0.0D0) THEN
          XWNEW=2.0D0
          W1=LOG(XWNEW/0.01D0)
          W2=LOG((XW-0.07D0)/0.0123D0)
        ENDIF
      ENDIF
      WNDADJ=W1/W2
C
C     ..ADJUST WIND (KM/DAY)
      UNEW=U*WNDADJ
C
C     ..ADJUST RESIDUE AGE (AGE IS RESET IN HARVST ROUTINE)
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     rma Liwang Ma, RZ-SHAW, need to check. may add on an hourly basis dday is hourly?
c     jak
c      IF ((DAYTIM+DELT).GE. (24.0D0-COR)) THEN
c           RESAGE=RESAGE+1
c	endif
c       if (pfirst) then
c          jbday = jday      
c          jpday = jday
c       endif
c       if (jday .gt. jpday) then
c          dday = dble(jday) - dble(jpday)
c          RESAGE = RESAGE + dday
c          jpday = jday
c       endif
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C
C     ..SOIL HEAT FLUX [MJ/M^2/DAY]
      Gflux=0.0D0
C
C     ..ENERGY CONSTANTS
      CALL ECONST(DEO,EA,ED,ELEV,PA,PSY,RH,RHOA,TMIN,TMAX,XLH,TA)
C
C     ..MAXIMUM SHORT WAVE RADIATION
C
      ORTS=RTS
      ORTH=RTH
C
      CALL MAXSW(ASPECT,JDAY,XLAT,RCHD,RN,RCH,S,PP,RCS,RCSHR,RCHHR)
c  use SHAW to partition measured solar radiation to direct and diffuse for each hour to take accout aspect and slope effects
C MOVED TO RZMAIN.FOR AFTER READING METEOLOGICAL DATA
C      RTS=0.0D0
C      NHRPDT=1
C      do i=1,24
C          SUNHOR = HRTS(i)
C      CALL SOLAR_SHAW(DIRECT,DIFFUS,SUNSLP,ALTITU,SUNHOR,
C     >                  I,XLAT,S,ASPECT,jday,NHRPDT)
C       HRTS(I)=(DIRECT+DIFFUS)
C       RTS=RTS+HRTS(I)*3.6d3/1.d6
C      enddo
c==== Use SHAW to partition 
      IF(RTStot.EQ.0.0D0.AND.IHOURLY.eq.0.and.jpenflux.eq.0) THEN
        RTS=SWSUN(FSUN,RCS)
        RTH=SWSUN(FSUN,RCH)
      else IF(RTStot.EQ.0.0D0.AND.IHOURLY.eq.1) THEN
c        RTS=SWSUN(FSUN,RCS)
c        RTH=SWSUN(FSUN,RCH)
        RTS=SWSUN(FSUN,RCSHR(itime))
        RTH=SWSUN(FSUN,RCHHR(itime))
        HRTS(itime)=SWSUN(FSUN,RCSHR(itime))*1.d6/3.6d3
        HRTH(itime)=SWSUN(FSUN,RCHHR(itime))*1.d6/3.6d3
      ELSE
        RTS=ORTS
        RTH=ORTH
      ENDIF
C
C     ..GET SOIL ALBEDO WEIGHTED BY MOISTURE CONTENT
      AS=ALBSWS(A0,AW,WC13,WC15,THETA,ICRUST,RR)
C
C     .. CALCULATE RESIDUE ALBEDO AS FUNCTION OF AGE
      AR=A0*(1.06D0+((ARI/A0)-1.06D0)*DEXP(-0.0255D0*RESAGE))
      AR=MAX(AR,0.0D0)
      IF(WRES.GT.0.D0) AR=.75D0*AR
C
          HRES = -1.0D5   !used for Penflux = h2
C     ..RESISTANCE TERMS FOR SHUTTLEWORTH
c      CALL RESIST(XWNEW,UNEW,LAI,RM,RAA,RAC,RAS,RSC,RSR,RSS,CS,IPL,
c     +    MXSPEC,TA,HEIGHT,HR,IRTYPE,CRES,RST,TLAI,IPR,ihourly,co2r,
c     +    ea,ed,trat)
C     ..RESISTANCE TERMS FOR SHUTTLEWORTH  RZ-Penflux
       hstubl = min(hstubl,sdead_height)
       wstubl = min(wstubl,sdead)
      CALL RESISThr(XWNEW,UNEW,LAI,RM,RAA,RAC,RAS,RSC,RSR,RSS,CS,IPL,
     +    MXSPEC,ta,HEIGHT,HR,IRTYPE,CRES,RST,TLAI,IPR,sai,hstubl,
     +    wstubl,stemai,zstubl,stublw,rbr,rcr,rhorb,hrm,theta,wcsat,
     +    wc13,starsoil,ihourly,ipenflux,ishaw,co2r,ea,ed,trat,
     +    rts*1.D6/3.6D3)
c
      CR=1.0D0-CS
      FTR=CS
C
      if (rts*1.D6/3.6D3.lt.10.d0) rsc=rsc*10.0d0    !nighttime canopy resistance is 10 times higher assumed.
C     ..LIGHT TRANMISSION FRACTION THRU THE CANOPY
      IF(TLAI.NE.0.0D0) THEN
        CO=DEXP(-0.594D0*TLAI)
      ELSE
        CO=1.0D0
      ENDIF
      CCL=1.0D0-CO
      FT=CO
C
C     ..ESTIMATE NET L-W RADIATION RNL
c      AL=1.1D0
c      BL=-0.1D0
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak Liwang Ma, RZ-SHAW
c     changed tl to tml
c     in RZWQM only "else" calculation is used
      tml=((TMAX+273.15D0)**4.0D0+(TMIN+273.15D0)**4.0D0)*0.5D0
      IF (NSP .GT. 0) THEN
      RB0 = (0.62D0-0.005D0*SQRT(ED))*SIGMA*tml   !commented out by Liwang Ma, need to understand more
      ELSE
      RB0=(0.39D0-0.158D0*SQRT(ED))*SIGMA*tml
      ENDIF
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee

      IF(RCS.LT.RTS) RCS=RTS
      IF(RCH.LT.RTH) RCH=RTH
      IF ((IHOURLY.EQ.1.or.jpenflux.eq.1).AND.
     +         RCHHR(ITIME).NE.0.0D0) THEN
          RSRATIO=HRTH(itime)*3.6D3/1.D6/RCHHR(ITIME)   !converts HRTH to MJ/M2 before deivided by RCHHR.
	ELSE IF (IHOURLY.EQ.0.AND.RCH.NE.0.0D0) THEN
          RSRATIO=RTH/RCH
      ELSE
      RSRATIO=HRTH(INT(PP(2)-2.0D0))*3.6D3/1.D6/RCHHR(INT(PP(2)-2.0D0))
c      ELSE IF (IWZONE.EQ.1) THEN
c	    RSRATIO=0.75D0
c	ELSE
c	    RSRATIO=0.50D0
	ENDIF
      RSRATIO=Max(RSRATIO,0.0D0)
      RSRATIO=Min(RSRATIO,1.0D0)
      RNL=-(AL(iwzone)*RSRATIO+BL(iwzone))*RB0
C
C     ... START E AND T CALCULATIONS ...
c moved to somewhere below, Liwang Ma, RZ-SHAW
C     ..CALCULATE EFFECTIVE NET RADIATIONS AT SOIL, RESIDUE, & CANOPY
      CALL NETRAD(AC,AR,AS,CCL,CO,CR,CS,RN,RNR,RNS,RTS,RNL,TTR,TTS
     &           ,rsns,rsnr,rsnc,ishaw)
C     ,LAI,TLAI)
C
C     ..IF USE ENERGY APPROACH OR PAN EVAPORATION (cm/day)...
      IF(EPAN*COEPAN.GT.0.0D0) THEN
        PANET=COEPAN*EPAN
C        IF(CCL.GT.0.0D0) THEN
C          PET=MAX(CCL,1.0D-1)*PANET
        IF(CCL.GT.0.0D0) THEN
          PET=MAX(CCL,1.0D-1)*PANET
        ELSE
          PET=0.0D0
        ENDIF
        IF(RM.GT.0.0D0) THEN
C         ... SUBSTRATE EVAPORATION IS PARTITIONED ACCORDING TO AREA
C         COVERED. HOWEVER, RATE OF RESIDUE EVAP IS ASSUMED
C         HALF THE BARE SOIL...   BY: HAMID J. FARAHANI
C
          PER=0.0D0 !(PANET-PET)/2.0D0*CR
          PES=PANET-PET-PER  !(PANET-PET)/2.0D0-PER
          PES=MAX(PES,0.0D0)
        ELSE
          PER=0.0D0
          PES=PANET-PET
        ENDIF
C THIS PORTION WAS ADDED ON BY RMA, LIWANG MA, RZ-SHAW
c          PET = PET / 24.0D0
c          PER = PER / 24.0D0
c          PES = PES / 24.0D0
        goto 505
        endif
C		rma
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c	jak need to fix ISHAW=1 for RZWQM original
      If(ISHAW.eq.1.or.Ipenflux.eq.1) then
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C       Set up S-W ET options, based on PENFLUX computation of energy 
C       balance equations for soil, residue and canopy surfaces
C       ISHAW switch features: 
C         1: isothermal S-W ET, identical to version 2.0 ET routines
C         2: PENFLUX and S-W ET, net long-wave radiation computed for soil,
C            residue and canopy sub-layers, Gflux obtained in energy balance
C         3: PENFLUX ET uses latent heat flux for soil, residue, and canopy
C            sub-layers based on computed temperatures and vapor pressure
C            deficits for soil and residue sources
C
C      ..CALCULATE EFFECTIVE NET RADIATIONS AT SOIL, RESIDUE, & CANOPY
c rma
C     obtain radiation on sloping surface
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
c        call maxsw(aspect,jday,xlat,rchd,rns,rch,s,pp,rcs,hafday,d)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
        rtslope = (rcs/rch)*rts
c
c       calculate effective net shortwave radiation	for canopy, residue
c       and soil layers; also net short and long-wave components for
c       isothermal Shuttleworth-Wallace algorithm
c
C     ..CALCULATE EFFECTIVE NET RADIATIONS AT SOIL, RESIDUE, & CANOPY
c      CALL NETRAD(AC,AR,AS,ccrop,CO,CR,CS,rnv3,RNR,RNS,RTS,RNL,TTR,TTS,
c     +rsns,rsnr,rsnc)
C 
C         INITIALIZE HYDRAULIC PRESSURE HEAD (CM) OF RESIDUE 
          HRES = -1.0D5
          IF (IHOURLY .EQ. 1.or.jpenflux.eq.1) THEN
c          IF (Ipenflux .EQ. 1.or.ishaw.eq.1) THEN   !hourly is used when penflux is used
C           ..NOTE: SHORTWAVE RADIATION input as J/M2 S
            RSNSW = RSNS*1.D6/3.6D3
            RSNRW = RSNR*1.D6/3.6D3
            RSNCW = RSNC*1.D6/3.6D3
c            RSNSW = RSNS
c            RSNRW = RSNR
c            RSNCW = RSNC
c            DELTSTP = DELT
             DELTSTP = 1.0d0
         ELSE
C           ..NOTE: CONVERT SHORTWAVE RADIATION FROM MJ/M2 DAY TO J/M2 S (W/M2)
            RSNSW = RSNS*1.D6/(3.6D3*24.0D0)
            RSNRW = RSNR*1.D6/(3.6D3*24.0D0)
            RSNCW = RSNC*1.D6/(3.6D3*24.0D0)
            DELTSTP = 24.0D0
          ENDIF
c          endif
c rma
        if (ipenflux .eq. 1.and.jpenflux.eq.1) then   !jpenflux is used to control when to enter penflux because it only needed during redistribution
C
C         ..USE PENFLUX TO GET G, net radiation components . . .
c
          TLTLB = TL(1)+TL(2)+TL(3)  !+TL(4)
C         ..COMPUTE SUPPLY LIMITING EVAPORATION RATE (W/M2)
c          ESMIN1 = MAX(0.0D0,QF2*XLH*1.0D7/3.6D3)
c rma
          ESMIN2 = (THETA-wc15)*XLH*1.0D7/(3.6D3)
c          ESMIN = ESMIN1 + ESMIN2
c           esmin = max(esmin2,aevap*xlh*1.d7/3.6d3)
           esmin = -aevap*xlh*1.d7/3.6d3
C         ..SEND IN DECIMAL RH
          DRH = RH*1.0D-2
c          clouds=0.5
c rma
          CALL PENFLUX(CSHSLAB,DELTSTP,DRH,pfirst,H(1),HRES,CR,CCL,PSY,
     +     RAA,RCR,RBR,RAS,RSC,rss,RHOA,RSNSW,RSNRW,RSNCW,THERMK,TAIR,
     +     tm,Tz1,Tzlb,ZNLB,TLTLB,Gflux,Eflux,Htflux,RLNSW,RLNRW,RLNCW,
     +     CLOUDS,ESMIN,starsoil)
C rma
         tm1 = tm(1)   ! update soil surface temperature after Penflux
         tm2 = tm(2)   !for future use to update resdiue temp
         tm4 = tm(4)   ! for future use to update canopy temp.
c          if (mod(daytim,1.0d0).lt.1d-5) then
c	     write (100,131) jday, int(daytim),iyyy,tm(1),tm(2),tz1
c131   format (i4,i4,i8,30(f7.2,1x,f7.2,1x))
c	    endif
        endif
c commented out to use Shuttleworth-Wallace equation with Gflux from Penflux
c in other words, PET is not from Penflux
c         if (ISHAW .eq. 3) then
c         pass and write PENFLUX energy balance terms
c
c         use PENFLUX energy balance terms for energy-limiting ET
C         Note: units converted from J/m2 s to cm/hr
c          pet = (eflux(3)*3.6d3)/(xlh*1.d7)
c          pes = ((eflux(1)+eflux(2))*3.6d3)/(xlh*1.d7)
c          efluxw = eflux(1)+eflux(2)+eflux(3)
c          hfluxw = htflux(1)+htflux(2)+htflux(3)
c          rlnsubw = rlnsw + rlnrw
c          rsnsubw = rsnsw + rsnrw
c          rnsubw = rsnsubw + rlnsubw
c          rnw = rnsubw + rsncw + rlncw
c          gflw = gflux
c          Gflux = Gflux*3.6D3/1.D6
c          per =eflux(2)*3.6d3/(xlh*1.d7)


c output statements need to be adapted for this configuration 
c rma 12/7/96
c          if (mod(daytim,1.0d0).lt.1d-5.and.ihflag) then
c            idytim=nint(daytim)
c            pesw = pes*xlh*1.0d7/3.6d3
c            enbal = rnw-(hfluxw+efluxw+gflw)

c     jak
c      if(ISHAW.ge.2)then  
c            write (87,111) jday,idytim,iyr,rlncw,0.0,rlnrw,rlnsw,
c     +      rsncw,0.0,rsnrw,rsnsw,rnw,htflux(1),htflux(2),htflux(3),
c     +      hfluxw,eflux(1),eflux(2),eflux(3),efluxw,gflw,enbal
c	else
c            write (87,112) jday,idytim,iyr,rlncw,0.0,rlnrw,rlnsw,
c     +      rsncw,0.0,rsnrw,rsnsw,rnw,hfluxw,efluxw,gflw,enbal
c	endif
c
c        endif
c
c this option is used in RZWQM2 ver 2.0. PET from Shuttleworth-Wallace 
c     with Rn and G from Penflux
c
c         elseif (ISHAW .eq. 2) then
c	pass PENFLUX Rn and G to Shuttleworth-Wallace algorithm
c
            IF (IHOURLY .EQ. 1.or.jpenflux.eq.1) THEN
c            IF (Ipenflux .EQ. 1 .or. ishaw.eq.1) THEN   !hourly is used when penflux is used
C             ..NOTE: CONVERT RADIATION FROM J/M2 S TO MJ/M2 HR
              RLNS = RLNSW*3.6D3/1.D6
              RLNR = RLNRW*3.6D3/1.D6
              RLNC = RLNCW*3.6D3/1.D6
c rma
              RsNS = RsNSW*3.6D3/1.D6
              RsNR = RsNRW*3.6D3/1.D6
              RsNC = RsNCW*3.6D3/1.D6
C             ..NOTE: CONVERT GFLUX FROM J/M2 S TO MJ/M2 HR, 
c             POSITIVE INTO SOIL
c rma
              gflw = -gflux
              Gflux = Gflux*3.6D3/1.D6
            ELSE
C             ..NOTE: CONVERT LONGWAVE RADIATION FROM W/M2 TO MJ/M2 DAY
              RLNS = RLNSW*3.6D3*24.0D0/1.D6
              RLNR = RLNRW*3.6D3*24.0D0/1.D6
              RLNC = RLNCW*3.6D3*24.0D0/1.D6
C             ..NOTE: CONVERT GFLUX FROM W/M2 TO MJ/M2 DAY, 
c             POSITIVE INTO SOIL
              gflw = -gflux
              Gflux = Gflux*3.6D3*24.0D0/1.D6
            ENDIF
c
C           ..COMPUTE NET RADIATION TERMS, USING LAYER-SPECIFIC 
C           LONGWAVE RADIATION MJ/m2 hr
c            RNSUB = RSNS + RSNR + RLNS + RLNR
c            RN = RSNS + RSNR + RSNC + RLNS + RLNR + RLNC
C rma
c         endif
        endif
c rma
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c	jak
c        if(ISHAW .eq. 0.or.itest.eq.1) then
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
c         initialize isothermal Shuttleworth-Wallace algorithm
C         ..COMPUTE NET RADIATION TERMS THE WAY ITS DONE IN V3.0
C           (THIS WAY SEEMS TO PREDICT MORE ET)
c          RNSUB = RNS + RNR
c          RN = RNV3
c rma     compute longwave radiation for output
c         if (ihourly .eq. 1) then
c         convert from J/m2 s to MJ/m2 hr
c          RNSUB = (RNS + RNR)*3.6d3/1.d6
c          RN = RNV3*3.6d3/1.d6
c         compute longwave radiation for output
c          rlnsw = (rns-rsns)    
c		if(rsnr.gt.0)then     
c          rlnrw = (rnr-rsnr)    
c		else
c		rlnrw=0.0
c		endif
c		if(rsnc.gt.0)then     
c          rlncw = (rnv3-rsnc)
c		else
c		rlncw=0.0
c		endif      
c         else
c rma     compute radiation  MJ/m2 day 
c          RNSUB = RNS + RNR
c          RN = RNV3
c rma     convert longwave radiation from MJ/m2 day to W/m2 for output
c          rlnsw = (rns-rsns)*1.d6/(3.6d3*24.d0)         
c		if(rsnr.gt.0)then
c          rlnrw = (rnr-rsnr)*1.d6/(3.6d3*24.d0) 
c        ELSE
c		rlnrw=0.0
c        ENDIF
c		if(rsnc.gt.0.)then        
c          rlncw = (rnv3-rsnc)*1.d6/(3.6d3*24.d0) 
c        ELSE
c		rlncw=0.
c		endif   
c        ENDIF
C
c      ELSE
C
        DEO=4098.0D0*EO(TA)/(TA+237.3D0)**2.0D0
        RNSUB=RNS+RNR
C     ..SOIL HEAT FLUX [MJ/M^2/DAY]
      if (ipenflux.ne.1.and.ishaw.ne.1) then
      if (ihourly.eq.0.and.jpenflux.eq.0) then
      Gflux=0.0D0
      else if (itime.ge.pp(1).and.itime.le.pp(2)) then
	Gflux=0.1*Rn
	else
	Gflux=0.5*Rn
      endif
      endif
        C1=DEO*(RN-Gflux)
        C2=K*RHOA*CP
C
        IF(LAI.GT.0.0D0.AND.HEIGHT.GT.0.0D0) THEN
          C3=C1+(C2*(EA-ED)-DEO*RAC*(RNSUB-Gflux))/(RAA+RAC)
          C4=DEO+PSY*(1.0D0+RSC/(RAA+RAC))
          PMC=MAX(C3/C4,0.0D0)
        ELSE
          PMC=0.D0
        ENDIF
C
        IF(CS.GT.0.0D0) THEN
          C5=C1+(C2*(EA-ED)-DEO*RAS*(RN-RNSUB))/(RAA+RAS)
          C6=DEO+PSY*(1.0D0+RSS/(RAA+RAS))
          PMS=MAX(C5/C6,0.0D0)
        ELSE
          PMS=0.0D0
        ENDIF
C       PRINT *,'RN-RNS =',RN-RNS,'EA-ED =',EA-ED
C
        IF(RM.GT.0.0D0) THEN
          C7=C1+(C2*(EA-ED)-DEO*RAS*(RN-RNSUB))/(RAA+RAS)
          C8=DEO+PSY*(1.0D0+(RSS+RSR)/(RAA+RAS))
          PMR=MAX(C7/C8,0.0D0)
        ELSE
          PMR=0.0D0
        ENDIF
C
C       ..GET COEFFICIENTS FOR SHUTTLEWORTH EVAP
        IF(RM.GT.0.0D0) THEN
          CRA=(DEO+PSY)*RAA
          CRS=(DEO+PSY)*RAS+PSY*RSS
          CRC=(DEO+PSY)*RAC+PSY*RSC
          CRR=(DEO+PSY)*RAS+PSY*(RSS+RSR)
          DENOM=CRC*CRS*CRR+CRA*CRS*CRR+CRA*CRC*CRR*CS+CRA*CRC*CRS*CR
          CCC=CRS*CRR*(CRC+CRA)/DENOM
          CCS=CRC*CRR*(CRS+CRA)*CS/DENOM
          CCR=CRC*CRS*(CRR+CRA)*CR/DENOM
        ELSE
          CRA=(DEO+PSY)*RAA
          CRS=(DEO+PSY)*RAS+PSY*RSS
          CRC=(DEO+PSY)*RAC+PSY*RSC
          IF(CRS.NE.0.0D0) THEN
            CCC=1.0D0/(1.0D0+CRC*CRA/(CRS*(CRC+CRA)))
          ELSE
            CCC=1.0D0
          ENDIF
          IF(CRC.NE.0.0D0) THEN
            CCS=1.0D0/(1.0D0+CRS*CRA/(CRC*(CRS+CRA)))
          ELSE
            CCS=1.0D0
          ENDIF
          CCR=0.0D0
        ENDIF
C
C       ..VAPOR FLUX DENSITY (MJ/(M^2.DAY))
C
        SWET=CCC*PMC+CCS*PMS+CCR*PMR
C
C       ..VAPOR PRESSURE DEFICIT AT CANOPY SOURCE HEIGHT
        C7=RAA/C2
        DOP=(EA-ED)+(C1-(DEO+PSY)*SWET)*C7
C
C       ....P O T E N T I A L    T R A N S P I R A T I O N....
C
c     rma THE ONES WITH a (c8a, c10a, c12a) were added by jak, Liwang Ma, RZ-SHAW
        IF(LAI.GT.0.0D0.AND.HEIGHT.GT.0.0D0) THEN
          C8=DEO*(RN-RNSUB)+C2*DOP/RAC
c     jak
          C8a = (psy*(1.d0+rsc/rac)*(RN-RNSUB)) - C2 * DOP / RAC
          C9=DEO+PSY*(1.0D0+RSC/RAC)
          PET=C8/C9
          pht = C8a / C9
        ELSE
          PET=0.0D0
	    pht=0.0
        ENDIF
C
C       .. P O T E N T I A L  SOIL   E V A P O R A T I O N
        C10=DEO*(RNSUB-Gflux)+C2*DOP/RAS
        C10a = (psy*(1.d0+rss/ras)*(RNSUB-Gflux)) - C2 * DOP / RAS
        C11=DEO+PSY*(1.0D0+RSS/RAS)
        PES=(C10/C11)*CS
 	  phs = (c10a / c11)*cs

C
C       .. P O T E N T I A L  RESIDUE   E V A P O R A T I O N
        C12=DEO*(RNSUB-Gflux)+C2*DOP/RAS
        C12a = (psy*(1.d0+(rss+rsr)/ras))*(RNSUB-Gflux)-C2*DOP/RAS
        C13=DEO+PSY*(1.0D0+(RSS+RSR)/RAS)
        PER=(C12/C13)*CR
        phr = (C12a / C13)*CR

c	  convert units from MJ/m^2 hr to W/m2		
         if (ihourly.eq.1.or.jpenflux.eq.1) then
            rnw = rn*1.0d6/3.6d3
cc            eflux1 = max(pes*1.0d6/3.6d3,0.0d0)  !no condensation, Liwang Ma		
cc            eflux2 = max(per*1.0d6/3.6d3,0.0d0)		
cc            eflux3 = max(pet*1.0d6/3.6d3,0.0d0)		
cc            hflux1 = phs*1.0d6/3.6d3		
cc            hflux2 = phr*1.0d6/3.6d3		
cc            hflux3 = pht*1.0d6/3.6d3
         else
c	  convert units from MJ/m^2 day to W/m2		
            rnw = rn*1.0d6/3.6d3/24.0d0
cc            eflux1 = max(pes*1.0d6/3.6d3/24.0d0,0.0d0)		
cc            eflux2 = max(per*1.0d6/3.6d3/24.0d0,0.0d0)		
cc            eflux3 = max(pet*1.0d6/3.6d3/24.0d0,0.0d0)		
cc            hflux1 = phs*1.0d6/3.6d3/24.0d0		
cc            hflux2 = phr*1.0d6/3.6d3/24.0d0		
cc            hflux3 = pht*1.0d6/3.6d3/24.0d0
         endif
C This uses eflux and htflux from Penflux, not from Shuttleworth-Wallace as above. Need to talk to Gerald F.
            eflux1 = eflux(1)		
            eflux2 = eflux(2)		
            eflux3 = eflux(3)		
            hflux1 = htflux(1)		
            hflux2 = htflux(2)		
            hflux3 = htflux(3)
C       ..CONVERT UNITS FROM MJ/M^2/DAY ==> CM/DAY
C       ..DIVIDE BY HEAT OF VAPORIZATION (MJ/KG)
        PET=MAX(PET/(XLH*10.0D0),0.0D0)
        PER=MAX(PER/(XLH*10.0D0),0.0D0)
        PES=MAX(PES/(XLH*10.0D0),0.0D0)
C
C       ..DETERMINE SNOW PACK SUBLIMATION POTENTIAL
        ESN=SNOWQE(UNEW,TMAX,TMIN,PKTEMP,XLH,RH)
        ESN=MIN(ESN,PES)
        ESN=0.011D0

        if (ipenflux.eq.1.and.jpenflux.eq.1) then
          efluxw = eflux1+eflux2+eflux3
          hfluxw = hflux1+hflux2+hflux3
          penfluxE = penfluxE+(eflux1+eflux2)*3.6d3/xlh/1.d7
          penfluxT = penfluxT+eflux3*3.6d3/xlh/1.d7
c          IF (mod(daytim,1.0d0).lt.1d-5) then
c            idytim=nint(daytim)
            enbal = rnw-(hfluxw+efluxw-gflw)
c         write (87,111) jday,idytim,iyyy,rsncw,rsnrw+rsnsw,rlncw,rlnsw
c     +   rlnr2,hflux3,hflux1+hflux2,eflux3,eflux1+eflux2,gflux,enbal

c            write (88,188) jday,idytim,iyyy,ta,rh,U
c     +            u   !,sunhor
c188   format (i3,i4,i5,5(f8.3,2x))
            write (87,112) jday,itime,iyyy,rlncw,0.0,rlnrw,rlnsw,
     +      rsncw,0.0,rsnrw,rsnsw,rnw,hfluxw,efluxw,gflw,enbal
c            write (87,111) jday,itime,iyyy,rlncw,0.0,rlnrw,rlnsw,
c     +      rsncw,0.0,rsnrw,rsnsw,rnw,htflux(1),htflux(2),htflux(3),
c     +      hfluxw,eflux(1),eflux(2),eflux(3),efluxw,gflw,enbal
            if (itime.eq.24) then
           write (100,113)jday,itime,penfluxE,penfluxT,penfluxE+penfluxT
            penfluxE=0.0d0
            penfluxT=0.0d0
            endif
        endif
c          ENDIF
C
c      ENDIF
C
505   continue
      RETURN
 
c     jak
111		format (i3,2x,i2,2x,i4,17(4x,f5.0),9x,f5.0,14x,f5.0,13x,f5.0,
     *            10x,f5.0)
112		format (i3,2x,i2,2x,i4,9(3x,f6.0),28x,f6.0,30x,f6.0,9x,f6.0,
     *            9x,f6.0)
113       format (i4,i4,4(f10.5))
c111		format (i3,2x,i2,2x,i4,17(3x,f5.0),6x,f5.0,9x,f6.0)
c112		format (i3,2x,i2,2x,i4,9(3x,f6.0),28x,f6.0,30x,f6.0,9x,f6.0,
c     *            9x,f6.0)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee

      END
C Liwang Ma, RZ-Penflux, June 2008
c      SUBROUTINE RESIST(XWNEW,UNEW,LAI,RM,RAA,RAC,RAS,RSC,RSR,RSS,CS,
c     +    IPL,MXSPEC,TA,HEIGHT,HR,IRTYPE,CRES,RST,TLAI,IPR)
C
      SUBROUTINE RESISThr(XWNEW,UNEW,LAI,RM,RAA,RAC,RAS,RSC,RSR,RSS,CS,
     +    IPL,MXSPEC,TA,HEIGHT,HR,IRTYPE,CRES,RST,TLAI,IPR,sai,hstubl,
     +    wstubl,stemai,zstubl,stublw,rbr,rcr,rhorb,hrm,theta,wcsat,
     +    wc13,starsoil,ihourly,ipenflux,ishaw,co2r,ea,ed,trat,rts2)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C======================================================================
C
C       PURPOSE: CALCULATE ALL THE RESISTANCE TERMS FOR SHUTTLEWORTH
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       CONVW  P  CONVERT WIND RUN (KM/DAY) ==> M/S
C       CRES   P  COEFF ACCOUNTS FOR RANDOMNESS OF RESIDUE.
C       CS        I/O EXPOSED SOIL, NOT COVERED BY RESIDUE
C       CST           BULK STOMATAL CONDUCTANCE (M/S)
C       D      L  ZERO PLANE DISPLACEMENT (M)
C       DP         L  PREFERED VALUE OF CROP DISPLACEMENT HEIGHT (M)
C       HEIGHT     O  CANOPY HEIGHT (CM)
C       HGHT      CANOPY HEIGHT (M)
C       HR         L  HEIGHT OF RESIDUE LAYER (cm)
C       HRM        L  HEIGHT OF RESIDUE LAYER (m)
C       K      P  VON KARMAN CONSTANT ( =.41)
C       LAI        I  CURRENT LEAF AREA INDEX
C       N      P  EDDY DIFFUSIVITY DECAY CONSTANT
C                 AERODYNAMIC RESISTANCE BETWEEN CANOPY AND
C                 MEASUREMENT HEIGHT
C       RAA       I/O   @ ABOVE CANOPY (S/M)
C       RAC       I/O   @ CANOPY (S/M)
C       RAS       I/O   @ SUBSTRATE (S/M)
C       RB         P  LEAF BOUNDARY LAYER RESISTANCE (S/M)
C       RDIA   I  MEAN RESIDUE DIAMETER (CM)
C       RHORS  I  SPECIFIC DENSITY OF RESIDUE (G/CM^3)
C       RHORB  L  BULK DENSITY OF RESIDUE (G/CM^3)
C       RM         I  MASS OF RESIDUE (T/HA)
C       RSS        O  SOIL SURFACE RESISTANCE (S/M)
C       RSR        O  RESIDUE SURFACE RESISTANCE (S/M)
C       RSC        O  CANOPY SURFACE (STOMATAL) RESISTANCE (S/M)
C       RST        P  MEAN STOMATAL RESISTANCE (S/M)
C       RTS2      SOLAR EADIATION (W/M^2)
C       UNEW   I  TOTAL WIND RUN (KM/DAY) AT MEASUREMENT HEIGHT
C       URES      WIND SPEED WITHIN RESIDUE (M/S)
C       US            WIND SPEED AT MEASUREMENT HEIGHT (M/S)
C       XW         I  WIND MEASUREMENT HEIGHT (M)
C       Z0         L  ROUGHNESS LENGTH FOR MOMENTUM TRANSFER (M)
C       ZP            PREFFERED VALUE OF CROP ROUGHNESS LENGTH (M)
C       Z0P        P  EFFECTIVE ROUGHNESS LENGTH FOR BARE SOIL (M)
C       Z0R        L  EFFECTIVE ROUGHNESS LENGTH FOR RESIDUE (M)
C New Variables
c       sai         stem area index (m2 standing stems/m2 ground)      
c       hstubl      actual height, standing stems    [cm]
c       wstubl      dry mass of standing residue     [Metric tons/ha]
c       stemai      stem area index (=sai)
c       ,zstubl,stublw,rbr,rcr,rhorb,hrm,theta,wcsat,
c     +    wc13,starsoil,ihourly
C       COMMENTS:  RHOR AND RDIA ARE IN CROP ORDER:
C               1) CORN
C               2) SOYBEAN
C               3) WHEAT
C
C       EXTERNAL REFERENCES:
C                 NONE
C
C       CALLED FROM:  POTEVP
C
C       PROGRAMMER:   KEN ROJAS
C
C       VERSION:      1.0
C
C======================================================================
C
C ... DECLARE VARIABLES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION K,LAI,N
      DIMENSION IRTYPE(MXSPEC),RST(MXSPEC)
      PARAMETER(K=4.1D-1,Z0P=1.0D-2,N=2.5D0,CD=0.07D0,MXRES=3,CONVW=
     +    1.0D3/(24.0D0*60.0D0*60.D0),RB=10.0D0,
     +  De=2.5d-5,cmult=2.d0,Tconw=0.57d0, Tcona=0.025,
     +  rhocpa=1.2d3, rhocpw=4.18d6, RHOL=1.0D3, slope1 = 2.d0, 
     +  tau=1.d0)   !,c0=5.d-4,c1=3.2d-5,c2=5.7d-5)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C
C     .. INITIALIZE CROP DEPENDENT RESIDUE VALUES
      DOUBLE PRECISION RHORS(MXRES),RDIA(MXRES)
      SAVE RHORS,RDIA,rss0
      DATA RHORS/0.15d0,0.17d0,0.18d0/,
     +     RDIA /1.0d0,0.5d0,0.25d0/
      common /shawp/ pplastic,emitc,emitr(10),emitsp,emits,emitf
     &      ,palbedo,ptransm
C
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
C     ..CONVERT WIND RUN (KM/DAY) ==> M/S
c - gnf 3/30/98
c       convert stubble height from cm to m
        zstubl=hstubl*1.0d-2
        ahr=zstubl
c       Define stem area index and mass for stubble
        stublw=wstubl
        stemai = sai
c
C rma
        IF (IPL .EQ. 0) THEN
          IR = 1
        ELSE
          IR = IRTYPE(IPL)
        ENDIF
c rma  LMA, check on UNEW unit.  Now km/day entered into the subroutine for both hourly and daily.
c        if (ihourly .eq. 1) then
c        if (ipenflux .eq. 1 .or. ishaw.eq.1) then  !hourly is used when penflux or shaw is used
c         units in m/s
c          us = unew   !in m/s alrady when SHAW or Penflux is used.
c        else 
C     ..CONVERT WIND RUN (KM/DAY) ==> M/S
         US=UNEW*CONVW   !all km/day as input to the subroutine
c        endif
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee

C     SET A CAP ON WIND SPEED.
C     IF(US.GT.3.50D0) THEN
C     US=3.50D0
C     ELSEIF (US.LT.0.50D0) THEN
C     US = 0.50D0
C     ENDIF
      DK=1.0D0/(K**2.0D0*US)
C
C     CONVERT PLANT HEIGHT FROM cm TO m.
      PLHT=MAX(HEIGHT/100.0D0,0.05D0)
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     rma
c     jak
      ahc = plht
     	ah = max(ahc,ahr)
      XLAI=MAX((LAI+TLAI)*0.5D0,sai,0.05D0)
C
C     ===::> C A N O P Y
C
C     .. BULK CANOPY LAYER RESISTANCE OF THE CANOPY
C
      IF(LAI.GT.0.0D0.AND.PLHT.GT.0.0D0.or. sai.gt. 0.0d0) THEN
C
C       ..GET CROP CHARACTERISTICS
        DP=6.3D-1*PLHT
        ZP=1.3D-1*PLHT

c     rma 
       if (ahr .gt. ahc) then
          cdres = 0.041312d0*log(ahr/rdia(ir))+0.3161d0
	      x = cdres*sai
        else
        X=CD*XLAI
        endif        
        
        
        D=1.10D0*PLHT*LOG(1.0D0+X**0.250D0)
C
        IF(X.GT.0.0D0.AND.X.LE.0.2D0) THEN
          Z0=Z0P+0.30D0*ah*(X**0.50D0)
        ELSEIF(X.GT.0.20D0) THEN
          Z0=0.30D0*ah*(1.0D0-D/ah)
        ENDIF
C
        RAC=RB/(2.0D0*XLAI)
C
C       ..BULK STOMATAL RESISTANCE OF THE CANOPY (MONTEITH)
C       (FARAHANI & BAUSCH, 1994)
C
        IF(LAI.GT.0.0D0) THEN
        IF(XLAI.LT.2.0D0) THEN
c          RSC=RST(IPL)/(2.0D0*LAI)/(1.4d0-0.4d0/330.0d0*co2r)  !from Claudio Stockle
          RSC=RST(IPL)/(2.0D0*LAI)*trat   !from DSSAT by Jagtap and Jones
        ELSEIF(XLAI.GT.3.0D0) THEN
c          RSC=RST(IPL)/3.0D0/(1.4d0-0.4d0/330.0d0*co2r)  !from Claudio Stockle
          RSC=RST(IPL)/3.0D0*trat   !from DSSAT by Jagtap and Jones
        ELSE
c          RSC=RST(IPL)/LAI/(1.4d0-0.4d0/330.0d0*co2r)  !from Claudio Stockle
          RSC=RST(IPL)/LAI*trat   !from DSSAT by Jagtap and Jones
        ENDIF
        else
c          standing stubble -- set stomatal resistance large
          RSC = 1.0d30
        endif
C
C       ..BULK STOMATAL RESISTANCE OF THE CANOPY (CERES-MAIZE)
C       HRT = -100.D0
C       A1 = (1.4D0+5.0D0*(-HRT/15000.0D0))*100.0D0
C       A2 = 1.0D0+0.02D0*(STMPOT-48.6D0)*(-HRT/15000.0D0)
C       RSC = A1 / A2
C     from Shuttleworth and Gurney (1990) and Uchijima 1976), not used so far
c       C0 = 1.0D-3
c       C1 = 6.0D-5
c       C2 = 4.0D-3
c       if (ihourly.eq.1) then
c           rts2=rts2
c       else
c           rts2=rts2/24.0d0
c       endif
C       RTS2 = RTS * 1.0D6/(24.0D0*3600.0D0)
c       CST = C0*XLAI+C1/(C2*0.60D0)*LOG((1.0D0+C2*0.6D0*RTS2)/
c       +   (1.0D0+C2*0.60d0*RTS2*EXP(-0.6D0*XLAI)))
C
c       RSC1 = 1.0D0/CST
C
C
C       @   CANOPY ==> RAS
C
        C1=LOG((XWNEW-D)/Z0)*DK
        C2=PLHT/(N*(PLHT-D))*EXP(N)*EXP(-N*Z0P/PLHT)
        C3=PLHT/(N*(PLHT-D))*EXP(N)*EXP(-N*(ZP+DP)/PLHT)
        IF(C2.LE.C3) THEN
          RAS=LOG(XWNEW/Z0)*LOG((XWNEW/2.0D0)/Z0)*DK
        ELSE
          RAS=C1*(C2-C3)
        ENDIF
C
C       @   CANOPY ==> RAA
C
        C1=LOG((XWNEW-D)/Z0)*DK
        C2=LOG((XWNEW-D)/(PLHT-D))
        C3=PLHT/(N*(PLHT-D))
        C4=(EXP(N*(1.0D0-(DP+ZP)/PLHT))-1.0D0)
        RAA=C1*(C2+C3*C4)

c     jak
c     using ELSE rather than elseif, above needs to be done if
c     there is standing stubble or a plant
      ELSEIF (LAI.LE.0.0D0.OR.PLHT.LE.0.0D0) THEN     
c      ELSE	
c        Z0 = MAX(Z0P,Z0R)
c        D = 0.0D0
c        RAS = LOG(XWNEW/Z0) * LOG((XWNEW/2.0D0)/Z0)*DK
c        RAA = (LOG(XWNEW/Z0))**2.0D0 * DK - RAS
        RSC=1.0D30
        RAC=1.0D30
      ENDIF
C
C     ===::> R E S I D U E
C
C     ..CALCULATE RESIDUE CHARACTERISTICS
      IF(RM.GT.1.0D-6 .AND. IPR.NE.0) THEN
C
C       ..CONVERT FROM KG/HA ==> T/HA
        TRM=RM*1.0D-3
        IF(CRES.LE.0.0D0) THEN
          CRES=1.320D0
        ENDIF
C
C       ... REVISED ALGORITHM TO ESTIMATE FRACTION OF SOIL COVERED
C       RHORB = RESIDUE BULK DENSITY
C
c        NDXR=IRTYPE(IPR)
        CS=EXP(-CRES*1.270D-2*TRM/(RDIA(IPR)*RHORS(IPR)))
C
C       ...       IF NO REISEDUE THICKNESS IS GIVEN BY USER, OR IF
C       ... THERE IS NO RESIDUE AT THE BEGINNING OF A CROP SEASON
C       ... AND THUS HR IS SET TO ZERO, THEN AT HARVEST, TRM BECOMES
C       ... A POSITIVE VALUE AND THUS HR AND RHOBR NEED TO BE
C       ... ESTIMATED.    THIS IS DONE BY ASSUMING A MEAN RESIDUE POROSITY
C       ... VALUE OF 0.8 FROM THE LITERATURE.
C       ... A RESIDUE POROSITY OF 0.8 MEANS RHORB = 0.2 * RHORS.
C
c        IF(HR.EQ.0.0D0) THEN
          RHORB=0.20D0*RHORS(IPR)
          HR=TRM*1.0D-2/((1.0D0-CS)*RHORB)
c        ELSE
c          RHORB=TRM*1.0D-2/((1.0D0-CS)*HR)
c        ENDIF
C       ...    CONVERT RESIDUE THICKNESS FROM cm TO m ....
C
        HRM=HR/100.0D0
        Z0R=1.97D-1*HRM
C
C       .. BULK EVAPORATIVE RESISTANCE OF THE RESIDUE
C       U2 = WIND SPEED AT 2 M ABOVE RESIDE LAYER (m/s)
C       RSR  = RESISTANCE TO EVAPORATION THROUGH RESIDUE (s/m)
C       RESP = RESIDUE POROSITY
C
        RESP=1.0D0-RHORB/RHORS(IPR)
        IF(RESP.LE.0.5D0.OR.RESP.GT.0.95D0) THEN
          RESP=0.8D0
        ENDIF
C
        U2=US*LOG(2.0D0/Z0R)/(LOG(XWNEW/Z0R))
        RSR=1.10D0*HRM/(2.12D-5*(1.D0+0.007D0*max(0.0d0,TA-20.0D0))*
     +      (1.0D0+(1.25D-3*RHORB**(-1.79D0))*U2)*RESP)
      ELSE
        RSR=0.0D0
        Z0R=0.0D0
        CS=1.0D0
      ENDIF
C
C     .. EVAPORATIVE RESISTANCE OF SOIL SURFACE
C     From Jackson (1973), for saturated soil.
C

c rma
C     H. Farahani development of soil resistance for dust mulch
C     in surface soil layer
C     compute thickness of dust mulch from surface layer water content
          tdml = 0.01d0*(1.d0-theta/wcsat)
C     compute resistance from molecular diffusivity through dust mulch
         if (rss.lt.0.0d0) then 
              rss0=rss
         else if (iifirst.eq.0) then
              rss1=rss
              iifirst=1
         endif
c
         if (int(rss0).eq.-1) then 
c            rss = (tdml*tau)/(De*wcsat)  ! do not know where from
C  the following original has: rss=(1-fsnow)*exp(8.206-4.255*theta/wcsat), doi:10.1029/2008JD010834
             rss=exp(8.206-4.255*theta/wcsat)  !Sakaguchi and Zeng, J. Geophsical Res. 114: D01107, 2009
         else if (int(rss0).eq.-2) then
             rss=39.0d0*(theta/wcsat)**(-2.59)  !Farahani and Bausch, 1995, Trans. ASAE
         endif
      IF (PPLASTIC.LT.1.0D0) THEN
      if (rss1.gt.0.0d0) then
      RSS=RSS1/(1.0D0-PPLASTIC)
      else
      RSS=RSS/(1.0D0-PPLASTIC)
      endif
      RSR=RSR/(1.0D0-PPLASTIC)
      ELSE
      RSS=1.0D30
      RSR=1.0D30
      ENDIF
c     commented out from RZWQM  Liwang Ma, please check, 3-22-2009
c      if ((ishaw.ne.1).and.(ipenflux.ne.1))  RSS=37.0D0
c       RSS=37.0D0
c      RSS=200.0D0
      IF(LAI.EQ.0.0D0.OR.PLHT.EQ.0.0D0) THEN
        Z0=MAX(Z0P,Z0R)
        d=0.0d0
        RAS=LOG(XWNEW/Z0)*LOG((XWNEW/2.0D0)/Z0)*DK
        RAA=(LOG(XWNEW/Z0))**2.0D0*DK-RAS
      ENDIF
C
C  the following are for PENFLUX only.
C     ===::> BARE SOIL W/O RESIDUE (NO CROP)

      IF (RM .GT. 0.0D0) THEN
C
C     Compute aerodynamic resistance within residue layer using modified
C     Tanner and Shen function (SSSAJ (1990) 54:945-951)
C
        ah0 = De/hrm
c       ..compute windspeed 10 mm above residue
        ustar = us*k/(log((xwnew+z0-d)/z0))
c       ..check canopy conditions
c rma
        if (lai.gt.0.0d0 .and. plht.gt.0.0d0 .or. sai .gt. 0.d0) then
          awind = min(lai+sai,4.0d0)
          windch = (ustar/k)*log((ah+z0-d)/z0)
c compare this form with GF code
          windrh = windch * exp(-awind)
          windr1 = windrh
        else
          windrh = (ustar/k)*log((hrm+z0-d)/z0)
          windr1 = (ustar/k)*log((hrm+0.01d0)/z0)
        endif
        rarlayer = 1.d0 /(ah0*(1.0d0+cmult*windr1))
C
C     Compute boundary layer resistance for residue elements using 
C     Campbell (1977) p. 67
C     Note: rdia converted from cm to m
C
        rdiam = rdia(ir)*0.01d0
        rbr = 307.d0*(rdiam/windrh)**0.5d0
C
C     Compute thermal conductivity of flat residue using Shen and Tanner
C       SSSAJ54:653-658 (1990) considering volume fractions, thermal
C       conductivities and heat capacities of air and water in residue
C
c        thetar = 0.1d0
c        Fwat = thetar*rhorb*1.0D3/rhol
c        Fair = 1.d0 - Fwat
c        Tcondmix = Tconw*Tcona/(Fwat*Tcona + Fair*Tconw)
c        TCmix = Tcondmix / hrm
c        rhocpmix = Fwat*rhocpw + Fair*rhocpa
cC       ..convert from W/m2 K to s/m using heat capacity of residue layer
c        rkr = rhocpmix / TCmix
C
C       Combine convective and conductive resistances
C
c        rcr = (rkr * rarlayer)/(rkr + rarlayer)
c
C     Compute aerodynamic resistance within residue layer using modified
C     Tanner and Shen function (SSSAJ (1990) 54:945-951)
C     Note: use half height of residue layer for molecular diffusion
c rma	
          ah0 = De/(hrm*0.5d0)
          rcr = 1.d0 /(ah0*(1.0d0+slope1*windr1))
c rma
          starsoil = (rcr + rss)/rcr
      ELSE
        RBR = 1.0D30
        RCR = RAA + RAS
c rma
        starsoil = (raa + rss)/raa
      ENDIF
C
      RETURN
      END
C
      FUNCTION SWSUN(FSUN,RCS)
C
C======================================================================
C
C       PURPOSE: ESTIMATE DAILY TOTAL S-W RAD FROM SUNSHINE FRACTION.
C
C       REF: SCHAFFER & LARSON, NTRM PROGRAM.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       FDEF   P  SUNSHINE FRACTION DEFAULT
C       FSUN   I  SUNSHINE FRACTION
C       RCS        I  DAILY TOTAL CLEAR-SKY RAD (DIR & DIFFUSE) ON
C                 SLOPING SURFACE [MJ/M^2/DAY].
C       RTS        L  DAILY ACTUAL TOTAL S-W RAD AT CANOPY CORRECTED
C                 FOR FRACTION [MJ/M^2/DAY]
C       SWSUN  O  DAILY ACTUAL TOTAL S-W RAD AT CANOPY
C                 [MJ/M^2/DAY]
C
C
C       CALLED FROM:  POTEVP
C
C       PROGRAMMER:   KEN ROJAS
C
C       VERSION:      1.0
C
C======================================================================
C
C     ..DECLARATION STATEMENTS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     ..SET SUNSHINE DEFAULT VALUE.
      PARAMETER(FDEF=0.8D0)
C
      IF(FSUN.GE.0.0.AND.FSUN.LE.1.0) THEN
        RTS=FSUN*RCS
      ELSE
        RTS=FDEF*RCS
      ENDIF
      SWSUN=RTS
C
      RETURN
      END
C
      FUNCTION SNOWQE(UNEW,TMAX,TMIN,PKTEMP,XLH,RH)
C
C======================================================================
C
C       PURPOSE:  THIS ROUTINE WILL DETERMINE THE DAILY LATENT HEAT FLUX
C             FROM THE SNOW SURFACE (EVAPORATION POTENTIAL). THIS
C             VALUE IS THEN MODIFIED BY THE SNOW ROUTINE TO DETERMINE
C             THE SUBLIMATION AMOUNT.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       CONV3  P  CONVERTS KPA ==> MB.
C       CONV4  P  CONVERTS KJ/M2/S ==> MJ/M2/DAY.
C       CONVW  P  CONVERTS KM/DAY ==> M/S WIND SPEED.
C       EAIR   L  AVERAGE SAT VAPOR PRESSURE IN THE AIR (MB)
C       ESNOW  L  AVERAGE SAT VAPOR PRESSURE AT THE SNOWPACK(MB)
C       QE         L  LATENT HEAT FLUX IN WATER EQUILIVALENCE (CM/DAY)
C       RH         I  RELATIVE HUMIDITY
C       SNOWQE     O  LATENT HEAT FLUX IN WATER EQUILIVALENCE (CM/DAY)
C       XLH        I  LATENT HEAT OF VAPORIZATION (MJ/KG)
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                 NONE
C
C       CALLED FROM:  POTEVP
C
C       PROGRAMMER:   KEN ROJAS
C
C       VERSION:      3.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(CONV3=0.1D0,CONVW=1.0D3/(24.0D0*60.0D0*60.D0),DE=2.17D0,
     +    CONV4=86.4D0)
C
C     ..STATEMENT FUNCTION FOR SATURATED VAPOR PRESSURE CURVE
      EO(T)=EXP((16.78D0*T-116.9D0)/(T+237.3D0))
C
C     ..DETERMINE Qe (LATENT HEAT FLUX) FOR SNOW SURFACE
      EA=(EO(TMAX)+EO(TMIN))*0.5D0
      EAIR=EA*CONV3*RH*1.0D-2
      ESNOW=EO(PKTEMP)*CONV3
c      if (ihourly.eq.1) then
c	UZ=Unew
c	else
      UZ=UNEW*CONVW
c	endif
      QE=DE*UZ*(EAIR-ESNOW)/(XLH*10.0D0)*CONV4
      SNOWQE=MAX(-QE,0.0D0)
C      PRINT*,QE,SNOWQE
C
      RETURN
      END
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     rma
c     jak  LIWANG MA, RZ-PENFLUX, JUNE 2008
c     substituted R. Aiken's revised version
      subroutine penflux(cshslab,delt,rh,pfirst,h1,h2,far,fac,psy,raa,
     +  rcr,rbr,ras,rsc,rss,rhoa,Rsns,Rsnr,Rsnc,thermk,ta,tm,tz1,tzlb,
     +  znlb,tltlb,Gfl,Eflux,Htflux,Rlns,Rlnr,Rlnc,clouds,esmin,
     +  starsoil)
C===========================================================================
C
c     Purpose: solves energy balance equation for soil, residue, and canopy 
c           surface temperatures,
C           then passes Gflux to SHUTTL and SHAWHT
C
C     REF:  Bristow (1987) Ag. and For. Meteor. 39:49-54
C     Luo et al. (1992) Ag. and For. Meteor. 61:23-38
C           Flerchinger (1987) Ph.D. Dissertation
C     Aiken et al. (1997) Agronomy J. 89:404-415.
C     Variable Definitions 
C     Variable  I/O Description
C     --------  --- -----------
c     cshslab       Volumetric heat capacity of soil
C     cpa           Heat capacity of air (J/kg K)
C     delt          Time step (s)
C     zs1           Depth of second soil node (m)
C     ed            Vapor pressure air at dewpoint temperature (kPa)
C     esmin         supply-limiting soil evaporation (W/m2)
C     ermin         supply-limiting residue evaporation (W/m2)
c     far           fraction of residue cover
c     fac           fraction of canopy cover
C     G             Gravitational force (m2/s)
C     H1            Soil water pressure head (m)
c     H2            residue water pressure head (m)
C     H2OMw         Molecular weight of water (kg/M) 
C     PSY        L  PSYCHROMETRIC CONSTANT (KPA/C)
C     R             Universal gas constant (J/mole K)
C     raa           Convective resistance (s/m)
c     ras           aerodynamic resistance below zero displacement plane
c     rbr           boundary layer resistance within flat residue layer
c     rcr=rhr       thermal convective resistance within residue layer           
c     rh            Relative humidity in air
C     rhoa          Density of air (kg/m3)
C     rhocpa        Volumetric heat capacity of air (J/m3 K)
c     rsc           resistance in canopy?
c     rss           soil dust mulch internal resistance to vapor flux
C     csh           Volumetric heat capacity of soil (J/m3 K)
C     rh            Relative humidity (%)
C     Rnl           Net long-wave radiation for layer (J/m2 s)
C     Rns           Net short-wave radiation for layer (J/m2 s)
C     RSNS          NET SHORTWAVE IRRADIANCE FOR SURFACE SOIL LAYER W m-2
C     RSNR          NET SHORTWAVE IRRADIANCE FOR FLAT RESIDUE LAYER W m-2
C     RSNC          NET SHORTWAVE IRRADIANCE FOR CROP CANOPY LAYER W m-2
C     sbt           Stephan-Boltzman constant (J/m2 K4 s)
C     Ta            Air temperature (2 m height, C)
c     tz1           temperatuer in the first soil layer 
C     thermk        Soil thermal conductivity (W/m K)
C     toler         Iteration tolerance criterion (C)
C     Tso           Soil temperature at time = t (C)
C     Tm(1)         Soil temperature at time = t + delt (C)
C     Tmz2          Soil temperature at current time step, node 2 (C)
C
C     =================================================================
c
       Implicit double precision (a-h,o-z)
c       
       Parameter (sbt=5.67d-8, e1=54.8781919d0, e2=6.7904985d3, 
     +    e3=5.02808d0, H2OMw=0.018d0, G=9.81d0, R=8.3143d0,
     +    toler=0.001d0, n=3, maxiter=100, cpa=1.013d3,delTa=2.0d0)
c                                                                
       dimension tm(5),tmk(5),svp(5),svpd1(5),svpd2(5),eps(5),Rl(4),
     +    dRl(4),dele(5),hum(n+1),rhs(n),djac(n,n),sav(n,n),y(n),
     +    Htflux(n),Eflux(n),head(2)
C
       logical pfirst, first, iconverg, erlim, eslim
       save tmzlbo,tmo
c
c     Initialize local variables
c     Temperature and Vapor Pressure Indices
c        1 -> soil
c        2 -> residue
c        3 -> air
c        4 -> canopy
c        5 -> air in residue
c
      if (pfirst) then
          tm(1) = tz1
          tm(2) = ta
          tmzlbo = tzlb
          deltsp = delt*3.6d3
          pfirst=.false.
      else
          tm(1) = tz1
      endif
C
C     set up predictor-corrector for lower boundary temperature
      delts = delt*3.6d3
c      tmzlb = tzlb + (tzlb-tmzlbo)/delts
      tmzlbpc = tzlb + delts*(tzlb-tmzlbo)/deltsp
      deltsp = delts
      tmzlbo = tzlb
c      tmzlb = tzlb
      tmzlb = tmzlbpc
      iconverg = .false.
      erlim = .true.
      eslim = .false.
      ermin = 0.0d0
      rhocpa=rhoa*cpa
      znmlb = 1.0d-2*znlb
      tltmlb = 1.0d-2*tltlb
      head(1) = h1*.01d0
      head(2) = h2*.01d0
      tm(3) = ta
      tm(4) = tm(3)
      tmo = tm(1)
      omfar = 1.0d0 - far
      omfac = 1.0d0 - fac
c      rbr = rcr
c
c     Newton-Rhapson loop
c
      do 500 k = 1,maxiter
c  
C     use resistance terms to compute weighted average air temperature 
C     at residue height
C
        wresist1 = (1/rcr + 1/rbr + 1/(raa+ras))
      if (far .gt. 0.d0) then
c     surface residues present, compute temperatures and vapor pressures
c        wresist2 = rbr/(raa+ras)
         wresist2 = (1/(rss + rcr) + 1/(ras + raa))
        tm(5) = (tm(1)/rcr + tm(2)/rbr + tm(3)/(raa+ras))/wresist1
      else
c     surface residues absent, set to ambient conditions
        tm(5) = tm(3)
      endif
C
c       Convert temperatures to Kelvin
c
        do 10 i=1,5
          tmk(i) = tm(i) + 273.16d0
10      continue
c
c       Calculate vapor pressure and derivatives, emissivity
c
        do 20 i=1,5
          tm1 = tmk(i)
          tm2 = tmk(i)*tmk(i)
          tm3 = tmk(i)*tmk(i)*tmk(i)
          svp(i) = 0.1d0*exp(e1 - e2/tm1 - e3*log(tm1))
                    svpd1(i) = svp(i)*(e2/tm2 - e3/tm1)
          svpd2(i) = svpd1(i)*(e2/tm2 - e3/tm1) +
     1               svp(i)*(-2.0d0*e2/tm3 + e3/tm2)
c          eps(i) = 1.0d0
C       soil emissivity based on Salisbury and D'Aria (1992)
C       Remote Sens. Environ. 42:157-165 data on Mollisols
          eps(i) = 0.96d0
20      continue
        svp(2) = (svp(1)+svp(2))/2.0d0
        svpd1(2) = (svp(2)-svp(1))/(tmk(2)-tmk(1))
        svpd2(2) = (svpd2(2)+svpd2(1))/2.0d0
        epsa = 1.0d0 - 0.261d0*exp(-7.77d-4*tm(3)*tm(3))
C
C       adjust for cloud cover Campbell (1977) p. 58  R.A. 6/30/94
C
        eps(3) = epsa + clouds*(1.0d0 - epsa - 4.0d0*delTa/tmk(3))
c
        do 25 i=1,2
          hum(i) = exp(h2omw*head(i)*g/(r*tmk(i)))
25      continue
c
        hum(3) = rh
        ea = hum(3)*svp(3)
        es = hum(1)*svp(1)
      if (far .gt. 0.d0) then
c     surface residues present, compute temperatures and vapor pressures
         ear = (es/(rss+rcr) + ea/(ras+raa))/wresist2
c        ear = (wresist2*ea + es)/(1+wresist2)
      else
c     surface residues absent, set to ambient conditions
        ear = ea
      endif
c
        do 30 i=1,3
          dele(i) = (1.0d0-hum(i))*svp(i)
30      continue
        dele(2) = max(0.d0,svp(2)-hum(3)*svp(3))
        if (far .eq. 0.d0) dele(2) = dele(3)
        dele(5) = max(0.d0,svp(5) - ear)
c       Longwave radiation and derivatives
c  
        do 40 i=1,4
          Rl(i) = eps(i)*sbt*(tmk(i)**4.0d0)
          dRl(i) = 4.0d0*eps(i)*sbt*(tmk(i)**3.0d0)
40      continue
c
c       Net longwave radiation and derivatives wrt soil, residue, canopy
c
        Rlns = omfac*omfar*Rl(3) + (fac-far*fac)*Rl(4) +
     1         far*Rl(2) - Rl(1)
        dRlnss = -1.0d0 * dRl(1)
        dRlnsr = far * dRl(2)
        dRlnsc = (fac-far*fac) * dRl(4)
c
        Rlnr = (omfac*Rl(3) + fac*Rl(4) + Rl(1) - 2.0d0*Rl(2))*far
        dRlnrs = far * dRl(1)
        dRlnrr = -2.0d0*far * dRl(2)
        dRlnrc = fac*far * dRl(4)
c
        Rlnc = (Rl(3) + Rl(2)*far + Rl(1)*omfar - 2.0d0*Rl(4))*fac
        dRlncs = omfar*fac * dRl(1)
        dRlncr = far*fac * dRl(2)
        dRlncc = -2.0d0*fac * dRl(4)
c
c       Gflux and derivative wrt soil
c
c        Gfl = thermk*(tm(1)-tmzlb)/znmlb + 
c     1        (tm(1)-tmo)*cshslab*tltmlb/delts
c        dGfl = thermk/znmlb + cshslab*tltmlb/delts
        Gflstor = cshslab*tltmlb*(Tm(1)-Tmo)/delts
        Gflgrad = thermk*(Tm(1)-Tmzlb)/znmlb 
        Gfl = Gflstor + Gflgrad
        dGfl = thermk/znmlb + cshslab*tltmlb/delts
        if (thermk.eq.0.0d0) then
        tmzlbgfl = tm(1)
         else
        tmzlbgfl = tm(1)-(znmlb/thermk)*(Gfl-Gflstor)
        endif
c       activate average of predictor-corrector and iterated soil slab
C       lower boundary temperature
c        tmzlb = 0.5d0*(tmzlbpc+tmzlbgfl)
C       activate iteration for soil slab lower boundary temperature
         tmzlb = tmzlbgfl
c
c       Set up right hand side vector with temperature functions
c        1 -> soil
c        2 -> residue
c        3 -> canopy
c       Use temporary variables for calculations so they don't
c        have to be repeated in Jacobian calculations
c
      if (eslim) then
c     set up energy balance with supply-limiting evaporation form
        srns = Rsns + Rlns - Gfl - esmin
        fracs = (rcr)/rhocpa
         rhs(1) = srns*fracs + Tm(5) - Tm(1)
c        rhs(1) = srns*fracs + Tm(2) - Tm(1)
      else
c     set up energy balance with energy-limiting evaporation form 
        srns = Rsns + Rlns - Gfl
c        sds = svpd1(1) + psy
c        sds = svpd1(5) + psy
C       starsoil adjusts for differance in vapor and heat diffusion
         sds = svpd1(5) + psy*starsoil
         fracs = rcr*psy*starsoil/rhocpa
c        fracs = rcr*psy/rhocpa
        rhs(1) = srns*fracs/sds + Tm(5) - Tm(1)
     1      - (dele(5)-dele(1))/sds 
      
        pens1=srns/sds   
        pens2=(dele(5)-dele(1))*rhocpa/(sds*rcr)
        Eflux(1) = svpd1(5)*pens1+pens2
        if (eflux(1) .gt. esmin) eslim = .true.
      endif
c
      if (erlim) then
c     set up energy balance with supply-limiting evaporation form
        srnr = Rsnr + Rlnr - ermin
C     use boundary layer resistance
        fracr = (rbr)/rhocpa
c       fracr = (ras+raa)/rhocpa
        rhs(2) = srnr*fracr + Tm(5) - Tm(2)
      else
c     set up energy balance with energy-limiting evaporation form 
        srnr = Rsnr+Rlnr
        sdr = svpd1(2) + psy
        fracr = (rbr)*psy/rhocpa
c       fracr = (ras+raa)*psy/rhocpa
        rhs(2) = srnr*fracr/sdr - (dele(5)-dele(2))/sdr + 
     1           Tm(5) - Tm(2)
        penr1=srnr/sdr
        penr2=(dele(3)-dele(2))*psy/(fracr*sdr)
        Eflux(2) = svpd1(2)*penr1+penr2
        if (eflux(2) .gt. ermin) erlim = .true.
      endif                         
c
        srnc = Rsnc+Rlnc
        psystar =  psy*(1.0d0+(rsc/raa))
        sdc = svpd1(4) + psystar
        fracc = raa*psy/rhocpa
        rhs(3) = srnc*fracc/sdc - dele(3)/sdc + Tm(3) - Tm(4)
C
        if (iconverg) goto 999
c             
c       Set up Jacobian matrix
c
      if (eslim) then
c       set up energy balance partials with supply-limiting evaporation form
        djac(1,1) = -1.0d0 + fracs*(dRlnss-dGfl) 
        djac(1,2) = fracs*dRlnsr + 1.0d0
        djac(1,3) = fracs*dRlnsc
      else
c       set up energy balance partials with energy-limiting evaporation form 
        djac(1,1) = -1.0d0 + fracs*(dRlnss-dGfl)/sds 
     1            - svpd2(5)*fracs*srns/(sds*sds) 
     2            + (dele(5)-dele(1))*svpd2(5)/(sds*sds) 
     3            + (1.0d0-hum(1))*svpd1(5)/sds
        djac(1,2) = fracs*dRlnsr/sds + 1.d0/rbr/wresist1
        djac(1,3) = fracs*dRlnsc/sds
      endif
c
      if (erlim) then
c       set up energy balance partials with supply-limiting evaporation form
        djac(2,1) = fracr*dRlnrs
        djac(2,2) = -1.0d0 + fracr*dRlnrr
        djac(2,3) = fracr*dRlnrc
      else
c       set up energy balance partials with energy-limiting evaporation form 
        djac(2,1) = fracr*dRlnrs/sdr
        djac(2,2) = -1.0d0 + fracr*dRlnrr/sdr 
     1            - svpd2(2)*fracr*srnr/(sdr*sdr) 
     2            + (dele(5)-dele(2))*svpd2(2)/(sdr*sdr) 
     3            + (1.0d0-hum(2))*svpd1(2)/sdr
        djac(2,3) = fracr*dRlnrc/sdr
      endif
c
        djac(3,1) = fracc*dRlncs/sdc
        djac(3,2) = fracc*dRlncr/sdc
        djac(3,3) = -1.0d0 + fracc*dRlncc/sdc 
     1            + (dele(3) - fracc*srnc)*svpd2(4)/(sdc*sdc)
c
c       Gaussian Elimination
c       Forward elimination
c
        do 100 i=1,n-1
          l = 99
          do 50 j=i,n
            if (djac(j,i).ne.0.0d0) l=min(l,j)
50        continue
          if (l.eq.99) then
            stop'no unique solution to penman-type eqns. exists'
          else if (l.ne.i) then
            do 60 j=1,n
              sav(i,j) = djac(i,j)
              djac(i,j) = djac(l,j)
              djac(l,j) = sav(i,j)
60          continue
            sv = rhs(i)
            rhs(i) = rhs(l)
            rhs(l) = rhs(i)
          endif
          do 70 j=i+1,n
            dm = djac(j,i)/djac(i,i)
            do 65 l=1,n
              djac(j,l) = djac(j,l) - dm*djac(i,l)
65          continue
            rhs(j) = rhs(j) - dm*rhs(i)
70        continue
100     continue
        if (djac(n,n).eq.0.0d0) then
          stop'no unique solution to penman-type eqns. exists'
        endif
c
c       Backward substitution
c
        y(n) = rhs(n)/djac(n,n)
        do 120 i=n-1,1,-1
          sum = 0.0d0
          do 110 j=i+1,n
            sum = sum+djac(i,j)*y(j)
110       continue
          y(i) = (rhs(i) - sum)/djac(i,i)
120     continue
c
c       Update temperatures
c
        tm(1) = tm(1) - y(1)
        tm(2) = tm(2) - y(2)
        tm(4) = tm(4) - y(3)
c        print*,k,(tm(i),i=1,4),ta
c
c       Find norm of y and check against tolerence; exit if appropriate
c
        sum = 0.0d0
        do 130 i=1,n
          sum = y(i)*y(i) + sum
130     continue
        dnorm = sqrt(sum)
        if (dnorm.le.toler) iconverg = .true.
c
c       Continue Newton's Iterations
c
500    continue
C
c      compute sensible and latent heat fluxes
C
999    continue
C
c     update soil temperature at lower depth if not updated in SHAWHT
c      if (tmz2o .eq. tmz2) then 
c         tmz2 = tm(1) - zslb/thermk * (Gfl -
c     +      (tm(1) - tmo) * csh1 * zslb)/(2 * delts)
c      endif
c      tmz2o = tmz2
c
      if (eslim) then
        htflux(1) = (tm(1) - tm(5))/fracs
c        htflux(1) = (tm(1) - tm(2))/fracs
        eflux(1) = esmin
      else
        pens1=srns/sds
        pens2=(dele(5)-dele(1))*rhocpa/(sds*rcr)
        Htflux(1)  = psy*starsoil*pens1-pens2
        Eflux(1) = svpd1(5)*pens1+pens2
      endif
c        
      if (erlim) then
        htflux(2) = (tm(2) - tm(5))/fracr
        eflux(2) = ermin
      else
        penr1=srnr/sdr
        penr2=(dele(5)-dele(2))*psy/(fracr*sdr)
        Htflux(2)  = psy*penr1-penr2
        Eflux(2) = svpd1(2)*penr1+penr2
      endif
c        
        penc1=srnc/sdc
        penc2=rhocpa*dele(3)/(raa*sdc)
        Htflux(3)  = psystar*penc1-penc2
        Eflux(3) = svpd1(4)*penc1+penc2
c
      end
c
      subroutine soilthrm(pfirst,fracom,ice,soilpp2,soilpp3,soilpp5,
     +  theta,csh,thermk)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
C 
C     SUBROUTINE HEAT CALCULATES THE THERMAL CONDUCTIVITY (LAMBDA) 
C     AND VOLUMETRIC HEAT CAPACITY (HCAP) FROM BULK DENSITY AND 
C     VOLUMETRIC WATER CONTENT. 
C 
C     BASED ON de Vries, D. A. 1966. Thermal properties of soils 
C     p.210-235. in W. R. van Wijk (ed.) Physics of plant environment. 2nd
C     ed. North-Holland, Amsterdam.
C
C     Shape factors (Gx) based on Milly, P.C.D.  "A simulation analysis
C     of thermal effects on evaporation from soil. Water Res. Res. 
C     20(8):1087-1098
C
C     by Joe Benjamin., modification by Rob Aiken 3/24/94
C     replaced by G. Flerchinger routines SOILTK and SOILHT
C     UNITS ARE g, J, m, s   lambda (J/msK)  Hcap (J/m3K)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        Implicit double precision (a-k,o-z)
        double precision LAMbDAA,LAMbDAQ,LAMbDAW,LAMbDAO,LAMbDAC,
     +    NCOEF,lambdai
        logical pfirst
        save kc, ko, kq, vc, vo, vq, vv, ratioa
        parameter (ncoef=0.46d0, GQ=0.125d0, GC=0.125d0, GO=0.5d0,
     +  LAMbDAA=0.025d0, LAMbDAQ=8.786d0, LAMbDAW=0.573d0, 
     +  LAMbDAO=0.25d0, LAMbDAC=2.929d0, lambdai=2.18d0, onet=1.d0/3.d0,
     +  twot=2.d0/3.d0)
c       
c       initialize invariate soil thermal properties
      if (pfirst) then
        PD = soilpp2
        bd=soilpp3
        pcq=soilpp5
        pco=fracom
c        
c       thermal conductivity of substance relative to water         
        RATIOQ = LAMbDAQ/LAMbDAW
        RATIOA = LAMbDAA/LAMbDAW
        RATIOO = LAMbDAO/LAMbDAW
        RATIOC = LAMbDAC/LAMbDAW
c
c       volume fraction of invariate soil properties
         VS = BD/PD
         VQ = VS*PCQ
         VO = VS*PCO*(pd/1.3d0)
         VC = VS-VQ-VO
         VV = 1.0d0-VS
c
c       weighting factors based on ellipsoid 'shape' ref: Milly
        KQ = twot*(1.d0/(1.d0+(RATIOQ-1.d0)*GQ)) +  
     +        onet*(1.d0/(1.d0+(RATIOQ-1.d0)*(1.d0-2.d0*GQ)))
c     
        KO = twot*(1.d0/(1.d0+(RATIOO-1.d0)*GO)) +  
     +        onet*(1.d0/(1.d0+(RATIOO-1.d0)*(1.d0-2.d0*GO)))
c    
        KC = twot*(1.d0/(1.d0+(RATIOC-1.d0)*GC)) +
     +        onet*(1.d0/(1.d0+(RATIOC-1.d0)*(1.d0-2.d0*GC)))
c   
      endif
c       compute variate properties, functions of soil water and ice content
        GA = 0.035d0 + (0.298d0/NCOEF)*(THETA+ice)
        Va = VV-(THETA+ice)         
      IF (Va .LE. 0.0d0) THEN
        Va = 0.0d0
      END IF
c
        KA = twot*(1.d0/(1.d0+(RATIOA-1.d0)*GA)) + 
     +        onet*(1.d0/(1.d0+(RATIOA-1.d0)*(1.d0-2.d0*GA)))
c         
c       compute thermal propertis for soil node
        thermk = ((KQ*VQ*LAMbDAQ+KA*Va*LAMbDAA+KO*VO*LAMbDAO
     +            +KC*VC*LAMbDAC+THETA*LAMbDAW+ice*lambdai)/
     +            (KQ*VQ+KA*Va+KO*VO+KC*VC+THETA+ice))
        csh = (2.01d6*(VQ+VC) + 2.51d6*VO + 4.184d6*THETA + 1.73d6*ice)    
c
      RETURN
      END 
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C***********************************************************************
C
      SUBROUTINE SOLAR_SHAW_new(DIRECT,DIFFUS,SUNSLP,ALTITU,SUNHOR,
     >                  HOUR,ALATUD,SLOPE,ASPECT,julian,NHRPDT)
C
C     THIS SUBROUTINE SEPARATES THE TOTAL RADIATION MEASURED ON THE
C     HORIZONTAL INTO THE DIRECT AND DIFFUSE ON THE LOCAL SLOPE.
C     if run daily, Hour=24 and NHRPDT=24
C***********************************************************************
c
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER THOUR,HOUR
C
      PARAMETER (SOLCON=1360.D0, HRNOON=12.0D0, DIFATM=0.76D0) !,NHRPDT=1
C===== section from cloudy_shaw
      DECLIN=0.4102D0*SIN(2*3.14159D0*(JULIAN-80)/365.0D0)
      COSHAF=-TAN(ALATUD)*TAN(DECLIN)
      IF (ABS(COSHAF) .GE. 1.0D0) THEN
         IF (COSHAF .GE. 1.0D0) THEN
C           SUN DOES NOT COME UP ON THIS DAY (WINTER IN ARCTIC CIRCLE)
            HAFDAY=0.0D0
          ELSE
C           SUN DOES NOT SET ON THIS DAY (SUMMER IN THE ARCTIC CIRCLE)
            HAFDAY=2*3.14159D0
         END IF
       ELSE
         HAFDAY=ACOS(COSHAF)
      END IF
      SUNMAX_day=24.0D0*SOLCON*(HAFDAY*SIN(ALATUD)*SIN(DECLIN)
     >                +COS(ALATUD)*COS(DECLIN)*SIN(HAFDAY))/3.14159D0
c      sunmax_day=sunmax_day*3.6d3/1.d6
C===== section from cloudy_shaw
c
C**** CHECK IF SUN HAS RISEN YET (OR IF IT HAS ALREADY SET)
      IF (SUNHOR .LE. 0.0d0) THEN
         DIRECT=0.0d0
         DIFFUS=0.0d0
         sunmax=0.0d0
         RETURN
      END IF
      SUNRIS=HRNOON - HAFDAY/0.261799d0
      SUNSET=HRNOON + HAFDAY/0.261799d0
C
C**** CALCULATE HOUR ANGLE AT WHICH THE SUN WILL BE DUE EAST/WEST IN
C     ORDER TO ADJUST AZIMUTH ANGLE FOR SOUTHERN AZIMUTHS
C     -- SIN(AZIMUTH) TELLS YOU ONLY THE EAST/WEST DIRECTION - NOT
C     WHETHER THE SUN IS NORTH/SOUTH.
      IF (ABS(DECLIN).GE.ABS(ALATUD)) THEN
C        LATITUDE IS WITHIN THE TROPICS (EQUATION WON'T WORK)
         HRWEST=3.14159D0
        ELSE
         HRWEST=ACOS(TAN(DECLIN)/TAN(ALATUD))
      END IF
C
C**** SUM UP VALUES AND FIND AVERAGE SUN POSITION FOR TIME STEP
      SINAZM=0.0d0
      COSAZM=0.0D0
      SUMALT=0.0D0
      COSALT=0.0D0
      SUNMAX=0.0d0
C      NHRPDT=NINT(DT/3600.D0)
C      IF (NHRPDT .LE. 0) NHRPDT=1
C     note: convert dtime from sec to hr  RMA: 1/28/94
C      DTIME=DT/(NHRPDT*3600.d0)
      DO 10 IHR=HOUR-NHRPDT, HOUR
         THOUR=IHR
C****    DETERMINE THE GEOMETRY OF THE SUN'S RAYS AT CURRENT TIME
         HRANGL=0.261799d0*(THOUR-HRNOON)
C
         IF (THOUR .GT. SUNRIS  .AND.  THOUR .LT. SUNSET) THEN
C           SUN IS ABOVE HORIZON -- CALCULATE ITS ALTITUDE ABOVE THE
C           HORIZON (ALTITU) AND ANGLE FROM DUE NORTH (AZMUTH)
            SINALT=SIN(ALATUD)*SIN(DECLIN)
     >             + COS(ALATUD)*COS(DECLIN)*COS(HRANGL)
            ALTITU=ASIN(SINALT)
            AZM = ASIN(-COS(DECLIN)*SIN(HRANGL)/COS(ALTITU))
C           CORRECT AZIMUTH FOR SOUTHERN ANGLES
            IF (ALATUD-DECLIN .GT. 0.0D0) THEN
C              NORTHERN LATITUDES   (HRANGL=0.0 AT NOON)
               IF (ABS(HRANGL).LT.HRWEST) AZM=3.14159D0-AZM
              ELSE
C              SOUTHERN LATITUDES
               IF (ABS(HRANGL).GE.HRWEST) AZM=3.14159D0-AZM
            END IF
C           SUM CONDITIONS TO GET AVERAGE ALTITUDE AND AZMUTH
C           (OBTAIN AVERAGE BY SUMMING VECTOR COMPONENTS)
            SUN=SOLCON*SINALT
            SUMALT=SUMALT+SUN*SINALT
            COSALT=COSALT+SUN*COS(ALTITU)
            SINAZM=SINAZM+SUN*SIN(AZM)
            COSAZM=COSAZM+SUN*COS(AZM)
            SUNMAX=SUNMAX+SUN
         END IF
C
   10 CONTINUE
C
C**** DETERMINE AVERAGE SOLAR RADIATION, AVERAGE ALTITUDE AND AZIMUTH OF
C     THE SUN AND ANGLE ON LOCAL SLOPE
      IF (SUNMAX .EQ. 0) THEN
         ALTITU=0.0d0
         SUNSLP=0.0d0
        ELSE
         ALTITU=ATAN(SUMALT/COSALT)
         AZMUTH=ATAN2(SINAZM,COSAZM)
         SUNMAX=SUNMAX/(NHRPDT+1)
         SUNSLP=ASIN( SIN(ALTITU)*COS(SLOPE)
     >           + COS(ALTITU)*SIN(SLOPE)*COS(AZMUTH-ASPECT))
      END IF
C
C**** SEPARATE THE SOLAR RADIATION INTO DIRECT AND DIFFUSE COMPONENTS
      IF (ALTITU .LE. 0.0d0) THEN
C     SUN IS BELOW THE HORIZON - ALL RADIATION MUST BE DIFFUSE
         DIFFUS=SUNHOR
         DIRECT=0.0d0
         RETURN
      END IF
      TTOTAL=SUNHOR/SUNMAX
C     LIMIT TOTAL TRANSMISSIVITY TO MAXIMUM (DIFATM) WHICH WILL
C     CAUSE TDIFFU TO BE 0.0
      IF (TTOTAL .GT. DIFATM) TTOTAL = DIFATM
      TDIFFU=TTOTAL*(1.d0-EXP(0.6d0*(1.d0-DIFATM/TTOTAL)
     +    /(DIFATM-0.4d0)))
      DIFFUS=TDIFFU*SUNMAX
      DIRHOR=SUNHOR-DIFFUS
C
C**** NOW CALCULATE THE DIRECT SOLAR RADIATION ON THE LOCAL SLOPE
      IF (SUNSLP .LE. 0.0d0) THEN
C        SUN HAS NOT RISEN ON THE LOCAL SLOPE -- NO DIRECT RADIATION
         DIRECT=0.0d0
       ELSE
         DIRECT=DIRHOR*SIN(SUNSLP)/SIN(ALTITU)
C        IF THE SUN'S ALTITUDE IS NEAR ZERO, THE CALCULATED DIRECT
C        RADIATION ON THE SLOPING SURFACE MAY BE UNREALISTICALLY LARGE --
C        LIMIT DIRECT TO 5*DIRHOR (THIS IS APPROXIMATELY THE CASE WHEN
C        THE SUN IS 10 DEGREES ABOVE THE HORIZON AND THE SLOPING SURFACE
C        IS PERPENDICULAR TO THE SUN`S RAYS
         IF (DIRECT .GT. 5.d0*DIRHOR) DIRECT=5.d0*DIRHOR
      END IF
C
      RETURN
      END
