
C***********************************************************************
C
      SUBROUTINE PHOTOSYNTHESIS(J,LYRLAI,TA,WIND,
     >	                      HUMD,GS,CFLUX,VM,VPDA,PN,PFD,CO2I)

C
C     THIS SUBROUTINE CALCULATES CANOPY PHOTOSYNTHESIS FOR SINGLE PLANT
C                                                         
C     QIANG YU, MAY 15, 2003                                         
C           
C                
C***********************************************************************
      COMMON /RADCAN/ TDIRCC(10),TDIFFC(10),DIRKL(9,10),DIFKL(9,10)
      COMMON /SOLARRAD/DIR(21),DOWN(21),DIRHOR,DIFFUSRAD
	COMMON /PLEAF/ PLEAF(10) 

      DIMENSION SOLARDIR(10),SOLARDIF(10)
      
	REAL GS,TS(50),TSDT(50),ZS(50),PN,PMAX,ALF,CO2I
	REAL AA,A,B,ALF0,CP,C,EA,GAMA,PSYCH,RO,RGAMA,R
	REAL RD,RATIO,RSOIL(11),RSOIL0,PFD,CORRFAC
      REAL K,KO,KC,KC0,KO0,TAO,TEMPER,THETA,TK,VM,VM0,VPD0,VPDA
      REAL WILTPOINT,POTENLEAFMAX
	REAL CANLAI,PAR,TMP,HUMD,CA
      REAL LYRLAI
	INTEGER J
C	INPUT OR PARAMETERS OF THE MODEL
C     PHYSICAL PARAMETERS
C             LYRLAI=0.5
C	WRITE(*,*)'TA=',TA
             CA   =   350
   		   TK   =   273.16
	       PSYCH=   0.66
             RO   =   1.293E-3
             CP   =   0.24
             KC0  =   30.
             KO0  =   30.

             RGAMA=   66
C            GASOUS CONSTANT(JMOL-1K-1)
	       R    =   8.31432
             RATIO=   4.18E6
C            4182000;//EATM SHOULD INCLUDE EMMIT-ABSORP//
C            THE RATIO EQUEALS TO 6000*697//
C            0.66     PA/0C

C     	   PHYSIOLOGICAL PARAMETERS
C            EXTINCTION COEFFICIENT
C	       K    =   0.5 
c           100 and 108  vpd0=3500; 117 vpd0=1500
	       VPD0 =   1500
C            INITIAL QUTUMN YIELD /0.06
             ALF0 =   0.06         
C            CO2 COMPENSATION POINT 
             GAMA =   50
C	       CONVEXITY 
	       THETA=   0.95
C            MAXIMUM PHOTOSYNTHESIS RATE LIMITED BY RUBISCO
C            100 AND 108 VN0=32; 117 VM0=35
             VM0  =   22
C            RECIPROCAL OF PARAMETER(A1) IN STOMATAL MODEL:0.05
	       AA   =   5
C              0.08;
C	         PARAMETERS IN TEMPERATURE REPONSE EQUATION
C              A AND B KJ/MOL,KJ/MOLK IN UNIT. 
	       A    =   212000
	       B    =   703
C	         PARAMTER IN CO2 RESPONSE EQUATION
	       C    =   100
C	         CA   = 350
C              LIGHT COMPENSATION POINT UMOL/MOL	
	       GAMA =   50
             
c                          			 
		   WILTPOINT     = -400
C			 M
		   POTENLEAFMAX  = 0

		   VPDA=6.11*EXP(17.27*TA/(TA+237.3))*(1-HUMD/100)*100 

			  TEMPER=  (TA-25)/10.
		   VM0   =  VM0*EXP(TEMPER*LOG(2.4))
		   VM    =  VM0/(1+EXP((-A+B*(TA+273))/(R*(TA+273))))
         RD    =  0.05*VM
c		   PN0   = 0
C	         CANOPY IS DIVIDED FOR ONE LAYER FOR EVERY 0.5 OF LAI
C              THE FOLLOWING LOOP IS TO CALCULATE CANOPY PHOTOSYNTHESIS FOR EACH PFD WITHIN EACH LAYER
************************************************************************
C              TO CALCULATE ALF, THE INITIAL PHOTO EFFICIENCY
C CALCULATE SOLAR RADIATION AT DIFFERENT DEPTH IN CANOPY FOR PHOTO
	   if ((dirhor+diffusrad) .gt. 0.0)	then
	   PFD=(DIR(J)+DOWN(J))*2.4
	else
	pfd=0
	endif
C        PMAX  =  VM*(CA-GAMA)/(CA+C)
	SF=0.05   ! SENSITIVITY PARAMETER
	W0=-100	  ! REFERENCE LEAF WATER POTENTIAL
	FV = (1+EXP(SF*W0))/(1+EXP(SF*(W0-PLEAF(J))))	   ! STOMATA TO LEAF WATER POTENTIAL
	CO2I = 200			! INITIAL INTERCELLULAR	CO2 CONCENTRATION

	IF (PFD.LT.100) THEN
110	   ALF   =  ALF0*(CO2I-GAMA)/(CO2I+2*GAMA)
         PMAX  =  VM*(CO2I-GAMA)/(CO2I+C)  
C             TO CALCULATE PN OF EACH LEAF IN UNIT LEAF AREA
	PN=(ALF*PFD+PMAX-SQRT((ALF*PFD+PMAX)*(ALF*PFD+PMAX)
     >           -4*THETA*ALF*PFD*PMAX))/2.0/THETA-RD
	 GS = 0.05

	CO2IR=CA-PN/GS
	IF (ABS(CO2IR-CO2I).GT.0.1 ) THEN
		CO2I=CO2IR
	GOTO 110
	ENDIF
	ELSE
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!		ITERATIVE ESTIMATE 
114	   ALF   =  ALF0*(CO2I-GAMA)/(CO2I+2*GAMA)
         PMAX  =  VM*(CO2I-GAMA)/(CO2I+C)  
C             TO CALCULATE PN OF EACH LEAF IN UNIT LEAF AREA
	   PN=(ALF*PFD+PMAX-SQRT((ALF*PFD+PMAX)*(ALF*PFD+PMAX)
     >      -4*THETA*ALF*PFD*PMAX))/2.0/THETA-RD
	GS = 0.015 + 8*PN*FV/(CO2I-GAMA)

	CO2IR=CA-PN/GS
C	PRINT*,CO2IR
	IF (ABS(CO2IR-CO2I).GT.0.1 ) THEN
		CO2I=CO2IR
	GOTO 114
	ENDIF
	ENDIF
         CFLUX  = PN*LYRLAI*0.044
	END
