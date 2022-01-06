*******************************************************************************
	SUBROUTINE SOILR(NS,ZS,TS,TSDT,SOILRSP)
C      THIS SUBROUTINE CALCULATED SOIL RESPIRATION
	REAL RSOIL0,TASOIL,SOILRSP
	REAL TSDT(50),ZS(50),TS(50)
	Q10=1.7;RSOIL0=0.12
      TASOIL=(TS(1)+TSDT(1))*(ZS(2)-ZS(1))/4.
      DO 10, I=2,NS
	   IF ((ZS(I)+ZS(I+1))/2. .LT. 0.20) THEN
            TASOIL=TASOIL+(TS(I)+TSDT(I))*(ZS(I+1)-ZS(I-1))/4.
	    ELSE
            TASOIL=TASOIL+(TS(I)+TSDT(I))*(0.20-(ZS(I)+ZS(I-1))/2.)/2.
            TASOIL=TASOIL/0.20
	      GO TO 20
	   END IF
   10 CONTINUE
   20 CONTINUE
      SOILRSP  = RSOIL0*Q10**((TASOIL-25.)/10.) 
	END