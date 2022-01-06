C
      SUBROUTINE LWRMAT(NC,NSP,NR,TC,TSP,TR,TS,
     +                   ICESPT,WDT,TDIFFC,TDIFFU,A1,B1,C1)
C
C     THIS SUBROUTINE CALCULATES THE CONTRIBUTION TO THE ENERGY BALANCE
C     MATRIX BY LONG-WAVE RADIATION.
C
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXNOD=300, NODRES=10, nodcan=11, NODSNO=100, 
     + NODTOT=MXNOD+NODCAN+NODRES+NODSNO)
C
      DOUBLE PRECISION TC(NODCAN),TSP(NODSNO),
     +  TR(NODRES),TDIFFC(NODCAN),
     +  TDIFFU(NODRES),A1(NODTOT),B1(NODTOT),C1(NODTOT),
     +  TS(MXNOD)
      INTEGER ICESPT(NODSNO)
C
      PARAMETER (STEFAN=5.6697D-08)
      common /shawp/ pplastic,emitc,emitr(10),emitsp,emits,emitf
     &      ,palbedo,ptransm
C
      N = 1
      MATERL=2
      B1(N) = 0.0d0
C
C     RADIATION BALANCE MATRIX COEFFICIENTS FOR CANOPY NODES
      IF (NC .GT. 0) THEN
         EMITNC=(1.d0-TDIFFC(1))*EMITC
C        TC3=STEFAN*4.d0*WDT*(TC(1)+273.16d0)**3.d0
C        B1(N) =-2.d0*EMITNC*TC3
C        C1(N) = EMITNC
C        A1(N+1) = EMITNC*TC3
         B1(N) =0.0d0
         C1(N) =0.0d0
         A1(N+1) =0.0d0

         N = N + 1
C
         DO 10 I=2,NC
            EMITNC=(1.d0-TDIFFC(I))*EMITC
c           TC3=STEFAN*4.d0*WDT*(TC(I)+273.16d0)**3.d0
c           C1(N-1)=C1(N-1)*EMITNC*TC3
c           A1(N) = A1(N)*EMITNC
c           B1(N) = -2.d0*EMITNC*TC3
c           C1(N)= EMITNC
c           A1(N+1) = EMITNC*TC3
            C1(N-1)=0.0d0
            A1(N) = 0.0d0
            B1(N) = 0.0d0
            C1(N)= 0.0d0
            A1(N+1) = 0.0d0
            N = N + 1
   10    CONTINUE
         MATERL=MATERL + 1
      END IF
C
C     RADIATION BALANCE MATRIX COEFFICIENTS FOR SNOW PACK
      IF (NSP .GT. 0) THEN
         IF (ICESPT(1) .GT. 0) THEN
C           TEMP IS KNOWN - ENERGY BALANCE BASED ON LIQUID CONTENT
            IF (MATERL .GT. 2) THEN
               A1(N) = A1(N)*EMITSP
               C1(N-1)=0.0d0
            END IF
            B1(N) = 0.0d0
          ELSE
C           ENERGY BALANCE BASED ON TEMPERATURE
            TSP3=STEFAN*4.d0*WDT*(TSP(1)+273.16d0)**3.d0
            IF (MATERL .GT. 2) THEN
C              ADJUST COEFFICIENTS FOR MATERIAL ABOVE SNOWPACK
               A1(N) = A1(N)*EMITSP
               C1(N-1)=C1(N-1)*EMITSP*TSP3
            END IF
            B1(N) = -EMITSP*TSP3
         END IF
         N = N + NSP
         MATERL=MATERL + 1
C        INITIALIZE COEFFICIENTS FOR BOTTOM NODE AND UNDERLYING MATERIAL
         IF (NSP .GT. 1) B1(N-1) = 0.0d0
         C1(N-1) = 0.0d0
         DO 5 I=1,NR+1
C           SNOW OVERLYING RESIDUE AND SOIL - INITIALIZE MATRIX COEFF.
            A1(N) = 0.0d0
            B1(N) = 0.0d0
            C1(N) = 0.0d0
            N = N + 1
    5    CONTINUE
         GO TO 50
      END IF
C
C     RADIATION BALANCE MATRIX COEFFICIENTS FOR RESIDUE NODES
C
      IF (NR .GT. 0) THEN
C        ADJUST FOR PLASTIC FILM WHICH REFLECTS SOME RADIATION BACK 
C        TO RESIDUE; ADJUST FOR WHAT IS NOT REFLECTED BACK
         EMITNC=(1-TDIFFU(1))*EMITR(1)     !*((1.0d0-pplastic)+emitf*pplastic)  LIWANG 2017
         TR3=STEFAN*4.d0*WDT*(TR(1)+273.16d0)**3.d0
         IF (MATERL .GT. 2) THEN
C           ADJUST COEFFICIENTS FOR MATERIAL ABOVE RESIDUE
            A1(N) = A1(N)*EMITNC
            C1(N-1)=C1(N-1)*EMITNC*TR3
         END IF
         B1(N) =-2.d0*EMITNC*TR3
C        RECOMPUTE EMITNC FOR LAYERS BELOW
         EMITNC=(1-TDIFFU(1))*EMITR(1)
         C1(N) = EMITNC
         A1(N+1) = EMITNC*TR3
         N = N + 1
C
         DO 20 I=2,NR
            EMITNC=(1-TDIFFU(I))*EMITR(I)
            TR3=STEFAN*4.d0*WDT*(TR(I)+273.16d0)**3.d0
            C1(N-1)=C1(N-1)*EMITNC*TR3
            A1(N) = A1(N)*EMITNC
            B1(N) = -2.d0*EMITNC*TR3
            C1(N) = EMITNC
            A1(N+1) =EMITNC*TR3
            N = N + 1
   20    CONTINUE
         MATERL = MATERL + 1
      END IF
C
C     RADIATION BALANCE MATRIX COEFFICIENTS FOR SOIL SURFACE
      TS3=STEFAN*4.d0*WDT*(TS(1)+273.16d0)**3.d0
      IF (MATERL .GT. 2) THEN
C        ADJUST COEFFICIENTS FOR MATERIAL ABOVE SOIL
         A1(N) = A1(N)*EMITS
         C1(N-1)=C1(N-1)*EMITS*TS3
      END IF
      B1(N)=-EMITS*TS3
cxxxx If no residue, soil is surface material - Use emmissivity of film
C      if (nr.eq.0) B1(N)=B1(N)*((1.0d0-pplastic)+emitf*pplastic)   !LIWANG 2017
   50 RETURN
      END
