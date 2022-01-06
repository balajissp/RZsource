C
      SUBROUTINE SWRCAN(NC,NR,NSP,SWCAN,SWRES,DIRECT,DIFFUS,SUNSLP,
     +  ALTITU,ALBNXT,ALBRES,CANALB,XANGLE,CLUMPNG,NPLANT,CANLAI,
     +  RCOVER,TDIFFC,DIFKL,TDIFFUswr,TDIFFU,FDDU,TOTLAI)
C
C     THIS SUBROUTINE CALCULATES THE AMOUNT OF SOLAR RADIATION ABSORBED
C     BY EACH CANOPY AND RESIDUE NODE
C
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (NODRES=10, nodcan=11, MXSPEC=10)
C
      DOUBLE PRECISION CANALB(MXSPEC),CANLAI(MXSPEC,NODCAN),
     + TDIRCC(NODCAN),TDIFFC(NODCAN),DIRKL(MXSPEC+1,NODCAN),
     + DIFKL(MXSPEC+1,NODCAN),TDIREC(NODRES),TDIFFU(NODRES),
     + TDIFFUswr(NODRES),
     + SWCAN(MXSPEC,NODCAN),SWRES(NODRES),DIRALB(NODCAN),DIFALB(NODCAN),
     + FBDU(MXSPEC),FDDU(MXSPEC),DIRTRN(NODCAN),DIFTRN(NODCAN),
     + FBREFU(NODCAN),FDREFU(NODCAN),FBTRNU(NODCAN),FDTRNU(NODCAN),
     + FBREFD(NODCAN),FDREFD(NODCAN),FBTRND(NODCAN),FDTRND(NODCAN),
     + A(2*(NODCAN+NODRES+1)),B(2*(NODCAN+NODRES+1)),
     + C(2*(NODCAN+NODRES+1)),D(2*(NODCAN+NODRES+1)),
     + DIR(NODCAN+NODRES+1),DOWN(NODCAN+NODRES+1),UP(NODCAN+NODRES+1),
     + XANGLE(MXSPEC),CLUMPNG(MXSPEC),TOTLAI(MXSPEC),albres(nodres)
      DOUBLE PRECISION CANTRN(MXSPEC)
      DATA CANTRN /MXSPEC*0.20D0/
      common /shawp/ pplastic,emitc,emitr(10),emitsp,emits,emitf
     &      ,palbedo,ptransm

C
C**** OBTAIN TRANSMISSIVITY TO DIRECT AND DIFFUSE RADIATION THROUGH
C     CANOPY AND/OR RESIDUE DEPENDING ON LAYERING OF MATERIALS
      IF (NC .GT. 0) THEN
         CALL TRANSC (NC,TDIRCC,TDIFFC,DIFKL,DIRKL,FBDU,FDDU,TOTLAI,
     >                SUNSLP,ALTITU,CANLAI,XANGLE,CLUMPNG,NPLANT)
C        CALCULATE WEIGHTED ALBEDO, TRANSMISSION AND SCATTERING FOR 
C        DIRECT AND DIFFUSE RADIATION OR EACH OF THE CANOPY LAYERS.
C        THE FRACTION OF DOWNWARD RADIATION TRANSMITTED THROUGH THE 
C        LEAVES AND SCATTERED DOWNWARD EQUALS DOWNWARD REFLECTED UPWARD.  
C        UPWARD SCATTERED REFLECTED AND TRANSMITTED (FBREFU AND FBTRNU) 
C        MUST BE WEIGHTED BASED ON ALBEDO AND TRANSMISSION RESPECTIVELY.
         DO 15 I=1,NC
            DIRALB(I)=0.0d0
            DIFALB(I)=0.0d0
            DIRTRN(I)=0.0d0
            DIFTRN(I)=0.0d0
            FBREFU(I)=0.0d0
            FDREFU(I)=0.0d0
            FBTRNU(I)=0.0d0
            FDTRNU(I)=0.0d0
            DO 10 J=1,NPLANT
               DIRALB(I) = DIRALB(I) + CANALB(J)*DIRKL(J,I)
               DIFALB(I) = DIFALB(I) + CANALB(J)*DIFKL(J,I)
               DIRTRN(I) = DIRTRN(I) + CANTRN(J)*DIRKL(J,I)
               DIFTRN(I) = DIFTRN(I) + CANTRN(J)*DIFKL(J,I)
               FBREFU(I) = FBREFU(I) + FBDU(J)*CANALB(J)*DIRKL(J,I)
               FDREFU(I) = FDREFU(I) + FDDU(J)*CANALB(J)*DIFKL(J,I) 
C              FOR A SINGLE PLANT SPECIES, FRACTION TRANSMITTED UPWARD 
C              IS EQUAL TO 1 - REFLECTED UPWARD (BUT NOT NECESSARILY FOR
C              CANOPY LAYER BECAUSE THEY ARE WEIGHTED BY LAI, K, ETC.) 
               FBTRNU(I) = FBTRNU(I)+(1.d0-FBDU(J))*CANTRN(J)*DIRKL(J,I)
               FDTRNU(I) = FDTRNU(I)+(1.d0-FDDU(J))*CANTRN(J)*DIFKL(J,I)
   10       CONTINUE
            IF (DIRALB(I) .GT. 0.0d0) THEN
               FBREFU(I)=FBREFU(I)/DIRALB(I)
               FDREFU(I)=FDREFU(I)/DIFALB(I)
            END IF
            IF (DIRTRN(I) .GT. 0.0d0) THEN
               FBTRNU(I)=FBTRNU(I)/DIRTRN(I)
               FDTRNU(I)=FDTRNU(I)/DIFTRN(I)
            END IF
C           FRACTION SCATTERED DOWNWARD IS 1 - SCATTERED UPWARD
            FBREFD(I)=1.d0 - FBREFU(I)
            FDREFD(I)=1.d0 - FDREFU(I)
            FBTRND(I)=1.d0 - FBTRNU(I)
            FDTRND(I)=1.d0 - FDTRNU(I)
            DIRALB(I)=DIRALB(I)/DIRKL(NPLANT+1,I)
            DIFALB(I)=DIFALB(I)/DIFKL(NPLANT+1,I)
            DIRTRN(I)=DIRTRN(I)/DIRKL(NPLANT+1,I)
            DIFTRN(I)=DIFTRN(I)/DIFKL(NPLANT+1,I)
   15    CONTINUE
      END IF
C
      IF (NR .GT. 0) 
     +   CALL TRANSR (NR,TDIREC,TDIFFUswr,TDIFFU,SUNSLP,RCOVER)
c rma/gf
      NR1=NR
      IF (NR.EQ.1) THEN
C        RADIATION EXCHANGE IS NOT ACCURATE WITH ONLY ONE RESIDUE LAYER.
C        SPLIT RESIDUE LAYER IN TWO AND REDEFINE TRANSMISSION COEFF.
C        FOR TWO RESIDUE LAYERS.
c rma\gf
         NR1=2
         TDIREC(1)=SQRT(TDIREC(1))
         TDIFFUswr(1)=SQRT(TDIFFUswr(1))
         TDIREC(2)=TDIREC(1)
         TDIFFUswr(2)=TDIFFUswr(1)
      END IF
C                                                                               
C**** RADIATION EXCHANGE WITHIN THE CANOPY AND RESIDUE
C     INITIALIZE MATRIX ELEMENTS
      N = 1
      A(1) = 0.0d0
      B(1) = 1.0d0
      C(1) = 0.0d0    
      D(1) = DIFFUS
      DIR(1)=DIRECT     
C
      DO 20 I=1,NC
        N = N + 1
C       
C       COMPUTE COEFFICIENTS FOR UPWELLING RADIATION ABOVE LAYER
        A(N)=-(1.d0-TDIFFC(I))*(FDREFU(I)*DIFALB(I)+FDTRNU(I)*DIFTRN(I))
        B(N) = 1.0d0
        C(N) = -TDIFFC(I) -
     >       (1.d0-TDIFFC(I))*(FDREFD(I)*DIFALB(I)+FDTRND(I)*DIFTRN(I))
        D(N)=(1.d0-TDIRCC(I))*(FBREFU(I)*DIRALB(I)+FBTRNU(I)*DIRTRN(I))
     >         *DIR(I)
C       COMPUTE COEFFICIENTS FOR DOWNWELLING RADIATION BELOW LAYER
        N = N + 1
        A(N) = C(N-1)
        B(N) = 1.0d0
        C(N) = A(N-1)
        D(N) =(1.d0-TDIRCC(I))*(FBREFD(I)*DIRALB(I)+FBTRND(I)*DIRTRN(I))
     >         *DIR(I)
C       COMPUTE DIRECT RADIATION PASSING THROUGH THIS LAYER
        DIR(I+1)=DIR(I)*TDIRCC(I)
   20 CONTINUE
C
      DO 30 I=1,NR1
      	  N = N + 1
C        COMPUTE COEFFICIENTS FOR UPWELLING RADIATION ABOVE LAYER
C        HORIZONTAL LEAF ORIENTATION IS ASSUMED FOR SCATTERING. 
C        THEREFORE ONLY BACKSCATTERING; NO FORWARD SCATTERING
         A(N) = -ALBRES(I)*(1.d0-TDIFFUswr(I))
         B(N) = 1.0d0
         C(N) = -TDIFFUswr(I)
         D(N) = ALBRES(I)*(1.d0-TDIREC(I))*DIR(NC+I)
C        COMPUTE COEFFICIENTS FOR DOWNWELLING RADIATION BELOW LAYER
         N = N + 1
         A(N) = C(N-1)
         B(N) = 1.0d0
         C(N) = A(N-1)
         D(N) = 0.0d0
C        COMPUTE DIRECT RADIATION PASSING THROUGH THIS LAYER
         DIR(NC+I+1)=DIR(NC+I)*TDIREC(I) 
   30 CONTINUE
C
C     COMPUTE MATRIX ELEMENTS FOR LOWER SURFACE BOUNDARY
      N = N + 1
      B(N) = 1.0d0
      C(N) = 0.0d0
      A(N) = -ALBNXT
      D(N) = ALBNXT*DIR(NC+NR1+1)
C
C     CALL MATRIX SOLVER
      NLAYR = NC + NR1
      CALL SOLVRAD (NLAYR,A,B,C,D,DOWN,UP)
C                                                                               
C**** DETERMINE THE AMOUNT OF RADIATION ABSORBED AT EACH CANOPY NODE BY 
C     WEIGHTING ACCORDING TO ABSORBTION, LAI, AND ATTENUATION FACTOR
      DO 65 I=1,NC
         TOTDIR=0.0d0
         TOTDIF=0.0d0
         DO 63 J=1,NPLANT
            TOTDIR = TOTDIR + (1.d0-CANALB(J)-CANTRN(J))*DIRKL(J,I)
            TOTDIF = TOTDIF + (1.d0-CANALB(J)-CANTRN(J))*DIFKL(J,I)
   63    CONTINUE
         SWCAN(NPLANT+1,I)=0.0d0
         DO 64 J=1,NPLANT
            FRACT1=(1.d0-CANALB(J)-CANTRN(J))*DIRKL(J,I)/TOTDIR
            FRACT2=(1.d0-CANALB(J)-CANTRN(J))*DIFKL(J,I)/TOTDIF
            SWCAN(J,I)= FRACT1*(1.d0-DIRALB(I)-DIRTRN(I))
     >                  *(1.d0-TDIRCC(I))*DIR(I)
     >                + FRACT2*(1.d0-DIFALB(I)-DIFTRN(I))
     >                  *(1.d0-TDIFFC(I))*(DOWN(I)+UP(I+1))
            SWCAN(NPLANT+1,I)=SWCAN(NPLANT+1,I)+SWCAN(J,I)
   64    CONTINUE
   65 CONTINUE
C**** DETERMINE THE AMOUNT OF RADIATION ABSORBED AT EACH RESIDUE NODE
      ABSORB=0.0d0                                                                
      DO 70 I=1,NR1                                                              
         K=I+NC
         SWRES(I)=DIR(K)+DOWN(K)+UP(K+1)-DIR(K+1)-DOWN(K+1)-UP(K)               
         ABSORB=ABSORB + SWRES(I)                                               
   70 CONTINUE                                                                  
      IF (NR .EQ. 1) THEN
         SWRES(1)=SWRES(1)+SWRES(2)
         SWRES(2)=0.0d0
      END IF
C                                                                               
C**** IF SNOW LAYER LIES ABOVE THE RESIDUE, ALL RADIATION LEAVING THE           
C     THE RESIDUE IS ASSUMED TO BE REFLECTED BACK AND ABSORBED.                 
C     SPLIT UP THIS RADIATION PROPORTIONATELY BETWEEN THE LAYERS                
      IF (NR.GT.0 .AND. NSP.GT.0) THEN 
         IF (ABSORB .GT. 0.0d0) THEN
         DO 80 I=1,NR
            SWRES(I)=SWRES(I) + UP(1)*SWRES(I)/ABSORB
   80    CONTINUE
         END IF
      END IF
C                                                                               
C**** DEFINE THE AMOUNT OF RADIATION AVAILABLE AT THE SOIL SURFACE              
      DIRECT=DIR(NC+NR1+1)
      DIFFUS=DOWN(NC+NR1+1)
C
      RETURN                                                                    
      END                                                                       
C***********************************************************************        
