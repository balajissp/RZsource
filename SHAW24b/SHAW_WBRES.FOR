C***********************************************************************
C
      SUBROUTINE WBRES(N,NR,NSP,TR,TRDT,GMC,GMCDT,VAPR,VAPRDT,EVAPR,
     +  EVAPK,WT,WDT,DT,WINDR,RHOR,ZR,RHOSP,ICESDT,ITER,PRESUR,
     +  QVR,evapmx,A2,B2,C2,D2,ISTRESS,RESTKB)
C
C     THIS SUBOUTINE CALCULATES THE NEWTON-RAPHSON COEFFICIENTS FOR THE
C     WATER BALANCE OF THE RESIDUE LAYERS.
C
CnT   THIS SUBROUTINE HAS BEEN MODIFIED TO USE TRANSPIRATION SPECIFIED
CnT   FROM OUTSIDE GOSHAW.  LINES THAT ARE NO LONGER NEEDED OR CHANGED
CnT   HAVE BEEN COMMENTED OUT BY "CnT" 
C
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXNOD=300, NODRES=10, NODSNO=100, 
     + NODTOT=MXNOD+NODRES+NODSNO)
C
      DOUBLE PRECISION TR(NODRES),TRDT(NODRES),GMC(NODRES),
     +  GMCDT(NODRES),VAPR(NODRES),VAPRDT(NODRES),CONVEC(NODRES),
     +  QVR(NODRES),CONV(NODRES),TKRES(NODRES),VAPCON(NODRES),
     +  A2(NODTOT),B2(NODTOT),C2(NODTOT),D2(NODTOT),RHOR(NODRES),
     +  ZR(NODRES),WINDR(NODRES),RESTKB(NODRES),
     +  EVAPR(NODRES),EVAPK(NODRES),RHOSP(NODSNO)
C
      INTEGER ICESDT(MXNOD)
      PARAMETER (RHOL=1.0D3,G=9.81D0,UGAS=8.3143D0)
      common /shawp/ pplastic,emitc,emitr(10),emitsp,emits,emitf
     &      ,palbedo,ptransm
C
C**** DETERMINE THE HEAT TRANSFER COEFFICIENT FOR EACH NODE
      CALL RESTK (NR,NSP,TKRES,CONVEC,TR,TRDT,GMC,GMCDT,WT,WDT,WINDR,
     +            RHOR,RHOSP,RESTKB,ZR)
      NR1=NR+1
      TKRES(NR1)=TKRES(NR)
      CONVEC(NR1) = CONVEC(NR)
C
C**** DETERMINE THE VAPOR TRANSPORT FROM THE THERMAL CONVECTION
      CALL RESVK (NR1,NSP,TR,TRDT,CONVEC,VAPCON,WT,WDT,PRESUR)
C
C**** DETERMINE THE CONDUCTANCE TERM FOR CONVECTIVE VAPOR TRANSPORT
      CALL CONDUC (NR1,ZR,VAPCON,CONV)
C
C**** DETERMINE THE VAPOR FLUX BETWEEN RESIDUE NODES
      CALL QVRES (NR,QVR,CONV,VAPR,VAPRDT,WT,WDT)
c gnf check if soil can supply water for evaporation
c      if (-qvr(nr).gt.evapmx*rhol) then    !five places this is done. look for evapmx
         if (istress.lt.3) then
         qvr(nr)=-evapmx*rhol
         conv(nr)=0.0
         else
c        Limit flux to residue since it is contolled by evapmx 
c         qvr(nr)=qvr(nr)*(1.0d0-pplastic)
c         conv(nr)=conv(nr)*(1.0d0-pplastic)   !LIWANG 2017
         endif
c      end if
c
C**** DETERMINE THE COEFFICIENTS FOR THE SURFACE LAYER
       IF (ISTRESS.GE.3) THEN
        C2(N)=WDT*CONV(1)
       ELSE
        C2(N)=CONV(1)
       ENDIF
cc Per Gerald Flchering suggesiton, remove residue storage to force QVR(NR)=Vflux, Dec 7, 2016
      IF (NR.GT.1) THEN
          IF (ISTRESS.GE.3) THEN
        B2(N)=B2(N) - WDT*(CONV(1) + EVAPK(1)) !- (ZR(2)-ZR(1))/(2*DT)
        D2(N)= D2(N) - QVR(1)- EVAPR(1)
c     >                - (ZR(2)-ZR(1))/(2*DT)*(VAPRDT(1)-VAPR(1))
         ELSE
         B2(N)=B2(N) - (CONV(1) + EVAPK(1))
         D2(N)= D2(N) - QVR(1)- EVAPR(1)
         ENDIF
      ELSE
          IF (ISTRESS.GE.3) THEN
        B2(N)=B2(N) - WDT*(CONV(1) + EVAPK(1)) !- (ZR(2)-ZR(1))/DT
        D2(N)= D2(N) - QVR(1)- EVAPR(1)
c     >                - (ZR(2)-ZR(1))/DT*(VAPRDT(1)-VAPR(1))
         ELSE
         B2(N)=B2(N) - (CONV(1) + EVAPK(1))
         D2(N)= D2(N) - QVR(1)- EVAPR(1)
         ENDIF
      END IF
C
C**** DETERMINE THE COEFFICIENTS FOR THE REST OF THE RESIDUE LAYERS
      DO 10 I=N+1,N+NR-1
         J=I-N+1
         IF (ISTRESS.GE.3) THEN
         A2(I)=WDT*CONV(J-1)
         C2(I)=WDT*CONV(J)
         ELSE
         A2(I)=CONV(J-1)
         C2(I)=CONV(J)
         ENDIF
         IF (J .NE. NR) THEN
             IF (ISTRESS.GE.3) THEN
          B2(I)=-WDT*(CONV(J-1)+CONV(J)+EVAPK(J))
     >            - (ZR(J+1)-ZR(J-1))/(2.0D0*DT)
          D2(I)= QVR(J-1) - QVR(J) - EVAPR(J)
     >      -(ZR(J+1)-ZR(J-1))/(2.D0*DT)*(VAPRDT(J)-VAPR(J))
            ELSE
            B2(I)=-(CONV(J-1)+CONV(J)+EVAPK(J))
            D2(I)= QVR(J-1) - QVR(J) - EVAPR(J)
            ENDIF
         ELSE
             IF (ISTRESS.GE.3) THEN
         B2(I)=-WDT*(CONV(J-1)+CONV(J)+EVAPK(J))
     >            - (ZR(J+1)-ZR(J)+(ZR(J)-ZR(J-1))/2)/DT
         D2(I)= QVR(J-1) - QVR(J) - EVAPR(J)
     >       - (ZR(J+1)-ZR(J)+(ZR(J)-ZR(J-1))/2)/DT*(VAPRDT(J)-VAPR(J))
            ELSE
            B2(I)=-(CONV(J-1)+CONV(J)+EVAPK(J))
            D2(I)= QVR(J-1) - QVR(J) - EVAPR(J)
            ENDIF
         END IF
   10 CONTINUE
      N=N+NR
C
C**** DETERMINE THE COEFFICIENTS SOIL SURFACE
        IF (ISTRESS.GE.3) THEN
      A2(N)=WDT*CONV(NR)/RHOL
       ELSE
      A2(N)=CONV(NR)/RHOL
       ENDIF
      IF (ICESDT(1) .EQ. 0) THEN
C        NO ICE IS PRESENT AT SOIL SURFACE
         B2(N)=-WDT*CONV(NR)*VAPRDT(NR+1)
     >        *0.018*G/UGAS/(TRDT(NR+1)+273.16)/RHOL
         C2(N-1)=C2(N-1)*VAPRDT(NR+1)*.018*G/UGAS/(TRDT(NR+1)+273.16)
        ELSE
C        ICE IS PRESENT AT SOIL SURFACE - WATER BALANCE FOR ICE CONTENT
         B2(N)=0.0
         C2(N-1)=0.0
      END IF
      D2(N) = QVR(NR)/RHOL
      RETURN
      END
