C***********************************************************************
C
      SUBROUTINE RESVAP (NR,EVAPR,EVAPK,TR,TRDT,GMC,GMCDT,VAPR,VAPRDT,
     +  WT,WDT,DT,RHOR,ZR,RESCOF,GMCMAX,ISTRESS)
C
C     THIS SUBROUTINE IS TO DETERMINE THE EVAPORATION WITHIN THE
C     RESIDUE LAYERS ASSUMING THE AVERAGE MOISTURE CONTENT OF THE
C     RESIDUE IS THE MOISTURE CONTENT AT THE BEGINNING OF THE TIME STEP
C
CnT   THIS SUBROUTINE HAS BEEN MODIFIED TO USE TRANSPIRATION SPECIFIED
CnT   FROM OUTSIDE GOSHAW.  LINES THAT ARE NO LONGER NEEDED OR CHANGED
CnT   HAVE BEEN COMMENTED OUT BY "CnT" 
C
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (NODRES=10)
C
      DOUBLE PRECISION RHOR(NODRES),ZR(NODRES),RESCOF(NODRES),
     +  GMCMAX(NODRES),EVAPR(NODRES),EVAPK(NODRES),TR(NODRES),
     +  TRDT(NODRES),GMC(NODRES),GMCDT(NODRES),VAPR(NODRES),
     +  VAPRDT(NODRES)
C
      DO 20 I=1,NR
C
C****    DETERMINE THE RELATIVE HUMIDITY IN THE RESIDUE
         CALL RESHUM (HUMT,DUMMY,GMC(I),TR(I))
         CALL RESHUM (HUMDT,DHDW,GMCDT(I),TRDT(I))
C
C****    DETERMINE THE SATURATED VAPOR DENSITY OF THE RESIDUE
         CALL VSLOPE (DUMMY,SATV,TR(I))
         CALL VSLOPE (DUMMY,SATVDT,TRDT(I))
C
         IF (I .EQ. 1) THEN
            DZ=ZR(2)-ZR(1)
            IF (NR .NE. 1) DZ=DZ/2.D0
           ELSE
            IF (I .NE. NR) THEN
               DZ=(ZR(I+1)-ZR(I-1))/2.D0
              ELSE
               DZ=ZR(I+1)-ZR(I)+(ZR(I)-ZR(I-1))/2.0D0
            END IF
         END IF
C  Talked to Gerald Flerchinger on Dec 6, 2016 to comment out evaporation from residue
c  this seems fix some problem with energy and flux discrapency. 
c  we assume dry residue as it is assumed in RZWQM.
C****    DEFINE EVAPK(I) -- FOR NOW, IT WILL SIMPLY BE SET EQUAL TO
C        THE INVERSE OF RESCOF(I), ASSUMING THAT RESCOF(I) IS THE VAPOR
C        RESISTANCE.  LATER WORK MAY REQUIRE THAT THE VAPOR TRANSPORT
C        RESISTANCE BE CALCULATED OR OBTAINED FROM A SUBROUTINE.
cc         IF (ISTRESS.EQ.3) THEN
cc         EVAPK(I)=1.d0/RESCOF(I)
cc         EVAPR(I)=EVAPK(I)*(WDT*(VAPRDT(I)-HUMDT*SATV)
cc     >                     +WT*(VAPR(I)-HUMT*SATV))
cc         GMCDT(I)= GMC(I) + EVAPR(I)*DT/DZ/RHOR(I)
cc         IF (GMCDT(I) .LT. 0.0d0) THEN
cc            GMCDT(I)=0.0d0
cc            EVAPR(I)= DZ*RHOR(I)*(GMCDT(I)-GMC(I))/DT
cc         END IF
cc         IF (GMCDT(I) .GT. GMCMAX(I)) THEN
cc            GMCDT(I)=GMCMAX(I)
cc            EVAPR(I)= DZ*RHOR(I)*(GMCDT(I)-GMC(I))/DT
cc         END IF
cc         ELSE
C        No evaporation from residue
         EVAPK(I)=0.0D0
         EVAPR(I)=0.0D0 
cc         ENDIF
C
   20 CONTINUE
      RETURN
      END
