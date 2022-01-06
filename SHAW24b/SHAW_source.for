C***********************************************************************
C
      SUBROUTINE SOURCE(NSP,NR,NS,SSP,SR,
     >   SS,SWSNOW,SWRES,SWSOIL,LWSNOW,LWRES,LWSOIL)
C
C     THIS SUBROUTINE SUMS UP THE SOURCE-SINK TERMS FOR EACH NODE.
C
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXNOD=300, NODRES=10, NODSNO=100)
C
      DOUBLE PRECISION SSP(NODSNO),SR(NODRES),SS(MXNOD),SWSNOW(NODSNO),
     +  SWRES(NODRES),SWSOIL,LWSNOW,LWRES(NODRES),LWSOIL
C
C**** SNOWPACK NODES
      IF (NSP .GT. 0) THEN
         DO 20 I=1,NSP
            SSP(I)=SWSNOW(I)
   20    CONTINUE
         SSP(1)=SSP(1)+LWSNOW
      END IF
C
C**** RESIDUE NODES
      DO 30 I=1,NR
         SR(I)=SWRES(I)+LWRES(I)
   30 CONTINUE
C**** SOIL NODES
      DO 50 I=1,NS
         SS(I)=0.0D0
   50 CONTINUE
      SS(1)=SS(1)+SWSOIL+LWSOIL
C
      RETURN
      END
