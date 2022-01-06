C
      SUBROUTINE BACKUP(NS,NR,NSP,NC,NPLANT,NSALT,ICES,ICESDT,TS,TSDT,
     + MAT,MATDT,CONC,CONCDT,VLC,VLCDT,VIC,VICDT,SALT,SALTDT,
     + TR,TRDT,VAPR,VAPRDT,GMC,GMCDT,TC,TCDT,TLC,TLCDT,VAPC,VAPCDT,
     + WCAN,WCANDT,PCAN,PCANDT,TSP,TSPDT,DLW,DLWDT,ICESP,ICESPT)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE SETS END OF TIME STEP VALUES BACK TO THOSE FOR
C     THE BEGINNING OF THE TIME STEP.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       CONC       I    conc of j-th solute in soil soln at time t (eq/kq)
C       CONCDT    I/O   conc of j-th solute in soil soln at time t+dt (eq/kg)
C       DLW        I    depth of liquid water for i-th snow pack node
C       DLWDT     I/O   liquid water depth for i-th snow pack node at
C                       end of time step
C       GMC        I    grav water content of i-th residue node at time t
C       GMCDT     I/O   grav water content of i-th residue node at t+dt
C       I          L    canopy, snow, residue or soil layer
C       ICES       I    1 if soil layer contains both ice and water at t
C       ICESDT    I/O   1 if soil layer contains both ice and water at t+dt
C       ICESP      I    1 if snow layer contains both ice and water at t
C       ICESPT    I/O   1 if snow layer contains both ice and water at t+dt
C       J          L    specifies solute species in soil or plant species
C       MAT        I    matric potential  (m)
C       MATDT     I/O   matric potential at t+dt  (m)
C       NR         I    num of matted residue layers
C       NS         I    num of soil layers
C       NSALT      I    num of different types of solutes to be simulated
C       NSP        I    num of layers in snowpack
C       SALT       I    amount of j-th solute in i-th soil node at t
C       SALTDT    I/O   amount of j-th solute in i-th soil node at t+dt
C       TR         I    temp of residue layer at time t
C       TRDT      I/O   temp of residue layer at time t + dt
C       TS         I    temp of soil layer at time t
C       TSDT      I/O   temp of soil layer at time t + dt
C       TSP        I    temp of canapy layer at time t
C       TSPDT     I/O   temp of snowpack layer at time t + dt
C       VIC        I    vol ice content
C       VICDT     I/O   vol ice content at t+dt
C       VLC        I    vol liquid content
C       VLCDT     I/O   vol liquid content at t+dt
C
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                       NONE
C
C       CALLED FROM:
C
C       PROGRAMMER:
C
C       VERSION:
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXNOD=300, MXCHEM=15, NODRES=10, nodcan=11, MXSPEC=10,
     +   NODSNO=100)
C
      DOUBLE PRECISION TS(MXNOD),TSDT(MXNOD),MAT(MXNOD),MATDT(MXNOD),
     +  CONC(MXCHEM,MXNOD),CONCDT(MXCHEM,MXNOD),VLC(MXNOD),VLCDT(MXNOD),
     +  VIC(MXNOD),VICDT(MXNOD),SALT(MXCHEM,MXNOD),SALTDT(MXCHEM,MXNOD),
     +  TR(NODRES),TRDT(NODRES),VAPR(NODRES),VAPRDT(NODRES),GMC(NODRES),
     +  GMCDT(NODRES),TC(NODCAN),TCDT(NODCAN),TLC(MXSPEC,NODCAN),
     +  TLCDT(MXSPEC,NODCAN),VAPC(NODCAN),VAPCDT(NODCAN),WCAN(NODCAN),
     +  WCANDT(NODCAN),PCAN(MXSPEC),PCANDT(MXSPEC),TSP(NODSNO),
     +  TSPDT(NODSNO),DLW(NODSNO),DLWDT(NODSNO)
C
      INTEGER ICES(MXNOD),ICESDT(MXNOD),ICESP(NODSNO),ICESPT(NODSNO)
C
C     SOIL PROPERTIES
      DO 15 I=1,NS
         TSDT(I)=TS(I)
         MATDT(I)=MAT(I)
         VLCDT(I)=VLC(I)
         VICDT(I)=VIC(I)
         ICESDT(I)=ICES(I)
         DO 10 J=1,NSALT
            SALTDT(J,I)=SALT(J,I)
            CONCDT(J,I)=CONC(J,I)
   10    CONTINUE
   15 CONTINUE
C
C     RESIDUE PROPERTIES
      DO 20 I=1,NR
         TRDT(I)=TR(I)
         VAPRDT(I)=VAPR(I)
         GMCDT(I)=GMC(I)
   20 CONTINUE
      IF (NR .GT. 0) THEN
         TRDT(NR+1) = TR(NR+1)
         VAPRDT(NR+1)=VAPR(NR+1)
      END IF
C
C     SNOWPACK PROPERTIES
      DO 30 I=1,NSP
         ICESPT(I) = ICESP(I)
         TSPDT(I) = TSP(I)
         DLWDT(I) = DLW(I)
   30 CONTINUE
      IF (NSP .GT. 0) TSPDT(NSP+1) = TSP(NSP+1)
C
C     CANOPY PROPERTIES
      DO 40 I=1,NC
         TCDT(I)=TC(I)
         VAPCDT(I)=VAPC(I)
         WCANDT(I)=WCAN(I)
         DO 35 J=1,NPLANT
            TLCDT(J,I)=TLC(J,I)
   35    CONTINUE
   40 CONTINUE
      IF (NC .GT. 0) THEN
         TCDT(NC+1) = TC(NC+1)
         VAPCDT(NC+1)=VAPC(NC+1)
         DO 45 J=1,NPLANT
            PCANDT(J)=PCAN(J)
   45    CONTINUE
      END IF
C
      RETURN
      END
