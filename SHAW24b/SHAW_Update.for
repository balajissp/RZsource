C
      SUBROUTINE UPDATE(NS,NR,NSP,NC,NPLANT,NSALT,ICES,ICESDT,TS,TSDT,
     + MAT,MATDT,CONC,CONCDT,VLC,VLCDT,VIC,VICDT,SALT,SALTDT,
     + TR,TRDT,VAPR,VAPRDT,GMC,GMCDT,TC,TCDT,TLC,TLCDT,VAPC,VAPCDT,
     + WCAN,WCANDT,PCAN,PCANDT,TSP,TSPDT,DLW,DLWDT,ICESP,ICESPT)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE UPDATES THE BEGINNING OF TIME STEP VALUES FOR THE
C     NEW TIME STEP.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       CONC      I/O   conc of j-th solute in soil soln at time t (eq/kq)
C       CONCDT     I    conc of j-th solute in soil soln at time t+dt (eq/kg)
C       DLW       I/O   depth of liquid water for i-th snow pack node
C       DLWDT      I    liquid water depth for i-th snow pack node at
C                       end of time step
C       GMC       I/O   grav water content of i-th residue node at time t
C       GMCDT      I    grav water content of i-th residue node at t+dt
C       I          L    canopy, snow, residue or soil layer
C       ICES      I/O   1 if soil layer contains both ice and water at t
C       ICESDT     I    1 if soil layer contains both ice and water at t+dt
C       ICESP     I/O   1 if snow layer contains both ice and water at t
C       ICESPT     I    1 if snow layer contains both ice and water at t+dt
C       J          L    specifies solute species in soil or plant species
C       MAT       I/O   matric potential  (m)
C       MATDT      I    matric potential at t+dt  (m)
C       NR         I    num of matted residue layers
C       NS         I    num of soil layers
C       NSALT      I    num of different types of solutes to be simulated
C       NSP        I    num of layers in snowpack
C       SALT      I/O   amount of j-th solute in i-th soil node at t
C       SALTDT     I    amount of j-th solute in i-th soil node at t+dt
C       TR        I/O   temp of residue layer at time t
C       TRDT       I    temp of residue layer at time t + dt
C       TS        I/O   temp of soil layer at time t
C       TSDT       I    temp of soil layer at time t + dt
C       TSP       I/O   temp of canapy layer at time t
C       TSPDT      I    temp of snowpack layer at time t + dt
C       VIC       I/O   vol ice content
C       VICDT      I    vol ice content at t+dt
C       VLC       I/O   vol liquid content
C       VLCDT      I    vol liquid content at t+dt
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
     +  NODSNO=100)
C
      DOUBLE PRECISION TS(MXNOD),TSDT(MXNOD),MAT(MXNOD),MATDT(MXNOD),
     +  CONC(MXCHEM,MXNOD),CONCDT(MXCHEM,MXNOD),VLC(MXNOD),VLCDT(MXNOD),
     +  VIC(MXNOD),VICDT(MXNOD),SALT(MXCHEM,MXNOD),SALTDT(MXCHEM,MXNOD),
     +  TR(NODRES),TRDT(NODRES),VAPR(NODRES),VAPRDT(NODRES),GMC(NODRES),
     +  GMCDT(NODRES),TC(NODCAN),TCDT(NODCAN),TLC(MXSPEC,NODCAN),
     +  TLCDT(MXSPEC,NODCAN),VAPC(NODCAN),VAPCDT(NODCAN),WCAN(NODCAN),
     +  WCANDT(NODCAN),PCAN(mxspec),PCANDT(MXSPEC),TSP(NODSNO),
     +  TSPDT(NODSNO),DLW(NODSNO),DLWDT(NODSNO)
C
      INTEGER ICES(MXNOD),ICESDT(MXNOD),ICESP(NODSNO),ICESPT(NODSNO)
c
c      write (*,*) "update tc(1),tcdt(1),vapc(1),vapcdt(1)",tc(1)
c     + tcdt(1),vapc(1),vapcdt(1)
C     SOIL PROPERTIES
      DO 15 I=1,NS
         TS(I)=TSDT(I)
         MAT(I)=MATDT(I)
         VLC(I)=VLCDT(I)
         VIC(I)=VICDT(I)
         ICES(I)=ICESDT(I)
         DO 10 J=1,NSALT
            SALT(J,I)=SALTDT(J,I)
            CONC(J,I)=CONCDT(J,I)
   10    CONTINUE
   15 CONTINUE
C
C     RESIDUE PROPERTIES
      DO 20 I=1,NR
         TR(I)=TRDT(I)
         VAPR(I)=VAPRDT(I)
         GMC(I)=GMCDT(I)
   20 CONTINUE
      IF (NR .GT. 0) THEN
         TR(NR+1) = TRDT(NR+1)
         VAPR(NR+1)=VAPRDT(NR+1)
      END IF
C
C     SNOWPACK PROPERTIES
      DO 30 I=1,NSP
         ICESP(I) = ICESPT(I)
         TSP(I) = TSPDT(I)
         DLW(I) = DLWDT(I)
   30 CONTINUE
      IF (NSP .GT. 0) TSP(NSP+1) = TSPDT(NSP+1)
C
C     CANOPY PROPERTIES
      DO 40 I=1,NC
         TC(I)=TCDT(I)
         VAPC(I)=VAPCDT(I)
         WCAN(I)=WCANDT(I)
         DO 35 J=1,NPLANT
            TLC(J,I)=TLCDT(J,I)
   35    CONTINUE
   40 CONTINUE
      IF (NC .GT. 0) THEN
         TC(NC+1) = TCDT(NC+1)
         VAPC(NC+1)=VAPCDT(NC+1)
         DO 45 J=1,NPLANT
            PCAN(J)=PCANDT(J)
   45    CONTINUE
      END IF
C
      RETURN
      END
