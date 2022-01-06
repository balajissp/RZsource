C
      SUBROUTINE SUMDT(NC,NSP,NR,NS,NPLANT,ISHUTL,QVC,QVSP,QVR,TRNSP,
     +   TPOTNL,XTRACT,VFLUX,NTIMES,TOPSNO,TQVSP,EVAPS,TRANSP,TPOT,
     +   ROOTXT,tswcan,tswsno,tswres,tswsoi,tlwcan,tlwsno,tlwres,tlwsoi,
     +   swcan,swsnow,swres,swsoil,lwcan,lwsnow,lwres,lwsoil,
     +   hsum,hflux,gsum,gflux,vLEsum,DT,srootxt,evap1)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE SUMS THE NECESSARY FLUXES EACH TIME STEP FOR
C     SNOWPACK ADJUSTMENTS DUE TO VAPOR TRANSFER AND EVAPORATION FROM
C     THE SOIL SURFACE
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       I          L    canopy, snow, residue or soil layer
C       NSP        I    num of layers in snowpack
C       NTIMES     I    num of current time step within the hour
C       QVSP       I    vapor flux between snow nodes i and i+1
C       RHOL       P    density of liquid water  (kg/m^3)
C       TOPSNO    I/O   sum of vapor entering the top of the snowpack
C       TQVSP     I/O   sum of vapor flux in snow pack over the hour
C       VFLUX      I    turbulent vapor transfer at surface node
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
      PARAMETER (MXNOD=300, NODRES=10, nodcan=11, MXSPEC=10, NODSNO=100)
C
      DOUBLE PRECISION QVC(NODCAN),QVSP(NODSNO),TQVSP(NODSNO),
     +  QVR(NODRES),TRNSP(MXSPEC),TPOTNL(MXSPEC),XTRACT(MXNOD),
     +  TRANSP(MXSPEC),TPOT(MXSPEC),ROOTXT(MXNOD),lwcan(mxspec,nodcan),
     +  lwres(nodres),lwsnow,lwsoil,swres(nodres),swcan(mxspec,nodcan),
     +  swsnow(nodsno)

C
      PARAMETER (RHOL=1.0D3,LV=2.5D6)
C
      IF (NTIMES .EQ. 1) THEN
C        INITIALIZE SUMMATIONS
         TOPSNO=0.0D0
         tswcan = 0.0d0
         tlwcan = 0.0d0
         tlwres = 0.0d0
         tswres = 0.0d0
         tlwsno = 0.0d0
         tswsno = 0.0d0
         tlwsoi = 0.0d0
         tswsoi = 0.0d0
         HSUM=0.0d0
         gsum=0.0d0
         vLEsum=0.0d0
CXXXX
         EVAPS=0.0D0
         SROOTXT = 0.0d0
         evap1=0.0d0
c
        DO 5 I=1,NSP
            TQVSP(I)=0.0D0
    5    CONTINUE
         DO 10 J=1,NPLANT
            TRANSP(J)=0.0D0
            TPOT(J)=0.0D0
   10    CONTINUE
         DO 15 i=1,NS
            ROOTXT(i)=0.0D0
   15    CONTINUE
      END IF
C
      IF (ISHUTL .EQ. 0) THEN
C
         IF (NSP .GT. 0) THEN
C           ..SUM THE VAPOR FLUX OCCURRING IN, ABOVE AND BELOW THE SNOWPACK
            DO 20 I=1,NSP
               TQVSP(I) = TQVSP(I) + QVSP(I)*DT
   20       CONTINUE
C
C           ..TQVSP(NSP) IS THE VAPOR LEAVING BOTTOM OF SNOWPACK
C             VAPOR FLUX ABOVE SNOWPACK IS FROM ATMOSPHERE
            TOPSNO = TOPSNO + VFLUX*DT
         END IF
C
C        SUM VAPOR TRANSFER FROM SOIL SURFACE FOR SOIL EVAPORATION,
C        TRANPIRATION BY PLANTS, AND ROOT EXTRACTION FROM SOIL LAYERS
         IF (NC .GT. 0) THEN
C           ..SUM TRANSPIRATION FROM EACH PLANT SPECIES
            DO 30 J=1,NPLANT
               TRANSP(J)=TRANSP(J) + TRNSP(J)*DT/RHOL   !include evaporation from intercepted rain and transpiration from living plants.
c               TPOT(J)=TPOT(J) + TPOTNL(J)*DT
               TPOT(J)=TPOT(J) + TPOTNL(J)*DT/RHOL
   30       CONTINUE
C           ..SUM ROOT EXTRACTION FROM EACH SOIL LAYER 
C     Note: rootxt is subscripted by plant species, how is xtract
C           partitioned among plant species?
            DO 40 I=1,NS
            ROOTXT(I)=ROOTXT(I)+XTRACT(I)*DT/RHOL   !only transpiration from living plants.
c            ROOTXT(I)=XTRACT(I)*DT/RHOL
cxxxx       CONVERT FROM KG/M2/S TO M/S
            SROOTXT=SROOTXT+XTRACT(I)*DT/RHOL
c            SROOTXT=SROOTXT+rootxt(i)
c               ROOTXT(I)=ROOTXT(I) + XTRACT(I)*DT/RHOL
c               ROOTXT(I)=ROOTXT(I) + XTRACT(I)*DT/RHOL  !RHOL is not in the new version, 7-14-2010
   40       CONTINUE
         END IF
C
         IF (NR .GT. 0) THEN
            EVAPS = EVAPS - QVR(NR)*DT/RHOL
         ELSE IF (NSP .GT. 0) THEN
            EVAPS = -TQVSP(NSP)/RHOL
         ELSE IF (NC .GT. 0) THEN
            EVAPS = EVAPS - QVC(NC)*DT/RHOL
         ELSE
            EVAPS = EVAPS - VFLUX*DT/RHOL
         END IF
      END IF

c      write (555,*) evaps, srootxt
C     sum heat flux components
C     snow
      IF (NSP .GT. 0) TLWSNO = TLWSNO + LWSNOW*DT
      do i=1,nsp
         tswsno = tswsno + swsnow(i)*dt
      end do
C     canopy
      do i=1,nc
        do j=1,nplant
          tswcan = tswcan + swcan(j,i)*dt
          tlwcan = tlwcan + lwcan(j,i)*dt
        end do
      end do
C     residue
      tswres = tswres + (swres(1)+swres(2))*dt
      tlwres = tlwres + (lwres(1)+lwres(2))*dt
C     soil
      tswsoi = tswsoi + swsoil*dt
      tlwsoi = tlwsoi + lwsoil*dt
C	  surface fluxes
      vLEsum = vLEsum + LV*vflux*dt
      hsum = hsum + hflux*dt
      gsum = gsum + gflux*dt
c      EVAP1 = EVAP1 - srootxt
      EVAP1 = EVAP1 + vflux/RHOL*DT
C
      RETURN
      END
