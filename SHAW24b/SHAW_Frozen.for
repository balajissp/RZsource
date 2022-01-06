C
      SUBROUTINE FROZEN(I,NS,VLC,VIC,MAT,CONC,TS,SALT,ICES,SOILHP,
     +                   NDXN2H,RHOB,NSALT,SALTKQ,SAT,Hmin)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE DETERMINES THE LIQUID AND ICE WATER CONTENT OF THE
C     SOIL FOR TMP < 0 C  AND DETERMINES IF ANY ICE MAY BE PRESENT.
C     IT THEN UPDATES THE MATRIC POTENTIAL AND SOLUTE CONCENTRATION.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       CONC      I/O   conc of j-th solute in soil soln at time t (eq/kq)
C       DELTA      L    matrix soln for change in para for i-th node
C       DLDM       L    derivative of liquid content with respect to
C                       matric potential(m^-1)
C       G          P    acceleration of gravity
C       I          I    canopy, snow, residue or soil layer
C       ICES      I/O   1 if soil layer contains both ice and water at t
C       ITER       L    current num of iter for energy and water balance
C       J          L    specifies solute species in soil or plant species
C       LF         P    latent heat of fusion
C       MAT       I/O   matric potential  (m)
C       MAT1       L    matric potential
C       NS         I    num of soil layers
C       NSALT      I    num of different types of solutes to be simulated
C       RHOB       I    bulk density of soil  (kg/m^3)
C       RHOI       P    density of ice   (kg/m^3)
C       RHOL       P    density of liquid water  (kg/m^3)
C       SALT       I    amount of j-th solute in i-th soil node at t
C       SALTKQ     I    adsorption char of solute j for node i  (kg/kg)
C       SAT        I    sat WC
C       TOTPOT     L    total water potential
C       TS         I    temp of soil layer at time t
C       UGAS       P    universal gas constant
C       VIC        I    vol ice content
C       VLC       I/O   vol liquid content
C
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                       SPMOIS
C                       WC
C                       WCH
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
      PARAMETER (MXNOD=300, MAXHOR=12, MXCHEM=15)
C
      DOUBLE PRECISION VLC(MXNOD),VIC(MXNOD),CONC(MXCHEM,MXNOD),
     +  MAT(MXNOD),TS(MXNOD),SALT(MXCHEM,MXNOD),SOILHP(13,MAXHOR),
     +  RHOB(MXNOD),SALTKQ(MXCHEM,MXNOD),SAT(MXNOD)
      INTEGER ICES(MXNOD),NDXN2H(NS)
      DOUBLE PRECISION LF
      DOUBLE PRECISION MAT1

      PARAMETER(LF=3.35D5,G=9.81D0,UGAS=8.3143D0,RHOL=1.0D3,RHOI=9.2D2)
c      PARAMETER (HMIN=-35000.0D0)
C
C     ITERATE TO FIND THE MAXIMUM WATER CONTENT FROM THE TOTAL, MATRIC
C     AND OSMOTIC POTENTIALS:
C               TOTAL - MATRIC - OSMOTIC = 0 => F
C     MATRIC AND OSMOTIC POTENTIALS ARE FUNCTIONS OF WATER CONTENT.
C     SOLVE BY NEWTON-RAPHSON ITERATION  =>  D(VLC) = -F/(DF/D(VLC)
C
      ICONV=0
    5 TMP=TS(I)+273.16D0
      TOTPOT = LF*TS(I)/(TMP*G)
      IF (TOTPOT.LT.HMIN/100.d0) TOTPOT = HMIN/100.d0
CCCC  CALL MATVL2 (I,TOTPOT,VLC1,DUMMY,B,ENTRY,SAT)
      JH=NDXN2H(I)
C LMA UPDATE WC FUNCTION
      VLC1 = WC(TOTPOT*1.0D2,SOILHP(1,JH),JH,0)
cccc  THE FOLLOWING SECTION WAS NOT WORKING AS INTENDED AND WAS 
cccc  COMMENTED OUT  ---->  GNF 04/29/11
c     check if water content is below matric potential limit
cccc  delmat=WCH(HTMP,vlc1,SOILHP(1,JH),JH,0)/1.0D2
cccc  if (totpot .lt. delmat-0.1d0) then
c        set vlc1 to minimum water content to start at edge of step function 
ccc      VLC1 = WC(delmat*1.0D2,SOILHP(1,JH),JH,0)
cccc     imnflg = 1
cccc    else
cccc     imnflg = 0
cccc  end if
C
C**** BEGIN ITERATIONS
      ITER=0
C     CALCULATE OSMOTIC POTENTIAL AND DERIVATIVE WITH RESPECT TO LIQUID
   10 OSMPOT=0.0D0
      DERIV=0.0D0
      DO 20 J=1,NSALT
         TERM = SALT(J,I)*UGAS*TMP/(SALTKQ(J,I)+VLC1*RHOL/RHOB(I))
         OSMPOT= OSMPOT-TERM
         DERIV= DERIV+TERM*RHOL/RHOB(I)/(SALTKQ(J,I)+VLC1*RHOL/RHOB(I))
   20 CONTINUE
      OSMPOT= OSMPOT/G
      DERIV= DERIV/G
C
C     CALCULATE MATRIC POTENTIAL AND DERIVATIVE WITH RESPECT TO LIQUID
CCCC  CALL MATVL1 (I,MAT1,VLC1,DUMMY,B,ENTRY,SAT)
CCCC  CALL MATVL3 (I,MAT1,VLC1,DLDM,B,ENTRY)
      HTMP = -(SOILHP(1,JH)+5.0D0)
C LMA UPDATE WCH
      MAT1 =WCH(HTMP,VLC1,SOILHP(1,JH),JH,0)/1.0D2
C LMA UPDATE SPMOIS
      DLDM=SPMOIS(MAT1*1.0D2,SOILHP(1,JH),JH)*1.0D2
      IF (DLDM .LE. 0.0D0) THEN
C        SOIL IS SATURATED - RECALCULATE BASED ON SAT. WATER CONTENT
         DLDM = (SOILHP(6,JH)-SOILHP(5,JH))
     >          *SOILHP(2,JH)/(SOILHP(1,JH)/100.D0)
cccc  THE FOLLOWING SECTION WAS NOT WORKING AND WAS COMMENTED OUT
cccc	IT NOW FUNCTIONS MORE LIKE THE ORIGINAL SHAW MODEL
cccc  ---- >  GNF 04/29/11
ccc     else
c        recalculate specific moisture to avoid problems when water
c        content goes below wilt point (RZWQM limits matric potential)
c        check which way the iteration needs to go to be careful around
c        the step function at the minimum water potential limit
ccc      if (osmpot+mat1 .lt. totpot) then 
ccc         delvlc = vlc1 + 1.0d-5
ccc         delmat=WCH(HTMP,delvlc,SOILHP(1,JH),JH,0)/1.0D2
ccc         dmdl = -(mat1-delmat)/1.0d-5
ccc        else 
ccc         delvlc = vlc1 - 1.0d-5
ccc         if (delvlc .le. 0.0d0) then delvlc = vlc1/2.d0
ccc         delmat=WCH(HTMP,delvlc,SOILHP(1,JH),JH,0)/1.0D2
ccc         dmdl =(mat1-delmat)/1.0d-5
ccc      end if 
ccc      if (dmdl .le. 0.0d0) dmdl = 0.0d0
      END IF
C
C     DERTERMINE ERROR IN WATER POTENTIAL AND ADJUST WATER CONTENT
      ERROR = TOTPOT - OSMPOT - MAT1
c      ERROR = TOTPOT  - MAT1
      DELTA = ERROR/(DERIV + 1.0D0/DLDM)
cccc  DELTA = ERROR/(DERIV + dmdl)
      VLC2 = VLC1 + DELTA
      IF (VLC2 .GT. SAT(I)) VLC2=SAT(I)
      IF (VLC2 .LE. VLC1/2.0D0) VLC2=VLC1/2.0D0
      DELTA = VLC2 - VLC1
      VLC1=VLC2
      IF (ABS(DELTA) .LT. 1.0D-5) GO TO 30
      ITER=ITER+1
      IF (ICONV .GT. 0)
     +WRITE (100,101) ITER,OSMPOT,MAT1,VLC1,DERIV,1.0D0/DLDM,ERROR
cccc +WRITE (100,*) ITER,OSMPOT,MAT1,VLC1,DERIV,1.0D0/dmdl,ERROR
      IF (ITER .GT. 30) THEN
         ICONV = ICONV+1
         WRITE (100,100)
         WRITE (100,101) I,TS(I),VLC(I),VIC(I),TOTPOT
         IF (ICONV .EQ. 1) GO TO 5
         STOP 'no convergence in frozen'
      END IF
      GO TO 10
C
C**** IF ACTUAL LIQUID PLUS ICE CONTENT IS LESS THAN THAT CALCULATED
C     FROM ABOVE, THERE IS NO ICE PRESENT IN THE LAYER.  (THE TOTAL
C     POTENTIAL IS SUFFICIENTLY NEGATIVE THAT THE WATER WILL NOT
C     FREEZE.)
   30 IF ((VLC(I)+VIC(I)*RHOI/RHOL).LT.VLC2) THEN
C*       NO ICE IS PRESENT
         ICES(I)=0
         VLC(I)=VLC(I) + VIC(I)*RHOI/RHOL
         VIC(I)=0.0D0
CCCC     CALL MATVL1 (I,MAT(I),VLC(I),DLDM,B,ENTRY,SAT)
         HTMP = -(SOILHP(1,JH)+5.0D0)
C LMA UPDATE WCH
         MAT(I) = WCH(HTMP,VLC(I),SOILHP(1,JH),JH,0)/1.0D2
        ELSE
C*       ICE MAY BE PRESENT --> VLC AND MAT ARE FUNCTION OF TEMP
         ICES(I)=1
         VIC(I)=VIC(I)+(VLC(I)-VLC2)*RHOL/RHOI
         VLC(I)=VLC2
         if (vlc(i)+vic(i)*rhoi/rhol - sat(i) .gt. 1.0d-5) then
        print*,'in frozen, thetai* + theta >sat/check FROZEN Subroutine'
        print*,i,vlc(i),vic(i),vlc(i)+vic(i)*rhoi/rhol,sat(i)
        write(9,*) 'in frozen'                         !print out to MBLWAT.OUT
        write(9,*) i,vlc(i),vic(i),vlc(i)+vic(i)*rhoi/rhol,sat(i)
         endif
         HTMP = -(SOILHP(1,JH)+5.0D0)
C LMA UPDATE WCH
         MAT(I) = WCH(HTMP,VLC(I),SOILHP(1,JH),JH,0)/1.0D2
      END IF
      DO 40 J=1,NSALT
         CONC(J,I)=SALT(J,I)/(SALTKQ(J,I) + VLC(I)*RHOL/RHOB(I))
   40 CONTINUE
      RETURN
  100 FORMAT (//10X,'*****  PROGRAM WAS STOPPED IN SUBROUTINE FROZEN DUE
     + TO CONVERGENCE PROBLEMS *****')
  101 FORMAT (10(G16.5,3X))
      END
C
