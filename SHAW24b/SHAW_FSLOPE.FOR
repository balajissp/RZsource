C
      SUBROUTINE FSLOPE(I,NS,DLDT,PTDLDT,TMP,MAT,CONC,VLC,SOILHP,
     +                   NDXN2H,RHOB,NSALT,SALTKQ)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE DETERMINES THE SLOPE OF THE LIQUID CONTENT -
C     TEMPERATURE CURVE (DLDT) AS WELL AS THE PARTIAL OF T*DLDT.
C     (OSMOTIC EFFECTS ARE INCLUDED.)
C     ---- TMP MUST BE IN DEGREES KELVIN
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       CONC       I    conc of j-th solute in soil soln at time t (eq/kq)
C       DLDM       L    derivative of liquid content with respect to
C                       matric potential(m^-1)
C       DLDT      I/O   derivitive of soil liquid content with respect to temp
C       G          P    acceleration of gravity
C       I          I    canopy, snow, residue or soil layer
C       J          L    specifies solute species in soil or plant species
C       LF         P    latent heat of fusion
C       MAT        I    matric potential  (m)
C       NS         I    num of soil layers
C       NSALT      I    num of different types of solutes to be simulated
C       RHOB       I    bulk density of soil  (kg/m^3)
C       RHOL       P    density of liquid water  (kg/m^3)
C       SALTKQ     I    adsorption char of solute j for node i  (kg/kg)
C       UGAS       P    universal gas constant
C       VLC        I    vol liquid content
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                       SPMOIS
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
      DOUBLE PRECISION MAT(MXNOD),CONC(MXCHEM,MXNOD),VLC(MXNOD),
     +  RHOB(MXNOD),SALTKQ(MXCHEM,MXNOD),SOILHP(13,MAXHOR)
      DOUBLE PRECISION LF
      INTEGER NDXN2H(NS)

      PARAMETER(LF=3.35D5,G=9.81D0,UGAS=8.3143D0,RHOL=1.0D3)
C
      CNCGAS=0.0D0
      CRT=0.0D0
      DO 10 J=1,NSALT
         DKQ=SALTKQ(J,I) + VLC(I)*RHOL/RHOB(I)
         CNCGAS=CNCGAS + CONC(J,I)*UGAS
         CRT=CRT + CONC(J,I)*UGAS*TMP*RHOL/RHOB(I)/DKQ
   10 CONTINUE
C
CCCC  CALL MATVL3 (I,MAT(I),VLC(I),DLDM,B,ENTRY)
      JH=NDXN2H(I)
C LMA UPDATE SPMOIS
ccc   gnf 12/18/02
      DLDM=SPMOIS(MAT(I)*1.0D2,SOILHP(1,JH),JH)*1.0D2
c     calculate specific moisture by slope method to avoid problems when
c     water content goes below wilt point(RZWQM limits matric potential)
cc      delvlc= vlc(i) - .001d0
cc      if (delvlc .le. 0.0d0) then delvlc = vlc(i)/2.d0
cc      delmat=WCH(HTMP,delvlc,SOILHP(1,JH),JH,0)/1.0D2
cc      dmdl =(mat(i)-delmat)/0.001d0
cc      if (dmdl .le. 0.0d0) dmdl = 0.0d0
C
      if (DLDM.gt.0.0d0) then
      DLDT=(LF/TMP + CNCGAS)/(G/DLDM + CRT)
      else
      DLDT=0.0d0
      endif
c      DLDT=(LF/TMP + CNCGAS)/(G*dmdl + CRT)
C
      RETURN
      END
C
