C======================================================================
C  SF_NFACTO, Subroutine
C
C  Determines N deficit in Maize
C----------------------------------------------------------------------
C  Revision history
C  xx/xx/19xx     Written
C  02/08/1993 PWW Header revision and minor changes              
C  03/29/2001 WDB Converted to modular form                      
C  12/01/2001 WDB Further modular conversion  
!  02/25/2005 CHP Check for NFAC < 0.                   
C----------------------------------------------------------------------
C
C  Called : SF_GROSUB
C
C  Calls  : None
C----------------------------------------------------------------------

      SUBROUTINE SF_NFACTO(DYNAMIC,GLFWT,STMWT,HEADWT,               !Control
     %    ISTAGE,TANC,TCNP,TMNC,STOVWT,SLFWT,XLCNP,                  !Inputs
     %    AGEFAC,NDEF3,NFAC,NSTRES,XSMNC,XLANC,XLMNC,XHMNC,RMNC,xstage,
     &    XSCNP,RCNP,XHCNP)               !Outputs
      
      USE ModuleDefs
      IMPLICIT NONE
      SAVE
C----------------------------------------------------------------------
C                     Variable Declaration
C----------------------------------------------------------------------
      REAL        AGEFAC      
      INTEGER     ISTAGE      
      REAL        NDEF3       
      REAL        NFAC 
      REAL        NSTRES      
      REAL        TANC        
      REAL        TCNP        
      REAL        TMNC
      REAL        XSTAGE,XLCNP,XLMNC,XSCNP,XSMNC,RCNP,RMNC,XHCNP,XHMNC
      REAL        SLFWT,STOVWT,GLFWT,HEADWT,STMWT,XLANC,CNSD1,CNSD2
      INTEGER     DYNAMIC     
C     ----------------------------------------------------------------


      IF(DYNAMIC.EQ.RUNINIT) THEN
          NSTRES = 1.0    
          AGEFAC = 1.0    
          NDEF3 = 1.0     
          NFAC = 1.0      

      ELSEIF(DYNAMIC.EQ.SEASINIT) THEN
          NSTRES = 1.0
          AGEFAC = 1.0
          NDEF3 = 1.0     
          NFAC = 1.0  

      ELSEIF(DYNAMIC.EQ.INTEGR) THEN


! If the actual nitrogen content in the above ground biomass (TANC) 
! decreases below a crtitical level (TCNP), then compute N stress 
! based on proportion nitrogen below critical level (TCNP-TANC) and 
! total nitrogen between the critical level and minimum 
! level (TCNP-TMNC). 

      !
      ! Calculate critical and minimum N concentrations
      !
      XLCNP  = (02.94 * EXP(-0.326*XSTAGE) + 3.26)/100.0
      XLMNC  = XLCNP - 0.025
      XSCNP  = (03.29 * EXP(-0.516*XSTAGE) + 1.25)/100.0
      XSMNC  = XSCNP - 0.0095
      RCNP   = (03.61 * EXP(-0.521*XSTAGE) + 1.05)/100.0
      RMNC   = RCNP  - 0.0062
      XHCNP  = (21.37 * EXP(-0.600*XSTAGE) + 1.60)/100.0
      XHMNC  = XHCNP - 0.00895
      !
      ! Calculate total N in plant parts
      !
      TCNP   = (XLCNP*GLFWT+XSCNP*STMWT+XHCNP*HEADWT)/(STOVWT-SLFWT)
      TMNC   = (XLMNC*GLFWT+XSMNC*STMWT+XHMNC*HEADWT)/(STOVWT-SLFWT)
      !
      ! N factor : 0-1
      !
      NFAC   = 1.0 - (TCNP-TANC)/(TCNP-TMNC)         
      NFAC   = AMIN1 (NFAC,1.000)
      NFAC   = AMAX1 (NFAC,0.001)
      NSTRES = 1.0
      AGEFAC = 1.0
      NDEF3  = 1.0
      AGEFAC = NFAC
c      write (100, *) nfac,tcnp,tanc,tmnc,xstage
      NSTRES =  1.0 - (XLCNP-XLANC)/(XLCNP-XLMNC)
      IF (NSTRES .GT. 0.5)  THEN
          NSTRES = NSTRES * 0.4  + 0.6
       ELSE
         IF (NSTRES .GT. 0.0) THEN
            NSTRES = NSTRES * 1.2  + 0.2
          ELSE
            NSTRES = 0.2
         ENDIF
      ENDIF
      NSTRES = AMIN1 (NSTRES,1.0)

      IF (NFAC .LT. 0.8) THEN
         NDEF3 = AMIN1 (0.2 + NFAC,1.0)
      ENDIF

      AGEFAC = AMIN1 (AGEFAC,1.0)
      NDEF3  = AMIN1 (NDEF3, 1.0)
      CNSD1  = CNSD1 + 1.0 - NSTRES
      CNSD2  = CNSD2 + 1.0 - AGEFAC
      ENDIF
 
      RETURN

      END SUBROUTINE SF_NFACTO


C----------------------------------------------------------------------
C                         DEFINITIONS
C----------------------------------------------------------------------
! AGEFAC      !Nitrogen stress factor affecting expansion (0-1)
! ISTAGE      !Plant growth staqe
! NDEF3       !Nitrogen stress factor affecting grains per plant (0-1)
! NFAC        !Nitrogen stress factor based on actual and critical 
!              nitrogen content in vegetative tisue
! NSTRES      !Nitrogen stress factor affecting growth (0-1)
! TANC        !Nitrogen content in above ground biomass, decimal
! TCNP        !Critical nitrogen concentration in tops, g N/g dry wt.
! TMNC        !Plant top minimum N concentration g N/g dry matter
! DYNAMIC     !Modular control variable
! RUNINIT     !DYNAMIC control variable
! SEASINIT    !DYNAMIC control variable
! RATE        !DYNAMIC control variable
! INTEGR      !DYNAMIC control variable
! OUTPUT      !DYNAMIC control variable
! FINAL       !DYNAMIC control variable
C----------------------------------------------------------------------

