!****************************************************************************
! SUBROUTINE DSSATDYN_PET_PART( ... )
!     Purpose: Calculate ET0 and partition int ETc (ETc_HR)
!               Call Partition subroutine to partition based on defined option (PARTFLAG)
!       
!     Called by PHYSCL?
!       VARIABLE DEFINITIONS:
!       TYP  VARIABLE    USE   DESCRIPTION
!       ---  --------   -----  -----------
!       
!****************************************************************************
 SUBROUTINE DSSATDYN_PET_PART(SWL1,DUL1,CANHT,CLOUDS,SRAD,TMIN,TMAX,    &
            WINDSP, TLAI, A0, ARI, RESAGE, PET, PER, PES,KC_CROP,       &
            rm, cres,co2r, CROP, PARTFLAG, KSEVAP, KTRANS, ihourly,     &
            ISTAGE, NFAC)
      USE RZ_ETC_PARTITION 
      IMPLICIT NONE
      SAVE
      DOUBLE PRECISION TMIN,TMAX,TAVG, SWL1, DUL1, CANHT, CLOUDS, SRAD, &
        WINDSP, TLAI, ALBRES, A0, ARI,PET, PER, PES, ETc_HR, KC_CROP,   &
        KC_CAL, rm, cres, co2r, KSEVAP, KTRANS, TDEW, RESAGE, NFAC
      REAL EO, MULCHCOVER, MULCHALB, MSALB, FF, SALB, SWALB
      INTEGER I, jday, RATE, PARTFLAG, ihourly, ISTAGE
      CHARACTER(LEN=2)  CROP
      
!*********************************************************************
!   Prepare other required data for dynamic PET via DSSAT:
!*********************************************************************
!   Calculate TAVG
    TAVG = (TMIN+TMAX)/2
!   Assume dewpoint temp is tmin
    TDEW = TMIN 
!   soil albedo
    SALB = REAL(A0)    
    !DSSAT:
    ! MULCHCOVER = 1.0 - EXP(-AM * 1.E-5 * MULCHMASS)
    ! RZ: (same thing) RM = MULCHMASS, CRES = Asub, MulchMass = kg/ha
    MULCHCOVER = REAL(1 - dexp(-(cres)*(rm/(10.0d0 * 1000.0d0))))
    ! Option: DSSAT hardcodes mulchalb to 0.45
    ! MULCHALB = 0.45
    ! RZ calculates via following equ:
    ! need albedo dry soil, abledo fresh residue, and residue age.
    MULCHALB   = REAL(A0*(1.06D0+((ARI/A0)-1.06D0)*DEXP(-0.0255D0*RESAGE)))
    MULCHALB   = MAX (MULCHALB,0.0)
! RMarquez -> From DSSAT 4.6, calculate MSALB, mulch + soil albedo
    FF = (SWL1 - 0.03) / (DUL1 - 0.03)
    FF = MAX(0.0, MIN(2.0, FF))
    !SWALB = SOILPROP % SALB * (1.0 - 0.45 * FF)
    SWALB = SALB * (1.0 - 0.45 * FF)
    MSALB = MULCHCOVER * MULCHALB + (1.0 - MULCHCOVER) * SWALB
! end calculate mulch + soil albedo
    
!*********************************************************************
!   Get PotentialEvap for the Crop -> call PETDYN, get back ET0
!*********************************************************************

    CALL PETDYN(REAL(CANHT/100.0d0), REAL(CLOUDS), MSALB, REAL(SRAD),   &   !INPUT
                REAL(TAVG), REAL(TDEW), REAL(TMAX), REAL(TMIN), &   !INPUT
                REAL(WINDSP),REAL(TLAI),                        &   !Input
                EO)                                                 !Output
      ETc_HR = dble(eo)
!*********************************************************************
!   Calculate Crop Coeff, two options:
!       1) Calculate vai LAI equation (can be 3.0 or 6.0, need to understand difference)
!       2) Use the Coefficient directly from user without new calculations.
!*********************************************************************
      !KC_CAL = KC_CROP
      !if (useLAIKC.eq.0) THEN
      ! RMarquez - two equations: LAI/6.0 or LAI/3.0? which is better/correct
      !    KC_Cal = 1.0 + (KC_CROP - 1.0)*(LAI/6.0)
      !IF (KC.gt.1.0D0 .and. LAI.lt.3.0d0) THEN
      !   KC_CAL = 1.0D0 + (KC_CROP - 1.0D0) * TLAI/3.0D0
      !    KC_CAL = 1.0D0 + (KC_CROP - 1.0D0) * TLAI/6.0D0
      !ENDIF
      !ENDIF
!*********************************************************************
!   Calculate ETc using Crop Coeff
!*********************************************************************      
          ETc_HR = ETc_HR/10.0D0 ! * KC_Cal
!*********************************************************************
!   Partition using method chosen by PARTFLAG
!*********************************************************************
      CALL ETC_PARTITION(PARTFLAG, ETc_HR, PET, PER, PES, TLAI, &
           RM, CRES, CO2R, CROP, KSEVAP, KTRANS, TMIN, TMAX,    &
           WINDSP, SRAD, ISTAGE, NFAC)
!*********************************************************************
!       Done
!********************************************************************* 
            END SUBROUTINE DSSATDYN_PET_PART
!****************************************************************************       

            !RMarquez - From DSSAT 4.6 -> calculate MSALB, extracted key parts, used above
!!=======================================================================
!      SUBROUTINE ALBEDO(KTRANS, MEINF, MULCH, SOILPROP, SW1, XHLAI)
!      !Update soil albedo based on mulch cover and soil water content in
!      !top layer
!
!!-----------------------------------------------------------------------
!!  REVISION HISTORY
!!  07/24/2006 CHP Written
!!  10/23/2007 CHP Use species-dependant KTRANS for canopy 
!!                    light interception
!!-----------------------------------------------------------------------
! 
!      USE ModuleDefs
!      IMPLICIT NONE
!
!      TYPE (MulchType) MULCH    !Surface mulch properties
!      TYPE (SoilType)  SOILPROP !Soil properties
!
!      CHARACTER*1 MEINF
!      REAL CANCOV, CMSALB, FF, KTRANS, MSALB, MULCHALB, MULCHCOVER
!      REAL SW1, SWALB, XHLAI
!
!!     ---------------------------------------------------
!!     Calculate albedo changes with soil water content
!      FF = (SW1 - 0.03) / (SOILPROP % DUL(1) - 0.03)
!      FF = MAX(0.0, MIN(2.0, FF))
!      SWALB = SOILPROP % SALB * (1.0 - 0.45 * FF)
!
!!     1/18/2008 chp change albedo calculations back to original at GH's request.
!!     Probably temporary-- temp chp
!!!     chp 12/21/2007
!!!     Based on Idso, Jackson et al., 1975. The dependence of bare soil 
!!!     albedo on soil water content. Journal of Applied Meteorology 14, 109-113. 
!!!     Max albedo ~0.30 at air dry, Min albedo ~DUL
!!      SWAD = 0.30 * LL1 !JTR 11/28/2006
!!      FF = (SW1 - SWAD) / (SOILPROP % DUL(1) - SWAD)
!!      FF = MAX(0.0,MIN(1.0,FF))
!!!     SWALB = SOILPROP % SALB * FF + 0.30 * (1. - FF)
!!
!!!     Dry soil albedo as function of wet soil albedo
!!!     Relationship from data of Post, et al.,  Soil Sci. Soc. Am. J. 64:1027-1034 (2000).
!!      Wet_alb = SOILPROP % SALB
!!      Dry_alb = Wet_alb * 1.7
!!      SWALB = Wet_alb * FF + Dry_alb * (1. - FF)
!!
!!!     Lobell and Asner, Moisture Effects on Soil Reflectance. Soil Sci. Soc. Am. J. 66:722-727 (2002).
!!!     Uses SAT to represent wet soils instead of DUL
!!!     R = Rsat + (Rdry - Rsat) * exp(-c*SW)
!!!     No data to parameterize, so don't use this method.
!
!!     IF (INDEX('RSN',MEINF) .LE. 0) THEN
!      IF (INDEX('RSM',MEINF) > 0) THEN   
!!       Update combined soil/mulch albedo
!!       Transfer local values from constructed variables
!        MULCHCOVER = MULCH % MULCHCOVER
!        MULCHALB   = MULCH % MULCHALB
!
!!       Albedo with mulch cover
!        MSALB = MULCHCOVER * MULCHALB + (1.0 - MULCHCOVER) * SWALB
!      ELSE
!        MSALB = SWALB
!      ENDIF
!
!!     Albedo with canopy cover
!!     CANCOV = 1.0 - EXP(-0.75 * XHLAI)
!      CANCOV = 1.0 - EXP(-KTRANS * XHLAI)
!      CMSALB = CANCOV * 0.23 + (1.0 - CANCOV) * MSALB
!
!      SOILPROP % CMSALB = CMSALB
!      SOILPROP % MSALB  = MSALB
!      SOILPROP % SWALB  = SWALB
!
!!!       Temporary -- print soil albedo stuff
!!     GET (CONTROL)
!!     CALL YR_DOY(CONTROL.YRDOY, YEAR, DOY)
!!        WRITE(2250,'(1X,I4,1X,I3.3,1X,I5,8F8.3)') YEAR, DOY, CONTROL.DAS, SOILPROP.SALB, 
!!     &      FF, SWALB, MULCHCOVER, MSALB, CANCOV, CMSALB
!
!      RETURN
!      END SUBROUTINE ALBEDO
!!=======================================================================