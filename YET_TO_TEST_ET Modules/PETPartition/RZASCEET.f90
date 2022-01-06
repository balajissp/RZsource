!****************************************************************************
! SUBROUTINE ASCE_PET_PART( ... )
!     Purpose: Calculate ET0 and partition into ETc (ETc_HR)
!              Call Partition subroutine to partition based on defined option (PARTFLAG)
!       
!     Called by PHYSCL?
!       VARIABLE DEFINITIONS:
!       TYP  VARIABLE    USE   DESCRIPTION
!       ---  --------   -----  -----------
!       
!****************************************************************************
 SUBROUTINE ASCE_PET_PART(I,tmin,tmax,hrts1,hrh1,hru1,elev,lat,long,  &
       jday,trat,xw,PET,PER,PES,KC_CROP,TLAI, rm, cres,co2r,          &
       CROP, PARTFLAG, KSEVAP, KTRANS, ihourly, ISTAGE, NFAC)
      USE RZ_ETC_PARTITION 
      implicit none
      double precision tmin,tmax, hrts1, hrh1, hru1, elev, lat, long,   &
       trat, xw, PET, PER, PES, ETr_hr_T, ETr_hr_S, ETc_HR, KC_CROP,    &
       TLAI,  KC_CAL, rm, cres, co2r, KSEVAP, KTRANS, TAVG, wind, NFAC
      INTEGER I, jday, PARTFLAG,ihourly, ISTAGE !, refheightflag
      CHARACTER(LEN=2)  CROP
!*********************************************************************
!   Prepare other required data for ASCE-REF_ET:
!********************************************************************* 
      IF (ihourly.eq.1) THEN
          TAVG = TMAX
      ELSE
          TAVG = (TMIN+TMAX)/2
      ENDIF
      wind = hru1
      if (ihourly.eq.0) then
          wind = hru1*(1000.)/86400. ! km/day to m/s
      endif
!*********************************************************************
!   Get PotentialEvap for the Crop -> call REF_ET, get back ET0
!*********************************************************************
      CALL REF_ET(I,tmin,tmax,hrts1,hrh1,wind,elev,lat,long,jday, &
      ETr_hr_T,ETr_hr_S,trat,xw, ihourly)
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
         KC_CAL = 1.0D0 + (KC_CROP - 1.0D0) * TLAI/3.0D0
      !    KC_CAL = 1.0D0 + (KC_CROP - 1.0D0) * TLAI/6.0D0
      !ENDIF
      !ENDIF
!*********************************************************************
!   Calculate ETc using Crop Coeff:
!           1) using Short grass (default)
!           2) using alfalfa (possible option in future?)
!*********************************************************************           
      ! if using shortgrass:
      !IF (refHeightFlag.eq.0) THEN ! IF using LAI impact on crop coeff:
          ETc_HR = ETr_hr_S/10.0D0 * KC_Cal
      !ELSE ! Else using alfala
      !    ETc_HR = ETr_hr_T/10.0D0 * KC_Cal
      !END IF
!*********************************************************************
!   Partition using method chosen by PARTFLAG
!********************************************************************* 
      CALL ETC_PARTITION(PARTFLAG, ETc_HR, PET, PER, PES, TLAI, &
           RM, CRES, CO2R, CROP, KSEVAP, KTRANS, TMIN, TMAX,    &
           hru1,hrts1, ISTAGE, NFAC)
!*********************************************************************
!       Done
!********************************************************************* 
END SUBROUTINE ASCE_PET_PART
!****************************************************************************       
