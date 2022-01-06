!****************************************************************************
! subroutine APSIM_WATERDEMAND( . . .)
!       
!
! **************
! Written 3.15.2016 -> APSIM approach to partitioning ETc into Potential Transpiration for the crop.
! **************
!   Calls:
!   TABEX
!****************************************************************************
subroutine APSIM_WATERDEMAND(TMIN,TMAX, CROP_TRANSFACT, CO2Y, CO2X, CO2,      &
                        SIZE_CO2TABLES, KTRANS, TLAI, TSTRESSY,TSTRESSX,      &
                        SIZE_TSTRESSTABLES, NFAC, CROP_NFACT, RUE_CURR_PHASE, &
                        SVP_FACTOR, SRAD, ETc, PET)
    implicit none
    !OUTPUT
    double precision, intent(OUT) :: PET !potential transpiration, min of (biomass demand and max potential transpiration)
    !INPUT
    double precision, intent(in) :: TMIN                ![oC]    Min temperature 
    double precision, intent(in) :: TMAX                ![oC]    Min temperature
    double precision, intent(in) :: CROP_TRANSFACT      ![kpa/g carbo per m^2 / mm water]     Crop transpiration efficiency factor
    double precision, intent(in) :: CO2                 ![ppm]  CO2 actual 
    double precision, intent(in) :: KTRANS              ![0+]     K coeff transpiration []
    double precision, intent(in) :: TLAI                ![0+]     Leaf area index       []
    double precision, intent(in) :: NFAC                ![0-1]     Nitrogen factor plant  []
    double precision, intent(in) :: CROP_NFACT          ![0+]     multipler for N deficit effect on photosynthesis []
    double precision, intent(in) :: RUE_CURR_PHASE      ![(g dm/mj)]     RUE for current phase
    double precision, intent(in) :: SVP_FACTOR          ![0-1]     SVP factor             []
    double precision, intent(in) :: SRAD                ![mj/m^2/(day or hr)] Solar rad
    double precision, intent(in) :: ETc                ![cm/(day or hr)] ET for crop
    
    integer, intent(in) :: SIZE_TSTRESSTABLES           ! Size of Temperature stress tables
    integer, intent(in) :: SIZE_CO2TABLES               ! Size of CO2 tables
    
    real, intent(in) :: TSTRESSY (SIZE_TSTRESSTABLES)        ! multiplier on RUE
    real, intent(in) :: TSTRESSX (SIZE_TSTRESSTABLES)        ! mean daily temperature
    real, intent(in) :: CO2Y(SIZE_CO2TABLES)             ! CO2Y crop
    real, intent(in) :: CO2X(SIZE_CO2TABLES)             ! CO2X crop

    !LOCAL
    double precision :: SVP_TMAX                        !SVP for max temp
    double precision :: SVP_TMIN                        !SVP for min temp
    double precision :: VPD                             !Vap Pressure Deficit
    double precision :: EOP_EFF                         !PET transpiration efficiency
    double precision :: CO2_FACT                        !CO2 factor on transpiration
    double precision :: CC                              !Crop Cover
    double precision :: TAVG                            !Temperature average
    double precision :: TEMPSTRESS_PHOTO                !Temperature stress photosynthesis
    double precision :: N_DEF                           !Nitro deficit
    double precision :: N_STRESS                        !Nitro stress
    double precision :: USRUE                           !RUE usage
    double precision :: BIOMASSPROD_POTENTIAL           !Biomass production potential
    double precision :: SWDEMAND_RAW                    !PET biomass production water demand
    double precision :: SWDEMAND_MAX                    !PET max using ETc and canopy cover
    double precision :: SOLARRAD                        !SolarRad adjusted for RUE
    !FUNCTIONS
    real :: tabex

    !Process:
    !   1) Calculate Biomass coeff
    !   2) Calculate crop transpiration effiency factor
    !   3) Calculate crop SW demand based on 1) and 2)
    !   4) bound sw demand: min of 3) and EOP calculated using canopy cover
!***************************************************************************
!   Step 1:
!***************************************************************************
    SVP_TMAX = 6.1078          &
              * exp (17.269*tmax/ (237.3 + tmax))          &
              * 0.1D0  ! mb2kpa = 1mb = 0.1kpa
    SVP_TMIN = 6.1078          &
              * exp (17.269*tmin/ (237.3 + tmin))          &
              * 0.1D0 ! mb2kpa = 1mb = 0.1kpa
    VPD = SVP_FACTOR * (SVP_TMAX - SVP_TMIN)
    VPD = max(VPD, 0.01D0) !kpa
    IF ((CROP_TRANSFACT + VPD)> 0.0D0) THEN
        EOP_EFF = (CROP_TRANSFACT/VPD)/(1.0D3/1.0D6) !g2mm 
    ELSE
        EOP_EFF = 0.0D0
    ENDIF
    !EOP_EFF = g dm/m^2/mm water
    ! need table of co2y, co2x values + current co2 value
    ! Co2y, co2x arrays of size (SIZE_CO2TABLES) (10 for mz)
    CO2_FACT = dble(TABEX (CO2Y,CO2X,real(CO2),SIZE_CO2TABLES))
    EOP_EFF = EOP_EFF * CO2_FACT
!***************************************************************************
!   Step 2:
!***************************************************************************
!             call crop_dm_pot_rue (
!     :          g%current_stage
!     :        , c%rue                
!     :        , g%radn_int
!     :        , g%temp_stress_photo  ! Temperature stress
!     :        , min(g%nfact_photo, PlantP_pfact_photo()) ! photosyn stress
!     :        , g%dlt_dm_light)

    CC = 1.0D0 - exp (-KTRANS*TLAI)
    SOLARRAD = CC * SRAD ! mj/m^2 / (day or hr)
    TAVG = (TMAX + TMIN) /2.0D0 ![oC]
!       Linear interpolate : TABEX (YTABLE, XTABLE, CURRENT VAL, SIZE(TABLES))
    TEMPSTRESS_PHOTO = TABEX (TSTRESSY,TSTRESSX,real(TAVG),SIZE_TSTRESSTABLES)
!            0.0 < TEMPSTRESS_PHOTO < 1.0
    TEMPSTRESS_PHOTO = 1.0d0- MIN (MAX(TEMPSTRESS_PHOTO, 1.0), 0.0) !factor [0-1]
!
!    <N_fact_photo description="multipler for N deficit effect on photosynthesis">1.25    </N_fact_photo>
!DSSAT Vs       NFACT from DSSAT = (1.0 - (TCNP-TANC)/(TCNP-TMNC))
! TANC        !Nitrogen content in above ground biomass, decimal
! TCNP        !Critical nitrogen concentration in tops, g N/g dry wt.
! TMNC        !Plant top minimum N concentration g N/g dry matter
! APSIM:
!       N_conc_ratio = divide ((N_conc_stover - N_conc_stover_min)          &
!        , (N_conc_stover_crit - N_conc_stover_min), 0.0)
!N_conc_stover      !tops (stover) actual N concentration
!N_conc_stover_crit !tops (stover) critical N concentration
!N_conc_stover_min  !tops (stover) minimum N concentration
! can do either, NFACT equation = APSIM equation for ratio:
!   Multiply NFAC by the crop photo value and we have N_STRESS
    N_DEF = CROP_NFACT * NFAC
    N_STRESS =  MIN (MAX(N_DEF, 0.0), 1.0) !factor [0-1]

    !rue(curr_phase) [(g dm/mj)]
    !stresses = factors [0-1]
! NOTE: ALTERNATE method uses CO2_FACT here as well, double dipping?
    USRUE = RUE_CURR_PHASE *          &
    min(TEMPSTRESS_PHOTO, N_STRESS)
    !USRUE =  [(g dm/mj)]
    
    BIOMASSPROD_POTENTIAL = USRUE * SOLARRAD 
    ! BIOMASSPROD_POTENTIAL =  g dm/mj * mj/m^2 = g biomass/m^2

    
!***************************************************************************
!   Step 3:
!***************************************************************************
    !         call cproc_sw_demand1(
!     :           g%dlt_dm_light  !biomass using RUE
!     :         , g%transp_eff    !transpiration 
!     :         , g%sw_demand_te  !Demand calculated using biomass
!     :         )
! 
    IF ((BIOMASSPROD_POTENTIAL + EOP_EFF) > 0.0) THEN
        SWDEMAND_RAW = BIOMASSPROD_POTENTIAL / EOP_EFF
    ELSE
        SWDEMAND_RAW = 0.0D0
    ENDIF
    !SWDEMAND RAW = (g dm/m^2) / (g dm/m^2/mm water) = mm water
!***************************************************************************
!   Step 4:
!***************************************************************************
!         call cproc_sw_demand_bound(
!     :         g%sw_demand_te   !Demand using biomass 
!     :        ,p%eo_crop_factor !Kc For crop (maize = 100.0) 
!     :        ,g%eo             !Pot Evapotrans
!     :        ,g%cover_green    !cover_green = (1.0 -exp (-extinct_coef*lai))
!     :        ,g%sw_demand)     !bounded SW demand for crop

     !ETC is in cm/[day or hr]
      SWDEMAND_MAX = ETc * CC
      SWDEMAND_RAW = SWDEMAND_RAW / 10.0d0 ! mm to cm
      ! PET partitioned into cm/unit of time
      PET = min(SWDEMAND_RAW, SWDEMAND_MAX)
      PET = max (0.0d0, PET) !limit to zero
 

!***************************************************************************
!   Done
!***************************************************************************
end subroutine APSIM_WATERDEMAND
!***************************************************************************

!
!!*     ===========================================================
!      subroutine Maize_water_demand (Option)
!!*     ===========================================================
!      implicit none
! 
!!*+  Sub-Program Arguments
!      integer    Option                   ! (INPUT) template option number
! 
!!*+  Purpose
!!*     Soil water demand
! 
!!*+  Mission Statement
!!*     Calculate the plant water demand
! 
!!*+  Changes
!!*     5/9/96 dph
!!*     970312 slw - templated
! 
!!*+  Constant Values
!      character  my_name*(*)           ! name of procedure
!      parameter (my_name = 'Maize_water_demand')
! 
!!*- Implementation Section ----------------------------------
!      call push_routine (my_name)
! 
!      if (Option .eq. 1) then
! 
!         call cproc_sw_demand1(
!     :           g%dlt_dm_light  !biomass using RUE
!     :         , g%transp_eff    !transpiration 
!     :         , g%sw_demand_te  !Demand calculated using biomass
!     :         )
! 
!         call cproc_sw_demand_bound(
!     :         g%sw_demand_te   !Demand using biomass 
!     :        ,p%eo_crop_factor !Kc For crop (maize = 100.0) 
!     :        ,g%eo             !Pot Evapotrans
!     :        ,g%cover_green    !cover_green = (1.0 -exp (-extinct_coef*lai))
!     :        ,g%sw_demand)     !bounded SW demand for crop
! 
!             ! Capping of sw demand will create an effective TE- recalculate it here
!             ! In an ideal world this should NOT be changed here - NIH
!         g%transp_eff = g%transp_eff
!     :                * divide(g%sw_demand_te, g%sw_demand, 1.0)
! 
!      else
!         call Fatal_error (ERR_internal, 'Invalid template option')
!      endif
! 
!      call pop_routine (my_name)
!      return
!      end subroutine
!
