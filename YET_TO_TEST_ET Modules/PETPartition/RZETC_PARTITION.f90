Module RZ_ETC_PARTITION
    IMPLICIT NONE
CONTAINS
!***************************************************************************
! SUBROUTINE ETC_PARTITION(...)
!***************************************************************************
SUBROUTINE ETC_PARTITION(PARTFLAG, ETC, PET, PER, PES, TLAI, &
           RM, CRES, CO2R, CROP, KSEVAP, KTRANS, TMIN, TMAX, &
           WNDSPD, SRAD, ISTAGE, NFAC)
    implicit none
! Input/Output section:
    double precision, intent(IN)    :: ETC      !Potential Evapotranspiration [CM/DAY OR CM/HR]
    double precision, intent(INOUT) :: PET      !Potential Transpiration [CM/DAY OR CM/HR]
    double precision, intent(INOUT) :: PER      !Potential Evaporation Residue [CM/DAY OR CM/HR]
    double precision, intent(INOUT) :: PES      !Potential Evaporation Soil [CM/DAY OR CM/HR]
    double precision, intent(IN)    :: TLAI     !Total Leaf Area Index [0+]
    double precision, intent(IN)    :: RM       !MASS OF RESIDUE [KG/HA]
    double precision, intent(IN)    :: CRES     !COEFF ACCOUNTS FOR RANDOMNESS OF RESIDUE.
    double precision, intent(IN)    :: CO2R     !Atmosphere CO2 content [ppm]
    double precision, intent(INOUT) :: KSEVAP   !Light extinction coefficient for potential plant transpiration
    double precision, intent(INOUT) :: KTRANS   !Light extinction coefficient for potential soil evaporation
    double precision, intent(IN)    :: WNDSPD   !Wind Speed [KM/DAY OR KM/HOUR]
    double precision, intent(IN)    :: SRAD     !Solar radiation (MJ/m2-d)
    double precision, intent(IN)    :: TMIN     !Minimum temperature [oC]
    double precision, intent(IN)    :: TMAX     !Maximum temperature [oC]
    double precision, intent(IN)    :: NFAC     !APSIM nstress factor for crop [0-1]
    integer, intent(IN)     :: PARTFLAG         !Flag to choose which partition option to use [0-3]
    integer, intent(IN)     :: ISTAGE           !GROWTH Stage for crop X (Only useful for APSIM, which is hardcoded for maize)
    character(LEN=2), intent(IN) :: CROP        !Two character crop type 
! Locals
    double precision :: CO          !fraction bare ground
    double precision :: CC          !fraction canopy coverage
    double precision :: CR          !fraction residue coverage
    double precision :: TAVG        !Avg Temperature
    double precision :: svp_fact    !APSIM svp factor (hardcoded for maize)
    double precision :: crop_transfac   !APSIM transpiration factor (hardcoded for maize)
    double precision :: rue_curr        !APSIM RUE factor (hardcoded for maize)
    double precision :: crop_nfact      !APSIM crop NFAC (hardcoded to get maize values)
    double precision :: eos_res_frac    !APSIM Residue coverage factor
    double precision :: eos_can_frac    !APSIM canopy coverage factor
    double precision :: TKTRANS         !Temporary KTRANS -> calculated via defaults for MAIZE in event of 0.0
    double precision :: TKSEVAP         !Temporary KSEVAP -> calculated via defaults for MAIZE in event of 0.0
    
    real, dimension (10) :: co2x        !APSIM CO2 x table (hardcoded for maize)
    real, dimension (10) :: co2y        !APSIM CO2 y table (hardcoded for maize)
    real, dimension (4) :: tstressy     !APSIM Temperature stress x table (hardcoded for maize)
    real, dimension (4) :: tstressx     !APSIM Temperature stress y table (hardcoded for maize)
    real :: EOP                         !Temp PET value for DSSAT partitioning
    real :: EOS                         !Temp PES value for DSSAT partitioning
    integer :: RATE
 
!****************************************************************************  
! End Declarations
! Begin preparation before partitioning
!****************************************************************************  

    TAVG = (TMIN+TMAX)/2.0D0

    ! Default = 0.85 for KCAN in ALT_PLANT
    if (KTRANS.lt.1.0E-5 .AND. (KSEVAP.GT.-99.0D0 .AND. KSEVAP.lt.1.0E-5)) THEN
        TKTRANS = 0.85D0/(1.0d0 - 0.07d0)*(1.0d0-0.25d0)
        TKSEVAP = 0.85D0/(1.0d0 - 0.07d0)*(1.0d0-0.25d0)
    else
        TKTRANS = KTRANS
        TKSEVAP = KSEVAP
    endif
!****************************************************************************       
    IF (PARTFLAG.EQ.1) THEN
!****************************************************************************   
!       CROPSYST approach to partitioning
!       Need canopy cover, residue cover, green leaf cover.
!****************************************************************************
        !Calculate canopy cover <?>
        IF(TLAI.NE.0.0D0) THEN
            CO=DEXP(-0.594D0*TLAI) ! I'A = 1 - E^(-K * LAI) (Canopy interceptance Value, 1 - I'A = canopy cover) 
        ELSE
            CO=1.0D0
        ENDIF
        CC=1.0D0-CO
        CR = 1 - exp(-(cres)*(rm/(10.0d0 * 1000.0d0)))
        !Potential Transpiration
        CALL PART_ETC_PET(ETC, CC, PET)
        PET = MAX(PET, 0.0)
        !Potential Residue evaporation
        CALL PART_ETC_PER(ETC, CC, CR, PER)
        PER = MAX(PER, 0.0)
        !Potential Soil evaporation
        CALL PART_ETC_PES(ETC, CC, CR, PES)
        PES = MAX(PES, 0.0)
        !Have ETc Partitioned in the approach outlined by CROPSYST
!**************************************************************************** 
    ELSEIF (PARTFLAG.EQ.2) THEN
!****************************************************************************   
!       DSSAT approach to partitioning
!       Need KSEVAP, KTRANS, DEFAULT IF ZERO
!       Modified DSSAT approach: literature says "limit potential trans by potential evap," doing so here
!       Do not have variables/calculations required for mulch, do not use/calculate mulch evap in this approach
!****************************************************************************
        ! Potential Soil Evap
        CALL PSE(real(ETC), real(TKSEVAP), real( TLAI), EOS)
        RATE = 3
        ! Potential Transpiration Evap
        CALL TRANS(RATE, real(CO2R), CROP, real(ETC), EOS ,real(TKTRANS),  &   !Input
        real(TAVG), real(WNDSPD), real(TLAI),                             &   !Input
        EOP)                                                                  !Ouput                              
        PER = 0.0d0 ! Missing key mulch variables in RZWQM, so skip mulch section of dssat process, set residue evap to 0   
        PES = dble(EOS)
        PET = dble(EOP)
        ! Have ETC partitioned in simplified approach outlined by DSSAT
!**************************************************************************** 
    ELSEIF (PARTFLAG.EQ.3) THEN
!****************************************************************************   
!       APSIM APPROACH
!       Have to calculate biomass potential use, water demand, atmospheric support of demand
!****************************************************************************
        ! RMarquez 3.25.2016 -> the APSIM approach is currently hardcoded for maize.
        svp_fact = 0.75D0 ! 0.75 SVP Factor
        crop_transfac = 0.009D0 ! maize 0.009 for all stages, 
        !units: kpa/g dm/ m^2 / mm water  
        select case(ISTAGE)
        case(1 : 4, 9) !between emergence and end of tasseling-> RUE highest
            rue_curr = 1.60d0
        case(5)! begin grainfill, lower RUE
            rue_curr = 1.40d0
        case(6)! end grainfill/maturity, lower RUE
            rue_curr = 1.30d0
        case default !no crop leaves/etc
            rue_curr = 0.0d0
        end select
        !Hard coded maize values for CO2 impacts from DSSAT
        !CO2X     0   220   280   330   400   490   570   750   990  9999
        !CO2Y  0.00  0.85  0.95  1.00  1.02  1.04  1.05  1.06  1.07  1.08
        co2x = (/0,220,280,330,400,490,570,750,990,9999/)
        co2y = (/0.00,0.85,0.95,1.00,1.02,1.04,1.05,1.06,1.07,1.08/)
            
        !<x_ave_temp units="oC" description="mean daily temperature">8 15 35 50</x_ave_temp> <!--PLP?  Carberry et al. 1989 0 20 40 50 -->
!                    <y_stress_photo description="multiplier on RUE">0  1  1  0</y_stress_photo>
        tstressx=(/8,15,35,50/)
        tstressy=(/0,1,1,0/)
        !<N_fact_photo description="multipler for N deficit effect on photosynthesis">1.25    </N_fact_photo>
        crop_nfact = 1.25d0
        ! RMarquez 3.25.2016 -> last part:
        ! Require NFAC, KSEVAP, KTRANS, ISTAGE -> map to 12 stages for APSIM?
            
        call APSIM_WATERDEMAND(TMIN,TMAX,crop_transfac,co2y,co2x,CO2R,10, &
                                TKTRANS,TLAI,tstressy,tstressx,4,NFAC,    &
                                crop_nfact,rue_curr,svp_fact,SRAD,ETC,PET) 
        !Calculate EOS
        IF(TLAI.NE.0.0D0) THEN
            CO=DEXP(-TKTRANS*TLAI) ! I'A = 1 - E^(-K * LAI) (Canopy interceptance Value, 1 - I'A = canopy cover) 
        ELSE
            CO=1.0D0
        ENDIF
        CC=1.0D0-CO
        CR = 1 - exp(-(cres)*(rm/(10.0d0 * 1000.0d0)))
            
        !REFERENCE:
        !subroutine soilwat2_pot_soil_evaporation (eos) in Soilwat.for (APSIM)
            
        !eos_canopy_fract = exp (-c%canopy_eos_coef * cover_tot_sum)
        !cover_tot_sum = 0 to 1.0 value of canopy covered)
        eos_can_frac = EXP(-1.7D0*CC)
        if (CR.ge.1.0d0) then
            eos_res_frac = 0.0d0
        else
            eos_res_frac = (1.0d0 - CR) ** (2.2D-4 / 5.0D-4)
        endif
                
        PES = MAX(0.0d0, ETc* eos_res_frac * eos_can_frac )
        ! Potential Res
        PER = 0.0d0

        
!****************************************************************************
    ELSE !(DEFAULT = HERMES APPROACH, simplest partitioning)
!****************************************************************************   
!       HERMES approach to partitioning
!       Calculate Potential soil 
!****************************************************************************
        ! Potential Evap
        PES = MAX(0.0d0, ETC*EXP(-0.5D0*TLAI))
        ! Potential Trans
        PET = MAX (ETC - PES, 0.0d0)
        ! Potential Res
        PER = 0.0D0
!****************************************************************************
    ENDIF ! END PARTITION OPTIONS
!****************************************************************************
END SUBROUTINE ETC_PARTITION
!****************************************************************************

!****************************************************************************
! PARTITION OPTION: CROPSYST APPROACH
!       PARTITION ET0 INTO PET, PER, PES USING CROP COVER APPROACH
!****************************************************************************

!****************************************************************************
SUBROUTINE PART_ETC_PET(ETc, FRACT_GREEN_COVER, PET)
!****************************************************************************
!     Purpose: partition ETc into Potential Transpiration for the crop
!     
!     Definitions:    
!     ETc                  !(INPUT) Potential Evapotranspiration for the crop (mm/hr) 
!     FRACT_GREEN_COVER    !(INPUT) Fract. incident radiation intercepted by crop green leaf area 
!     PET                  !(OUPUT) Potential transpiration for the Crop (mm/hr)?
!****************************************************************************
    IMPLICIT NONE
    DOUBLE PRECISION,  INTENT(IN) :: ETc           
    DOUBLE PRECISION,  INTENT(IN) :: FRACT_GREEN_COVER
    DOUBLE PRECISION,  INTENT(INOUT) :: PET              

    PET = ETc * FRACT_GREEN_COVER

END SUBROUTINE PART_ETC_PET
!****************************************************************************

!****************************************************************************      
SUBROUTINE PART_ETC_PER(ETc, FRACT_CANOPY_COVER, &
FRACT_RESIDUE_COVER, PER)
!****************************************************************************
!     Purpose: partition ETc  into Potential Transpiration for the crop
!     
!     Definitions:    
!     ETc                  !(INPUT) Potential Evapotranspiration for the crop (mm/hr) 
!     FRACT_CANOPY_COVER   !(INPUT) Fract. incident radiation intercepted by crop canopy 
!     FRACT_RESIDUE_COVER  !(INPUT) Fract. incident radiation intercepted by residue 
!     PER                  !(OUPUT) Potential Residue Evaporation (mm/hr?)
!****************************************************************************   
    IMPLICIT NONE
    double precision, intent(IN)  :: ETc
    double precision, intent(IN)  :: FRACT_CANOPY_COVER
    double precision, intent(IN)  :: FRACT_RESIDUE_COVER
    double precision, intent(OUT) :: PER

    PER = FRACT_RESIDUE_COVER * (1.0D0 -FRACT_CANOPY_COVER) * ETc

END SUBROUTINE PART_ETC_PER
!**************************************************************************** 

!**************************************************************************** 
SUBROUTINE PART_ETC_PES(ETc, FRACT_CANOPY_COVER, &
FRACT_RESIDUE_COVER, PES)
!****************************************************************************
!     Purpose: partition ETc into Potential Residue Evaporation for the crop
!     
!     Definitions:    
!     ETc                  !(INPUT) Potential Evapotranspiration for the crop (mm/hr) 
!     FRACT_CANOPY_COVER   !(INPUT) Fract. incident radiation intercepted by crop canopy 
!     FRACT_RESIDUE_COVER  !(INPUT) Fract. incident radiation intercepted by residue 
!     PES                  !(OUPUT) Potential Soil Evaporation (mm/hr?)
!****************************************************************************
    IMPLICIT NONE
    double precision, intent(IN)  :: ETc
    double precision, intent(IN)  :: FRACT_CANOPY_COVER
    double precision, intent(IN)  :: FRACT_RESIDUE_COVER
    double precision, intent(OUT) :: PES

    PES = (1.0d0-FRACT_CANOPY_COVER) * &
    (1.0d0-FRACT_RESIDUE_COVER) * ETc

END SUBROUTINE PART_ETC_PES
!****************************************************************************

END Module RZ_ETC_PARTITION