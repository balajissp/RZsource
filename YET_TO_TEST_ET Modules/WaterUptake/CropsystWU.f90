!*********************************************************************
! Cropsyst approach to crop water uptake
! [Actual transpiration]
!*********************************************************************
subroutine cropsyst_wateruptake(NL, pet, TLAI, KTRANS,              &
    max_crop_wateruptake, leafwatpot_onset_stress,                  &
    plant_hyd_conductance_fact,root_depth, leafwat_wp, dlayr, wp,   &
    act_trans, water_stress_idex, sw_uptake)
    implicit none
    !NOTE: assume if you change the inputs from mm/day to cm/day the outputs will change accordingly;
    !OUTPUT
    double precision, intent(inout) :: act_trans                 !Total water uptake [mm/day]
    double precision, intent(inout) :: water_stress_idex         !Water stress index 
    double precision, intent(inout), dimension (NL) :: sw_uptake !Water uptake layer L [mm/day] 
    !INPUT
    integer, intent(in) :: NL                                   !The number of layers in the profile
    double precision, intent(in) :: pet                         !Potential Transpiration            [mm/day] <- Change to cm? 
    double precision, intent(in) :: max_crop_wateruptake        !Maximum full cover transpiration   [mm/day] <- Change to cm? 
    double precision, intent(in) :: TLAI                        !Total Leaf Area Index              [0+]
    double precision, intent(in) :: KTRANS                      !Light extinction coeff
    double precision, intent(in) :: leafwatpot_onset_stress     !Leaf water potential at the onset of stomatal closure [J/kg or m2/s2]
    double precision, intent(in) :: plant_hyd_conductance_fact  !Conductance factor                 [0-1]
    double precision, intent(in) :: root_depth                  !Root depth                         [m] <- Change to cm?
    double precision, intent(in) :: leafwat_wp                  !Leaf water potential at wilting (zero trans) [J/kg or m2/s2]
    double precision, intent(in), dimension (NL) :: dlayr       !Soil thickness in layer L          [m] <- Change to cm?
    double precision, intent(in), dimension (NL) :: wp          !Water potential layer L            [J/kg or m2/s2]
    
    !LOCALS
    double precision :: CC                                      !Canopy cover [0 to 1]
    double precision :: swp_fc                                  !Soil water potential at field capacity
    double precision :: potential_uptake                        !Potential water uptake of the plant
    double precision :: max_trans                               !Maximum transpiration rate (min of PT and PU)
    double precision :: plant_hyd_cond                          !Plant hydraulic conductance value
    double precision :: lyr_btm_dpth                            !Total depth of the profile
    double precision :: root_fract_sum                          !Total sum of root fraction in the profile
    double precision :: newroot_fract_sum                       !Variable used to adjust root fractions for shallow soils
    double precision :: rt_hyd_cond                             !Root hydraulic conductivity
    double precision :: tp_hyd_cond                             !Top hydraulic conductivity
    double precision :: rt_cond_adj                             !Total adjusted root conductivity value
    double precision :: salinity_reduc_fac                      !Salinity reduction factor
    double precision :: temporary_fac                           !Temp variable for calculating the root activity fraction
    double precision :: avg_soil_wp                             !Average soil water potential
    double precision :: leaf_water_pot                          !Water potential for the leaf
    double precision :: trans_ratio                             !Ratio of attainable transpiration rate to maximum transpiration rate
    double precision :: crop_wu                                 !Actual uptake by the crop
    double precision :: attainable_trans                        !The attainable transpiration rate for the crop
    double precision, dimension(NL) :: root_frac                !Fraction of total root length present in layer L
    double precision, dimension(NL) :: root_activity_fac        !Fraction of activity in the root for layer L
    double precision, dimension(NL) :: lyr_salinity_red_fac     !Salinity reduction factor for layer L
    double precision, dimension(NL) :: lyr_root_cond_adj        !Adjusted root conductivity for layer L
    double precision, dimension(NL) :: lyr_root_hyd_cond        !Root hydraulic conductivity for layer L
    double precision, dimension(NL) :: lyr_top_cond             !Top conductivity for layer L
    double precision, dimension(NL) :: lyr_plant_hyd_cond       !Plant hydraulic conductivity for layer L
    integer :: indx
    
    !FUNCTION:
    double precision :: calc_root_fract
!*********************************************************************
! Calculate maximum crop transpiration rate (kg/m2/d = mm/d) and Hyd conductivity
!*********************************************************************
    swp_fc = -33
    CC = 1.0D0 - exp (-KTRANS*TLAI)
    !already have PET
    potential_uptake = max_crop_wateruptake * CC
    
    max_trans = min(pet, potential_uptake)
    
    plant_hyd_cond = (potential_uptake             &
                    / (swp_fc - leafwatpot_onset_stress)) &
                    * plant_hyd_conductance_fact
    
!*********************************************************************
!   'Calculate root fraction per soil layer  
!*********************************************************************    
  
    lyr_btm_dpth = 0
    root_fract_sum = 0
    
    do indx = 1, NL
        if (dlayr(indx) > 0) then
            lyr_btm_dpth = lyr_btm_dpth + dlayr(indx)
            root_frac(indx) = calc_root_fract(lyr_btm_dpth, dlayr(indx), root_depth)
            root_fract_sum = root_fract_sum + root_frac(indx)
        else
            exit
        endif
    enddo

!*********************************************************************
! Adjust root fraction for shallow soils to ensure that the sum of root fraction of all layers
! is equal to 1 
!*********************************************************************    
    if ((root_depth > lyr_btm_dpth) .and. (root_fract_sum < 1.0d0)) then
        newroot_fract_sum = 0
        do indx = 1, NL
            root_frac(indx) = root_frac(indx) / root_fract_sum
            newroot_fract_sum = newroot_fract_sum + root_frac(indx)
        enddo
        root_fract_sum = newroot_fract_sum
    endif 
!*********************************************************************
! 'Adjust plant hydraulic conductance based on soil dryness
!*********************************************************************        
    rt_hyd_cond = plant_hyd_cond / 0.65d0
    tp_hyd_cond = plant_hyd_cond / 0.35d0
    
    rt_cond_adj = 0
 
!*********************************************************************
! calculate layer root hydraulic conductivity factors
!*********************************************************************    
    salinity_reduc_fac = 1.0d0
    
    do indx = 1, NL
       temporary_fac = 1 - (((wp(indx) - swp_fc) / (leafwat_wp - swp_fc)) ** 8)
       temporary_fac = max(0d0, min(1.0d0, temporary_fac))
       root_activity_fac(indx) = temporary_fac
       lyr_salinity_red_fac(indx) = salinity_reduc_fac
       lyr_root_cond_adj(indx) = root_activity_fac(indx) * &
                           root_frac(indx) * lyr_salinity_red_fac(indx)
       rt_cond_adj = rt_cond_adj + lyr_root_cond_adj(indx)
       lyr_root_hyd_cond(indx) = rt_hyd_cond * lyr_root_cond_adj(indx)
    enddo
    
!*********************************************************************
! calculate layer root hydraulic conductivity
!*********************************************************************       
    do indx = 1, NL
    if (lyr_root_cond_adj(indx) > 0) then
        lyr_top_cond(indx) = tp_hyd_cond * lyr_root_cond_adj(indx) /    &
                               rt_cond_adj
        lyr_plant_hyd_cond(indx) = ( lyr_root_hyd_cond(indx) *             & 
                        lyr_top_cond(indx)) / ( lyr_root_hyd_cond(indx) +  &
                        lyr_top_cond(indx))
    else
        lyr_plant_hyd_cond(indx) = 0
    endif
    enddo
    
!*********************************************************************
! calculate total root and plant hydraulic conductivity
!*********************************************************************  
    rt_hyd_cond = rt_hyd_cond * rt_cond_adj
    plant_hyd_cond = (rt_hyd_cond * tp_hyd_cond) / &
                     (rt_hyd_cond + tp_hyd_cond)
 
!************** Begin Uptake *****************************************
! When there is plant hydraulic conductivity
!********************************************************************* 
    if (plant_hyd_cond > 0) then
    
        avg_soil_wp = 0
!    Calculate average soil water potential (J/kg)
        do indx = 1, NL
            avg_soil_wp = avg_soil_wp + wp(indx)* lyr_root_cond_adj(indx)/ &
                            rt_cond_adj
        enddo
!    Calculate leaf water potential
        leaf_water_pot = avg_soil_wp - pet / plant_hyd_cond
    
        if (leaf_water_pot < leafwatpot_onset_stress ) then
            leaf_water_pot = (plant_hyd_cond * avg_soil_wp *                &
              (leafwatpot_onset_stress - leafwat_wp) + leafwat_wp * pet) /  &
              (plant_hyd_cond * (leafwatpot_onset_stress - leafwat_wp) + pet)
        endif
    
        if ( leaf_water_pot < leafwat_wp) then
            leaf_water_pot = leafwat_wp
        endif
    
!    Reduce transpiration when leaf water potential is less than the critical leaf water
!    potential at the onset of stomatal closure    
        if ( leaf_water_pot < leafwatpot_onset_stress) then
            attainable_trans = max_trans * &
              ( leaf_water_pot -  leafwat_wp) / &
              (leafwatpot_onset_stress - leafwat_wp)
            trans_ratio = attainable_trans / max_trans
        else
            attainable_trans = max_trans
            trans_ratio = 1
        endif
    
         crop_wu = 0
!    Calculate crop water uptake (kg/m2/d = mm/d)     
         do indx=1, NL
             sw_uptake(indx) = lyr_plant_hyd_cond(indx) *   &
                (wp(indx) - leaf_water_pot) * trans_ratio
             crop_wu = crop_wu + sw_uptake(indx)
         enddo
!*********************************************************************
! No plant hydraulic conductivity, no uptake
!********************************************************************* 
     else 
     
         do indx=1, NL
         sw_uptake(indx) = 0
         crop_wu = crop_wu + sw_uptake(indx)
         enddo
     endif
 !********  End Uptake ***********************************************
 act_trans = crop_wu
 water_stress_idex = act_trans / max_trans
 ! For check: Act_Transp should be equal to attainable_Transp
     
!*********************************************************************  
end subroutine cropsyst_wateruptake
!*********************************************************************

!*********************************************************************
! Cropsyst support function for crop water uptake
! [calculates root fraction by layer]
!*********************************************************************
double precision function calc_root_fract(layr_btm_dpth, &
                    layer_thickness, root_depth)
    implicit none
    double precision, intent (in) :: layr_btm_dpth
    double precision, intent (in) :: layer_thickness
    double precision, intent (in) :: root_depth
    double precision :: calc
    if ( root_depth > layr_btm_dpth) then
        calc = layr_btm_dpth * ( 2 *                              &
                (root_depth - layr_btm_dpth) + layer_thickness) / &
                (root_depth * root_depth)
    else if (root_depth < (layr_btm_dpth - layer_thickness + 0.000001)) then
        calc = 0
    else
        calc =((root_depth - layr_btm_dpth) + layer_thickness) / &
                root_depth
        calc = calc * calc
    endif
    calc_root_fract = calc
!*********************************************************************
end function calc_root_fract
!*********************************************************************
        
        
        
        