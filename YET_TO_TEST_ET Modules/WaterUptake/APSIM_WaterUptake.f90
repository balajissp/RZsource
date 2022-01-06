!*********************************************************************
! APSIM approach to crop water uptake
! [Actual transpiration]
!*********************************************************************    
subroutine APSIM_swuptake(theta, dlayr, ll, kl, NL, root_depth, sw_uptake)
!
    !Inputs:
    ! SWC(NL) soil water content of layer L (mm)
    ! SWP(NL) soil water content of layer L (mm)
    ! DLAYER(NL) thickness of soil layer I (mm)
    ! WATER_DENSITY 1000 KG/M3
    ! rootlength density factor 
    !ouput:
    !potential crop water uptake from each layer (mm) (supply to roots)

    !Thickness = (100, 100, 100, 200, 200, 200, 200, 200, 200, 200) mm?
    !LL = (0.11, 0.14, 0.16, 0.17, 0.19, 0.20, 0.20, 0.20, 0.20, 0.20)
    !KL = (0.08, 0.08, 0.08, 0.08, 0.06, 0.04, 0.03, 0.02, 0.01, 0.01)
!output
    double precision, intent(inout), dimension(NL) :: sw_uptake     !Actual uptake by layer
!input
    integer, intent(in) :: NL                                       !Number of layers
    double precision, intent(in) :: root_depth                      !maximum depth of roots
    double precision, intent(in), dimension(NL) :: theta            !SWC by layer
    double precision, intent(in), dimension(NL) :: dlayr            !Depth of each layer
    double precision, intent(in), dimension(NL) :: LL               !Lower Limit (or wilting point?) of each layer
    double precision, intent(in), dimension(NL) :: KL               !Plant extraction factor by layer
!local
    integer :: deepest_layer                                        !index of deepest layer                            
    double precision :: sw_avail                                    !temp calculation value
    double precision, dimension(NL) :: sw_supply                    !sw supply by layer
    integer :: layer                                                !Layer index for loops
!function
    integer :: find_layer_no                                        !function that finds deepest roots layer index
    
!************************************************************************************************    
!    1) find deepest layer with roots:
!    deepest_layer = find_layer_no (root_depth, dlayer, num_layer)
!    find_layer_no = get_cumulative_index_real (depth, dlayr, num_layers)
!    get_cumulative_index_real(cum_sum, array, size_of)
!************************************************************************************************
    deepest_layer = find_layer_no(root_depth, dlayr, NL)
!************************************************************************************************    
!   2) calculate sw_supply(:) and sum    
!crop_sw_supply(num_layer, dlayer, root_depth, sw_dep,kl, ll_dep, sw_supply)
!deepest_layer = find_layer_no (root_depth, dlayer, num_layer)
!sw_supply(layer) = max((((sw_dep(layer) - ll_dep(layer)) * kl(layer)), 0.0)
!sw_supply(deepest_layer) = sw_supply(deepest_layer) * root_proportion(sw_supply(deepest_layer), dlayer, rootdepth)
!************************************************************************************************
    sum_supply = 0.0d0
    do layer = 1, deepest_layer - 1
        sw_avail = theta(layer) - ll(layer)
        sw_supply(layer) = max (sw_avail * kl(layer), 0.0d0) ! don't let supply be negative
        sum_supply = sum_supply + sw_supply(layer)
    enddo
    !root proportion in last layer
    depth_to_layer_top = sum(dlayr(1:deepest_layer)) - dlayr(deepest_layer)
    depth_to_root = min( depth_to_layer_top , root_depth )
    depth_root_layer = dim (depth_to_root, depth_layer_top)
    root_prop = depth_root_layer / dlayr(deepest_layer)
    !calculate last layer available sw
    sw_supply(deepest_layer) = ((theta(deepest_layer) - ll(deepest_layer))*kl(deepest_layer))* root_prop
    sum_supply = sum_supply + sw_supply(deepest_layer)
!************************************************************************************************    
    !3) 
    !if supply > 0 and PET > 0
    !   if supply > PET   : uptake = portion of sw available in layer
    !   else PET > supply : uptake = available water in layer
    !else no supply or PET:  uptake = 0    
!************************************************************************************************    

    sw_uptake = 0 !zero init array
    if (sum_supply > 0.0d0 .and. PET > 0.0d0) then
        if (PET < sum_supply) then
            do layer = 1, deepest_layer
                sw_uptake(layer) = - (sw_supply(layer) / sum_supply) * PET
            enddo
        else !PET >= sw_supply
            do layer = 1, deepest_layer
                sw_uptake(layer) = - sw_supply(layer)
            enddo
        endif
    endif
!************************************************************************************************  
end subroutine !APSIM_swuptake
!************************************************************************************************  