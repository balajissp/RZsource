!*********************************************************************
! apsim photosynthesis stress calculation
!*********************************************************************
subroutine apsim_swdef_photo(NL,dlayr, root_depth, sw_demand, sw_supply, swdef)
    implicit none
    !OUTPUT
    double precision, intent(out) :: swdef
    !INPUT
    integer, intent (in)         :: NL
    double precision, intent(in) :: dlayr(*)
    double precision, intent(in) :: root_depth
    double precision, intent(in) :: sw_demand
    double precision, intent(in) :: sw_supply(*)

    !LOCALS
    integer :: deepest_layer
    double precision :: sw_supply_sum
    double precision :: sw_demand_ratio
    !FUNCTION:
    integer :: find_layer_no
    double precision :: sum_double_array
    
!*********************************************************************
    !deepest_layer = find_layer_no (root_depth, dlayer, num_layer)
    deepest_layer = find_layer_no (root_depth, dlayr, NL)
    
    ! get potential water that can be taken up when profile is full
    !sw_supply_sum = sum_real_array (sw_supply, deepest_layer)
    sw_supply_sum = sum_double_array (sw_supply, deepest_layer)
    
    ! divide function takes care of overflow,underflow, division by zero.
    !   dont think we are too worried about over/underflow, will handle div by zero
    !sw_demand_ratio = divide (sw_supply_sum, sw_demand, 1.0)
    if (sw_demand == 0.0) then
        sw_demand_ratio = 0.0
    else
        sw_demand_ratio = sw_supply_sum/sw_demand
    endif
    !swdef = bound (sw_demand_ratio , 0.0, 1.0)
    ! bound : 0.0 <= swdef <= 1.0
    swdef = max( min(sw_demand_ratio, 1.0) , 0.0)


endsubroutine apsim_swdef_photo



