integer function find_layer_no(depth, dlayr, NL)
    implicit none
    
    !*INPUT
    integer, intent(in) :: NL
    double precision, intent(in) :: depth
    double precision, intent(in) :: dlayr (*)
    
    !*LOCAL
    integer :: indx
    double precision :: accumulated_depth
!*****************************************************************************
    find_layer_no = NL
    accumulated_depth = 0.0
    do indx = 1, NL
        accumulated_depth = accumulated_depth + dlayr(indx)
        if (accumulated_depth >= depth .and. find_layer_no == NL) then
            find_layer_no = indx
        endif
    enddo
 !*****************************************************************************   
end function

!*****************************************************************************   
double precision function sum_double_array(array, num_indx)
    implicit none
    
    !*INPUT
    integer, intent(in) :: num_indx
    double precision, intent(in) :: array (*)
    
    !*LOCAL
    integer :: indx
    double precision :: accum_sum
!*****************************************************************************
    accum_sum = 0.0
    do indx = 1, num_indx
        accum_sum = accum_sum + array(indx)
    enddo
    sum_double_array = accum_sum
    return
!*****************************************************************************   
end function