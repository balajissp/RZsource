!****************************************************************************
!  subroutine WEPS_calc_capilrate ( ... )
!     Purpose: Calculate the maximum soil evaporation rate using the approach in WEPS.
!       
!     Called by PHYSCL?
!       VARIABLE DEFINITIONS:
!       TYP  VARIABLE    USE   DESCRIPTION
!       ---  --------   -----  -----------
!       
!**************************************************************************** 
subroutine WEPS_calc_capilrate (bd1, dist1, frac_clay1, frac_organic1,   & ! Input
                frac_sand1, frac_silt1, ksat1, rh, soiltemp1, tair,      & ! Input
                theta1, thetar1, thetas1, thetaw1,                       & ! Input
                soilrh1,surface_capil_rate)                                 ! Output
    implicit none
    double precision, intent(out)   :: surface_capil_rate
    real, intent (inout)            :: soilrh1
    
    real, intent (in)   :: bd1              ! [Mg/m3]           
    real, intent (in)   :: dist1            ! [m]
    real, intent (in)   :: frac_clay1       ! [0-1]
    real, intent (in)   :: frac_organic1    ! [0-1]
    real, intent (in)   :: frac_sand1       ! [0-1]
    real, intent (in)   :: frac_silt1       ! [0-1]
    real, intent (in)   :: ksat1            ! [m/s]
    real, intent (in)   :: rh               ! [0-1]
    real, intent (in)   :: soiltemp1        ! [0C]
    real, intent (in)   :: tair             ! [0C]
    real, intent (in)   :: theta1           ! [m3/m3]
    real, intent (in)   :: thetar1          ! [m3/m3]
    real, intent (in)   :: thetas1          ! [m3/m3]
    real, intent (in)   :: thetaw1          ! [m3/m3]
    !locals
    real        :: potes1
    real        :: gmd1
    real        :: gsd1
    real        :: bh0cb1
    real        :: bheaep1
    real        :: airentry1
    real        :: lambda1
    real        :: theta80rh1
    real        :: potm1
    real        :: swm1
    real        :: cond1
    real        :: air_mat_pot
    !Functions:
    real        :: matricpot_from_rh
    real        :: unsatcond_bc
    real, parameter :: p_claygrav80rh = 0.3
    real, parameter :: p_orggrav80rh = 0.27
    include 'vapprop.inc'

!****************************************************************************
!   Calculate airentry for layer 1:
!       airentry(i) = bheaep(i) / gravconst
!****************************************************************************
    call psd(frac_sand1, frac_silt1, frac_clay1, gmd1, gsd1)
    potes1 = -0.2 * gmd1**(-0.5)                 !H-77 ??
    bh0cb1 = -2. * potes1 + 0.2 * gsd1           !H-78 ?? 
    bheaep1 = potes1*(bd1/1.3)**(0.67*bh0cb1)    !H-79 ??
    airentry1 = bheaep1 / gravconst
!****************************************************************************
!   Calculate lambda for layer 1:
!       lambda1 = 1/bh0cb1
!****************************************************************************
    lambda1 = 1.0 / bh0cb1
!****************************************************************************
!   Calculate thetha80rh for layer 1:
!frac_clay1        bsfcla(*)  - fraction of soil mineral content which is clay (unitless)
!frac_organic1     bsfom(*)   - fraction of total soil which is organic (unitless)
!****************************************************************************
    theta80rh1 = (bd1  / denwat) * ( frac_clay1 * (1.0-frac_organic1)* p_claygrav80rh    &
                   + frac_organic1 * p_orggrav80rh )
!****************************************************************************
!   Calculate swm1
!       swm1 = potm1 - dist1 (swm1 = potential - layer depth)
!****************************************************************************
    call matricpot_bc(theta1, thetar1, thetas1, airentry1,       &
                      lambda1, thetaw1, theta80rh1, soiltemp1,   &
                      potm1, soilrh1 )
    swm1 = potm1 - dist1    ! depth(1) = dist(1), depth (i) = accumulated depth for layer i
!****************************************************************************
!   Calculate air_mat_pot
!       air_mat_pot = matricpot_from_rh( airhumid, airtemp )
!**************************************************************************** 
    air_mat_pot = matricpot_from_rh(rh, tair) ! hourly air temp/rh
    
    cond1 = unsatcond_bc(theta1,thetar1,thetas1,ksat1,lambda1)
!****************************************************************************
!   Calculate surface_capil_rate
!       surface_capil_rate = (swm(1) - air_mat_pot) *0.5*cond(1) / dist(1)
!****************************************************************************   
    surface_capil_rate = dble((swm1 - air_mat_pot) *0.5*cond1 / dist1)


!**************************************************************************** 
end subroutine WEPS_calc_capilrate
!****************************************************************************

!Goal:    
!    surface_capil_rate = (swm(1) - air_mat_pot) *0.5*cond(1) / dist(1)
!   Where:
!   dist(1)/depth(1) = 1/2 (layer thickness) [m]
!   swm(1) = potm(1) - depth(1)
!       potm = matricpot_from_rh (soilrh, soiltemp)
!   matricpot_from_rh = rgas*(soiltemp+zerokelvin)*(log(soilrh)) / (molewater * gravconst)
!   air_mat_pot = matricpot_from_rh (airhumid, airtemp)
!   cond(1) = unsatcond_bc(theta(lrx),thetar(lrx), thetas(lrx), ksat(lrx),lambda(lrx))
