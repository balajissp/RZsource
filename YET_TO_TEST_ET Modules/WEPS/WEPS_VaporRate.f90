!****************************************************************************
! subroutine WEPS_calc_vaporrate( ... )
!     Purpose: Calculate the surface vapor rate using the process outlined in WEPS
!****************************************************************************
subroutine WEPS_calc_vaporrate(atmpres,dist1,rh,soiltemp1,soilrh1,tair,     &
                               theta1,thetas1,surface_vapor_rate)           
    implicit none
    ! arguments    
    double precision, intent(out) :: surface_vapor_rate
    real, intent(in)    :: atmpres      ![kPa]
    real, intent(in)    :: dist1        ![m]
    real, intent(in)    :: rh           ![0-1]
    real, intent(in)    :: soiltemp1    ![0C]
    real, intent(in)    :: soilrh1      ![0-1]
    real, intent(in)    :: tair         ![0C]
    real, intent(in)    :: theta1       ![m3/m3]
    real, intent(in)    :: thetas1      ![m3/m3]
    ! locals
    real    :: airvapden
    real    :: soilvapden1
    real    :: soildiffu1
    !functions
    real    :: vaporden
    real    :: diffusive
    
    include 'vapprop.inc'
!****************************************************************************
!   Calculate vapor density of the air
!      airvapden = vaporden(tair, rh)
!**************************************************************************** 
    airvapden = vaporden(tair, rh)
!****************************************************************************
!   Calculate soil vapor density
!      soilvapden1 = vaporden( soiltemp1, soilrh1 )
!****************************************************************************
    soilvapden1 = vaporden( soiltemp1, soilrh1 )
!****************************************************************************
!   Calculate soil diffusivity 
!      soildiffu1 = diffusive(theta1, thetas1,soiltemp1, atmpres)
!****************************************************************************    
    soildiffu1 = diffusive(theta1,thetas1,soiltemp1,atmpres)
!****************************************************************************
!   Calculate surface_vapor_rate
!      surface_capil_rate = ((soilvapden(1)-airvapden) * soildiffu(1) / denwat) / dist(1)
!****************************************************************************   
     surface_vapor_rate = dble(((soilvapden1-airvapden) * soildiffu1 / denwat) / dist1)
!**************************************************************************** 
end subroutine WEPS_calc_vaporrate
!****************************************************************************
 