!****************************************************************************
! SUBROUTINE WEPS_MAXSOILEVAP( ... )
!     Purpose: Calculate the maximum soil evaporation rate using the approach in WEPS.
!       
!       
!****************************************************************************
    subroutine WEPS_MAXSOILEVAP(atmpres,bd1,dist1,frac_clay1,frac_organic1,frac_sand1, & !in
               frac_silt1,ksat1,rh,soiltemp1,tair,theta1,thetar1,thetas1,thetaw1,EOS,  & !in
               hourlyrad, hour,sunrise,sunset,                                         & !in
               evap)                                                                     !out
    implicit none
    !Ouput
    double precision, intent (out)  :: evap
    !double precision, intent(inout) :: GH(2)
    !integer, intent(inout) :: IHBC(2)
    !Input
    double precision, intent(in) :: atmpres          !Atmospheric pressure [kPa]
    double precision, intent(in) :: bd1              !Bulk density layer 1[Mg/m3] -> Convert from g/cm3 (x*1.0D9)
    double precision, intent(in) :: dist1            !depth layer 1 [m]  -> convert from cm (x/100)
    double precision, intent(in) :: frac_clay1       !Fraction clay [0-1] -> fract
    double precision, intent(in) :: frac_organic1    !Fraction organic [0-1] -> fract
    double precision, intent(in) :: frac_sand1       !Fraction sand [0-1] -> fract
    double precision, intent(in) :: frac_silt1       !Fraction silt [0-1] -> fract
    double precision, intent(in) :: ksat1            !saturated conductivity [m/s]   ->  convert from cm/hr   (x/2.7778D6)
    double precision, intent(in) :: rh               !relative humidity air [0-1]    fract
    double precision, intent(in) :: soiltemp1        !Soil temp layer 1 [0C] good
    double precision, intent(in) :: tair             !Temperature air [0C]   good    
    double precision, intent(in) :: theta1           !volumetric swc layer 1 [m3/m3]   -> convert from cm3/cm3 (x/1.0E6)
    double precision, intent(in) :: thetar1          !residual volumetric swc layer 1 [m3/m3] -> convert from cm3/cm3 (x/1.0D6)
    double precision, intent(in) :: thetas1          !saturated swc layer 1 [m3/m3] -> convert from cm3/cm3 (x/1.0E6)
    double precision, intent(in) :: thetaw1          !wilting point layer 1 [m3/m3] -> convert from cm3/cm3 (x/1.0E6) 
    double precision, intent(in) :: EOS              !Potential evap(+residue?) [m/s] -> convert from cm/hr   (x/2.7778D6)
    double precision, intent(in) :: hourlyrad        ! hourly radiation value:
    
    double precision, intent(in) :: hour
    double precision, intent(in) :: sunset
    double precision, intent(in) :: sunrise
    !locals
    double precision    :: surf_capil_rate  
    double precision    :: surf_vapor_rate
    double precision    :: max_evap_rate
    double precision    :: tempEOS2
    double precision    :: soil_evap_rate   !will be m/s -> convert to cm/hr (*3.6D5)
    double precision    :: evapamp
    double precision    :: lenday
    real    :: soilrh1
    integer :: k
    
!****************************************************************************
!    call   WEPS_calc_capilrate (bd1, dist1, frac_clay1, frac_organic1,   & ! Input
!                frac_sand1, frac_silt1, ksat1, rh, soiltemp1, tair,      & ! Input
!                theta1, thetar1, thetas1, thetaw1,                       & ! Input
!                soilrh1, surf_capil_rate)                                  ! Output
!****************************************************************************
    call WEPS_calc_capilrate(real(bd1),real(dist1),real(frac_clay1),  &
         real(frac_organic1),real(frac_sand1),real(frac_silt1),real(ksat1), &
         real(rh), real(soiltemp1),real(tair),real(theta1),real(thetar1),   &
         real(thetas1),real(thetaw1),soilrh1,surf_capil_rate)                                         
!****************************************************************************
!    call WEPS_calc_vaporrate(atmpres,dist1,rh,soiltemp1,soilrh1,tair,   &  ! Input
!                             theta1,thetas1,                            &  ! Input
!                             surf_vapor_rate)                              ! Output
!****************************************************************************  
    call WEPS_calc_vaporrate(real(atmpres),real(dist1),real(rh),real(soiltemp1), &
        soilrh1,real(tair),real(theta1),real(thetas1),surf_vapor_rate)
!****************************************************************************      
    soil_evap_rate = surf_vapor_rate + surf_capil_rate
! Setup EOS for use:
    !tempEOS = EOS * ((1.0D0 / 100.0D0) * (1.0D0 / 3600.0D0)) ! cm/hr to m/s -> * 2.7778e-6
    max_evap_rate = EOS * 0.01  !cm/day to m/day
    max_evap_rate = max_evap_rate * (0.1d0/86400.d0)
    lenday = sunset-sunrise
    evapamp = 3.1415927d0 * 0.9d0 * (EOS * 0.01)  / lenday / 2.0d0
    if (hour > sunrise .and. hour < sunset) then
        max_evap_rate = max_evap_rate + evapamp * sin(3.1415927d0*(hour-sunrise)/lenday)
    endif
                             
    !
    soil_evap_rate = max( -max_evap_rate, soil_evap_rate )
    soil_evap_rate = min( max_evap_rate, soil_evap_rate )
    soil_evap_rate = soil_evap_rate*((100.0D0) * (3600.0D0)) ! m/s to cm/hr -> * 3.600E5  

    soil_evap_rate = max (0.0d0, soil_evap_rate)
    evap =-soil_evap_rate

!****************************************************************************
    end subroutine WEPS_MAXSOILEVAP
!****************************************************************************

    

!****************************************************************************
!
!****************************************************************************    
! Yanked code. Source: dvolw.for -> lines 185-230 (3.14.2016)
!!     calculate maximum evaporation rate
!!     this method using a lenday that is 12 hours (43200 sec) allows
!!     running time continuously through many days
!!      wfluxn(3) = max(0.0,evapamp * sin(pi*(tday-sunrise)/lenday))
!!     this method assumes that tday and sunrise and sunset are in the
!!     same day and lenday can be actual daylight hours
!      ! evapamp accounts for 90% of daily evaporation
!      ! 10% remaining is distributed over the remainder of the day
!      max_evap_rate = evap_dissag(                                      &
!     &               tday, sunrise, sunset, lenday, evapamp, evapdaypot)
!
!!     find air relative humidity from diurnal air temperature
!!     and dewpoint temperature
!      if( tday .lt. 21600 ) then
!          airtemp = airtempsin( tday, airtmaxprev, airtmin )
!      else if( tday .lt. 64800 ) then
!          airtemp = airtempsin( tday, airtmax, airtmin )
!      else
!          airtemp = airtempsin( tday, airtmax, airtminnext )
!      end if
!
!      airvappres = satvappres(tdew)
!      airsatvappres = satvappres(airtemp)
!      airhumid = airvappres/airsatvappres
!      airvapden = vaporden( airtemp, airhumid )
!
!!      if( (soilrh(1).ge.airhumid).and.(airhumid.le.0.99) ) then
!!      if( (airhumid.le.0.99) ) then
!          ! calculate reduction in evaporation rate due to dry soil
!!          soil_evap_rate = max_evap_rate                                &
!!     &                   * (soilrh(1)-airhumid)/(1.0-airhumid)
!!      else
!!          soil_evap_rate = 0.0
!!      end if
!
!      surface_vapor_rate = ((soilvapden(1)-airvapden)                   &
!     &                 * soildiffu(1) / denwat) / dist(1)
!
!      air_mat_pot = matricpot_from_rh( airhumid, airtemp )
!      surface_capil_rate = (swm(1) - air_mat_pot) *0.5*cond(1) / dist(1)
!
!      soil_evap_rate = surface_vapor_rate + surface_capil_rate
!      soil_evap_rate = max( -max_evap_rate, soil_evap_rate )
!      soil_evap_rate = min( max_evap_rate, soil_evap_rate )