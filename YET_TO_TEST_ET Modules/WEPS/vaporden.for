!$Author: joelevin $
!$Date: 2011-03-24 10:33:26 -0600 (Thu, 24 Mar 2011) $
!$Revision: 11724 $
!$HeadURL: https://infosys.ars.usda.gov/svn/code/weps1/trunk/weps.src/src/lib_hydro/vaporden.for $

      real function vaporden( airtemp, relhum )

!     returns the water vapor density in air (kg/m^3)
!     calculated directly from PV = nRT

!*** Argument declarations ***
      real airtemp, relhum
!     airtemp - the temperature of the air (C)
!     relhum - relative humidity of the air (fraction)

!*** function declarations ***
      real satvappres
!     satvappres - function to find the saturated vapor pressure (Pascals)

!*** Local declarations ***
      real actvappres
!     actvappres - actual vapor pressure (kPa)

      actvappres =  relhum * satvappres(airtemp)

!      if(actvappres.lt.1.0e-34) write(*,*) 'vaporden:',relhum,airtemp
      vaporden = 2.166 * actvappres / (airtemp + 273.15)

      return
      end
      
