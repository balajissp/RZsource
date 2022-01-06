!$Author: joelevin $
!$Date: 2011-03-24 10:33:26 -0600 (Thu, 24 Mar 2011) $
!$Revision: 11724 $
!$HeadURL: https://infosys.ars.usda.gov/svn/code/weps1/trunk/weps.src/src/lib_hydro/matricpot_from_rh.for $

      real function matricpot_from_rh( soilrh, soiltemp )

!     returns: matricpot
!     returns the matric potential in meters of water as defined by the 
!     clay and organic matter adsorption isotherms.

!*** Argument declarations ***
      real  soilrh, soiltemp
!     soilrh     - relative humidity of soil air (fraction)
!     soiltemp   - soil temperature (C)

!*** Include files ***
!      include 'hydro/vapprop.inc'
      include 'vapprop.inc'

      matricpot_from_rh = rgas*(soiltemp+zerokelvin)*(log(soilrh))      &
     &                  / (molewater * gravconst)

      return
      end

