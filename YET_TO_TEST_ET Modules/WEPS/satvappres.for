!$Author: fredfox $
!$Date: 2013-07-09 17:01:19 -0600 (Tue, 09 Jul 2013) $
!$Revision: 12671 $
!$HeadURL: https://infosys.ars.usda.gov/svn/code/weps1/trunk/weps.src/src/lib_hydro/satvappres.for $

      real function satvappres( airtemp )

      ! returns the saturated vapor pressure for water (kPa)
      ! Approximation from Jensen ASCE manual 70 evapotranspiration
      ! referenced to Tetens (1930), and transformed by Murray (1966)
      ! Converted here to use temperature in (C)
      ! valid in normal climatic condition range

      real airtemp
      ! airtemp - the temperature of the air (C)

      real c1, c2, c3
      ! c1, c2, c3 -  coefficients for saturated equation
      parameter (c1 = 0.611, c2 = 17.27, c3 = 237.3)

      satvappres = c1 * exp( c2 * airtemp/( airtemp + c3 ) )

      return
      end
      
