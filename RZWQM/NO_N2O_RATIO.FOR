C      FUNCTION NO_N2O(ppt,snow,afiel,bulkd,partdens,avgwfps)
C========================================================================================
C ADAPTED FROM CENTURY MODEL, DAYCENT
C========================================================================================
C      IMPLICIT NONE
C	DOUBLE PRECISION PI,KRAINNO,NO_N2O_RATIO,dDO
C      DATA PI/3.1415926/
C
C      krainNO = nox_pulse(ppt, snow)
C	 
C      dDO=diffusiv(afiel, bulkd, avgwfps,partdens) 
C
C      NO_N2O_ratio=8.0D0+(18.0D0*atan(0.75D0*PI*(10.D0*dDO-1.86D0)))/PI 
C
C
C****************************************************************************
C              Copyright 1993 Colorado State University                    */
C                      All Rights Reserved                                 */
C
C****************************************************************************
C*
C*  FILE:      diffusiv.c
C*
C*  FUNCTION:  real*4 diffusiv()
C*
C*  PURPOSE:   For estimating normalized diffusivity in soils.  Method of
C*             Millington and Shearer (1971) Soil Science Literature Source:
C*             Davidson, E.A. and S.E. Trumbore (1995).
C*             Contributed by Chris Potter, NASA Ames
C*  
C*  INPUTS:
C*    A       - is the fraction of soil bed volume occuppied by field capacity
C*              (intra-aggregate pore space), units 0-1
C*    bulkden - bulk density (g/cm3)
C*    wfps    - water-filled pore space (fraction 0.0-1.0)
C*              (fraction of a porespace that is filled with water)
C*              volumetric water / porosity
C*
C*  GLOBAL VARIABLES:
C*    None
C*
C*  LOCAL VARIABLES:
C*    debug      - flag to set debugging mode, 0 = off, 1 = on
C*    my_theta_V - testing alternate way of computing the volumetric water
C*                 content of the soil bed volume
C*    PARTDENS   - particle density (g/cm3)
C*    pfc        - water content of soil expressed as a percent of field
C*                 capacity (%)
C*    porosity   - the fraction of soil bed volume occupied by pore space (P)
C*                 (A + inter-aggregate pore space), units 0-1
C*                  = 1 - bulkden/particle density
C*    sw_p       - the fractional liquid saturation of the P component of
C*                 total pore volume
C*    s_wat      - the fractional liquid saturation of the A component of
C*                 total pore volume
C*    theta_A    - the volume of water per unit bed volume contained in
C*                 intra-aggregate pore space
C*    theta_P    - the volume of water per unit bed volume contained in
C*                 inter-aggregate pore space
C*    theta_V    - the volumetric water content of the soil bed volume
C*    tp1 .. tp8 - intermediate variables
C*    vfrac      - volumetric fraction    
C*
C*  OUTPUT:
C*    dDO - the normalized diffusivity in aggregate soil media, units 0-1
C*
C*  CALLED BY:
C*    dailymoist()
C*
C*  CALLS:
C*    None
C*
C****************************************************************************/

C#include <math.h>
C#include <stdio.h>
C#include <stdlib.h>
C#include <string.h>
C
C#define PARTDENS 2.65           /* Particle Density (g/cm3) */
C/*#define min(a,b) (((a) < (b)) ? (a) : (b))
C#define max(a,b) (((a) > (b)) ? (a) : (b))*/

C      function diffusiv( A, bulkden, wfps)
      function diffusiv( A, bulkden, wfps,partdens)
C     Add passing vars
      double precision A, bulkden, wfps
	double precision partdens 
      double precision porosity 
      double precision theta_A, theta_P, theta_V, my_theta_V 
      double precision tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8 
      double precision vfrac 
      double precision s_wat, sw_p 
      double precision pfc 
	double precision diffusiv 
      double precision dDO 
      integer   debug 
C Add double precision varialbles for power functions
      double precision ds_wat,dporosity,dtp2,dtp5,dsw_p, doubletemp 


C      /* Initialization */
      debug = 0 
c      partdens=2.65
      porosity = 1.0 - (bulkden)/PARTDENS 
c      wfps=theta/porosity
C      if ((porosity - ( A)) < 0.0) {
C        printf("ERROR:  Mis-match between bulk density and field capacity\n") 
C        printf("value in soils.in file.  Ending simulation!\n") 
C        exit(1) 
C      }

      pfc = ( wfps)*100 /(( A)/porosity) 
 
      if (pfc >= 100.0) then
        vfrac = (( wfps)*porosity - A) / (porosity - ( A)) 
      else 
        vfrac = 0.0 
      endif

      if (pfc < 100.0) then
	theta_V = (pfc/100.0)*( A) 
      else 
      theta_V = ( A) + (min(vfrac,1.0D0 ) * (porosity - ( A)))  ! RM - changed the constant to a double.
      endif

      my_theta_V =  wfps * (1.0  -  bulkden/partdens) 
C  translation of: theta_P = (theta_V < ( A)) ? 0.0  : theta_V - ( A) 
	if (theta_V < ( A))then
      theta_P = 0.0
	else
	theta_P= theta_V - (A) 
	endif
C  translation of: theta_A = (theta_V > ( A)) ? ( A) : theta_V 
      if (theta_V > ( A))then
      theta_A = (A)
	else
	theta_A= theta_V 
	endif
      


C      if (debug) {
C        printf("In SUB, vfrac = %f\n", vfrac) 
C        printf("In SUB, porosity - A = %f\n", (porosity - (*A))) 
C        printf("In SUB, (*A) = %f\n", (*A)) 
C        printf("In SUB, (*wfps) = %f\n", (*wfps)) 
C        printf("In SUB, pfc = %f\n", pfc) 
C        printf("In SUB, porosity = %f\n", porosity) 
C        printf("In SUB, theta_V = %f\n", theta_V) 
C        printf("In SUB, my_theta_V = %f\n", my_theta_V) 
C        printf("In SUB, theta_P = %f\n", theta_P) 
C        printf("In SUB, theta_A = %f\n", theta_A) 
C      }

      s_wat = min(1.0D0 , theta_V/( A))  ! RM - changed the constant to a double.
      sw_p = min(1.0D0 , theta_P/(porosity-( A)))  ! RM - changed the constant to a double.

CC    Initialize doubles before power functions 
	ds_wat=s_wat
	dporosity = porosity
	


	tp1 = (1.0 - ds_wat)**2.0 

      tp2 = (( A) - theta_A) / (( A) + (1.0  - porosity)) 

      dtp2=tp2 
COriginal in C  tp3 = (float)pow((double)tp2,(0.5*(double)tp2 + 1.16)) 

      tp3 = dtp2 **(0.5*dtp2 + 1.16) 
C Original tp4 = (float)(1.0 - pow(   (double)((porosity-(*A))), (  0.5* (double)((porosity-(*A))) + 1.16 )   )     ) 

      tp4 = (1.0 - (dporosity-( A))**(0.5*(dporosity-( A)) + 1.16) ) 

      tp5 = (porosity-( A))-theta_P 

	dtp5=tp5 

      if (tp5 > 0.0) then
        tp6 = dtp5**(0.5*dtp5 + 1.16) 
       else 
        tp6 = 0.0 
      endif

	
	dsw_p=sw_p 
C tp7 = (float)pow((1.0-(double)sw_p), 2.0) 
      tp7 = (1.0-dsw_p)**2.0 
      tp8 = max(0.0D0, (tp1*tp3*tp4*(tp5-tp6)) / ! RM - changed the constant to a double.
     1                 (1.0E-6  + (tp1*tp3*tp4) + tp5 - tp6) * 1.0E7 ) 

C      if (debug) printf("From SUB: %f %f %f %f %f %f %f %f\n",
C                        tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8) 

      dDO = max(0.0D0, (tp8/1.0E7  + tp7*tp6))  ! RM - changed the constant to a double.
C      if (debug) {
C	    printf("In SUB, dDO = %f\n", dDO) 
C      }


Creturns a single precision real*4
Cmust return name of function var return(dDO) 
      diffusiv=dDO  
      end function diffusiv

C*****************************************************************************
C*              Copyright 1993 Colorado State University                    */
C*                      All Rights Reserved                                 */
C
C*****************************************************************************
C*
C*  FILE:      nox_pulse.c
C*
C*  FUNCTION:  real*4nox_pulse() 
C*
C*  PURPOSE:   
C*  
C*  INPUTS:
C*    ppt  - daily precip (cm)
C*    snow - snow cover (cm SWE); SNP in RZWQM2
C*
C*  GLOBAL VARIABLES:
C*    None
C*
C*  LOCAL VARIABLES:
C*    cumppt() - circular array holding precipitation values
C*    ii       - loop control variable
C*    indx     - current array index
C*    mptr     - starting position in mtplr circular array 
C*    mtplr()  - circular array ??
C*    nph      - starting position in ph circular array
C*    npl      - starting position in pl circular array
C*    npm      - starting position in pm circular array
C*    nppt     - starting position in cumppt circular array
C*    pflag    - ??
C*    ph()     - circular array ??
C*    PHDAYS   - ?? (13)
C*    pl()     - circular array ??
C*    PLDAYS   - ?? (2)
C*    pm()     - circular array ??
C*    PMDAYS   - ?? (6)
C*    PPTDAYS  - number of consecutive days to track precipitation values (15)
C*    retval   - return value, increase of NO due to moisture and rain >= 1.0
C*    sumppt   - sum of precipitation values in cumppt array
C*
C*  OUTPUTS:
C*    retval - increase of NO due to moisture and rain >=1.0
C*
C*  CALLED BY:
C*    trace_gas_model()
C*
C*  CALLS:
C*    max3 - return the maximum of three input values
C*
C****************************************************************************/
C
C#include <stdio.h>
C#include <stdlib.h>
C#include <math.h>

C#define PPTDAYS 15
C#define PLDAYS 2
C#define PMDAYS 6
C#define PHDAYS 13

Creal*4 max3(real*4a, real*4b, real*4c);

      function nox_pulse(ppt, snow)
C
	integer PPTDAYS,PLDAYS,PMDAYS,PHDAYS
	
	double precision cumppt(0:14)
	double precision pl(0:1)
	double precision pm(0:5)
	double precision ph(0:12)
	double precision mtplr(0:12)
	integer npl, npm, nph, nppt, mptr
      integer pflag
	double precision sumppt,PPT,SNOW
      double precision retval,nox_pulse
      integer ii, indx
C
	pptdays=15
	pldays=2
	pmdays=6
	phdays=13



      data cumppt /15*0.0d0/
	data pl /2*1.0d0/
	data pm /6*1.0d0/
      data ph /13*1.0d0/
      data mtplr /13*1.0d0/
      
C     Initialize
      data npl/0/,npm/0/,nph/0/,nppt/0/,mptr/0/,pflag/0/
c      save npl,npm,nph,nptr,nppt
      sumppt = 0.0
      cumppt(nppt) = ppt
C Liwang should this start at 2 if c code started at 1 instead of 0
      do ii=1,PPTDAYS;
C Original c code indx = (nppt+ii) % PPTDAYS;
        indx = int(mod((nppt+ii),PPTDAYS))
        sumppt =sumppt + cumppt(indx)
      end do 

C     /* Comparisons with data showed NO emissions to be about twice of */
C      /* what they should be, reduce the pulse multipliers, cak - 09/24/03 */
C     If (snow) in C language
      if (snow>0.0) then
	mtplr(mptr) = 0.0

	else if ((sumppt <= 1.0) .and. (ppt > 0.1)) then
C        /* initiate new pulse */
		if (ppt < 0.5) then
		do ii=0,PLDAYS-1
C Original indx = (npl+ii) % PLDAYS;
 	     indx = mod((npl+ii),PLDAYS)
C/*            pl(indx) = 11.19f * (float)exp(-0.805 * (double)(ii+1)); */
C/*            pl(indx) = 5.6f * (float)exp(-0.805 * (double)(ii+1)); */
C     pat chk if no double typecast to ii+1 ok
            pl(indx) = 2.8 * exp( -0.805*(ii+1))  
          end do
          pflag = 2
	
		else if ((ppt >= 0.5) .and. (ppt <= 1.5)) then
		 do ii=0,PMDAYS-1
C           indx = (npm+ii) % PMDAYS;
            indx = mod((npm+ii),PMDAYS)
C/*            pm(indx) = 14.68f * (float)exp(-0.384 * (double)(ii+1)); */
C/*            pm(indx) = 7.34f * (float)exp(-0.384 * (double)(ii+1)); */
            pm(indx) = 3.67 * exp(-0.384 * (ii+1))
          end do
          pflag = 6
		
		else  
          do ii=0,PHDAYS-1
C            indx = (nph+ii) % PHDAYS;
             indx = mod((nph+ii),PHDAYS)
C/*            ph(indx) = 18.46f * (float)exp(-0.208 * (double)(ii+1)); */
C/*            ph(indx) = 9.23f * (float)exp(-0.208 * (double)(ii+1)); */
            ph(indx) = 4.615 * exp(-0.208 * (ii+1))
          end do
          pflag = 13

		endif

		mtplr(mptr) = max(pl(npl),pm(npm),ph(nph))
C         Pat check interp pflag--;
          pflag=pflag-2
		
		else if (pflag > 0) then
		
		mtplr(mptr) = max(pl(npl),pm(npm),ph(nph))
		pflag=pflag-2
		
		else 
		mtplr(mptr) = 1.0d0
		endif

      retval = mtplr(mptr)
      pl(npl) = 1.0d0
      pm(npm) = 1.0d0
      ph(nph) = 1.0d0

C      /* increment pointers in circular arrays */
      npl = mod((npl+1) , PLDAYS)
      npm = mod((npm+1) , PMDAYS)
      nph = mod((nph+1) , PHDAYS)
      nppt = mod((nppt+1) , PPTDAYS)
      mptr = mod((mptr+1) , PHDAYS)

C      return(retval);
      nox_pulse = retval
      End function nox_pulse

C/*****************************************************************************
C*
C*  FUNCTION:  real*4max3() 
C*
C*  PURPOSE:   Return the maximum value from three input values 
C*  
C*  INPUTS:
C*    a - first input value
C*    b - second input value
C*    c - third input value
C*
C*  GLOBAL VARIABLES:
C*    None
C*
C*  LOCAL VARIABLES:
C*    m - temporary storage variable
C*
C*  OUTPUTS:
C*    m - maximum input value
C*
C*  CALLED BY:
C*    nox_pulse()
C*
C*  CALLS:
C*    None
C*
C****************************************************************************/

      function max3(a, b, c)
      
      double precision max3, a, b, c
   
      if (a > b)then 
        max3 = a;
      else
        max3 = b;
	endif

      if (c > max3) max3 = c;


C      return(max3);
      end function max3
c==============================================================================================      
        function Rn2n2o(dD0_fc,co2ppm,wfps,nitratePPM)
c
      DOUBLE PRECISION k1,dD0_fc,fRno3_co2,nitrateppm,co2ppm,fRwfps,wfps
      double precision Rn2n2o

c        newCO2 the amount of microbial respiration that has occurred in the past day (g/m2)

c        grams_soil(ilyr) = bulkden(layer) * tl(layer) * 1.0d2 * 1.0d2

c        co2PPM(layer) = rdf(layer) * (*newCO2) / grams_soil(layer) * 1.0E-6
        k1 = max(1.5d0, 38.4d0 - 350.0d0 * dD0_fc)
        if (co2ppm.lt.1.0d-7) then
           fRno3_co2=0.16d0*k1
        else
        fRno3_co2=max(0.16d0*k1,k1*exp(-0.8d0*nitratePPM/co2PPM))
        endif

        fRwfps = max(0.1d0, 0.015d0 * wfps*100.0d0 - 0.32d0)

        Rn2n2o = fRno3_co2 * fRwfps
         
        if (Rn2n2o < 0.1d0) then
          Rn2n2o = 0.1d0
        endif
        
c        ntotflux(Layer) = RDEN * grams_soil(layer) * 1.0E-6
c        n2oflux = ntotflux(layer)/(Rn2n2o + 1.0)
      end
