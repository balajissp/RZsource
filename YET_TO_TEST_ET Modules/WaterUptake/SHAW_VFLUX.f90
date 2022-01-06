!****************************************************************************
! subroutine SHAW_VFLUX(. . .)
!       
! ***********
! Written 3.30.2016 -> calculate VFLUX for possible use as the boundary flux condition when solving richards equation
!
! ***********
!   CALLS:
!   STAB
!****************************************************************************
subroutine SHAW_VFLUX(WT,TA,WDT,TADT,HUM,HUMDT,HEIGHT,ZM,ZH,ZERO,WIND, &
    T,TDT,VAP,VAPDT,VFLUX, ZC, NC,PRESUR)
    implicit none
!****************************************************************
! Reference: SHAW_ATSTAB.for
! Begin Declarations
!****************************************************************
!   Constants
    double precision, parameter :: LV=2.5D6         ! latent heat of vaporization
    double precision, parameter :: RHOL=1.0D3       ! density of liquid water  (kg/m^3)       
    double precision, parameter :: RHOA=1.25D0      ! density of air  (kg/m^3)
    double precision, parameter :: CA=1.006D3       ! spec heat of air  (J/kg-K)
    double precision, parameter :: LS=2.835D6       ! latent heat of sublimation
    double precision, parameter :: VONKRM=0.4D0     ! von Karman constant
    double precision, parameter :: VDIFF=2.12D-5    ! vap diffusivity in still air
    double precision, parameter :: P0=1.013D5       ! stand atm pressure at sea level  (101,300 Pa)
    double precision, parameter :: TKA=0.025D0      ! thermal cond of air
    integer, parameter :: nodcan=11
!   Output
    double precision, intent(out) :: vflux      ! turbulent vapor transfer at surface node
!   Input
    double precision, intent(in)  :: WT         ! weighting factor for beginning of time step values
    double precision, intent(in)  :: TA         ! air temp at beginning of time step
    double precision, intent(in)  :: TADT       ! air temp at end of time step
    double precision, intent(in)  :: WDT        ! weighting factor for end of time step values
    double precision, intent(in)  :: HUM        ! humidity at beginning of time step
    double precision, intent(in)  :: HUMDT      ! humidity at end of time step
    double precision, intent(in)  :: HEIGHT     ! height of windspeed and temp measurements
    double precision, intent(in)  :: ZM         ! aerodynamic roughness length for momentum transfer (m)
    double precision, intent(in)  :: ZERO       ! plane of zero displacement in wind profile calc.
    double precision, intent(in)  :: WIND       ! ave wind speed for the time step (m/s)
    double precision, intent(in)  :: ZH         ! aerodynamic roughness length for heat transfer (m)
    double precision, intent(in)  :: T          ! Air Temp
    double precision, intent(in)  :: TDT        ! temp at end of time step
    double precision, intent(in)  :: VAP        ! vapor density of the surface node at t (kg/m^3)
    double precision, intent(in)  :: VAPDT      ! vapor density of surface node at t+dt (kg/m^3)
    double precision, intent(in)  :: PRESUR     ! ave atm pressure at site  (N/m^2)
    double precision, intent(in)  :: ZC(NODCAN) ! > ??????
    integer, intent(in) :: NC                   !
!   Locals
    double precision :: DUMMY   ! holds sat vapor pressure curve, not used
    double precision :: SATV    ! sat vapor density
    double precision :: SATVDT  ! sat vapor density at end of time step
    double precision :: TMPAIR  ! temperature of the air?
    double precision :: VAPAIR  ! vapor pressure air?
    double precision :: ZMLOG   !
    double precision :: ZHLOG   !
    double precision :: PSIM    ! diabatic wind speed profile correction for momentum transfer
    double precision :: PSIH    ! diabatic temperature profile correction for heat transfer
    double precision :: TMPSFC  ! temperature surface
    double precision :: VAPSFC  ! Vapor pressure surface
    double precision :: HFLUX   ! turbulent heat flux at surface node
    double precision :: RV      ! resistance to surface heat transfer (s/m)
    double precision :: RH      ! resistance to surface heat transfer (s/m)
    double precision :: VMIN    ! minimum turbulent vapor transfer at surface node
    double precision :: USTAR   ! friction velocity (m/s)
    double precision :: STABLE  ! atmospheric stability (ratio of thermal to mechanical turbulence)
    double precision :: DV      ! effective vapor diffusion coefficient through soil (m2 s!1)
    integer :: iter             ! current num of iter for energy and water balance
!*************************************************************************************************
! End Declarations -> Begin Code
!*************************************************************************************************    
    iter = 0
    !NOTE: TADT = TA, T = current temperature
    !       All 3 are same value?
    
    !RMarquez -> calculate all of the required info for VFLUX/STAB Call.
    !        IF (ITER .LE. 2) THEN
    !           IF (ITER .LE. 1) THEN
    !****         CALCULATE CONSTANTS FOR TIME STEP
    CALL VSLOPE (DUMMY,SATV,TA)
    CALL VSLOPE (DUMMY,SATVDT,TADT)
    TMPAIR=WT*TA + WDT*TADT
    VAPAIR=WT*HUM*SATV + WDT*HUMDT*SATVDT
    ZMLOG=LOG((HEIGHT+ZM-ZERO)/ZM)
    ZHLOG=LOG((HEIGHT+ZH-ZERO)/ZH)
    !           END IF      
    !****      DEFINE INITIAL ASSUMPTIONS FOR CURRENT TIME STEP
    PSIM=0.0D0
    PSIH=0.0D0
    USTAR=WIND*VONKRM/(ZMLOG + PSIM)
    !        END IF

    TMPSFC=WT*T + WDT*TDT
    VAPSFC=WT*VAP + WDT*VAPDT
    HFLUX=0.0D0
    VFLUX=0.0D0
    IF (WIND .GT. 0.0D0) THEN
        CALL STAB(iter,HEIGHT,HFLUX,PSIH,RH,TMPSFC,TMPAIR,ZH,ZERO, &
                 ZMLOG,ZHLOG,WIND,USTAR,STABLE,NC,ZC(NC+1))

        RV=RH
        VFLUX=(VAPAIR-VAPSFC)/RV
    ENDIF

   !****   NOW COMPARE VAPOR FLUXES
    DV=VDIFF*(((TMPAIR+273.16D0)/273.16D0)**2.0D0)*(P0/PRESUR)
    if (nc .eq. 0) then
        VMIN=DV*(VAPAIR-VAPSFC)/(HEIGHT-ZERO)
    else
        VMIN=DV*(VAPAIR-VAPSFC)/(HEIGHT-ZC(NC+1))
    end if
    IF (ABS(VMIN) .GT. ABS(VFLUX)) THEN
        !          ..FLUX IS SUFFICIENTLY LOW TO BE CONSIDERED MINIMUM
        VFLUX=VMIN
    END IF
    
    !done, vflux is calculated.

!*************************************************************************************************
! End
end subroutine SHAW_VFLUX
!*************************************************************************************************
