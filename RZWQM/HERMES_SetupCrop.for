      SUBROUTINE hermes_setupcrop(WULAEN,GEHOB,WUGEH,DAUERKULT,INFIX,
     + PHYLLO,VERNTAGE,trootsum,ASPOO,komp,PLNAME,IPL,WORG,MaxNup,
     + NRKOM,CO2METH,LEGUM,NGEFKT,NRENTW,LAIFKT,LUKRIT,PLHGHT,PLALFA,
     + pltpop,WORG2MIN,WORG3MIN,BAS,GSPhase)
C
C======================================================================
C
C       PURPOSE:  READ IN PLANT PARAMETERS FROM PLGEN.DAT
C
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C
C       EXTERNAL REFERENCES:
C
C       CALLED FROM: RZMAN(?)
C
C       PROGRAMMER:
C
C       VERSION: 1.1
C
C======================================================================
C
      implicit none

      double precision,intent(inout) :: WULAEN            !(WUMAS*100000*100./7.)/(.015^2.*PI) total root length over profile (Crop initialization)
      double precision,intent(inout) :: PHYLLO            !only 0 if non perennial (From crop file), Non 0 if perennial cross year? Phyllo is the sum of the modified temperature sums
      double precision,intent(inout) :: VERNTAGE          !only 0 if non perennial (From crop file) accumulated vernalization days
      double precision,intent(inout) :: trootsum          !only 0 if perennial (From crop file)     accumulated soil temperature sum at current rooting depth
      double precision,intent(inout) :: ASPOO             !0 from Crop initialization             temporary assimilation pool      
      integer, intent(inout) :: komp(5)
      integer, intent(in) :: IPL
      double precision :: TENDSUM,lured,ad,maxNup         !TENDSUM + TSUM(i) (sum of temperature days across phases
     
      
      common/HERMES/Kc,TSUM,VSCHWELL,DAYL,DLBAS,DRYswell,PRO,    !Array
     + DEAD,WGMAX,MAIRT,Kcini,WUMAXPF,MAXAMAX,
     + YIFAK,MINTMP,TEMPTYP,YORGAN,TENDSUM
     
      
      integer NUMPHASES,NUMORGANS,INUMB
      PARAMETER (NUMPHASES = 10,NUMORGANS = 5)
      double precision :: Kc (10)                     !kc factor for evapotranspiration at end of phase (range = 1 to NRENTW) (From crop file)
      integer :: GSPhase (10)                         !GS at end of phase (range = 1 to NRENTW) (From crop file)
      double precision :: TSUM(10)                    !required development phase temperature sums (range = 1 to NRENTW) (From crop file)
      double precision :: WORG(5)                     !(initial and current) weight kg d.m./ha per ORGAN (range = 1  to NumCompartment) (From crop file)
      double precision :: WDORG(5)                    !weight dead organ (kg d.m./ha) per organ (range = 1 to NumCompartment)
      character(len=2) :: CROPR                       !RMarquez -> Note this is an array of 3char values.(Crop abbreviations)       
      double precision :: VSCHWELL(10)                !Vernalisation requirements (days) (range = 1 to NRENTW)(From crop file)
      double precision :: DAYL(10)                    !day length requirements (hours) (range = 1 to NRENTW)(From crop file)
      double precision :: DLBAS(10)                   !Base day length in phase 1(hours)(range = 1 to NRENTW)(From crop file)
      double precision :: DRYswell(10)                !drought stress below ETA/ETP-quotient (range = 1 to NRENTW)(From crop file)      
      double precision :: PRO(10,5)                   !Phase N Partitioning array, (x,y) where X = phase, Y = crop organ # (From crop file)
      double precision :: DEAD(10,5)                  !Phase Death rate array, (x,y) where X = phase, Y = crop organ #   
      double precision :: LAIFKT(10)                  !specific leave area (area per mass) (m2/m2/kg TM) (range = 1 to NRENTW)(From crop file)
      double precision :: BAS(10)                     !Base temperature in phase (range = 1 to NRENTW)(From crop file)
      double precision :: WGMAX(10)                   !N-content root end of phase (range = 1 to NRENTW)(From crop file)         
      double precision :: MAIRT(5)                    !Maintainance rates of organs (From crop file)      
      double precision :: LUKRIT(10)                  !Critical air content in topsoil per phase      
      double precision :: Kcini                       !initial kc factor for evapotranspiration (From crop file)
      double precision :: GEHOB                       !Start and current concentration N in ab. gr. biomass (% i. d.m.) (From crop file)
      double precision :: WUGEH                       !Start and current concentration N in roots (% i. d.m.) (From crop file)
      double precision :: WUMAXPF                     !maximum effective rooting depth (From crop file)      
      double precision :: MINTMP                      !Minimum temperature crop growth    (Grad C) (From crop file)
      double precision :: maxamax                     !AMAX: Max. CO2 assimilation rate (kg CO2/ha leave/h) (From crop file)
      double precision :: yifak
      integer          :: yorgan
      integer          :: NGEFKT                      !crop N-content function no. (critical and max. N-contents) (From crop file)
      integer          :: NRKOM                       !Number of compartments for plant (From crop file)
      integer          :: NRENTW                      !number of development phases (From crop file)
      integer          :: TEMPTYP                     !Type of temperature dependency (C3 = 1/ C4 = 2) (From crop file)      
      integer          :: co2meth                     !CO2METHOD
      character(len=1) :: DAUERKULT                   !Perennial crop (From crop file) D=perennial crop
      character(len=1) :: LEGUM                       !Legume crop (From crop file)    L=legume

      double precision :: dummy,PLHGHT,PLALFA,pltpop,WORG2MIN,WORG3MIN
      
      character :: cropname*30, PLNAME*30
c      character(len=30), intent(in) :: PLNAME
      integer :: HERMESFILE = 6985
      integer :: cropnum
      integer :: cropptr
      integer :: NUMCROPS
      integer :: INDX
      integer :: I
      integer :: INDX2
      integer :: INDX3
      integer :: leafflag
      integer :: stemflag
      integer :: earflag
      integer :: otherflag
      integer :: perennial
      integer :: legume
      integer :: WUFKT
      logical :: fexist
      INTEGER INFIX,K,itrim
      CHARACTER STRING*255
      
      
      character(len=1) UPCASE

!outline:
! figure out which crop line to look at:
! in each data section, read x number of lines to get this particular crops row num
! populate all necessary info based off specified crop data, fulfill all loops/etc as processing/reading data.

!///////////////////////////// end control     //////////////////////////////////
!///////////////////////////// setup init values based on toher stuff //////////////////////////////////
!           ! Assumption: perennial crops start at development tage 3, others at 0. Therefore, sum(i) is kept for sum(1 and 2) for perennials
!           IF Dauerkult$ <> "D" then
!                DOUBLE,ASIP,ADATE,MDATE,ENDPRO = 0
!                PHYLLO,VERNTAGE = 0
!              FOR I = 1 to 5
!                  FOR i2 = 1 to 10
!                        SUM(I2) = 0
!                        PRO(I2,I),DEAD(I2,I) = 0
!                  NEXT I2
!              NEXT I
!           ELSE
!              FOR I = 1 to 5
!                  FOR i2 = 3 to 10
!                        SUM(I2) = 0
!                        PRO(I2,I),DEAD(I2,I),trootsum = 0
!                  NEXT I2
!              NEXT I
!           END IF
!           ! Keep N content of roots and above ground biomass for perennials, or read initials from parameter file for annuals
!           IF Dauerkult$ = "D" AND FRUCHT$(IPL) = FRUCHT$(IPL-1) and IPL > 2 then
!                Gehob = gehob
!                WUGEH = WUGEH
!              !print gehob,wugeh
!              !get key wart
!           ELSE
!                GEHOB = VAL(LINE06$(66:maxnum))/100
!                WUGEH = VAL(LINE06b$(66:maxnum))/100
!           END IF  
!           INPUT #4: LINE1C$
!           ! Keep weight of organs for perennials, or read initials from parameter file for annuals
!           FOR I = 1 to NRKOM
!               IF Dauerkult$ = "D" AND FRUCHT$(IPL) = FRUCHT$(IPL-1) and IPL > 2 then
!                    WORG(I) = WORG(I)
!               ELSE
!                    WORG(I) = VAL(LINE1b$(26+8*i:30+8*i))
!               END IF
!                 MAIRT(I)= VAL(LINE1C$(26+8*i:30+8*i))
!                 WDORG(I)= 0
!           NEXT I

!///////////////////////////// read phase data //////////////////////////////////
!               FOR L = 1 to NRKOM
!                     PRO(I,L) = VAL(LINE9$(26+8*l:30+8*l))
!                     dead(I,L) = VAL(LINE9b$(26+8*l:30+8*l))
!               NEXT L
!                 TENDSUM = TENDSUM + TSUM(i)
!           NEXT I
!           CLOSE #4
!		 !end of reading plant prameter file. 
!           ! Assumption: perennial crops start at development tage 3, others at 0
!           IF Dauerkult$ = "D" AND FRUCHT$(IPL) = FRUCHT$(IPL-1) and IPL > 2 then
!                ISTAGE = 2
!                sum(1) = TSUM(1)+1
!                Sum(2) = Min(Phyllo,TSUM(2)*0.5)
!                Phyllo = Min(Phyllo,TSUM(2)*0.5)
!                obmas = 0
!           ELSE
!                ISTAGE = 1
!                OBMAS = 0
!           END IF     
!           ! Identifying above ground crop compartments and estimation of abgr. biomass
!             nrkob = len(progip1$)
!           FOR i = 1 to nrkob
!                 komp= VAL(PROGIP1$(i:i))
!                 OBMAS=OBMAS + WORG(KOMP)      ! + 
!           NEXT i
!             ASPOO = 0          !.24 * WORG(5)
!             LAI = (WOrg(2))*LAIFKT(ISTAGE)
!             WUMAS=WORG(1)
!             WULAEN=(WUMAS*100000*100./7.)/(.015^2.*PI)
!           ! Including beet/tuber mass in calculation of N-content for sugar beets and potatoes 
!           IF Frucht$(IPL) = "ZR " or Frucht$(IPL) = "K  " then
!                PESUM = (OBMAS+WORG(4))*gehob+WUMAS*WUGEH
!           ELSE
!                PESUM =(OBMAS*gehob+WUMAS*WUGEH)    !*.007
!           END IF
!           FOR I = 1 to n
!                 wudich(i) = 0
!           NEXT i
!        END IF
 

      inquire (FILE = 'hermes.dat', EXIST = FEXIST)
      if (.NOT.FEXIST) then
          PRINT*,'HERMES.DAT IS NOT EXISIT'
          STOP
          ! error message here, file doesn't exist, exit program?
      end if
      OPEN(HERMESFILE,FILE='HERMES.DAT',STATUS='UNKNOWN') 
C     =================================================================
C     == MATCH PLANT INDEX WITH POINTER TO APPROPRIATE SPECIES IN   ===
C     == THE GENERIC PLANT DATA BASE       ===
C     =================================================================

C     ...READ NUMBER OF SPECIES IN FILE
      call ECHO(HERMESFILE)
      READ(HERMESFILE,*) NUMCROPS
!     File is ready to go:
!     Find which row # of data contains the crop we are looking for: 
      call ECHO(HERMESFILE)
      cropptr = -1
      do indx=1,NUMCROPS
        READ(HERMESFILE,1000) cropName
        ! call toupper(cropname)
        DO I = 1, LEN(cropName)
         cropName(I:I) = UPCASE(cropName(I:I))
        END DO
        
        ! Index(PLNAME,cropname) > 0
        IF(index(cropName,PLNAME(1:itrim(PLNAME))).ne.0) THEN
          cropptr=indx
C          IPL=INDX
          go to 50 !exit loop, found crop index
        ENDIF
50      continue
      enddo
      if (cropptr.eq.-1) then
          ! hermes.dat did not contain crop
          ! exit prog with error
      endif
!==============================================================================      
!     ...READ IN INITIAL HERMES CROP VALUES
!==============================================================================
      CALL ECHO(HERMESFILE)
      do INDX=1,NUMCROPS
        if (INDX.eq.CROPPTR) then 
        read (HERMESFILE,1110) string
        k=inumb(string)
        backspace (HERMESFILE)
 1110 FORMAT(A255)
	 if (k.eq.12) then
!         Read in the line of data
           READ(HERMESFILE,*) MAXAMAX,TEMPTYP,MINTMP,WUMAXPF, WUFKT,
     +      NGEFKT,KCINI,GEHOB,WUGEH,NRENTW,NRKOM,co2meth
           MaxNup=6.0d0
           Infix=0
           ELSE IF (K.EQ.14) THEN
           READ(HERMESFILE,*) MAXAMAX,TEMPTYP,MINTMP,WUMAXPF, WUFKT,
     +      NGEFKT,KCINI,GEHOB,WUGEH,NRENTW,NRKOM,co2meth,MaxNup,INFIX
           ENDIF
           WUMAXPF=WUMAXPF/10.0d0
           if (maxnup.eq.0.0d0) MaxNup=6.0d0
            GEHOB = GEHOB/100.d0
            WUGEH = WUGEH/100.d0
        else
!         row is not what we need, dump it.
           READ(HERMESFILE,*) DUMMY            
        endif
      enddo         
!==============================================================================      
!     ...READ IN PLANT DESC VARIABLES
!==============================================================================
      CALL ECHO(HERMESFILE)
      do INDX=1,NUMCROPS
        if (INDX.eq.CROPPTR) then
!         Read line:
c        read (HERMESFILE,1110) string
c        k=inumb(string)
c        backspace (HERMESFILE)
c	  if (k.eq.10) then
c           READ(HERMESFILE,*) (KOMP(indx2),indx2=1,4),
c     +      yorgan,yifak,perennial,legume,PLHGHT,PLALFA
c        ELSE
           READ(HERMESFILE,*) (KOMP(indx2),indx2=1,4),
     +     WORG2MIN,WORG3MIN,perennial,legume,PLHGHT,PLALFA
         IF (WORG2MIN.LT.10.0D0.OR.WORG3MIN.LT.10.0D0) THEN
         WORG2MIN=720.D0
         WORG3MIN=100.D0
         ENDIF
c        ENDIF        
           if (PLALFA.gt.100.0d0) then
              PLALFA=-2.0D0*PLHGHT*LOG(0.5D0)/(PLALFA/10.d0/pltpop)
           elseif (PLALFA.gt.0.0d0) then
              PLALFA=-2.0D0*PLHGHT*LOG(0.5D0)/PLALFA
           endif
!         Process above ground flags:
          do indx2 = 1, 4
              KOMP(indx2) = KOMP(indx2)*(indx2+1)
          enddo
!         Process perennial/legume flags:
           if (perennial.eq.1) then
               DAUERKULT="D"
           else
               DAUERKULT="N"
           endif
           
           if (legume.eq.1) then
               LEGUM ="L"
           else
               LEGUM ="N"
           endif             
        else
           READ(HERMESFILE,*) DUMMY            
        endif
      enddo 
!==============================================================================      
!     ...READ IN PLANT ORGAN WEIGHTS
!==============================================================================
      CALL ECHO(HERMESFILE)
      do INDX=1,NUMCROPS
        if (INDX.eq.CROPPTR) then
!         Read line:
           READ(HERMESFILE,*) (WORG(indx2),indx2=1,5)
        else
           READ(HERMESFILE,*) DUMMY            
        endif
      enddo 
!==============================================================================      
!     ...READ IN PLANT MAINTENANCE RATES
!==============================================================================
      CALL ECHO(HERMESFILE)
      do INDX=1,NUMCROPS
        if (INDX.eq.CROPPTR) then
!         Read line:
           READ(HERMESFILE,*) (MAIRT(indx2),indx2=1,5)
        else
           READ(HERMESFILE,*) DUMMY            
        endif
      enddo 
!==============================================================================      
!     ...READ Temp Sum                 TSUM(i) = VAL(LINE4$(66:maxnum))
!==============================================================================
      CALL ECHO(HERMESFILE)
      do INDX=1,NUMCROPS
        if (INDX.eq.CROPPTR) then
!         Read line:
           READ(HERMESFILE,*) (TSUM(indx2),indx2=1,NRENTW)
        else
           READ(HERMESFILE,*) DUMMY            
        endif
      enddo
!==============================================================================
! sum of temperature from all growth stages
!==============================================================================
        TENDSUM=0.0D0
        DO INDX2=1,NRENTW
                 TENDSUM = TENDSUM + TSUM(INDX2)
      ENDDO
!
!                 BAS(i) = VAL(LINE5$(66:maxnum))
!==============================================================================      
!     ...READ BAS TEMP
!==============================================================================
      CALL ECHO(HERMESFILE)
      do INDX=1,NUMCROPS
        if (INDX.eq.CROPPTR) then
!         Read line:
           READ(HERMESFILE,*) (BAS(indx2),indx2=1,NRENTW)
        else
           READ(HERMESFILE,*) DUMMY            
        endif
      enddo       
!                 VSCHWELL(i) = VAL(LINE6$(66:maxnum))
!==============================================================================      
!     ...READ VSCHWELL per phase 
!==============================================================================
      CALL ECHO(HERMESFILE)
      do INDX=1,NUMCROPS
        if (INDX.eq.CROPPTR) then
!         Read line:
           READ(HERMESFILE,*) (VSCHWELL(indx2),indx2=1,NRENTW)
        else
           READ(HERMESFILE,*) DUMMY            
        endif
      enddo 
!                 DAYL(i) = VAL(LINE7$(66:maxnum))
!==============================================================================      
!     ...READ DAYL per phase 
!==============================================================================
      CALL ECHO(HERMESFILE)
      do INDX=1,NUMCROPS
        if (INDX.eq.CROPPTR) then
!         Read line:
           READ(HERMESFILE,*) (DAYL(indx2),indx2=1,NRENTW)
        else
           READ(HERMESFILE,*) DUMMY            
        endif
      enddo     
!                 DLBAS(i) = VAL(LINE7b$(66:maxnum))
!==============================================================================      
!     ...READ DLBAS per phase 
!==============================================================================
      CALL ECHO(HERMESFILE)
      do INDX=1,NUMCROPS
        if (INDX.eq.CROPPTR) then
!         Read line:
           READ(HERMESFILE,*) (DLBAS(indx2),indx2=1,NRENTW)
        else
           READ(HERMESFILE,*) DUMMY            
        endif
      enddo     
!                 DRYswell(i) = VAL(LINE8$(66:maxnum))
!==============================================================================      
!     ...READ DRYswell per phase 
!==============================================================================
      CALL ECHO(HERMESFILE)
      do INDX=1,NUMCROPS
        if (INDX.eq.CROPPTR) then
!         Read line:
           READ(HERMESFILE,*) (DRYswell(indx2),indx2=1,NRENTW)
        else
           READ(HERMESFILE,*) DUMMY            
        endif
      enddo     
!                 LUKRIT(i) = VAL(LINE8b$(66:maxnum))
!==============================================================================      
!     ...READ LUKRIT per phase 
!==============================================================================
      CALL ECHO(HERMESFILE)
      do INDX=1,NUMCROPS
        if (INDX.eq.CROPPTR) then
!         Read line:
           READ(HERMESFILE,*) (LUKRIT(indx2),indx2=1,NRENTW)
        else
           READ(HERMESFILE,*) DUMMY            
        endif
      enddo   
!                 LAIFKT(i) = VAL(LINE8c$(66:maxnum))
!==============================================================================      
!= 7)Critical air content in topsoil per phase
!     ...READ LAIFKT per phase
!==============================================================================
      CALL ECHO(HERMESFILE)
      do INDX=1,NUMCROPS
        if (INDX.eq.CROPPTR) then
!         Read line:
           READ(HERMESFILE,*) (LAIFKT(indx2),indx2=1,NRENTW)
        else
           READ(HERMESFILE,*) DUMMY            
        endif
      enddo   
!                 WGMAX(i) = VAL(LINE8d$(66:maxnum))
!==============================================================================      
!     ...READ WGMAX per phase
!==============================================================================
      CALL ECHO(HERMESFILE)
      do INDX=1,NUMCROPS
        if (INDX.eq.CROPPTR) then
!         Read line:
           READ(HERMESFILE,*) (WGMAX(indx2),indx2=1,NRENTW)
        else
           READ(HERMESFILE,*) DUMMY            
        endif
      enddo   
!             Kc
!==============================================================================      
!     ...READ Kc per phase
!==============================================================================
      CALL ECHO(HERMESFILE)
      do INDX=1,NUMCROPS
        if (INDX.eq.CROPPTR) then
!         Read line:
           READ(HERMESFILE,*) (Kc(indx2),indx2=1,NRENTW)
        else
           READ(HERMESFILE,*) DUMMY            
        endif
      enddo
      
!ouble precision :: PRO(NUMPHASES,NUMORGANS)    !Phase N Partitioning array, (x,y) where X = phase, Y = crop organ # (From crop file)
!double precision :: DEAD(NUMPHASES,NUMORGANS)     

!             GS
!==============================================================================      
!     ...READ GS per phase
!==============================================================================
      CALL ECHO(HERMESFILE)
      do INDX=1,NUMCROPS
        if (INDX.eq.CROPPTR) then
!         Read line:
           READ(HERMESFILE,*) (GSPhase(indx2),indx2=1,NRENTW)
        else
           READ(HERMESFILE,*) DUMMY            
        endif
      enddo
      
!==============================================================================      
!     ...READ Partition rate per phase
!==============================================================================
      do indx3=1, 5 !number of compartments possible
        CALL ECHO(HERMESFILE)
        do INDX=1,NUMCROPS
          if (INDX.eq.CROPPTR) then
!         Read line:
            READ(HERMESFILE,*) (PRO(indx2,indx3),indx2=1,NRENTW)
           else
            READ(HERMESFILE,*) DUMMY            
         endif
        enddo
      enddo
!==============================================================================      
!     ...READ Death rate per phase
!==============================================================================
      do indx3=1, 5 !number of compartments possible
        CALL ECHO(HERMESFILE)
        do INDX=1,NUMCROPS
          if (INDX.eq.CROPPTR) then
!         Read line:
            READ(HERMESFILE,*) (DEAD(indx2,indx3),indx2=1,NRENTW)
           else
            READ(HERMESFILE,*) DUMMY            
         endif
        enddo
        enddo
!==============================================================================
! sum of temperature from all growth stages
!==============================================================================
        TENDSUM=0.0D0
        do INDX=1,NUMCROPS
                 TENDSUM = TENDSUM + TSUM(INDX)
        ENDDO
       CLOSE(HERMESFILE)
!
!==============================================================================      
!     ...HERMES.DAT read in.
!==============================================================================
 1000 FORMAT(A30)
      END
!==============================================================================
