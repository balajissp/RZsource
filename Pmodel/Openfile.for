      SUBROUTINE OPENPFILE
      USE VARIABLE
      CHARACTER(LEN=255) :: Floc   
      
     
     !! Floc = TRIM(Scnraioname(1:LEN(TRIM(Scnraioname))-10)//
     !!+'/inisoilp.txt')
      
      
       
     !! OPEN(UNIT=105,FILE=Floc,                                    ! Opening of The Soil Data File
     !!+  STATUS='OLD', ACTION='READ')
     !! 
     !!
     !!
     !! READ(UNIT=105,FMT=*)                                                      ! Reading of Soil Data File Starts
     !! READ(UNIT=105,FMT=*)
     !! DO i = 1, Nsoil
     !!   READ(Unit=105,FMT=*)
     !!+     Labp(i),Stabop(i),Frsop(i) 
     !! END DO  
                                                            ! Reading of Soil Data File Ends
C      DO i = 1, Nsoil
C         Orgm(i) = 4.30
C      END DO
      
     !! Floc = TRIM(Scnraioname(1:LEN(TRIM(Scnraioname))-10)//
     !!+'/Pcoeffip.txt')
     !! 
     !! OPEN(UNIT=106,FILE=Floc,                                  ! Opening of The P Coefficient Data File
     !!+  STATUS='OLD', ACTION='READ')
     !! 
     !!                                                                                         
     !!                                                                         ! Reading of P Coefficient Data File Starts 
     !!  READ(UNIT=106,FMT=*)
     !!  READ(UNIT=106,FMT=*)!Kr
     !!  READ(UNIT=106,FMT=*)
     !!  READ(UNIT=106,FMT=*)
     !!  READ(UNIT=106,FMT=*)!Kdh
     !!  READ(UNIT=106,FMT=*)
     !!  READ(UNIT=106,FMT=*)
     !!  READ(UNIT=106,FMT=*)!Kf  
     !!  READ(UNIT=106,FMT=*)
     !!  READ(UNIT=106,FMT=*)
     !!  READ(UNIT=106,FMT=*)!Pextr    
     !!  READ(UNIT=106,FMT=*)
     !!  READ(UNIT=106,FMT=*)
     !!  READ(UNIT=106,FMT=*)(CPini(i),i=1,2)
     !!  READ(UNIT=106,FMT=*)
     !!  READ(UNIT=106,FMT=*)
     !!  READ(UNIT=106,FMT=*)(CPini(i),i=3,5)
     !!  READ(UNIT=106,FMT=*)
     !!  READ(UNIT=106,FMT=*)
     !!  READ(UNIT=106,FMT=*)CP0
     !!  DO i = 1,Nsoil
     !!   DO j = 1,5
     !!       CP(i,j) = CPini(j)
     !!   END DO
     !!  END DO
     !!  
                                                                              ! Reading of P Coefficient Data File Ends
      
     !!  Floc = TRIM(Scnraioname(1:LEN(TRIM(Scnraioname))-10)//
     !!+'/Fertilizerp.txt')
     !  
     !  
     !! OPEN(UNIT=107,FILE=Floc,        ! Opening of The Fertilizer Data File
     !!+  STATUS='OLD', ACTION='READ')
      
     !!  READ(UNIT=107,FMT=*)                                                     ! Reading of Fertilizer Data File Starts 
     !!  CALL Countdata(107,Nfert) 
     !!  ALLOCATE(Fday(Nfert),Fyear(Nfert),Fmon(Nfert),Fertp(Nfert),
     !!+         Fertd(Nfert),Flefts(Nfert),Avfertp(Nfert),
     !!+         Daysfert(Nfert),Resfertp(Nfert),Fertpfr(Nfert),
     !!+         Scov(Nfert),Fertpsorp(Nfert),Appdayavfertp(Nfert),
     !!+         Appdayresfertp(Nfert),Dummy2(Nfert),
     !!+         Dailyfertpsop(Nfert),Norain(Nfert),Fertprelease(Nfert),
     !!+         Fertpcrunoff(Nfert),Fertpmrunoff(Nfert),PPavfertp(Nfert),
     !!+         PPresfertp(Nfert),PPmavfertp(Nfert),
     !!+         PPcavfertp(Nfert), PPcresfertp(Nfert),PPmresfertp(Nfert))
     !!  
     !!  READ(UNIT=107,FMT=*) 
     !!  DO i = 1,Nfert
     !!    READ(UNIT=107,FMT=*) Fday(i),Fmon(i),Fyear(i),Fertp(i),
     !!+       Fertd(i),Flefts(i),Scov(i)
     !!  END DO                                                                   ! Reading of Fertilizer Data File Ends   
    
     !! Floc = TRIM(Scnraioname(1:LEN(TRIM(Scnraioname))-10)//
     !!+'/Manurep.txt') 
     !!  
     !!  
     !!   OPEN(UNIT=108,FILE=Floc,                                 ! Opening of The Manure Data File
     !!+  STATUS='OLD', ACTION='READ')
     !!  
     !!   READ(UNIT=108,FMT=*)                                                    ! Reading of Manure Data File Starts
     !!   READ(UNIT=108,FMT=*)
     !!   CALL Countdata(108,Nman)
     !!   ALLOCATE(Manmass(Nman),Manpmass(Nman),Mantpper(Nman),
     !!+  Manweipper(Nman),Manweopper(Nman),Manwcper(Nman),Manfc(Nman),
     !!+  Mand(Nman), Mlefts(Nman),Mday1(Nman),Mmon(Nman),Myear1(Nman),
     !!+  Manwip(Nman),Manwop(Nman),Mansop(Nman),Mansip(Nman),Mtype(Nman),
     !!+  Mancova(Nman),Manmoist(Nman),Appdaymanmass(Nman),Mandcom(Nman),
     !!+  Mancovadcom(Nman),Mansipdcom(Nman),Mansopdcom(Nman),
     !!+  Manwopdcom(Nman),Manasim(Nman),Mancovaasim(Nman),
     !!+  Manwipasim(Nman),Manwopasim(Nman),Mansipasim(Nman),
     !!+  Mansopasim (Nman),Maniprelease(Nman),Manoprelease(Nman),
     !!+  Manipcrunoff(Nman),Manopcrunoff(Nman),Manipmrunoff(Nman),
     !!+  Manopmrunoff(Nman),Mandcomr(Nman),Manasimr(Nman),
     !!+  Manextrc(Nman),PPmanwip(Nman),PPmanwop(Nman),PPmansop(Nman),
     !!+  PPmansip(Nman),PPcmanwop(Nman),PPcmanwip(Nman),PPcmansip(Nman),
     !!+  PPcmansop(Nman),PPmmanwip(Nman),PPmmanwop(Nman),PPmmansip(Nman),
     !!+  PPmmansop(Nman))
     !!   
     !!   READ(UNIT=108,FMT=*)                                       
     !!   READ(UNIT=108,FMT=*)
     !!   DO i = 1,Nman
     !!   READ(UNIT=108,FMT=*)Mday1(i),Mmon(i),Myear1(i),Mtype(i),
     !!+  AppdayManmass(i),Mantpper(i),Manweipper(i),Manweopper(i),
     !!+  Manwcper(i), Manfc(i),Mand(i),Mlefts(i)
     !!   END DO              
   
     !!  Floc = TRIM(Scnraioname(1:LEN(TRIM(Scnraioname))-10)//
     !!+'/Tillagep.txt') 
     !!     
     !!   
     !!   OPEN(UNIT=109,FILE=Floc,            ! Opening of The Tillage Data File
     !!+  STATUS='OLD', ACTION='READ')
     !!      
     !!  READ(UNIT=109,FMT=*)                                                   ! Reading of Tillage Data File Starts
     !!  CALL Countdata(109,Ntill)  
     !!  ALLOCATE(Tday1(Ntill),Tmon(Ntill),Tyear(Ntill),Tillinceffi(Ntill)
     !!+ ,Tillmixeffi(Ntill),Tilld(Ntill)) 
     !!  READ(UNIT=109,FMT=*)   
     !!  DO i = 1,Ntill
     !!  READ(UNIT=109,FMT=*)Tday1(i),Tmon(i),Tyear(i),Tillinceffi(i),
     !!+ Tillmixeffi(i),Tilld(i)
     !!  END DO 
      
     !!  Floc = TRIM(Scnraioname(1:LEN(TRIM(Scnraioname))-10)//
     !!+'/Cropp.txt') 
     !!  
     !!  
     !! OPEN(UNIT=110,FILE=Floc,                ! Opening of The Crop Data File
     !!+  STATUS='OLD', ACTION='READ') 
     !!   
     !!                                                                  ! Reading of Crop Data File Starts
     !! READ(UNIT=110,FMT=*)
     !! READ(UNIT=110,FMT=*)
     !! DO i = 1, Ncrop
     !! READ(UNIT=110,FMT=*) Cropname(i), 
     !!+ Biopfracemg(i),Biopfracmat(i),Biopfrac50mat(i),Basetemp(i),
     !!+ Pupdist(i),Phumat(i)
     !! END DO
      
     !!  Floc = TRIM(Scnraioname(1:LEN(TRIM(Scnraioname))-10)//
     !!+'/Irrigationp.txt') 
     !! 
     !! OPEN(UNIT=111,FILE=Floc,            ! Opening of The Irrigation Data File
     !!+  STATUS='OLD', ACTION='READ',IOSTAT=istat)
      
     !! READ(UNIT=111,FMT=*)                                                      ! Reading of Irrigation Data File Starts
     !! CALL Countdata(111,Nirri)
     !! ALLOCATE(Iday1(Nirri),Imon(Nirri),Iyear1(Nirri),Irri(Nirri),
     !!+         Appeffi(Nirri))
     !! READ(UNIT=111,FMT=*) 
     !! DO i=1,Nirri
     !! READ(22,*) Iday1(i),Imon(i),Iyear1(i),Irri(i),Appeffi(i)       
     !! END DO                                                                   ! Reading of Irrigation Data File Ends
      
      
c      Floc = TRIM(Scnraioname(1:LEN(TRIM(Scnraioname))-10)//
c     +'/SoilP.out') 
      
      
       OPEN(UNIT=112,FILE='SoilP.out',                 ! Opening of Soil P Outputfile
     +      STATUS='REPLACE',ACTION='WRITE')    
    
c      Floc = TRIM(Scnraioname(1:LEN(TRIM(Scnraioname))-10)//
c     +'/Ploss.out')  
       
       OPEN(UNIT=113,FILE='Ploss.out',                 ! Opening of P Loss Outputfile
     +      STATUS='REPLACE',ACTION='WRITE') 
       
c      Floc = TRIM(Scnraioname(1:LEN(TRIM(Scnraioname))-10)//
c     +'/Pflux.out')   
       
       OPEN(UNIT=114,FILE='Pflux.out',                 ! Opening of P P Flux Outputfile
     +      STATUS='REPLACE',ACTION='WRITE') 
        
c       Floc = TRIM(Scnraioname(1:LEN(TRIM(Scnraioname))-10)//
c     +'/PModelLog.log')   
          
      OPEN(UNIT=115,FILE='PModelLog.log',               ! Opening of P Model Log File
     +      STATUS='REPLACE',ACTION='WRITE') 
      
c       Floc = TRIM(Scnraioname(1:LEN(TRIM(Scnraioname))-10)//
c     +'/FertP&ManP.out')   
          
      OPEN(UNIT=116,FILE='FertP&ManP.out',               ! Opening of Fertilizer and Manure P pools output file
     +      STATUS='REPLACE',ACTION='WRITE') 
      
    
c      Floc = TRIM(Scnraioname(1:LEN(TRIM(Scnraioname))-10)//
c     +'/Pbalance_Daily.out')   
          
      OPEN(UNIT=117,FILE='Pbalance_Daily.out',               ! Opening of P Balance output filev: Daily
     +      STATUS='REPLACE',ACTION='WRITE') 
      
      
c      Floc = TRIM(Scnraioname(1:LEN(TRIM(Scnraioname))-10)//
c     +'/Pbalance_Yearly.out')   
          
      OPEN(UNIT=118,FILE='Pbalance_Yearly.out',               ! Opening of P Balance output file : Yearly
     +      STATUS='REPLACE',ACTION='WRITE') 
      
c      Floc = TRIM(Scnraioname(1:LEN(TRIM(Scnraioname))-10)//
c     +'/MBLPHOS.out')   
          
      OPEN(UNIT=120,FILE='MBLPHOS.out',               ! Opening of P Balance output file : Yearly
     +      STATUS='REPLACE',ACTION='WRITE') 
      
      
      END SUBROUTINE OPENPFILE