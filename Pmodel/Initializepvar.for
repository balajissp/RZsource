      SUBROUTINE Initializepvar
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The Subroutine Initializes Soil P Pools and Variables
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  18/07/2016       D SADHUKHAN          ORIGNIAL CODE
C 2.  30/06/2021       Zhiming Qi           Added RatioSIPtoAIP next to Frsop; 
C                                             Change Stable Inorganic P:Active Inorganic P from 4 to user defined
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C 1.  Nsoil              INTEGER             -                Number of soil layer
C 2.  Psc                REAL                -                Phosphorus Sorption Coefficient
C 3.  Clayp               REAL                %                Clayp percentage of soil
C 4.  Orgm               REAL                %                Soil organic matter
C 5.  Labp               REAL              Kg/ha              Soil labile P
C 6.  Actp               REAL              Kg/ha              Soil Active P
C 7.  Stabip             REAL              Kg/ha              Soil Stable inorganic P
C 8.  Stabop             REAL              Kg/ha              Soil Stable organic P
C 9.  Frsop              REAL              Kg/ha              Soil Fresh organic P
C 10. Manwip             REAL              Kg/ha              Manure Water Extractable Inorganic P
C 11. Manwop             REAL              Kg/ha              Manure Water Extractable Organic P
C 12. Mansop             REAL              Kg/ha              Manure Stable Organic P
C 13. Mansio             REAL              Kg/ha              Manure Stable Inorganic P
C 14. Crpres             REAL              Kg/ha              Crop Residue of the soil surface
C 15. Areap               REAL              ha                 Areap of the field
C 16. Orgc               REAL              %                  Soil organic carbon
C 17. i                  INTEGER           -                  Loop Counter
C 18. Tsoil              REAL              m                  Thickness of the soil layer
C 19. Areap               REAL              ha                 Areap of the field   
C 20. Bdsoil             REAL              Kg/m3              Bulk Density of Soil
C 21. Orgc               REAL              %                  Soil organic carbon
C 22. Avfrtp             REAL              Kg                 Fertilizer P Available readily directly after
C                                                             fertilizer application
C 23. Tavfertp           REAL              Kg                 Total fertilizer P Available readily directly after
C                                                             fertilizer application
C 24. Daysfert           INTEGER           -                  Days since fertilizer applied
C 25. Slnf               INGEGER           -                  Seraial No of The Ferilizer Applied
C 26. RatioSIPtoAIP      REAL             -                   Stable Inorganic P:Active Inorganic P
C FUNCTIONS/SUBROUTINE CALLED ::
C
C       NAME                    DESCRIPTION
C    ----------              ------------------           
C 
C
C===============================================================================
C                         END OF SPECIFICATION
C===============================================================================
      USE VARIABLE
      IMPLICIT NONE
      
      INTEGER :: i
      
      DO i = 1, Nsoil
          Labp(i) = Labp(i)/(Tsoil(i)*Bdsoil(i)*0.01)             ! Converts Labp Kg/ha to mg/Kg
          Orgc(i) = Orgm(i)*0.58
          Psc(i)=-0.045*LOG(Clayp(i))+0.001*Labp(i)-0.035*Orgc(i)+0.43 
          IF (Psc(i) > 0.7) THEN 
              Psc(i) = 0.7
          END IF
          IF (Psc(i) < 0.1) THEN 
              Psc(i) = 0.1
          END IF
          Labp(i) = Labp(i)*(Tsoil(i)*Bdsoil(i)*0.01)             ! Converts Labp mg/Kg to Kg/Ha
          Actp(i) = Labp(i)*(1-Psc(i))/Psc(i)
          Stabip(i) = RatioSIPtoAIP(i)*Actp(i)                   !Zhiming changed 4 to this parameter
          Inicrpres(i) = Crpres(i) 
          Daysrp(i) =0
          Daydesrp(i)=0
          Actpuptake(i)=0.0
      END DO  
      
      Slnf = 0
      Do i = 1,Nfert
          AvfertP(i) = 0.0
          Daysfert(i) = 0.0
          Resfertp(i) = 0.0
          Fertpsorp(i) = 0.0
          Dummy2(i) = 0.0
          Fertprelease(i)=0.0
          Appdayavfertp(i)=0.0
          Appdayresfertp(i)=0.0
          Fertpmrunoff(i)=0.0
          Fertpcrunoff = 0.0
          FertPLabP1(i) = 0.0
          FertPLabP2(i) = 0.0
      END DO
      
      Slnm = 0
      DO i = 1,Nman
          Manwip(i) = 0.0
          Manwop(i) = 0.0
          Mansop(i) = 0.0
          Mansip(i) = 0.0 
          Mancova(i)  = 0.0
          Manmass(i) = 0.0
          Manmoist(i)=0.0
          Mancova(i)=0.0
          Manextrc(i)=0.0
          Maniprelease(i)=0.0
          Manipmrunoff(i)=0.0
          Manoprelease(i)=0.0
          Manopmrunoff(i)=0.0
          ManSO_WI(i) = 0.0
          ManSI_WI(i) = 0.0
          ManWO_WI(i) = 0.0
          ManSO_WO (i) = 0.0
          ManWI_LabP1(i) = 0.0
          ManWI_LabP2(i) = 0.0
          ManWI_LabP3(i) = 0.0
          ManWO_LabP1(i) = 0.0
          ManWO_LabP2(i) = 0.0
          ManWO_LabP3(i) = 0.0
          ManSO_LabP1(i) = 0.0
          ManSO_LabP2(i) = 0.0
          ManSO_LabP3(i) = 0.0
          ManSI_ActP1(i) = 0.0         
          ManSI_ActP2(i) = 0.0
          ManSI_ActP3(i) = 0.0
      END DO
      
      Slnt = 0
      Slnc = 0
      Cropday=.False. 
      Flag = .False.
      Countman = 0
      Countfert = 0
      Counttill = 0
      DO i = 1, Ncrop
           Hu(i) = 0.0
           Biopact(i) = 0.0
      END DO
      
      
      DO i = 1,Ncrop
          Pstress(i) = 1.0
      END DO
      
      FertilizerP = 0.0
      ManuP = 0.0
      ResP = 0.0
      PlntP_Up = 0.0
      Cdrpgw = 0.0
      Hgwt  = 0.0
      !Inidrpmgw = 0.60
      !Inippmgw = 1.25         ! m
      Mgwp=0.0
      !Inidrpmgw = 0.7
      !Inippmgw = 1.25        ! if
      Ms = 0
      Tdparti = 0.0
      Counternode1 = 1
      !Psts = .False.
      RETURN
      END SUBROUTINE Initializepvar