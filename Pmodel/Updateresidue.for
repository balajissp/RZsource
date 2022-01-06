      SUBROUTINE UPDATERESIDUE(ID,IM,IYYY)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The Subroutine updates the crop residue in the above ground and below ground soil profile
C     at the day of harvest
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  22/08/2016       D SADHUKHAN          ORIGNIAL CODE
C
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C      i                  INTEGER           -                  Loop Counter
C      j                  INTEGER           -                  Loop Counter
C      
C FUNCTIONS/SUBROUTINE CALLED ::
C
C       NAME                    DESCRIPTION
C    ----------              ------------------           
C 1. 
C
C===============================================================================
C                         END OF SPECIFICATION
C===============================================================================  
      USE VARIABLE
      IMPLICIT NONE
      INTEGER :: i,j,k
      INTEGER,INTENT(IN) :: ID,IM,IYYY
      INTEGER :: Day,Month,Year
      DOUBLE PRECISION :: ratio,BlgrP,AvgrP,GrainP
      DAY = ID
      Month = IM
      Year = IYYY
      
      DO i = 1, Ncrop
        IF(Day==Chday(i).AND.Month==Chmon(i).AND.Year==Chyear(i)) THEN
          Avgrres(i) = Avgrbiom(Day,Month,Year)-Yeild(Day,Month,Year)
          Crpresleft(i) = Avgrres(i) + Blgrbiom(Day,Month,Year)
       IF (Avgrbiom(Day,Month,Year)+ Blgrbiom(Day,Month,Year)> 0) THEN
       
          BlgrP = (Biopact(i)*Blgrbiom(Day,Month,Year))/  
     +     (Avgrbiom(Day,Month,Year)+ Blgrbiom(Day,Month,Year))
          AvgrP = (Biopact(i)*
     +         (Avgrbiom(Day,Month,Year)-Yeild(Day,Month,Year)))/  
     +           (Avgrbiom(Day,Month,Year)+ Blgrbiom(Day,Month,Year))
          GrainP = (Biopact(i)*Yeild(Day,Month,Year))/  
     +           (Avgrbiom(Day,Month,Year)+ Blgrbiom(Day,Month,Year))
        ELSE
           BlgrP =0.0
           AvgrP =0.0
           GrainP = 0.0
       ENDIF
          
        ResdueP0 = Tempu1 + AvgrP + BlgrP
       ! CP0 = ResdueC0/ResdueP0
          
          
       IF(Rootd(Day,Month,Year) > 0.0) THEN
       
          DO j = 1, Nsoil
           IF (Dsoil(j) >= Rootd(Day,Month,Year)) EXIT
          END DO 
          
          DO k = 1,j-1
            Ratio = Tsoil(k)/Rootd(Day,Month,Year)
            Blgrbiomly(k) = Blgrbiom(Day,Month,Year)*ratio
            BlgrPly(k) = BlgrP * Ratio
          END DO
            ratio = (Rootd(Day,Month,Year)- Dsoil(j-1))
     +                              /Rootd(Day,Month,Year)
            Blgrbiomly(j) = Blgrbiom(Day,Month,Year)*ratio
            BlgrPly(j) = BlgrP * Ratio
         
          
         DO k=1,J 
             CProot(k) = (Blgrbiomly(k)*0.4)/ BlgrPly(k)
             SOroot(k) =  (1/CProot(k)-1/CP(k,1))/(1/CP(k,2)-1/CP(k,1))
             IF (SOroot(k) > 1) SOroot(k) = 1.0
             IF (SOroot(k) < 0) SOroot(k) = 0.0
         END DO 
          
      ENDIF 
       
        
        
        
        !  
        !DO k =1,j
        !  ResdueP1(k) = ResdueP1(k) + BlgrPly(j)*SOroot(k)
        !  ResdueP2(k) = ResdueP2(k) + BlgrPly(j)*(1-SOroot(k))
        !END DO  
          
          IF(Nsoil ==1) THEN
              Inicrpres(1) = Inicrpres(1) + Crpresleft(i)*1.0
              Crpres(1) = Crpres(1) +  Crpresleft(i)*1.0
          ELSEIF(Nsoil==2) THEN
              Inicrpres(1) = Inicrpres(1) + Crpresleft(i)*0.8
              Inicrpres(2) = Inicrpres(2) + Crpresleft(i)*0.2
              Crpres(1) = Crpres(1) +  Crpresleft(i)*0.8
              Crpres(2) = Crpres(2) +  Crpresleft(i)*0.2
          ELSE
              Inicrpres(1) = Inicrpres(1) + Crpresleft(i)*0.6
              Inicrpres(2) = Inicrpres(2) + Crpresleft(i)*0.3
              Inicrpres(3) = Inicrpres(3) + Crpresleft(i)*0.1
              Crpres(1) = Crpres(1) +  Crpresleft(i)*0.6
              Crpres(2) = Crpres(2) +  Crpresleft(i)*0.3
              Crpres(3) = Crpres(3) +  Crpresleft(i)*0.1
          END IF
              WRITE(115,'(A,I2,X,A)')'Crop No',i,"is Harvested"
     !!         WRITE(115,'(/,A)')"Soil Crop Residue is Updated"
     !!         DO j = 1,Nsoil
     !!          WRITE(115,'(/,A5,I2,/)')'Layer',j
     !!          WRITE(115,'(2(A30,2X))')'Initial Crop Residue(Kg/ha)'
     !!+         ,'Crop Residue(Kg/ha)'
     !!          WRITE(115,'(2(F30.3,2X))')Inicrpres(j),Crpres(j)
              !END DO
        END IF
        END DO
      
        !Pflowresfrso1 = ResdueP0*0.0015
        !Frsop(1) = Frsop(1) + Pflowresfrso1
        
       END  SUBROUTINE UPDATERESIDUE