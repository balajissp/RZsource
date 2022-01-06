C***********************************************************************
C
      SUBROUTINE SOLVRAD (NLAYR,A,B,C,D,RD,RU)
C
C     THIS SUBROUTINE SOLVES THE MATRIX OF EQUATIONS FOR RADIATION 
C     TRANSFER WITHIN THE CANOPY.  THE FORM OF THE MATRIX IS:
C
C     THIS SUBROUTINE SOLVES A TRI-DIAGONAL MATRIX OF THE FORM :
C
C              | B1  C1   0   0  .  .  .   0 | |RD1|   | D1|
C              | A2  B2   0  C2  .  .  .   0 | |RU1|   | D2|
C              | A3   0  B3  C3  .  .  .   0 | |RD2| = | D3|
C              |  .   .   .   .     .  .   . | | . |   |  .|
C              |  0   . AI1 BI1    0 CI1   0 | |RU |   |DI1|
C              |  0   . AI+1  0 BI+1 CI+1  0 | |RD | = |DI2|
C              |  .   .   .   .     .  .   . | | . |   |  .|
C              |  0   0   0   0  .  . AN  BN | |RUN|   | DN|
C
C
C     RD(I) IS THE DOWN FLUX ABOVE LAYER I
C     RU(I) IS THE UPWARD FLUX ABOVE LAYER I
C     (C1 IS NORMALLY ZERO, BUT IT DOES NOT HAVE TO BE)
C
C***********************************************************************
      PARAMETER (NODRES=10, nodcan=11)
      DOUBLE PRECISION RD(NODCAN+NODRES+1),RU(NODCAN+NODRES+1),
     +  A(2*(NODCAN+NODRES+1)),B(2*(NODCAN+NODRES+1)),
     +  C(2*(NODCAN+NODRES+1)),D(2*(NODCAN+NODRES+1))
C     
      DO 40 N=1,NLAYR
C        ELIMINATE EACH COEFFICIENT BELOW DIAGONAL IN COLUMN K
         K = 2*N - 1
         I = K + 1
C        DETERMINE MULTIPLICATION FACTOR TO ELIMATE A(I)
         QUOT = A(I)/B(K)
C        ADJUST ELEMENTS IN ROW (EQUATION) I
         B(I)=B(I)-QUOT*C(K)
         D(I)=D(I)-QUOT*D(K)
         A(I) = 0.0d0
C
C        DETERMINE MULTIPLICATION FACTOR TO ELIMATE A(I), BUT ZERO 
C        ELEMENT IN ARRAY NOW TEMPORARILY ASSIGNED TO A(I)
         I = K + 2
         QUOT = A(I)/B(K)
C        ADJUST ELEMENTS IN ROW (EQUATION) I
         A(I)=-QUOT*C(K)
         D(I)=D(I)-QUOT*D(K)
C
         K = K + 1
C        DETERMINE MULTIPLICATION FACTOR TO ELIMATE A(I)
         QUOT = A(I)/B(K)
C        ADJUST ELEMENTS IN ROW (EQUATION) I
         C(I)=C(I)-QUOT*C(K)
         D(I)=D(I)-QUOT*D(K)
         A(I) = 0.0d0
C
   40 CONTINUE
C
      K = 2*(NLAYR+1) - 1
      I = K + 1
      QUOT = A(I)/B(K)
      B(I)=B(I)-QUOT*C(K)
      D(I)=D(I)-QUOT*D(K)
      A(I)=0.0d0
C
C     COMPUTE UPWELLING RADIATION FROM SNOW OR SOIL
      RU(NLAYR+1)=D(2*(NLAYR+1))/B(2*(NLAYR+1))
C
C     Remainder of back-substitution
      DO 60 N=NLAYR,1,-1
         I = 2*N + 1
C        COMPUTE DOWNWELLING RADIATION BELOW LAYER N
         RD(N+1) = (D(I) - C(I)*RU(N+1))/B(I)
         I = 2*N
C        COMPUTE UPWELLING RADIATION ABOVE LAYER N
         RU(N) = (D(I) - C(I)*RU(N+1))/B(I)
   60 CONTINUE
      RD(1) = D(1)/B(1)
C
      RETURN
      END
