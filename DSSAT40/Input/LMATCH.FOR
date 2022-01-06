C=======================================================================
C  LMATCH, Subroutine
C
C  Converts input soil layer data into fixed output soil layers
C  Created by J. W. Jones to create fixed increments for soil
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                 J. W. Jones       5- 7-91
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : NLAYRI,DI,VI,DLAYR
C
C  LOCAL  : J,K,L,VS,ZIL,ZOL,SUMZ,SUMV,ZT,ZB
C
C  OUTPUT : DS,VS
C-----------------------------------------------------------------------
C  Called : IPSOIL IPSLIN
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================
      SUBROUTINE LMATCH (NLAYRI,DLAYR,VI,NLAYRO,DS)

      IMPLICIT NONE

      INTEGER  J,K,L,NLAYRI,NLAYRO

C-RZ      REAL     VI(20),DS(20),VS(20),DLAYR(20)
      REAL     VI(40),DS(40),VS(40),DLAYR(40)
      REAL     ZIL,ZOL,SUMZ,SUMV,ZT,ZB

      IF (NLAYRI .LT. 1) RETURN

C*----------------------------------------------------------------------
C     Definition of layers and soil depths moved to IPEXP
C*----------------------------------------------------------------------

C
C     This subroutine assumes that DLAYR(L) values are depths to the
C     bottom of layer L
C
C-RZ      DO L = 1, 20
      DO L = 1, 40
         VS(L) = 0.0
      END DO

      K   = 1
      ZIL = 0.0
      ZOL = 0.0

      DO L = 1, 40
         SUMZ = 0.0
         SUMV = 0.0

         DO WHILE (.TRUE.)
            ZT   = MAX (ZOL,ZIL)
            ZB   = MIN (DS(L),DLAYR(K))
            SUMZ = SUMZ + (ZB - ZT)
            SUMV = SUMV + VI(K)*(ZB - ZT)
            IF (DS(L) .LT. DLAYR(K)) GOTO 10
C
C           Either establish last layer or go to next input layer
C
            IF (K .EQ. NLAYRI) GOTO 20
C
C           Go to next input layer to add characteristics
C
            ZIL = DLAYR(K)
            K   = K + 1
C-RZ            IF (K .GT. 20) THEN
            IF (K .GT. 40) THEN
               WRITE (*,15) K
  15           FORMAT(' More than 20 layers in soil profile : ',I3,/,
     &                ' Please fix soil profile.')
            STOP
            ENDIF
         END DO

10       VS(L) = VI(K)
         IF (SUMZ .GT. 0.0) THEN
            VS(L) = SUMV/SUMZ
         ENDIF
         ZOL = DS(L)
      END DO

      RETURN
C
C     Set last layer characteristics, and depth of profile
C
20    VS(L) = VI(K)
      IF (SUMZ .GT. 0.0) THEN
          VS(L) = SUMV/SUMZ
      ENDIF
      NLAYRO = L
      DO J = 1, NLAYRO
         VI(J) = VS(J)
      END DO

      RETURN

      END

C=======================================================================
C  LYRSET, Subroutine
C
C  Converts input soil layer data into fixed output soil layers
C  Created by J. W. Jones to create fixed increments for soil
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                 J. W. Jones       5- 7-91
C  2  Modified by                                   G.H.
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : NLAYRI,DI,ZLAYR,DS
C
C  LOCAL  : J,K,L,ZIL,ZOL,DI
C
C  OUTPUT : NLAYRO,DS,VS
C-----------------------------------------------------------------------
C  Called : IPSOIL
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE LYRSET (NLAYRI,ZLAYR,NLAYRO,DS,DLAYR,DEPMAX)

      IMPLICIT NONE

      INTEGER  J,K,L,NLAYRI,NLAYRO,mxnod
	parameter (mxnod=300)

C-RZ      REAL     DI(20),DS(20),ZLAYR(20),DLAYR(20)
      REAL     DI(mxnod),DS(20),ZLAYR(mxnod),DLAYR(20)
      REAL     ZIL,ZOL,DEPMAX

C*----------------------------------------------------------------------
C     Definition of layers and soil depths moved to IPEXP
C*----------------------------------------------------------------------

C
C     This subroutine assumes that DI(L) values are depths to the
C     bottom of layer L
C
C      DO L = 1, 20
      DO L = 1, mxnod
         DI(L) = ZLAYR(L)
      END DO

      K = 1

      DO L = 1, 20
         DO WHILE (.TRUE.)
            IF (DS(L) .LT. DI(K)) GOTO 10
C
C           Either establish last layer or go to next input layer
C
            IF (K .EQ. NLAYRI) GOTO 20
C
C           Go to next input layer to add characteristics
C
            ZIL = DI(K)
            K   = K + 1
C-RZ            IF (K .GT. 20) THEN
            IF (K .GT. mxnod) THEN
                WRITE (*,*) 'K = ',K
                ! RM - replaced pause with READ
                READ(*,*)
            ENDIF
         END DO
 10      CONTINUE
         ZOL = DS(L)
      END DO
C
C     Set last layer characteristics, and depth of profile
C
 20   CONTINUE
      DS(L)    = DI(K)
      NLAYRO   = L
      DLAYR(1) = DS(1)

      IF (NLAYRO .GT. 1) THEN
         DO J = 2, NLAYRO
            DLAYR(J) = DS(J) - DS(J - 1)
         END DO
         IF (DLAYR (NLAYRO) .LT. DLAYR(NLAYRO - 1) .AND.
     &       DLAYR (NLAYRO) .LT. 15.0) THEN
             DLAYR (NLAYRO)   = (DLAYR(NLAYRO) + DLAYR(NLAYRO-1))/2
             DLAYR (NLAYRO-1) =  DLAYR(NLAYRO)
             DS (NLAYRO-1)    = DS (NLAYRO-2)  + DLAYR(NLAYRO-1)
         ENDIF
      ENDIF

      DEPMAX = DS(NLAYRO)
      RETURN

      END
C ==========================================================
      INTEGER FUNCTION ITLEN (STRING)
C
C======================================================================
C
C       PURPOSE:  THIS PROGRAM WAS USED TO COUNT NUMBER OF INPUT FILES
C                IN EACH LINE
C
C       PROGRAMMER:  LIWANG MA
C
C       VERSION:   1
C
C-----------------------------------------------------------------------
C
      CHARACTER STRING*(*)
      INTEGER I, J, K
C
       I = 1
	 K = ITRIM(STRING)
       DO 10 J = 2, K
        IF ((STRING(J:J) .EQ. ' '). and .(STRING(J+1:J+1).NE.' '))
     +                    I = I + 1
10     CONTINUE
        ITLEN = I
      END

C========== DEVELOPED BY LIWANG MA === FOR Single PRECISIOIN ===
C ==============================================================
      SUBROUTINE REALMATCH (NLAYRI,DLAYR,VI,NLAYRO,DS)

      IMPLICIT Real (A-H,O-Z)

      INTEGER  J,K,L,NLAYRI,NLAYRO,mxnod
      parameter (mxnod=300)

C-RZ      REAL     VI(20),DS(20),VS(20),DLAYR(20)
      DIMENSION    VI(mxnod),DS(nlayro),VS(mxnod),DLAYR(nlayri)
C      DOUBLE PRECISION    ZIL,ZOL,SUMZ,SUMV,ZT,ZB

      IF (NLAYRI .LT. 1) RETURN
      IF (NLAYRO .LT. 1) RETURN

C*----------------------------------------------------------------------
C     Definition of layers and soil depths moved to IPEXP
C*----------------------------------------------------------------------

C
C     This subroutine assumes that DLAYR(L) values are depths to the
C     bottom of layer L
C
C-RZ      DO L = 1, 20
      DO L = 1, mxnod
         VS(L) = 0.0
      END DO
c check total 'mass' in the soil profile initially for correct transfer
c between two grid systems
      TSUMI = 0.0d0
c      DO L=2,NLAYRI
c     TSUMI=TSUMI + VI(L) * (DLAYR(L)-DLAYR(L-1))
c     END DO
C

      K   = 1
      ZIL = 0.0
      ZOL = 0.0
         SUMZ = 0.0
         SUMV = 0.0

      DO 200 L = 1, NLAYRO
C
201         ZT   = MAX (ZOL,ZIL)
            ZB   = MIN (DS(L),DLAYR(K))
            SUMZ = SUMZ + (ZB - ZT)
            SUMV = SUMV + VI(K)*(ZB - ZT)
            IF (DS(L) .LT. DLAYR(K)) THEN
                   VS(L) = VI(K)
                IF (SUMZ .GT. 0.0) THEN
                   VS(L) = SUMV/SUMZ
                ENDIF
            TSUMI=TSUMI + sumv
            ZOL = DS(L)
            SUMZ = 0.0d0
          SUMV = 0.0d0
c            SUMZ = DLAYR (K) - DS(L)
c            SUMV = SUMZ*VI(K)
c           K = K + 1
            GOTO 200
            ENDIF
C
            IF (DS(L).EQ.DLAYR(K)) THEN
                  VS(L) = VI(K)
                IF (SUMZ .GT. 0.0) THEN
                   VS(L) = SUMV/SUMZ
                ENDIF
            TSUMI=TSUMI + sumv
            SUMZ = 0.0
            SUMV = 0.0
            ZOL = DS(L)
            ZIL = DLAYR (K)
            K = K+1
         if (k.gt.nlayri) then
           goto 450
         endif
            GOTO 200
            ENDIF
C
c100    CONTINUE
         if (k.eq.nlayri) then
           goto 450
         else
            ZIL = DLAYR (K)
            K = K+1
            goto 201
         endif
C
200    CONTINUE

C     Set last layer characteristics, and depth of profile
C
450      DO 400 J = 1, NLAYRO
         VI(J) = VS(J)
400   CONTINUE
      IF ((NLAYRO.GT.0).AND.(NLAYRO.LT.NLAYRI)) THEN
      DO 500 J = 1+NLAYRO, NLAYRI
         VI(J) = 0.0D0
500   CONTINUE
      ENDIF
c check total 'mass' in the soil profile initially for correct transfer
c between two grid systems
      TSUMO = VI(1) * DS(1)
      DO L=2,NLAYRO
      TSUMO=TSUMO + VI(L) * (DS(L)-DS(L-1))
      END DO
      if (tsumo.eq.0.0) return
C
      IF (ABS(TSUMI/TSUMO-1.0).GT.0.01) THEN
c      IF (ABS(TSUMI-TSUMO).GT.0.001) THEN
      STOP 'YOU HAVE A PROBLEM WITH TRANSFORMATION BETWEEN GRIDS
     &       in REALMATCH'
      ENDIF

      RETURN

      END

C========== DEVELOPED BY LIWANG MA === FOR Double PRECISIOIN ===
C ==============================================================
      SUBROUTINE DBLEMATCH (NLAYRI,DLAYR,VI,NLAYRO,DS)

      IMPLICIT double precision (A-H,O-Z)

      INTEGER  J,K,L,NLAYRI,NLAYRO,mxnod
      parameter (mxnod=300)

C-RZ      REAL     VI(20),DS(20),VS(20),DLAYR(20)
      DIMENSION    VI(mxnod),DS(nlayro),VS(mxnod),DLAYR(nlayri)
C      DOUBLE PRECISION    ZIL,ZOL,SUMZ,SUMV,ZT,ZB

      IF (NLAYRI .LT. 1) RETURN
      IF (NLAYRO .LT. 1) RETURN

C*----------------------------------------------------------------------
C     Definition of layers and soil depths moved to IPEXP
C*----------------------------------------------------------------------

C
C     This subroutine assumes that DLAYR(L) values are depths to the
C     bottom of layer L
C
C-RZ      DO L = 1, 20
      DO L = 1, mxnod
         VS(L) = 0.0d0
      END DO
c check total 'mass' in the soil profile initially for correct transfer
c between two grid systems
      TSUMI = 0.0d0
c      DO L=2,NLAYRI
c     TSUMI=TSUMI + VI(L) * (DLAYR(L)-DLAYR(L-1))
c     END DO
C

      K   = 1
      ZIL = 0.0d0
      ZOL = 0.0d0
         SUMZ = 0.0d0
         SUMV = 0.0d0

      DO 200 L = 1, NLAYRO
C
201         ZT   = MAX (ZOL,ZIL)
            ZB   = MIN (DS(L),DLAYR(K))
            SUMZ = SUMZ + (ZB - ZT)
            SUMV = SUMV + VI(K)*(ZB - ZT)
            IF (DS(L) .LT. DLAYR(K)) THEN
                   VS(L) = VI(K)
                IF (SUMZ .GT. 0.0d0) THEN
                   VS(L) = SUMV/SUMZ
                ENDIF
            TSUMI=TSUMI + sumv
            ZOL = DS(L)
            SUMZ = 0.0d0
          SUMV = 0.0d0
c            SUMZ = DLAYR (K) - DS(L)
c            SUMV = SUMZ*VI(K)
c           K = K + 1
            GOTO 200
            ENDIF
C
            IF (DS(L).EQ.DLAYR(K)) THEN
                  VS(L) = VI(K)
                IF (SUMZ .GT. 0.0d0) THEN
                   VS(L) = SUMV/SUMZ
                ENDIF
            TSUMI=TSUMI + sumv
            SUMZ = 0.0d0
            SUMV = 0.0d0
            ZOL = DS(L)
            ZIL = DLAYR (K)
            K = K+1
            if (k.gt.nlayri) then
               goto 450
            endif
            GOTO 200
            ENDIF
C
c100    CONTINUE
         if (k.eq.nlayri) then
           goto 450
         else
            ZIL = DLAYR (K)
            K = K+1
            goto 201
         endif
C
200    CONTINUE

C     Set last layer characteristics, and depth of profile
C
450      DO 400 J = 1, NLAYRO
         VI(J) = VS(J)
400   CONTINUE
      IF ((NLAYRO.GT.0).AND.(NLAYRO.LT.NLAYRI)) THEN
      DO 500 J = 1+NLAYRO, NLAYRI
         VI(J) = 0.0D0
500   CONTINUE
      ENDIF
c check total 'mass' in the soil profile initially for correct transfer
c between two grid systems
      TSUMO = VI(1) * DS(1)
      DO L=2,NLAYRO
      TSUMO=TSUMO + VI(L) * (DS(L)-DS(L-1))
      END DO
      if (tsumo.eq.0.0d0) return
C
      IF (ABS(TSUMI/TSUMO-1.0).GT.0.01d0) THEN
      STOP 'YOU HAVE A PROBLEM WITH TRANSFORMATION BETWEEN GRIDS
     &       in DBLEMATCH'
      ENDIF

      RETURN

      END
