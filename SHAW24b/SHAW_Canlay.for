C
C
      SUBROUTINE CANLAY(NPLANT,NC,NS,PLANTZ,PLANTW,ZC,WCANDT,TCDT,TLCDT,
     +  VAPCDT,ITYPE,DCHAR,DRYCAN,CANLAI,TOTLAI,RLEAF0,RLEAF,TOTROT,
     +  ROOTDN,RROOT0,RROOT)
C
C      This subroutine splits the canopy into layers.  Each layer
C      will have the same total leaf area index, but the actual layer
C      dimension will vary.  The different plant variables are
C      dimensioned by plant type and layer, and their values are
C      apportioned according to their representation in the layer.
C
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXNOD=300, nodcan=11, MXSPEC=10)
C
      DOUBLE PRECISION PLANTZ(MXSPEC),PLANTW(MXSPEC),ZC(NODCAN),
     +  WCANDT(NODCAN),TCDT(NODCAN),TLCDT(MXSPEC,NODCAN),VAPCDT(NODCAN),
     +  DCHAR(MXSPEC,NODCAN),DRYCAN(MXSPEC,NODCAN),
     +  CANLAI(MXSPEC,NODCAN),TOTLAI(MXSPEC),RLEAF0(MXSPEC),
     +  RLEAF(MXSPEC,NODCAN),TOTROT(MXSPEC),ROOTDN(MXSPEC,MXNOD),
     +  RROOT0(MXSPEC),RROOT(MXSPEC,MXNOD)
C
C     >>> LOCAL VARIABLES
      DOUBLE PRECISION  LAISUM,LAILYR,LYRTOP,LYRTOT,DZMAX,TMP(NODCAN),
     +         A,DZ(NODCAN),TMPSUM,FACTOR,TMPLAI
C
      INTEGER  NC,NPLANT,ITYPE(MXSPEC),HTINDX,HTORDR(MXSPEC),I,J,LAYER,
     +  OLDNC
C
C     -- Local variables:
C     A         Leaf area index per layer.
C     DZMAX     Maximum theoretical layer thickness for current values.
C     HTINDX    Index for ordered plant height - in increasing order.
C     HTORDR    Array of plant numbers ordered by height (1 = smallest)
C     LAILYR    Current accumulated LAI for current layer.
C     LAISUM    The delta LAI/unit distance for current calculations.
C     LAYER     Current layer number, used for loop control.
C     LYRTOP    Height of top of current layer.
C     LYRTOT    Current accumulated layer thickness for current layer.
C     TMPSUM    Sum of values for WCANDT, for apportioning to layers.
C     TMPLAI    Sum of total leaf area index for all plants.
C
C**   --Initialization
      LAILYR = 0.0D0
      LYRTOT = 0.0D0
      LAISUM = 0.0D0
      TMPLAI = 0.0D0
      TMPSUM = 0.0D0
      HTINDX = 1
      LYRTOP = 0.0D0
      OLDNC = NC

C     -- Initialize temporary variable used for sorting,
C        sum of total leaf area index for all plants,
C        and sum of delta LAI per unit distance.
      DO 10 I=1, NPLANT
         IF (PLANTZ(I)*totlai(I) .NE. 0.0) THEN   !pltlai is not in RZ-SHAW
         TMP(I) = PLANTZ(I)
         TMPLAI = TMPLAI + TOTLAI(I)
         LAISUM = LAISUM + TOTLAI(I) / PLANTZ(I)
            ELSE
            TMP(I) = 9999.
            NZEROS=NZEROS+1
            HTORDR(NZEROS)=I
            TOTLAI(I) = 0.0
         END IF
  10  CONTINUE

C**   Arrange the plants by height - in increasing order
      DO 30 HTINDX = 1, NPLANT
         HTORDR(HTINDX) = 1
         DO 20 I = 1, NPLANT
            IF (TMP(HTORDR(HTINDX)).GT.TMP(I)) HTORDR(HTINDX) = I
   20    CONTINUE
         TMP(HTORDR(HTINDX)) = 9999.0D0
C        -- Initialize DCHAR
         DO 30 J = 1, NC
            DCHAR(HTINDX,J) = DCHAR(HTINDX,1)
   30 CONTINUE


C     -- Sum of values for WCANDT, used for apportioning to layers.
      DO 50 I = 1, OLDNC
         TMPSUM = TMPSUM + WCANDT(I)
   50 CONTINUE

      HTINDX = 1
cxxxx NC = max(1,NINT (TMPLAI / 0.75D0))
      NC = max(1,NINT (TMPLAI / 0.50D0))
C     -- Set upper and lower limit for number of canopy layers.
         IF (NC.GT.10) NC = 10

	ZC(NC+1) = PLANTZ(HTORDR(NPLANT))

C
      A  = TMPLAI / NC
      LAYER  = NC
C**   --Main loop   ***

C     --Calculate theoretical maximum layer based on current LAISUM.
  100 IF (LAYER .NE. 1) THEN
         DZMAX = (A - LAILYR) / LAISUM
        ELSE
         DZMAX = PLANTZ(HTORDR(NPLANT)) - LYRTOP
      END IF
C
      IF(LYRTOP+DZMAX.LE.PLANTZ(HTORDR(HTINDX)).OR.LAYER.EQ.1)THEN
C**   -- Top of layer is below or equal to next tallest plant.

      LYRTOP = LYRTOP + DZMAX
      DZ(LAYER) = LYRTOT + DZMAX

C**      --Apportioning routine start.
      DO 110 J = 1, NPLANT
        IF (PLANTZ(J).GE.LYRTOP) THEN
C               -- Plant fully contained in layer, full apportioning.
          FACTOR = DZ(LAYER) / PLANTZ(J)
        ELSE IF (PLANTZ(J).GT.LYRTOP-DZ(LAYER)) THEN
C          -- Plant partially contained in layer, partial apportioning.
          FACTOR = (PLANTZ(J)-(LYRTOP-DZ(LAYER))) / PLANTZ(J)
        ELSE
C          -- Plant is not contained in layer, set values to zero.
          FACTOR = 0.0D0
        END IF

        CANLAI(J,LAYER) = TOTLAI(J) * FACTOR
        DRYCAN(J,LAYER) = PLANTW(J) * FACTOR
        IF (ITYPE(J).NE.0) THEN
C          -- If plant is not dead.
              IF (FACTOR*TOTLAI(J).NE.0) THEN
C             -- Plant is within layer.
              RLEAF(J,LAYER) = RLEAF0(J)*(CANLAI(J,LAYER)/TOTLAI(J))
            ELSE
C             -- Plant is outside of layer.
              RLEAF(J,LAYER) = 0.0D0
           END IF
        ELSE
C          -- Plant is dead.
           RLEAF (J,LAYER) = 0.0D0
        END IF

C**        -- End apportioning routine.
  110    CONTINUE


C          --Update variables if more layers were added.
        IF (OLDNC.LT.LAYER) THEN
          TCDT(LAYER) = TCDT(OLDNC)
          VAPCDT(LAYER) = VAPCDT(OLDNC)
          WCANDT(LAYER) = WCANDT(OLDNC)
          DO 120 J = 1, NPLANT
             DCHAR(J,LAYER) = DCHAR(J,1)
             TLCDT(J,LAYER)=TLCDT(J,OLDNC)
  120     CONTINUE
        END IF

C          --Calc midpoint of the layer
        IF (LAYER.EQ.1) THEN
            ZC(1) = 0.0D0
        ELSE
            ZC(LAYER)=PLANTZ(HTORDR(NPLANT))-(LYRTOP-DZ(LAYER)/2.D0)
        END IF

C          Check if done.  If so, drop out of loop
        IF (LAYER.EQ.1) THEN
          IF (ABS(PLANTZ(HTORDR(NPLANT))-LYRTOP).GT.0.0001D0) THEN
               WRITE (6,*)'ERROR MATCHING TOP OF CANOPY'
               WRITE (6,*)'  LYRTOP =  ', LYRTOP
          END IF
          GO TO 190
        END IF

C**        -- If top of layer equaled top of the next tallest plant
        IF (LYRTOP .GE. PLANTZ(HTORDR(HTINDX))-.0001) THEN
          LAISUM = LAISUM -
     >             TOTLAI(HTORDR(HTINDX))/PLANTZ(HTORDR(HTINDX))
          HTINDX = HTINDX + 1
        END IF

        LYRTOT = 0.0D0
        LAILYR = 0.0D0
        LAYER = LAYER - 1

      ELSE
C**   -- Top of layer extends beyond top of plant being considered.
C          -- Update calcs based on limitation of top of next plant.
        LYRTOT = LYRTOT + PLANTZ(HTORDR(HTINDX))-LYRTOP
        LAILYR = LAILYR + (A-LAILYR) *
     >              ((PLANTZ(HTORDR(HTINDX))-LYRTOP)/DZMAX)
        LAISUM = LAISUM -
     >             TOTLAI(HTORDR(HTINDX))/PLANTZ(HTORDR(HTINDX))
        LYRTOP = PLANTZ(HTORDR(HTINDX))
        HTINDX = HTINDX + 1
      END IF
      GO TO 100
C     -- End of loop

  190 CONTINUE
C
      DO 230 J=1,NPLANT
         IF (ITYPE(J) .NE. 0) THEN
C           TRANSPIRING PLANT -- DEFINE ROOT RESISTANCE FOR EACH SOIL LAYER
            TOTROT(J)=0.0D0
            DO 210 I=1,NS
               TOTROT(J)=TOTROT(J) + ROOTDN(J,I)
  210       CONTINUE
            DO 220 I=1,NS
               RROOT(J,I)=RROOT0(J)*ROOTDN(J,I)/TOTROT(J)
  220       CONTINUE
         END IF
  230 CONTINUE
C
      RETURN
      END
