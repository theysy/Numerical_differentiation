C-----------------------------------------------------------------------
C     DEVELOPER: SEONGYONG YOON
C     CONTACT: s.yoon@mpie.de
C     AFFILIATION: Max-Planck-Institut für Eisenforschung GmbH
C-----------------------------------------------------------------------
C     THIS CODE RETURNS THE DIFFERENTIATION VALUES OF
C     THE VON MISES YIELD FUNCTION
C-----------------------------------------------------------------------
      INCLUDE 'LIBRARY\CODELIST.FOR'
      USE GLOBAL; USE LIBRARY_REAL8;
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION SIG(3)
      REAL*8, DIMENSION(:), ALLOCATABLE :: DFDS
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: DDFDDS
C-----------------------------------------------------------------------
C     [0] DIMENSION
      GRAD_PAR=3.D0
      NDIM3=SIZE(SIG);
      IF(NDIM3.EQ.3) THEN
            NDIM1=2; NDIM2=1
            NDIM4=NDIM3+1
      ELSEIF(NDIM3.EQ.6) THEN
            NDIM1=3; NDIM2=3
            NDIM4=NDIM3
      END IF
C
      ALLOCATE(DFDS(NDIM3), DDFDDS(NDIM3,NDIM3))
C     [1] DEFINE STRESS TENSOR
C
      SIG=0.D0
      SIG(1)=1.D0
      CALL DIFF(3,SIG,NDIM3,DFDS,DDFDDS)
C     [2] RESULTS
      PRINT *, DFDS
      PRINT *, '-----------------------------------'
      DO I=1, NDIM3
            PRINT *, DDFDDS(I,:)
      END DO

      DEALLOCATE(DFDS, DDFDDS)
      END
C-----------------------------------------------------------------------
