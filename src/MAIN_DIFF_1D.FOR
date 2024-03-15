C-----------------------------------------------------------------------
C     DEVELOPER: SEONGYONG YOON
C     CONTACT: s.yoon@mpie.de
C     AFFILIATION: Max-Planck-Institut für Eisenforschung GmbH
C-----------------------------------------------------------------------
C     THIS CODE RETURNS THE DIFFERENTIATION VALUES OF
C     THE VON MISES YIELD FUNCTION
C-----------------------------------------------------------------------
      INCLUDE 'LIBRARY\CODELIST.FOR'
      USE GLOBAL; USE LIBRARY_REAL8
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8, DIMENSION(:), ALLOCATABLE :: DFDS
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: DDFDDS
C     [0] DIMENSION
      NDIM=1
      ALLOCATE(DFDS(NDIM), DDFDDS(NDIM,NDIM))
C
      GRAD_PAR=3.D0
      X=1; NDIM=1;
      CALL DIFF(1,[X],NDIM,DFDS,DDFDDS)
C
      PRINT *, DFDS
      PRINT *, '-----------------------------------'
      PRINT *, DDFDDS

      END
C-----------------------------------------------------------------------
