C-----------------------------------------------------------------------
      MODULE GLOBAL
      PUBLIC
      INTEGER :: NDIM1, NDIM2, NDIM3, NDIM4, NDIM5, NDIM6, NDIM7
      REAL*8 :: HARD_PAR, YLD_PAR, FLOW_PAR, SUA_PAR, GRAD_PAR
      REAL*8, PARAMETER :: RAD=DATAN(1.D0)/45.D0
      SAVE
      END MODULE GLOBAL
C-----------------------------------------------------------------------
      MODULE LIBRARY_HDUAL
      USE HYPER_DUAL
      IMPLICIT TYPE(HDUAL) (A-H,O-W)
      IMPLICIT REAL*8 (X,Y,Z)
C
      CONTAINS
      INCLUDE 'LIBRARY.FOR'
      END MODULE LIBRARY_HDUAL
C-----------------------------------------------------------------------
      MODULE LIBRARY_CMPLX
      IMPLICIT DOUBLE COMPLEX (A-H,O-Z)
C
      CONTAINS
      INCLUDE 'LIBRARY.FOR'
      END MODULE LIBRARY_CMPLX
C-----------------------------------------------------------------------
      MODULE LIBRARY_REAL8
      IMPLICIT REAL*8 (A-H,O-Z)
C
      CONTAINS
      INCLUDE 'LIBRARY.FOR'
      END MODULE LIBRARY_REAL8
C-----------------------------------------------------------------------