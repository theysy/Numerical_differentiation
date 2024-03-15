C-----------------------------------------------------------------------
      SUBROUTINE FDM(IOPT,X,NDIM,DFDX,DDFDDX)
C     THIS SUBROUTINE RETURNS THE 1ST & 2ND-ORDER DERIVATIVES OF
C     A RANDOM OBJECTIVE FUNCTION USING FINITE DIFFERENCE METHOD.
      USE GLOBAL; USE LIBRARY_REAL8
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION X(NDIM), DFDX(NDIM), DDFDDX(NDIM,NDIM)
      DIMENSION ARG(3,NDIM), ANS(3)
C-----------------------------------------------------------------------
      DFDX=0.D0; DDFDDX=0.D0
C     # DEFINE THE STEP SIZE
      H=1.D-6
C     [1] 1ST ORDER DERIVATIVE
      DO I=1,NDIM
            ARG(1,:)=X; ARG(1,I)=X(I)+H
            CALL OBJ_FUNC(IOPT, ARG(1,:), NDIM, ANS(1))
            ARG(2,:)=X; ARG(2,I)=X(I)-H
            CALL OBJ_FUNC(IOPT, ARG(2,:), NDIM, ANS(2))
            DFDX(I)=(ANS(1)-ANS(2))/(2.D0*H)
      END DO
C-----------------------------------------------------------------------
C     [2] 2ND ORDER DERIVATIVE
C     [2.1] DIAGONAL COMPONENTS
      H=1.D-3
      CALL OBJ_FUNC(IOPT, X, NDIM, ANS(1))
      DO I=1,NDIM
            ARG(2,:)=X; ARG(2,I)=X(I)+H
            CALL OBJ_FUNC(IOPT, ARG(2,:), NDIM, ANS(2))
            ARG(3,:)=X; ARG(3,I)=X(I)-H
            CALL OBJ_FUNC(IOPT, ARG(3,:), NDIM, ANS(3))
            DDFDDX(I,I)=(ANS(2)+ANS(3)-2.D0*ANS(1))/(H*H)
      END DO
C     [2.1] OFF-DIAGONAL COMPONENTS
      DO I=1,NDIM
      DO J=I+1,NDIM
            ARG(2,:)=X; ARG(2,I)=X(I)+H; ARG(2,J)=X(J)+H
            CALL OBJ_FUNC(IOPT, ARG(2,:), NDIM, ANS(2))
            ARG(3,:)=X; ARG(3,I)=X(I)-H; ARG(3,J)=X(J)-H
            CALL OBJ_FUNC(IOPT, ARG(3,:), NDIM, ANS(3))
            DDFDDX(I,J)=(ANS(2)+ANS(3)-2.D0*ANS(1))/(2.D0*H*H)
            DDFDDX(I,J)=DDFDDX(I,J)-0.5D0*(DDFDDX(I,I)+DDFDDX(J,J))
            DDFDDX(J,I)=DDFDDX(I,J)
      END DO
      END DO
C-----------------------------------------------------------------------

      RETURN
      END SUBROUTINE FDM
C-----------------------------------------------------------------------
      SUBROUTINE GCSDA(IOPT,X,NDIM,DFDX,DDFDDX)
C     THIS SUBROUTINE RETURNS THE 1ST & 2ND-ORDER DERIVATIVES OF
C     A RANDOM OBJECTIVE FUNCTION USING GENERALIZED COMPLEX NUMBER.
      USE GLOBAL; USE LIBRARY_CMPLX;
      IMPLICIT REAL*8 (A-H,O-Z)
      DOUBLE COMPLEX :: ARG(3,NDIM), ANS(3), HI
      DIMENSION X(NDIM), DFDX(NDIM), DDFDDX(NDIM,NDIM)
C-----------------------------------------------------------------------
C     [0] STEP SIZE
      H=1.D-6;
C     [1] IMAGINARY NUMBER ANGLE
      TH=60.D0
      HI=H*CMPLX(DCOS(TH*RAD),DSIN(TH*RAD))
C     [2] FIRST-ORDER DERIVATIVE
      DO I=1,NDIM
            ARG(1,:)=X; ARG(1,I)=X(I)+HI
            CALL OBJ_FUNC(IOPT, ARG(1,:), NDIM, ANS(1))
            ARG(2,:)=X; ARG(2,I)=X(I)-HI
            CALL OBJ_FUNC(IOPT, ARG(2,:), NDIM, ANS(2))
            DFDX(I)=AIMAG((ANS(1)-ANS(2)))/AIMAG(2.D0*HI)
      END DO
C-----------------------------------------------------------------------
C     [3] SECOND-ORDER DERIVATIVE [LAI ET AL., 2008]
C     [3.1] DIAGONAL COMPONENTS
      H=1.D-3; ARG(1,:)=X
      CALL OBJ_FUNC(IOPT, ARG(1,:), NDIM, ANS(1))
      DO I=1, NDIM
            ARG(2,:)=X; ARG(2,I)=X(I)+HI
            CALL OBJ_FUNC(IOPT, ARG(2,:), NDIM, ANS(2))
            ARG(3,:)=X; ARG(3,I)=X(I)-HI
            CALL OBJ_FUNC(IOPT, ARG(3,:), NDIM, ANS(3))
            DDFDDX(I,I)=(AIMAG(ANS(2)+ANS(3)-2.D0*ANS(1)))/AIMAG(HI*HI)
      END DO
C     [3.2] OFF-DIAGONAL COMPONENTS
      DO I=1, NDIM
      DO J=I+1, NDIM
            ARG(2,:)=X; ARG(2,I)=X(I)+HI; ARG(2,J)=X(J)+HI
            CALL OBJ_FUNC(IOPT, ARG(2,:), NDIM, ANS(2))
            ARG(3,:)=X; ARG(3,I)=X(I)-HI; ARG(3,J)=X(J)-HI
            CALL OBJ_FUNC(IOPT, ARG(3,:), NDIM, ANS(3))
C-----------------------------------------------------------------------
            DDFDDX(I,J)=AIMAG(ANS(2)+ANS(3)-2.D0*ANS(1))/
     &      AIMAG(2.D0*HI*HI)
            DDFDDX(I,J)=DDFDDX(I,J)-0.5D0*(DDFDDX(I,I)+DDFDDX(J,J))
            DDFDDX(J,I)=DDFDDX(I,J)
      END DO
      END DO

      RETURN
      END SUBROUTINE GCSDA
C-----------------------------------------------------------------------
      SUBROUTINE HDNM(IOPT,X,NDIM,DFDX,DDFDDX)
C     THIS SUBROUTINE RETURNS THE 1ST & 2ND-ORDER DERIVATIVES OF
C     A RANDOM OBJECTIVE FUNCTION USING HYPER-DUAL NUMBER METHOD.
      USE HYPER_DUAL; USE GLOBAL; USE LIBRARY_HDUAL
      IMPLICIT REAL*8 (A-H,O-Z)
      TYPE(HDUAL) :: ARG(3,NDIM), ANS(3)
      DIMENSION HM(3,3)
      DIMENSION X(NDIM), DFDX(NDIM), DDFDDX(NDIM,NDIM)
C-----------------------------------------------------------------------
C     [0] STEP SIZE
      H=1.D-20;
C
      HM=0.D0
      HM(1,1)=H; HM(2,2)=H; HM(3,1:2)=H;
C     [1] 1ST-ORDER DERIVATIVE
      DO I=1, NDIM
            ARG(1,:)=X
            ARG(1,I)=HDUAL(X(I),HM(1,:))
            CALL OBJ_FUNC(IOPT, ARG(1,:), NDIM, ANS(1))
            DFDX(I)=ANS(1)%DX(1)/ARG(1,I)%DX(1)
      END DO
C     [3] 2ND-ORDER DERIVATIVE
C     [3.1] DIAGONAL COMPONENTS
      CALL OBJ_FUNC(IOPT, ARG(1,:), NDIM, ANS(1))
      DO I=1, NDIM
            ARG(1,:)=X; ARG(1,I)=HDUAL(X(I),HM(3,:))
            CALL OBJ_FUNC(IOPT, ARG(1,:), NDIM, ANS(1))
            ARG(2,:)=X; ARG(2,I)=HDUAL(X(I),-HM(3,:))
            CALL OBJ_FUNC(IOPT, ARG(2,:), NDIM, ANS(2))
            DDFDDX(I,I)=(ANS(1)%DX(3)+ANS(2)%DX(3))/(2.D0*H*H)
      END DO
C     [3.2] OFF-DIAGONAL COMPONENTS
      DO I=1, NDIM
      DO J=I+1, NDIM
            ARG(1,:)=X;
            ARG(1,I)=HDUAL(X(I),HM(3,:))
            ARG(1,J)=HDUAL(X(J),HM(3,:))
            CALL OBJ_FUNC(IOPT, ARG(1,:), NDIM, ANS(1))
            ARG(2,:)=X;
            ARG(2,I)=HDUAL(X(I),-HM(3,:))
            ARG(2,J)=HDUAL(X(J),-HM(3,:))
            CALL OBJ_FUNC(IOPT, ARG(2,:), NDIM, ANS(2))
            DDFDDX(I,J)=(ANS(1)%DX(3)+ANS(2)%DX(3))/(4.D0*H*H)
            DDFDDX(I,J)=DDFDDX(I,J)-(0.5D0)*(DDFDDX(I,I)+DDFDDX(J,J))
            DDFDDX(J,I)=DDFDDX(I,J)
      END DO
      END DO

      RETURN
      END SUBROUTINE HDNM
C-----------------------------------------------------------------------