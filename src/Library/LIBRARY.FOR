C-----------------------------------------------------------------------
      SUBROUTINE DIFF(IOPT,X,NDIM,DFDX,DDFDDX)
      USE GLOBAL
      DIMENSION X(NDIM), DFDX(NDIM), DDFDDX(NDIM,NDIM)
C-----------------------------------------------------------------------
      DFDX=0.D0; DDFDDX=0.D0
      IF(GRAD_PAR.EQ.1.D0) THEN
            CALL FDM(IOPT,X,NDIM,DFDX,DDFDDX)
      ELSEIF(GRAD_PAR.EQ.2.D0) THEN
            CALL GCSDA(IOPT,X,NDIM,DFDX,DDFDDX)
      ELSEIF(GRAD_PAR.EQ.3.D0) THEN
            CALL HDNM(IOPT,X,NDIM,DFDX,DDFDDX)
      END IF

      RETURN
      END SUBROUTINE DIFF
C-----------------------------------------------------------------------
      SUBROUTINE OBJ_FUNC(IOPT, ARG, NDIM, ANS)
      USE GLOBAL; DIMENSION ARG(NDIM)

      IF(IOPT.EQ.1) THEN
            CALL X3(ARG(1), ANS)
      ELSEIF(IOPT.EQ.2) THEN
            CALL SIN_COS(ARG(1), ANS)
      ELSEIF(IOPT.EQ.3) THEN
            CALL VON_MISES(ARG, ANS)
      END IF

      RETURN
      END SUBROUTINE OBJ_FUNC
C-----------------------------------------------------------------------
      SUBROUTINE X3(ARG, ANS)

      ANS=ARG**3.D0

      RETURN
      END SUBROUTINE X3
C-----------------------------------------------------------------------
      SUBROUTINE SIN_COS(ARG, ANS)
      USE GLOBAL

      ARG=ARG*RAD
      ANS=SIN(ARG)+COS(ARG)

      RETURN
      END SUBROUTINE SIN_COS
C-----------------------------------------------------------------------
      SUBROUTINE VON_MISES(SIG, PHI)
      USE GLOBAL
      DIMENSION SIG(NDIM3), DEV_SIG(NDIM4)


      CALL DEVIATORIC(SIG, DEV_SIG)
      CALL DOUBLE_DOT(DEV_SIG, DEV_SIG, NDIM4, PSI)
      PHI= SQRT((3.D0/2.D0)*PSI)

      RETURN
      END SUBROUTINE VON_MISES
C-----------------------------------------------------------------------
      SUBROUTINE DOUBLE_DOT(A,B,NDIM,DOUBLE)
C     #   A:B = A_IJ*B_IJ = TR(A^T*B)
C         A:B = A11*B11 + A12*B12 + A13*B13 + A21*B21 + A22*B22 + A23*B23 + A31*B31 + A32*B32 + A33*B33
      USE GLOBAL
      DIMENSION A(NDIM), B(NDIM)

      DOUBLE = 0.D0
      NDI= NDIM-NDIM2
      DO I=1,NDI
          DOUBLE = DOUBLE + A(I)*B(I)
      END DO
      DO I=NDI+1, NDIM
          DOUBLE= DOUBLE + 2.D0*A(I)*B(I)
      END DO

      RETURN
      END SUBROUTINE DOUBLE_DOT
C-----------------------------------------------------------------------
      SUBROUTINE DEVIATORIC(SIG, DEV_SIG)
C     #   THIS SUBROUTINE CALCULATE DEVIATORIC STRESS.
      USE GLOBAL
      DIMENSION SIG(NDIM3), DEV_SIG(NDIM4), DELTA(NDIM4)

      DEV_SIG=0.D0
      NDI=NDIM4-NDIM2
C     INITIAL DEVIATORIC STRESS TENSOR
      IF(NDIM3 .EQ. 3) THEN ! PLANE STRESS
          DEV_SIG(1)= SIG(1)
          DEV_SIG(2)= SIG(2)
          DEV_SIG(3)= 0.D0
          DEV_SIG(4)= SIG(3)
      ELSE
          DEV_SIG= SIG
      END IF

      SIG_KK=0.D0
      DELTA=0.D0
      DO I=1, NDI
          SIG_KK=SIG_KK+DEV_SIG(I)/3.D0
          DELTA(I)=1.D0
      END DO
      DEV_SIG=DEV_SIG-SIG_KK*DELTA

      RETURN
      END SUBROUTINE DEVIATORIC
C-----------------------------------------------------------------------
      SUBROUTINE DEVIATORIC_TRANS(TENS, TENS0, IOPT)
C     #   IOPT=1: THIS SUBROUTINE REMOVES COMPONENT SIG_33.
C         -> RETURN: TENS0
C     #   IOPT=2: THE RECOVERY TO THE ORIGINAL DIMENSION OF THE DEVIATORIC TENSOR.
c         -> RETURN: TENS
C     #   NOTE: THIS SUBROUTINE IS NOT NECESSARY FOR THE 3D STRESS STATE.
      USE GLOBAL
      DIMENSION TENS(NDIM4), TENS0(NDIM3)

      IF(NDIM3 .EQ. 3) THEN
          IF(IOPT .EQ. 1) THEN
C             A. DIAGONAL COMPONENTS
              DO I=1, NDIM1
                  TENS(I)=TENS0(I)
              END DO
C             B. SHEAR COMPONENTS
              TENS(3)=-(TENS0(1)+TENS0(2))
              TENS(4)= TENS0(3)
          ELSEIF(IOPT .EQ. 2) THEN
C             A. DIAGONAL COMPONENTS
              TENS0(1)= TENS(1)
              TENS0(2)= TENS(2)
C             B. SHEAR COMPONENTS
              TENS0(3)= TENS(4)
          END IF
      ELSEIF(NDIM3 .EQ. 6) THEN
          IF(IOPT .EQ. 1) THEN
              TENS=TENS0
          ELSEIF(IOPT .EQ. 2) THEN
              TENS0=TENS
          END IF
      END IF

      RETURN
      END SUBROUTINE DEVIATORIC_TRANS
C-----------------------------------------------------------------------
