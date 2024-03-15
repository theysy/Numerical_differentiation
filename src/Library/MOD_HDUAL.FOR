C-----------------------------------------------------------------------
C     DEVELOPER: SEONGYONG YOON
C     CONTACT: s.yoon@mpie.de
C     AFFILIATION: Max-Planck-Institut für Eisenforschung GmbH
C-----------------------------------------------------------------------
C     THIS MODULE CONTAINS A COSTOMIZED NUMERIC TYPE (HYPER-DUAL NUMBER)
C     AND ASSOCIATED BASIC OPERATIONS.
C-----------------------------------------------------------------------
      MODULE HYPER_DUAL

      IMPLICIT REAL*8 (A-H,O-Z)

      PRIVATE
      REAL*8 :: NEGATIVE_ONE = -1.D0
      INTEGER, PUBLIC :: NDV=3
      TYPE,PUBLIC:: HDUAL  ! MAKE THIS PRIVATE WILL CREATE DIFFICULTY TO USE THE
      ! ORIGINAL WRITE/READ COMMANDS, HENCE X AND DX ARE
      ! VARIABLES WHICH CAN BE ACCESSED USING D%X AND D%DX IN
      ! OTHER UNITS USING THIS MODULE IN WHICH D IS DEFINED
      ! AS TYPE(HDUAL).
            SEQUENCE
            REAL*8 :: X         ! REAL PART
            REAL*8 :: DX(3)     ! IMAGINARY PART: E1 / E2 / E3=E1*E2
      END TYPE HDUAL
C-----------------------------------------------------------------------
!******** INTERFACES FOR OPERATOR OVERLOADING
      PUBLIC ASSIGNMENT (=)
      INTERFACE ASSIGNMENT (=)
      MODULE PROCEDURE ASSIGN_DD
      MODULE PROCEDURE ASSIGN_DI  ! DUAL=INTEGER, ELEMENTAL
      MODULE PROCEDURE ASSIGN_DR  ! DUAL=REAL*8, ELEMENTAL
      MODULE PROCEDURE ASSIGN_ID  ! INTEGER=DUAL, ELEMENTAL
      END INTERFACE


      PUBLIC OPERATOR (+)
      INTERFACE OPERATOR (+)
      MODULE PROCEDURE ADD_D   ! +DUAL NUMBER, ELEMENTAL
      MODULE PROCEDURE ADD_DD  ! DUAL + DUAL, ELEMENTAL
      MODULE PROCEDURE ADD_DI  ! DUAL + INTEGER, ELEMENTAL
      MODULE PROCEDURE ADD_DR  ! DUAL + REAL*8, ELEMENTAL
      MODULE PROCEDURE ADD_ID  ! INTEGER + DUAL, ELEMENTAL
      MODULE PROCEDURE ADD_RD  ! REAL + DUAL, ELEMENTAL
      END INTERFACE

      PUBLIC OPERATOR (-)
      INTERFACE OPERATOR (-)
      MODULE PROCEDURE MINUS_D   ! NEGATE A DUAL NUMBER,ELEMENTAL
      MODULE PROCEDURE MINUS_DD  ! DUAL -DUAL,ELEMENTAL
      MODULE PROCEDURE MINUS_DI  ! DUAL-INTEGER,ELEMENTAL
      MODULE PROCEDURE MINUS_DR  ! DUAL-REAL*8,ELEMENTAL
      MODULE PROCEDURE MINUS_ID  ! INTEGER-DUAL,ELEMENTAL
      MODULE PROCEDURE MINUS_RD  ! REAL-DUAL,ELEMENTAL
      END INTERFACE

      PUBLIC OPERATOR (*)
      INTERFACE OPERATOR (*)
      MODULE PROCEDURE MULT_DD    ! DUAL*DUAL, ELEMENTAL
      MODULE PROCEDURE MULT_DI    ! DUAL*INTEGER,ELEMENTAL
      MODULE PROCEDURE MULT_DR    ! DUAL*REAL*8,ELEMENTAL
      MODULE PROCEDURE MULT_ID    ! INTEGER*DUAL,ELEMENTAL
      MODULE PROCEDURE MULT_RD    ! REAL*DUAL,ELEMENTAL
      END INTERFACE

      PUBLIC OPERATOR (/)
      INTERFACE OPERATOR (/)
      MODULE PROCEDURE DIV_DD ! DUAL/DUAL,ELEMENTAL
      MODULE PROCEDURE DIV_DI ! DUAL/INTEGER, ELEMENTAL
      MODULE PROCEDURE DIV_DR ! DUAL/REAL*8,EMENTAL
      MODULE PROCEDURE DIV_ID ! INTEGER/DUAL, ELEMENTAL
      MODULE PROCEDURE DIV_RD ! REAL/DUAL, ELEMENTAL
      END INTERFACE

      PUBLIC OPERATOR (**)
      INTERFACE OPERATOR (**)
      MODULE PROCEDURE POW_I ! DUAL NUMBER TO AN INTEGER POWER,ELEMENTAL
      MODULE PROCEDURE POW_R ! DUAL NUMBER TO A REAL POWER, ELEMENTAL
      MODULE PROCEDURE POW_D ! DUAL NUMBER TO A DUAL POWER, ELEMENTAL
      END INTERFACE

      PUBLIC OPERATOR (==)
      INTERFACE OPERATOR (==)
      MODULE PROCEDURE EQ_DD ! COMPARE TWO DUAL NUMBERS, ELEMENTAL
      MODULE PROCEDURE EQ_DI ! COMPARE A DUAL AND AN INTEGER, ELEMENTAL
      MODULE PROCEDURE EQ_DR ! COMPARE A DUAL AND A REAL*8, ELEMENTAL
      MODULE PROCEDURE EQ_ID ! COMPARE INTEGER WITH A DUAL NUMBER, ELEMENTAL
      MODULE PROCEDURE EQ_RD ! COMPARE A REAL WITH A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC OPERATOR (<=)
      INTERFACE OPERATOR (<=)
      MODULE PROCEDURE LE_DD  ! COMPARE TWO DUAL NUMBERS, ELEMENTAL
      MODULE PROCEDURE LE_DI  ! COMPARE A DUAL AND AN INTEGER, ELEMENTAL
      MODULE PROCEDURE LE_DR  ! COMPARE A DUAL AND A REAL*8,ELEMENTAL
      MODULE PROCEDURE LE_ID ! COMPARE INTEGER WITH A DUAL NUMBER, ELEMENTAL
      MODULE PROCEDURE LE_RD ! COMPARE A REAL WITH A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC OPERATOR (<)
      INTERFACE OPERATOR (<)
      MODULE PROCEDURE LT_DD  !COMPARE TWO DUAL NUMBERS, ELEMENTAL
      MODULE PROCEDURE LT_DI  !COMPARE A DUAL AND AN INTEGER, ELEMENTAL
      MODULE PROCEDURE LT_DR  !COMPARE DUAL WITH A REAL*8, ELEMENTAL
      MODULE PROCEDURE LT_ID ! COMPARE INTEGER WITH A DUAL NUMBER, ELEMENTAL
      MODULE PROCEDURE LT_RD ! COMPARE A REAL WITH A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC OPERATOR (>=)
      INTERFACE OPERATOR (>=)
      MODULE PROCEDURE GE_DD ! COMPARE TWO DUAL NUMBERS, ELEMENTAL
      MODULE PROCEDURE GE_DI ! COMPARE DUAL WITH INTEGER, ELEMENTAL
      MODULE PROCEDURE GE_DR ! COMPARE DUAL WITH A REAL NUMBER, ELEMENTAL
      MODULE PROCEDURE GE_ID ! COMPARE INTEGER WITH A DUAL NUMBER, ELEMENTAL
      MODULE PROCEDURE GE_RD ! COMPARE A REAL WITH A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC OPERATOR (>)
      INTERFACE OPERATOR (>)
      MODULE PROCEDURE GT_DD  !COMPARE TWO DUAL NUMBERS, ELEMENTAL
      MODULE PROCEDURE GT_DI  !COMPARE A DUAL AND AN INTEGER, ELEMENTAL
      MODULE PROCEDURE GT_DR  !COMPARE DUAL WITH A REAL*8, ELEMENTAL
      MODULE PROCEDURE GT_ID ! COMPARE INTEGER WITH A DUAL NUMBER, ELEMENTAL
      MODULE PROCEDURE GT_RD ! COMPARE A REAL WITH A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC OPERATOR (/=)
      INTERFACE OPERATOR (/=)
      MODULE PROCEDURE NE_DD  !COMPARE TWO DUAL NUMBERS, ELEMENTAL
      MODULE PROCEDURE NE_DI  !COMPARE A DUAL AND AN INTEGER, ELEMENTAL
      MODULE PROCEDURE NE_DR  !COMPARE DUAL WITH A REAL*8, ELEMENTAL
      MODULE PROCEDURE NE_ID ! COMPARE INTEGER WITH A DUAL NUMBER, ELEMENTAL
      MODULE PROCEDURE NE_RD ! COMPARE A REAL WITH A DUAL NUMBER, ELEMENTAL
      END INTERFACE


C-----------------------------------------------------------------------
C INTERFACES FOR INTRINSIC FUNCTIONS OVERLOADING
      PUBLIC REAL
      INTERFACE REAL
      MODULE PROCEDURE REAL_D  ! REAL PART OF DUAL NUMBER
      END INTERFACE

      PUBLIC AIMAG
      INTERFACE AIMAG
      MODULE PROCEDURE AIMAG_D ! IMAGINARY PART OF DUAL NUMBER
      END INTERFACE

      PUBLIC ABS
      INTERFACE ABS
      MODULE PROCEDURE ABS_D  ! ABSOLUTE VALUE OF A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC DABS
      INTERFACE DABS
      MODULE PROCEDURE ABS_D ! SAME AS ABS, USED FOR SOME OLD FORTRAN COMMANDS
      END INTERFACE

      PUBLIC ACOS
      INTERFACE ACOS
      MODULE PROCEDURE ACOS_D ! ARCCOSINE OF A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC ASIN
      INTERFACE ASIN
      MODULE PROCEDURE ASIN_D ! ARCSINE OF A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC ATAN
      INTERFACE ATAN
      MODULE PROCEDURE ATAN_D ! ARCTAN OF A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC ATAN2
      INTERFACE ATAN2
      MODULE PROCEDURE ATAN2_D ! ARCTAN OF A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC COS
      INTERFACE COS
      MODULE PROCEDURE COS_D ! COSINE OF A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC DCOS
      INTERFACE DCOS
      MODULE PROCEDURE COS_D ! COSINE OF A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC DOT_PRODUCT
      INTERFACE DOT_PRODUCT
      MODULE PROCEDURE DOT_PRODUCT_DD ! DOT PRODUCT TWO DUAL NUMBER VECTORS
      END INTERFACE

      PUBLIC EXP
      INTERFACE EXP
      MODULE PROCEDURE EXP_D ! EXPONENTIAL OF A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC INT
      INTERFACE INT
      MODULE PROCEDURE INT_D ! INTEGER PART OF A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC LOG
      INTERFACE LOG
      MODULE PROCEDURE LOG_D ! LOG OF A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC LOG10
      INTERFACE LOG10
      MODULE PROCEDURE LOG10_D ! LOG OF A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC MATMUL
      INTERFACE MATMUL
      MODULE PROCEDURE MATMUL_DD ! MULTIPLY TWO DUAL MATRICES
      MODULE PROCEDURE MATMUL_DV ! MULTIPLY A DUAL MATRIX WITH A DUAL VECTOR
      MODULE PROCEDURE MATMUL_VD ! MULTIPLY A DUAL VECTOR WITH A DUAL MATRIX
      END INTERFACE

      PUBLIC MAX
      INTERFACE MAX
      MODULE PROCEDURE MAX_DD ! MAX OF FROM TWO TO FOUR DUAL NUMBERS, ELEMENTAL
      MODULE PROCEDURE MAX_DI ! MAX OF A DUAL NUMBER AND AN INTEGER, ELEMENTAL
      MODULE PROCEDURE MAX_DR ! MAX OF A DUAL NUMBER AND A REAL*8, ELEMENTAL
      MODULE PROCEDURE MAX_RD ! MAX OF A REAL*8,AND A DUAL NUMBER,  ELEMENTAL
      END INTERFACE

      PUBLIC DMAX1
      INTERFACE DMAX1
      MODULE PROCEDURE MAX_DD ! MAX OF FROM TWO TO FOUR DUAL NUMBERS, ELEMENTAL
      END INTERFACE

      PUBLIC MAXVAL
      INTERFACE MAXVAL
      MODULE PROCEDURE MAXVAL_D ! MAXVAL OF A DUAL NUMBER VECTOR
      END INTERFACE

      PUBLIC MIN
      INTERFACE MIN
      MODULE PROCEDURE MIN_DD ! MIN OF FROM TWO TO FOUR DUAL NUMBERS, ELEMENTAL
      MODULE PROCEDURE MIN_DR ! MIN OF A DUAL AND A REAL*8, ELEMENTAL
      END INTERFACE

      PUBLIC DMIN1
      INTERFACE DMIN1
      MODULE PROCEDURE MIN_DD ! MIN OF FROM TWO TO FOUR DUAL NUMBERS, ELEMENTAL
      END INTERFACE

      PUBLIC MINVAL
      INTERFACE MINVAL
      MODULE PROCEDURE MINVAL_D ! OBTAIN THE MAXVAL  OF A DUAL NUMBER VECTGOR
      END INTERFACE

      PUBLIC NINT
      INTERFACE NINT
      MODULE PROCEDURE NINT_D ! NEAREST INTEGER TO THE ARGUMENT, ELEMENTAL
      END INTERFACE

      PUBLIC SIGN
      INTERFACE  SIGN
      MODULE PROCEDURE  SIGN_DD ! SIGN(A,B) WITH TWO DUAL NUMBERS, ELEMENTAL
      MODULE PROCEDURE  SIGN_RD ! SIGN(A,B) WITH A REAL AND A DUAL, ELEMENTAL
      END INTERFACE

      PUBLIC SIN
      INTERFACE SIN
      MODULE PROCEDURE SIN_D ! OBTAIN SINE OF A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC DSIN
      INTERFACE DSIN
      MODULE PROCEDURE SIN_D ! OBTAIN SINE OF A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC TAN
      INTERFACE TAN
      MODULE PROCEDURE TAN_D ! OBTAIN SINE OF A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC DTAN
      INTERFACE DTAN
      MODULE PROCEDURE TAN_D ! OBTAIN SINE OF A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC SQRT
      INTERFACE SQRT
      MODULE PROCEDURE SQRT_D ! OBTAIN THE SQRT OF A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC DSQRT
      INTERFACE DSQRT
      MODULE PROCEDURE SQRT_D ! OBTAIN THE SQRT OF A DUAL NUMBER, ELEMENTAL
      END INTERFACE

      PUBLIC SUM
      INTERFACE SUM
      MODULE PROCEDURE SUM_D ! SUM A DUAL ARRAY
      END INTERFACE

      PUBLIC MAXLOC
      INTERFACE MAXLOC
      MODULE PROCEDURE MAXLOC_D ! LOCATION OF MAX IN A DUAL ARRAY
      END INTERFACE
C-----------------------------------------------------------------------
      CONTAINS
      !*********BEGIN: FUNCTIONS/SUBROUTINES FOR OVERLOADING OPERATORS

      !******* BEGIN: (=)
      !---------------------
      !-----------------------------------------
      ! DUAL = INTEGER
      ! <U, DU> = <I, 0>
      !-----------------------------------------
      ELEMENTAL SUBROUTINE ASSIGN_DI(U, I)
      TYPE(HDUAL), INTENT(OUT) :: U
      INTEGER, INTENT(IN) :: I

      U%X = REAL(I)  ! THIS IS FASTER THAN DIRECT ASSIGNMENT
      U%DX = 0.0

      END SUBROUTINE ASSIGN_DI


      !-----------------------------------------
      ! DUAL = REAL(DOUBLE)
      ! <U, DU> = <R, 0>
      !-----------------------------------------
      ELEMENTAL SUBROUTINE ASSIGN_DR(U, R)
      TYPE(HDUAL), INTENT(OUT) :: U
      REAL*8, INTENT(IN) :: R

      U%X = R
      U%DX = 0.0

      END SUBROUTINE ASSIGN_DR

      !-----------------------------------------
      ! INTEGER = DUAL
      ! I = <U, DU>
      !-----------------------------------------
      ELEMENTAL SUBROUTINE ASSIGN_ID(I, V)
      TYPE(HDUAL), INTENT(IN) :: V
      INTEGER, INTENT(OUT) :: I

      I = INT(V%X)

      END SUBROUTINE ASSIGN_ID

      !-----------------------------------------
      ! DUAL = DUAL
      ! <U, DU> = <U, DU>
      !-----------------------------------------
      ELEMENTAL SUBROUTINE ASSIGN_DD(U, V)
      TYPE(HDUAL), INTENT(IN) :: V
      TYPE(HDUAL), INTENT(OUT) :: U

      U%X=V%X
      U%DX=V%DX

      END SUBROUTINE ASSIGN_DD

      !******* END: (=)
      !---------------------

      !******* BEGIN: (+)
      !---------------------
      !-----------------------------------------
      ! UNARY POSITIVE
      ! <RES, DRES> = +<U, DU>
      !-----------------------------------------
      ELEMENTAL FUNCTION ADD_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      TYPE(HDUAL) :: RES

      RES = U  ! FASTER THAN ASSIGNING COMPONENT WISE

      END FUNCTION ADD_D


      !-----------------------------------------
      ! DUAL + DUAL
      ! <RES, DRES> = <U, DU> + <V, DV> = <U + V, DU + DV>
      !-----------------------------------------
      ELEMENTAL FUNCTION ADD_DD(U, V) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U, V
      TYPE(HDUAL) :: RES

      RES%X = U%X + V%X
      RES%DX = U%DX + V%DX

      END FUNCTION ADD_DD


      !-----------------------------------------
      ! DUAL + INTEGER
      ! <RES, DRES> = <U, DU> + I = <U + I, DU>
      !-----------------------------------------
      ELEMENTAL FUNCTION ADD_DI(U, I) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      INTEGER, INTENT(IN) :: I
      TYPE(HDUAL) :: RES

      RES%X = REAL(I) + U%X
      RES%DX = U%DX

      END FUNCTION ADD_DI


      !-----------------------------------------
      ! DUAL + DOUBLE
      ! <RES, DRES> = <U, DU> + <R, 0> = <U + R, DU>
      !-----------------------------------------
      ELEMENTAL FUNCTION ADD_DR(U, R) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      REAL*8, INTENT(IN) :: R
      TYPE(HDUAL) :: RES

      RES%X = R + U%X
      RES%DX = U%DX

      END FUNCTION ADD_DR


      !-----------------------------------------
      ! INTEGER + DUAL
      ! <RES, DRES> = <I, 0> + <V, DV> = <I + V, DV>
      !-----------------------------------------
      ELEMENTAL FUNCTION ADD_ID(I, V) RESULT(RES)
      INTEGER, INTENT(IN) :: I
      TYPE(HDUAL), INTENT(IN) :: V
      TYPE(HDUAL) :: RES

      RES%X = REAL(I) + V%X
      RES%DX = V%DX

      END FUNCTION ADD_ID


      !-----------------------------------------
      ! DOUBLE + DUAL
      ! <RES, DRES> = <R, 0> + <V, DV> = <R + V, DV>
      !-----------------------------------------
      ELEMENTAL FUNCTION ADD_RD(R, V) RESULT(RES)
      REAL*8, INTENT(IN) :: R
      TYPE(HDUAL), INTENT(IN) :: V
      TYPE(HDUAL) :: RES

      RES%X = R + V%X
      RES%DX = V%DX

      END FUNCTION ADD_RD

      !******* END: (+)
      !---------------------


      !******* BEGIN: (-)
      !---------------------

      !-------------------------------------------------
      ! NEGATE A DUAL
      ! <RES, DRES> = -<U, DU>
      !-------------------------------------------------
      ELEMENTAL FUNCTION MINUS_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      TYPE(HDUAL) :: RES

      RES%X = -U%X
      RES%DX = -U%DX

      END FUNCTION MINUS_D


      !-------------------------------------------------
      ! DUAL - DUAL
      ! <RES, DRES> = <U, DU> - <V, DV> = <U - V, DU - DV>
      !-------------------------------------------------
      ELEMENTAL FUNCTION MINUS_DD(U, V) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U, V
      TYPE(HDUAL) :: RES

      RES%X = U%X - V%X
      RES%DX = U%DX - V%DX

      END FUNCTION MINUS_DD

      !-------------------------------------------------
      ! DUAL - INTEGER
      ! <RES, DRES> = <U, DU> - I = <U - I, DU>
      !-------------------------------------------------
      ELEMENTAL FUNCTION MINUS_DI(U, I) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      INTEGER, INTENT(IN) :: I
      TYPE(HDUAL) :: RES

      RES%X = U%X - REAL(I)
      RES%DX = U%DX

      END FUNCTION MINUS_DI


      !-------------------------------------------------
      ! DUAL - DOUBLE
      ! <RES, DRES> = <U, DU> - R = <U - R, DU>
      !-------------------------------------------------
      ELEMENTAL FUNCTION MINUS_DR(U, R) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      REAL*8,INTENT(IN) :: R
      TYPE(HDUAL) :: RES

      RES%X = U%X - R
      RES%DX = U%DX

      END FUNCTION MINUS_DR


      !-------------------------------------------------
      ! INTEGER - DUAL
      ! <RES, DRES> = I - <V, DV> = <I - V, -DV>
      !-------------------------------------------------
      ELEMENTAL FUNCTION MINUS_ID(I, V) RESULT(RES)
      INTEGER, INTENT(IN) :: I
      TYPE(HDUAL), INTENT(IN) :: V
      TYPE(HDUAL) :: RES

      RES%X = REAL(I) - V%X
      RES%DX = -V%DX

      END FUNCTION MINUS_ID


      !-------------------------------------------------
      ! DOUBLE - DUAL
      ! <RES, DRES> = R - <V, DV> = <R - V, -DV>
      !-------------------------------------------------
      ELEMENTAL FUNCTION MINUS_RD(R, V) RESULT(RES)
      REAL*8, INTENT(IN) :: R
      TYPE(HDUAL), INTENT(IN) :: V
      TYPE(HDUAL) :: RES

      RES%X = R - V%X
      RES%DX = -V%DX

      END FUNCTION MINUS_RD

      !******* END: (-)
      !---------------------


      !******* BEGIN: (*)
      !---------------------

      !----------------------------------------
      ! DUAL * DUAL
      ! <RES, DRES> = <U, DU> * <V, DV> = <U * V, U * DV + V * DU>
      !----------------------------------------
      ELEMENTAL FUNCTION MULT_DD(U, V) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U, V
      TYPE(HDUAL) :: RES

      RES%X = U%X * V%X
      RES%DX = U%X * V%DX + V%X * U%DX
      IF(NDV.EQ.3)THEN
            RES%DX(3)=RES%DX(3)+V%DX(1)*U%DX(2)+V%DX(2)*U%DX(1)
      END IF

      END FUNCTION MULT_DD


      !-----------------------------------------
      ! DUAL * INTEGER
      ! <RES, DRES> = <U, DU> * I = <U * I, DU * I>
      !-----------------------------------------
      ELEMENTAL FUNCTION MULT_DI(U, I) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      INTEGER, INTENT(IN) :: I
      TYPE(HDUAL) :: RES

      REAL*8 :: R

      R = REAL(I)
      RES%X = R * U%X
      RES%DX = R * U%DX

      END FUNCTION MULT_DI

      !-----------------------------------------
      ! DUAL * DOUBLE
      ! <RES, DRES> = <U, DU> * R = <U * R, DU * R>
      !----------------------------------------
      ELEMENTAL FUNCTION MULT_DR(U, R) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      REAL*8, INTENT(IN) :: R
      TYPE(HDUAL) :: RES

      RES%X = U%X * R
      RES%DX = U%DX * R

      END FUNCTION MULT_DR


      !-----------------------------------------
      ! INTEGER * DUAL
      ! <RES, DRES> = I * <V, DV> = <I * V, I * DV>
      !-----------------------------------------
      ELEMENTAL FUNCTION MULT_ID(I, V) RESULT(RES)
      INTEGER, INTENT(IN) :: I
      TYPE(HDUAL), INTENT(IN) :: V
      TYPE(HDUAL) :: RES

      REAL*8 :: R

      R = REAL(I)
      RES%X = R * V%X
      RES%DX = R * V%DX

      END FUNCTION MULT_ID


      !-----------------------------------------
      ! DOUBLE * DUAL
      ! <RES, DRES> = R * <V, DV> = <R * V, R * DV>
      !-----------------------------------------
      ELEMENTAL FUNCTION MULT_RD(R, V) RESULT(RES)
      REAL*8, INTENT(IN) :: R
      TYPE(HDUAL), INTENT(IN) :: V
      TYPE(HDUAL) :: RES

      RES%X = R * V%X
      RES%DX = R * V%DX

      END FUNCTION MULT_RD

      !******* END: (*)
      !---------------------


      !******* BEGIN: (/)
      !---------------------

      !-----------------------------------------
      ! DUAL / DUAL
      ! <RES, DRES> = <U, DU> / <V, DV> = <U / V, DU / V - U * DV / V^2>
      !-----------------------------------------
      ELEMENTAL FUNCTION DIV_DD(U, V) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U, V
      TYPE(HDUAL) :: RES
      REAL*8 :: INV

      INV = 1.D0 / V%X
      RES%X = U%X * INV
      RES%DX = (U%DX - RES%X * V%DX) * INV

      END FUNCTION DIV_DD


      !-----------------------------------------
      ! DUAL / INTEGER
      ! <RES, DRES> = <U, DU> / I = <U / I, DU / I>
      !-----------------------------------------
      ELEMENTAL FUNCTION DIV_DI(U, I) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      INTEGER, INTENT(IN) :: I
      TYPE(HDUAL) :: RES
      REAL*8 :: INV

      INV = 1.D0 / REAL(I)
      RES%X = U%X * INV
      RES%DX = U%DX * INV

      END FUNCTION DIV_DI


      !-----------------------------------------
      ! DUAL / DOUBLE
      ! <RES, DRES> = <U, DU> / R = <U / R, DU / R>
      !----------------------------------------
      ELEMENTAL FUNCTION DIV_DR(U, R) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      REAL*8, INTENT(IN) :: R
      TYPE(HDUAL):: RES

      REAL*8 :: INV

      INV = 1.0 / R
      RES%X = U%X * INV
      RES%DX = U%DX * INV

      END FUNCTION DIV_DR


      !-----------------------------------------
      ! INTEGER / DUAL
      ! <RES, DRES> = I / <V, DV> = <I / V, -I / V^2 * DU>
      !-----------------------------------------
      ELEMENTAL FUNCTION DIV_ID(I, V) RESULT(RES)
      INTEGER, INTENT(IN) :: I
      TYPE(HDUAL), INTENT(IN) :: V
      TYPE(HDUAL) :: RES

      REAL*8 :: INV

      INV = 1.0 / V%X
      RES%X = REAL(I) * INV
      RES%DX = -RES%X * INV * V%DX

      END FUNCTION DIV_ID


      !-----------------------------------------
      ! DOUBLE / DUAL
      ! <RES, DRES> = R / <U, DU> = <R / U, -R / U^2 * DU>
      !-----------------------------------------
      ELEMENTAL FUNCTION DIV_RD(R, V) RESULT(RES)
      REAL*8, INTENT(IN) :: R
      TYPE(HDUAL), INTENT(IN) :: V
      TYPE(HDUAL) :: RES

      REAL*8 :: INV

      INV = 1.0 / V%X
      RES%X = R * INV
      RES%DX = -RES%X * INV * V%DX

      END FUNCTION DIV_RD

      !******* END: (/)
      !---------------------

      !******* BEGIN: (**)
      !---------------------

      !-----------------------------------------
      ! POWER(DUAL, INTEGER)
      ! <RES, DRES> = <U, DU> ^ I = <U ^ I, I * U ^ (I - 1) * DU>
      !-----------------------------------------
      ELEMENTAL FUNCTION POW_I(U, I) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      INTEGER, INTENT(IN) :: I
      TYPE(HDUAL) :: RES

      REAL*8 :: POW_X

      POW_X = U%X ** (I - 1)
      RES%X = U%X * POW_X
      RES%DX = REAL(I) * POW_X * U%DX
      IF(NDV.EQ.3) THEN
            RES%DX(3)=I*(I-1)*U%DX(1)*U%DX(2)
      END IF

      END FUNCTION POW_I

      !-----------------------------------------
      ! POWER(DUAL, DOUBLE)
      ! <RES, DRES> = <U, DU> ^ R = <U ^ R, R * U ^ (R - 1) * DU>
      !-----------------------------------------
      ELEMENTAL FUNCTION POW_R(U, R) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      REAL*8, INTENT(IN) :: R
      TYPE(HDUAL) :: RES

      REAL*8 :: POW_X

      POW_X = U%X ** (R - 1.0)
      RES%X = U%X * POW_X
      RES%DX = R * POW_X * U%DX
      IF(NDV.EQ.3) THEN
            RES%DX(3)=R*(R-1.D0)*U%DX(1)*U%DX(2)
      END IF

      END FUNCTION POW_R

      !-----------------------------------------
      ! POWER DUAL NUMBERS TO A DUAL POWER
      ! <RES, DRES> = <U, DU> ^ <V, DV>
      !     = <U ^ V, U ^ V * (V / U * DU + LOG(U) * DV)>
      !-----------------------------------------
      ELEMENTAL FUNCTION POW_D(U, V) RESULT(RES)
      TYPE(HDUAL), INTENT(IN)::U, V
      TYPE(HDUAL) :: RES

      RES%X = U%X ** V%X
      RES%DX = RES%X * (V%X / U%X * U%DX + LOG(U%X) * V%DX)

      END FUNCTION POW_D

      !******* END: (**)
      !---------------------


      !******* BEGIN: (==)
      !---------------------
      !-----------------------------------------
      ! COMPARE TWO DUAL NUMBERS,
      ! SIMPLY COMPARE THE FUNCTIONAL VALUE.
      !-----------------------------------------
      ELEMENTAL FUNCTION EQ_DD(LHS, RHS) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: LHS, RHS
      LOGICAL :: RES

      RES = (LHS%X == RHS%X)

      END FUNCTION EQ_DD


      !-----------------------------------------
      ! COMPARE A DUAL WITH AN INTEGER,
      ! SIMPLY COMPARE THE FUNCTIONAL VALUE.
      !-----------------------------------------
      ELEMENTAL FUNCTION EQ_DI(LHS, RHS) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: LHS
      INTEGER, INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (LHS%X == REAL(RHS))

      END FUNCTION EQ_DI


      !-----------------------------------------
      ! COMPARE A DUAL NUMBER WITH A REAL NUMBER,
      ! SIMPLY COMPARE THE FUNCTIONAL VALUE.
      !-----------------------------------------
      ELEMENTAL FUNCTION EQ_DR(LHS, RHS) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: LHS
      REAL*8, INTENT(IN) :: RHS
      LOGICAL::RES

      RES = (LHS%X == RHS)

      END FUNCTION EQ_DR


      !-----------------------------------------
      ! COMPARE AN INTEGER WITH A DUAL,
      ! SIMPLY COMPARE THE FUNCTIONAL VALUE.
      !----------------------------------------
      ELEMENTAL FUNCTION EQ_ID(LHS, RHS) RESULT(RES)
      INTEGER, INTENT(IN) :: LHS
      TYPE(HDUAL), INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (LHS == RHS%X)

      END FUNCTION EQ_ID


      !-----------------------------------------
      ! COMPARE A REAL WITH A DUAL,
      ! SIMPLY COMPARE THE FUNCTIONAL VALUE.
      !-----------------------------------------
      ELEMENTAL FUNCTION EQ_RD(LHS, RHS) RESULT(RES)
      REAL*8, INTENT(IN) :: LHS
      TYPE(HDUAL), INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (LHS == RHS%X)

      END FUNCTION EQ_RD

      !******* END: (==)
      !---------------------


      !******* BEGIN: (<=)
      !---------------------
      !-----------------------------------------
      ! COMPARE TWO DUAL NUMBERS, SIMPLY COMPARE
      ! THE FUNCTIONAL VALUE.
      !-----------------------------------------
      ELEMENTAL FUNCTION LE_DD(LHS, RHS) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: LHS, RHS
      LOGICAL :: RES

      RES = (LHS%X <= RHS%X)

      END FUNCTION LE_DD


      !-----------------------------------------
      ! COMPARE A DUAL WITH AN INTEGER,
      ! SIMPLY COMPARE THE FUNCTIONAL VALUE.
      !-----------------------------------------
      ELEMENTAL FUNCTION LE_DI(LHS, RHS) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: LHS
      INTEGER, INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (LHS%X <= RHS)

      END FUNCTION LE_DI


      !-----------------------------------------
      ! COMPARE A DUAL NUMBER WITH A REAL NUMBER,
      ! SIMPLY COMPARE THE FUNCTIONAL VALUE.
      !-----------------------------------------
      ELEMENTAL FUNCTION LE_DR(LHS, RHS) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: LHS
      REAL*8, INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (LHS%X <= RHS)

      END FUNCTION LE_DR


      !-----------------------------------------
      ! COMPARE A DUAL NUMBER WITH AN INTEGER,
      ! SIMPLY COMPARE THE FUNCTIONAL VALUE.
      !-----------------------------------------
      ELEMENTAL FUNCTION LE_ID(I, RHS) RESULT(RES)
      INTEGER, INTENT(IN) :: I
      TYPE(HDUAL), INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (I <= RHS%X)

      END FUNCTION LE_ID


      !-----------------------------------------
      ! COMPARE A REAL WITH A DUAL,
      ! SIMPLY COMPARE THE FUNCTIONAL VALUE.
      !-----------------------------------------
      ELEMENTAL FUNCTION LE_RD(LHS, RHS) RESULT(RES)
      REAL*8, INTENT(IN) :: LHS
      TYPE(HDUAL), INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (LHS <= RHS%X)

      END FUNCTION LE_RD

      !******* END: (<=)
      !---------------------

      !******* BEGIN: (<)
      !---------------------
      !-----------------------------------------
      ! COMPARE TWO DUAL NUMBERS, SIMPLY COMPARE
      ! THE FUNCTIONAL VALUE.
      !-----------------------------------------
      ELEMENTAL FUNCTION LT_DD(LHS, RHS) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: LHS, RHS
      LOGICAL :: RES

      RES = (LHS%X < RHS%X)

      END FUNCTION LT_DD

      !-----------------------------------------
      ! COMPARE A DUAL WITH AN INTEGER,
      ! SIMPLY COMPARE THE FUNCTIONAL VALUE.
      !-----------------------------------------
      ELEMENTAL FUNCTION LT_DI(LHS, RHS) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: LHS
      INTEGER, INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (LHS%X < RHS)

      END FUNCTION LT_DI


      !-----------------------------------------
      ! COMPARE A DUAL NUMBER WITH A REAL NUMBER, SIMPLY COMPARE
      ! THE FUNCTIONAL VALUE.
      !----------------------------------------
      ELEMENTAL FUNCTION LT_DR(LHS, RHS) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: LHS
      REAL*8, INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (LHS%X < RHS)

      END FUNCTION LT_DR


      !-----------------------------------------
      ! COMPARE A DUAL NUMBER WITH AN INTEGER
      !-----------------------------------------
      ELEMENTAL FUNCTION LT_ID(I, RHS) RESULT(RES)
      INTEGER, INTENT(IN) :: I
      TYPE(HDUAL), INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (I < RHS%X)

      END FUNCTION LT_ID

      !-----------------------------------------
      ! COMPARE A REAL WITH A DUAL
      !----------------------------------------
      ELEMENTAL FUNCTION LT_RD(LHS, RHS) RESULT(RES)
      REAL*8, INTENT(IN) :: LHS
      TYPE(HDUAL), INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (LHS < RHS%X)

      END FUNCTION LT_RD

      !******* END: (<)
      !---------------------

      !******* BEGIN: (>=)
      !---------------------
      !-----------------------------------------
      ! COMPARE TWO DUAL NUMBERS, SIMPLY COMPARE
      ! THE FUNCTIONAL VALUE.
      !----------------------------------------
      ELEMENTAL FUNCTION GE_DD(LHS, RHS) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: LHS, RHS
      LOGICAL :: RES

      RES = (LHS%X >= RHS%X)

      END FUNCTION GE_DD


      !-----------------------------------------
      ! COMPARE A DUAL WITH AN INTEGER
      !-----------------------------------------
      ELEMENTAL FUNCTION GE_DI(LHS, RHS) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: LHS
      INTEGER, INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (LHS%X >= RHS)

      END FUNCTION GE_DI


      !-----------------------------------------
      ! COMPARE A DUAL NUMBER WITH A REAL NUMBER, SIMPLY COMPARE
      ! THE FUNCTIONAL VALUE.
      !-----------------------------------------
      ELEMENTAL FUNCTION GE_DR(LHS, RHS) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: LHS
      REAL*8, INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (LHS%X >= RHS)

      END FUNCTION GE_DR


      !-----------------------------------------
      ! COMPARE A DUAL NUMBER WITH AN INTEGER
      !-----------------------------------------
      ELEMENTAL FUNCTION GE_ID(I, RHS) RESULT(RES)
      INTEGER, INTENT(IN) :: I
      TYPE(HDUAL), INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (I >= RHS%X)

      END FUNCTION GE_ID


      !-----------------------------------------
      ! COMPARE A REAL WITH A DUAL
      !-----------------------------------------
      ELEMENTAL FUNCTION GE_RD(LHS, RHS) RESULT(RES)
      REAL*8, INTENT(IN) :: LHS
      TYPE(HDUAL), INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (LHS >= RHS%X)

      END FUNCTION GE_RD
      !******* END: (>=)
      !---------------------

      !******* BEGIN: (>)
      !---------------------
      !-----------------------------------------
      ! COMPARE TWO DUAL NUMBERS, SIMPLY COMPARE
      ! THE FUNCTIONAL VALUE.
      !-----------------------------------------
      ELEMENTAL FUNCTION GT_DD(LHS, RHS) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: LHS, RHS
      LOGICAL :: RES

      RES = (LHS%X > RHS%X)

      END FUNCTION GT_DD


      !-----------------------------------------
      ! COMPARE A DUAL WITH AN INTEGER
      !-----------------------------------------
      ELEMENTAL FUNCTION GT_DI(LHS, RHS) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: LHS
      INTEGER, INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (LHS%X > RHS)

      END FUNCTION GT_DI


      !-----------------------------------------
      ! COMPARE A DUAL NUMBER WITH A REAL NUMBER, SIMPLY COMPARE
      ! THE FUNCTIONAL VALUE.
      !-----------------------------------------
      ELEMENTAL FUNCTION GT_DR(LHS, RHS) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: LHS
      REAL*8, INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (LHS%X > RHS)

      END FUNCTION GT_DR


      !-----------------------------------------
      ! COMPARE A DUAL NUMBER WITH AN INTEGER
      !-----------------------------------------
      ELEMENTAL FUNCTION GT_ID(I, RHS) RESULT(RES)
      INTEGER, INTENT(IN) :: I
      TYPE(HDUAL), INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (I > RHS%X)

      END FUNCTION GT_ID


      !-----------------------------------------
      ! COMPARE A REAL WITH A DUAL
      !-----------------------------------------
      ELEMENTAL FUNCTION GT_RD(LHS, RHS) RESULT(RES)
      REAL*8, INTENT(IN) :: LHS
      TYPE(HDUAL), INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (LHS > RHS%X)

      END FUNCTION GT_RD
      !******* END: (>)
      !---------------------

      !******* BEGIN: (/=)
      !---------------------
      !-----------------------------------------
      ! COMPARE TWO DUAL NUMBERS, SIMPLY COMPARE
      ! THE FUNCTIONAL VALUE.
      !-----------------------------------------
      ELEMENTAL FUNCTION NE_DD(LHS, RHS) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: LHS, RHS
      LOGICAL :: RES

      RES = (LHS%X /= RHS%X)

      END FUNCTION NE_DD


      !-----------------------------------------
      ! COMPARE A DUAL WITH AN INTEGER
      !-----------------------------------------
      ELEMENTAL FUNCTION NE_DI(LHS, RHS) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: LHS
      INTEGER, INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (LHS%X /= RHS)

      END FUNCTION NE_DI


      !-----------------------------------------
      ! COMPARE A DUAL NUMBER WITH A REAL NUMBER, SIMPLY COMPARE
      ! THE FUNCTIONAL VALUE.
      !-----------------------------------------
      ELEMENTAL FUNCTION NE_DR(LHS, RHS) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: LHS
      REAL*8, INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (LHS%X /= RHS)

      END FUNCTION NE_DR


      !-----------------------------------------
      ! COMPARE A DUAL NUMBER WITH AN INTEGER
      !-----------------------------------------
      ELEMENTAL FUNCTION NE_ID(I, RHS) RESULT(RES)
      INTEGER, INTENT(IN) :: I
      TYPE(HDUAL), INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (I /= RHS%X)

      END FUNCTION NE_ID


      !-----------------------------------------
      ! COMPARE A REAL WITH A DUAL
      !-----------------------------------------
      ELEMENTAL FUNCTION NE_RD(LHS, RHS) RESULT(RES)
      REAL*8, INTENT(IN) :: LHS
      TYPE(HDUAL), INTENT(IN) :: RHS
      LOGICAL :: RES

      RES = (LHS /= RHS%X)

      END FUNCTION NE_RD

      !******* END: (/=)
      !---------------------

      !-----------------------------------------
      ! REAL PART OF DUAL NUMBERS
      !----------------------------------------
      ELEMENTAL FUNCTION REAL_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      REAL*8 :: RES

      RES=U%X

      END FUNCTION REAL_D

      !-----------------------------------------
      ! IMAGINARY PART OF DUAL NUMBERS
      !----------------------------------------
      FUNCTION AIMAG_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      REAL*8 :: RES(NDV)
      INTEGER :: I

      DO I=1,NDV
            RES(I)=U%DX(I)
      END DO

      END FUNCTION AIMAG_D

      !---------------------------------------------------
      ! ABSOLUTE VALUE OF DUAL NUMBERS
      ! <RES, DRES> = ABS(<U, DU>) = <ABS(U), DU * SIGN(U)>
      !---------------------------------------------------
      ELEMENTAL FUNCTION ABS_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      TYPE(HDUAL) :: RES
      INTEGER :: I

      IF(U%X > 0) THEN
      RES%X = U%X
      RES%DX = U%DX
      ELSE IF (U%X < 0) THEN
      RES%X = -U%X
      RES%DX = -U%DX
      ELSE
      RES%X = 0.0
      DO I = 1, NDV
          IF (U%DX(I) .EQ. 0.0) THEN
              RES%DX(I) = 0.0
          ELSE
              RES%DX(I) = SET_NAN()
          END IF
      END DO
      ENDIF

      END FUNCTION ABS_D


      !-----------------------------------------
      ! ACOS OF DUAL NUMBERS
      ! <RES, DRES> = ACOS(<U, DU>) = <ACOS(U), -DU / SQRT(1 - U^2)>
      !----------------------------------------
      ELEMENTAL FUNCTION ACOS_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      TYPE(HDUAL) :: RES
      REAL*8 :: F, DF, DDF

      F=ACOS(U%X);
      DF=-1.D0/((1.D0-U%X*U%X)**0.5D0);
      DDF=-U%X/((1.D0-U%X*U%X)**1.5D0);

      RES%X       = F

      IF (U%X == 1.0 .OR. U%X == -1.0) THEN
      RES%DX = SET_NAN()  ! UNDEFINED DERIVATIVE
      ELSE
      RES%DX(1:2) = U%DX(1:2)*DF
      RES%DX(3)   = U%DX(3)*DF+U%DX(1)*U%DX(2)*DDF
      END IF

      END FUNCTION ACOS_D


      !-----------------------------------------
      ! ASIN OF DUAL NUMBERS
      ! <RES, DRES> = ASIN(<U, DU>) = <ASIN(U), DU / SQRT(1 - U^2)>
      !----------------------------------------
      ELEMENTAL FUNCTION ASIN_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      TYPE(HDUAL) :: RES

      REAL*8 :: F, DF, DDF

      F=ASIN(U%X);
      DF=1.D0/((1.D0-U%X*U%X)**0.5D0);
      DDF=U%X/((1.D0-U%X*U%X)**1.5D0);

      RES%X       = F

      IF (U%X == 1.0 .OR. U%X == -1.0) THEN
      RES%DX = SET_NAN()  ! UNDEFINED DERIVATIVE
      ELSE
      RES%DX(1:2) = U%DX(1:2)*DF
      RES%DX(3)   = U%DX(3)*DF+U%DX(1)*U%DX(2)*DDF
      END IF

      END FUNCTION ASIN_D


      !-----------------------------------------
      ! ATAN OF DUAL NUMBERS
      ! <RES, DRES> = ATAN(<U, DU>) = <ATAN(U), DU / (1 + U^2)>
      !----------------------------------------
      ELEMENTAL FUNCTION ATAN_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      TYPE(HDUAL) :: RES
      REAL*8 :: F, DF, DDF

      F=ATAN(U%X);
      DF=1.D0/(U%X*U%X+1.D0);
      DDF=-(2.D0*U%X)/((U%X*U%X+1.D0)**2.D0);

      RES%X       = F
      RES%DX(1:2) = U%DX(1:2)*DF
      RES%DX(3)   = U%DX(3)*DF+U%DX(1)*U%DX(2)*DDF

      END FUNCTION ATAN_D


      !-----------------------------------------
      ! ATAN2 OF DUAL NUMBERS
      ! <RES, DRES> = ATAN2(<U, DU>, <V, DV>)
      !             = <ATAN2(U, V), V / (U^2 + V^2) * DU - U / (U^2 + V^2) * DV>
      !----------------------------------------
      ELEMENTAL FUNCTION ATAN2_D(U, V) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U, V
      TYPE(HDUAL) :: RES
      REAL*8 :: XVAL
      REAL*8 :: F, DF1, DDF1, DF2, DDF2

      XVAL=U%X**2 + V%X**2
      F=ATAN2(U%X,V%X);
      DF1=-U%X/(U%X**2 + V%X**2);
      DF2= V%X/(U%X**2 + V%X**2);

      RES%X  = F
      RES%DX = U%DX*DF1+V%DX*DF2

      END FUNCTION ATAN2_D


      !-----------------------------------------
      ! COS OF DUAL NUMBERS
      ! <RES, DRES> = COS(<U, DU>) = <COS(U), -SIN(U) * DU>
      !----------------------------------------
      ELEMENTAL FUNCTION COS_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      TYPE(HDUAL) :: RES
      REAL*8 :: F, DF, DDF

      F=DCOS(U%X); DF=-DSIN(U%X); DDF=-DCOS(U%X);

      RES%X       = F
      RES%DX(1:2) = U%DX(1:2)*DF
      RES%DX(3)   = U%DX(3)*DF+U%DX(1)*U%DX(2)*DDF

      END FUNCTION COS_D


      !-----------------------------------------
      ! DOT PRODUCT TWO DUAL NUMBER VECTORS
      ! <RES, DRES> = <U, DU> . <V, DV> = <U . V, U . DV + V . DU>
      !-----------------------------------------
      FUNCTION DOT_PRODUCT_DD(U, V) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U(:), V(:)
      TYPE(HDUAL) :: RES

      INTEGER :: I

      RES%X = DOT_PRODUCT(U%X, V%X)
      DO I = 1, NDV
      RES%DX(I) = DOT_PRODUCT(U%X, V%DX(I))
     &                 + DOT_PRODUCT(V%X, U%DX(I))
      END DO

      END FUNCTION DOT_PRODUCT_DD


      !-----------------------------------------
      ! EXPONENTIAL OF DUAL NUMBERS
      ! <RES, DRES> = EXP(<U, DU>) = <EXP(U), EXP(U) * DU>
      !-----------------------------------------
      ELEMENTAL FUNCTION EXP_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      TYPE(HDUAL) :: RES
      REAL*8 :: F, DF, DDF

      F=DEXP(U%X); DF=DEXP(U%X); DDF=DEXP(U%X);

      RES%X       = F
      RES%DX(1:2) = U%DX(1:2)*DF
      RES%DX(3)   = U%DX(3)*DF+U%DX(1)*U%DX(2)*DDF


      END FUNCTION EXP_D


      !-----------------------------------------
      ! CONVERT DUAL TO INTEGER
      ! I = INT(<U, DU>) = INT(U)
      !----------------------------------------
      ELEMENTAL FUNCTION INT_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      INTEGER :: RES

      RES = INT(U%X)

      END FUNCTION INT_D


      !-----------------------------------------
      ! LOG OF DUAL NUMBERS,DEFINED FOR U%X>0 ONLY
      ! THE ERROR CONTROL SHOULD BE DONE IN THE ORIGINAL CODE
      ! IN OTHER WORDS, IF U%X<=0, IT IS NOT POSSIBLE TO OBTAIN LOG.
      ! <RES, DRES> = LOG(<U, DU>) = <LOG(U), DU / U>
      !----------------------------------------
      ELEMENTAL FUNCTION LOG_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      TYPE(HDUAL) :: RES
      REAL*8 :: F, DF, DDF

      F=DLOG(U%X); DF=1.D0/U%X; DDF=-1.D0/(U%X*U%X);

      RES%X       = F
      RES%DX(1:2) = U%DX(1:2)*DF
      RES%DX(3)   = U%DX(3)*DF+U%DX(1)*U%DX(2)*DDF

      END FUNCTION LOG_D


      !-----------------------------------------
      ! LOG10 OF DUAL NUMBERS,DEFINED FOR U%X>0 ONLY
      ! THE ERROR CONTROL SHOULD BE DONE IN THE ORIGINAL CODE
      ! IN OTHER WORDS, IF U%X<=0, IT IS NOT POSSIBLE TO OBTAIN LOG.
      ! <RES, DRES> = LOG10(<U, DU>) = <LOG10(U), DU / (U * LOG(10))>
      ! LOG<U,UP>=<LOG(U),UP/U>
      !----------------------------------------
      ELEMENTAL FUNCTION LOG10_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      TYPE(HDUAL) :: RES
      REAL*8 :: F, DF, DDF

      F=DLOG(U%X)/DLOG(10.D0);
      DF=1.D0/(U%X*DLOG(10.D0));
      DDF=-1.D0/(U%X*U%X*DLOG(10.D0));

      RES%X       = F
      RES%DX(1:2) = U%DX(1:2)*DF
      RES%DX(3)   = U%DX(3)*DF+U%DX(1)*U%DX(2)*DDF

      END FUNCTION LOG10_D

C-----------------------------------------------------------------------
      !-----------------------------------------
      ! MULTIPLY TWO DUAL NUMBER MATRICES
      ! <RES, DRES> = <U, DU> . <V, DV> = <U . V, DU . V + U . DV>
      !----------------------------------------
      FUNCTION MATMUL_DD(U,V) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U(:,:), V(:,:)
      TYPE(HDUAL) :: RES(SIZE(U,1), SIZE(V,2))
      INTEGER :: I

      RES%X = MATMUL(U%X, V%X)
      DO I = 1, NDV
      IF(I.NE.NDV) THEN
            RES%DX(I) = MATMUL(U%DX(I), V%X) + MATMUL(U%X, V%DX(I))
      ELSE
            RES%DX(I) = MATMUL(U%DX(1),V%DX(2))+MATMUL(U%DX(2),V%DX(1))
      END IF
      END DO

      END FUNCTION MATMUL_DD
C-----------------------------------------------------------------------

      !-----------------------------------------
      ! MULTIPLY A DUAL NUMBER MATRIX WITH A DUAL NUMBER
      ! VECTOR
      !
      ! <U,UP>.<V,VP>=<U.V,UP.V+U.VP>
      !----------------------------------------
      FUNCTION MATMUL_DV(U, V) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U(:,:), V(:)
      TYPE(HDUAL) :: RES(SIZE(U,1))
      INTEGER :: I

      RES%X = MATMUL(U%X, V%X)
      DO I = 1, NDV
      IF(I.NE.NDV) THEN
            RES%DX(I) = MATMUL(U%DX(I), V%X) + MATMUL(U%X, V%DX(I))
      ELSE
            RES%DX(I) = MATMUL(U%DX(1),V%DX(2))+MATMUL(U%DX(2),V%DX(1))
      END IF
      END DO

      END FUNCTION MATMUL_DV


      !-----------------------------------------
      ! MULTIPLY A DUAL VECTOR WITH A  DUAL MATRIX
      !
      ! <U,UP>.<V,VP>=<U.V,UP.V+U.VP>
      !----------------------------------------
      FUNCTION MATMUL_VD(U, V) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U(:), V(:,:)
      TYPE(HDUAL) :: RES(SIZE(V, 2))
      INTEGER::I

      RES%X = MATMUL(U%X, V%X)
      DO I = 1, NDV
      RES%DX(I) = MATMUL(U%DX(I), V%X) + MATMUL(U%X, V%DX(I))
      END DO

      END FUNCTION MATMUL_VD

      !-----------------------------------------
      ! OBTAIN THE MAX OF 2 TO 5 DUAL NUMBERS
      !----------------------------------------
      ELEMENTAL FUNCTION MAX_DD(VAL1, VAL2, VAL3, VAL4,VAL5)
     &  RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: VAL1, VAL2
      TYPE(HDUAL), INTENT(IN), OPTIONAL :: VAL3, VAL4,VAL5
      TYPE(HDUAL) :: RES

      IF (VAL1%X > VAL2%X) THEN
      RES = VAL1
      ELSE
      RES = VAL2
      ENDIF
      IF(PRESENT(VAL3))THEN
      IF(RES%X < VAL3%X) RES = VAL3
      ENDIF
      IF(PRESENT(VAL4))THEN
      IF(RES%X < VAL4%X) RES = VAL4
      ENDIF
      IF(PRESENT(VAL5))THEN
      IF(RES%X < VAL5%X) RES = VAL5
      ENDIF

      END FUNCTION MAX_DD
      !-----------------------------------------
      ! OBTAIN THE MAX OF A DUAL NUMBER AND AN INTEGER
      !----------------------------------------
      ELEMENTAL FUNCTION MAX_DI(U, I) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      INTEGER, INTENT(IN) :: I
      TYPE(HDUAL) :: RES

      IF (U%X > I) THEN
      RES = U
      ELSE
      RES = I
      ENDIF

      END FUNCTION MAX_DI

      !-----------------------------------------
      ! OBTAIN THE MAX OF A DUAL NUMBER AND A REAL NUMBER
      !----------------------------------------
      ELEMENTAL FUNCTION MAX_DR(U, R) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      REAL*8, INTENT(IN) :: R
      TYPE(HDUAL) :: RES

      IF (U%X > R) THEN
      RES = U
      ELSE
      RES = R
      ENDIF

      END FUNCTION MAX_DR


      !---------------------------------------------------
      ! OBTAIN THE MAX OF A REAL AND A DUAL
      !---------------------------------------------------
      ELEMENTAL FUNCTION MAX_RD(N, U) RESULT(RES)
      REAL*8, INTENT(IN) :: N
      TYPE(HDUAL), INTENT(IN) :: U
      TYPE(HDUAL) :: RES

      IF (U%X > N) THEN
      RES = U
      ELSE
      RES = N
      ENDIF

      END FUNCTION MAX_RD


      !-----------------------------------------
      ! OBTAIN THE MAX VALUE OF VECTOR U
      !----------------------------------------
      FUNCTION MAXVAL_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U(:)
      INTEGER :: ILOC(1)
      TYPE(HDUAL) :: RES

      ILOC=MAXLOC(U%X)
      RES=U(ILOC(1))

      END FUNCTION MAXVAL_D


      !-----------------------------------------
      ! OBTAIN THE MIN OF 2 TO 4 DUAL NUMBERS
      !----------------------------------------
      ELEMENTAL FUNCTION MIN_DD(VAL1, VAL2, VAL3, VAL4) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: VAL1, VAL2
      TYPE(HDUAL), INTENT(IN), OPTIONAL :: VAL3, VAL4
      TYPE(HDUAL) :: RES

      IF (VAL1%X < VAL2%X) THEN
      RES = VAL1
      ELSE
      RES = VAL2
      ENDIF
      IF(PRESENT(VAL3))THEN
      IF(RES%X > VAL3%X) RES = VAL3
      ENDIF
      IF(PRESENT(VAL4))THEN
      IF(RES%X > VAL4%X) RES = VAL4
      ENDIF

      END FUNCTION MIN_DD


      !-----------------------------------------
      ! OBTAIN THE MIN OF A DUAL AND A DOUBLE
      !----------------------------------------
      ELEMENTAL FUNCTION MIN_DR(U, R) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      REAL*8, INTENT(IN) :: R
      TYPE(HDUAL) :: RES

      IF (U%X < R) THEN
      RES = U
      ELSE
      RES = R
      ENDIF

      END FUNCTION MIN_DR


      !-----------------------------------------
      ! OBTAIN THE MIN VALUE OF VECTOR U
      !----------------------------------------
      FUNCTION MINVAL_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U(:)
      INTEGER :: ILOC(1)
      TYPE(HDUAL) :: RES

      ILOC=MINLOC(U%X)
      RES=U(ILOC(1))

      END FUNCTION MINVAL_D


      !------------------------------------------------------
      !RETURNS THE NEAREST INTEGER TO U%X, ELEMENTAL
      !------------------------------------------------------
      ELEMENTAL FUNCTION NINT_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      INTEGER :: RES

      RES=NINT(U%X)

      END FUNCTION NINT_D


      !----------------------------------------------------------------
      ! SIGN(A,B) WITH TWO DUAL NUMBERS AS INPUTS,
      ! THE RESULT WILL BE |A| IF B%X>=0, -|A| IF B%X<0,ELEMENTAL
      !----------------------------------------------------------------
      ELEMENTAL FUNCTION SIGN_DD(VAL1, VAL2) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: VAL1, VAL2
      TYPE(HDUAL) :: RES

      IF (VAL2%X < 0.0) THEN
      RES = -ABS(VAL1)
      ELSE
      RES =  ABS(VAL1)
      ENDIF

      END FUNCTION SIGN_DD


      !----------------------------------------------------------------
      ! SIGN(A,B) WITH ONE REAL AND ONE DUAL NUMBER AS INPUTS,
      ! THE RESULT WILL BE |A| IF B%X>=0, -|A| IF B%X<0,ELEMENTAL
      !----------------------------------------------------------------
      ELEMENTAL FUNCTION SIGN_RD(VAL1, VAL2) RESULT(RES)
      REAL*8, INTENT(IN) :: VAL1
      TYPE(HDUAL), INTENT(IN) :: VAL2
      TYPE(HDUAL) :: RES

      IF (VAL2%X < 0.0) THEN
      RES = -ABS(VAL1)
      ELSE
      RES = ABS(VAL1)
      ENDIF

      END FUNCTION SIGN_RD


      !-----------------------------------------
      ! SIN OF DUAL NUMBERS
      ! <RES, DRES> = SIN(<U, DU>) = <SIN(U), COS(U) * DU>
      !----------------------------------------
      ELEMENTAL FUNCTION SIN_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      TYPE(HDUAL) :: RES
      REAL*8 :: F, DF, DDF

      F=DSIN(U%X); DF=DCOS(U%X); DDF=-DSIN(U%X);

      RES%X       = F
      RES%DX(1:2) = U%DX(1:2)*DF
      RES%DX(3)   = U%DX(3)*DF+U%DX(1)*U%DX(2)*DDF

      END FUNCTION SIN_D


      !-----------------------------------------
      ! TAN OF DUAL NUMBERS
      ! <RES, DRES> = TAN(<U, DU>) = <TAN(U), DU / COS(U)^2>
      !----------------------------------------
      ELEMENTAL FUNCTION TAN_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      TYPE(HDUAL) :: RES
      REAL*8 :: F, DF, DDF

      F=DTAN(U%X);
      DF=DTAN(U%X)*DTAN(U%X)+1.D0;
      DDF=2.D0*DTAN(U%X)*(DTAN(U%X)*DTAN(U%X)+1.D0)

      RES%X       = F
      RES%DX(1:2) = U%DX(1:2)*DF
      RES%DX(3)   = U%DX(3)*DF+U%DX(1)*U%DX(2)*DDF

      END FUNCTION TAN_D


      !-----------------------------------------
      ! SQRT OF DUAL NUMBERS
      ! <RES, DRES> = SQRT(<U, DU>) = <SQRT(U), DU / (2 * SQRT(U))>
      !----------------------------------------
      ELEMENTAL FUNCTION SQRT_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U
      TYPE(HDUAL) :: RES
      INTEGER :: I
      REAL*8 :: F, DF, DDF

      F=DSQRT(U%X);
      DF=1.D0/(2.D0*DSQRT(U%X));
      DDF=-1.D0/(4.D0*U%X*DSQRT(U%X))

      RES%X       = F
      IF(RES%X .NE. 0.D0) THEN
            RES%DX(1:2) = U%DX(1:2)*DF
            RES%DX(3)   = U%DX(3)*DF+U%DX(1)*U%DX(2)*DDF
      ELSE
      DO I=1, NDV
          IF (U%DX(I) .EQ. 0.0) THEN
              RES%DX(I) = 0.D0
          ELSE
              RES%DX(I) = SET_NAN()
          END IF
      END DO
      END IF

      END FUNCTION SQRT_D


      !-----------------------------------------
      ! SUM OF A DUAL ARRAY
      !-----------------------------------------
      FUNCTION SUM_D(U) RESULT(RES)
      TYPE(HDUAL), INTENT(IN) :: U(:)
      TYPE(HDUAL) :: RES
      INTEGER :: I

      RES%X = SUM(U%X)
      DO I = 1, NDV
      RES%DX(I) = SUM(U%DX(I))
      END DO

      END FUNCTION SUM_D

      !-----------------------------------------
      ! FIND THE LOCATION OF THE MAX VALUE IN AN
      ! ARRAY OF DUAL NUMBERS
      !-----------------------------------------
      FUNCTION MAXLOC_D(ARRAY) RESULT(IND)
      TYPE(HDUAL), INTENT(IN) :: ARRAY(:)
      INTEGER :: IND(1)

      IND = MAXLOC(ARRAY%X)

      END FUNCTION MAXLOC_D


      ELEMENTAL FUNCTION SET_NAN() RESULT(RES)
      REAL*8 :: RES

      RES = SQRT(NEGATIVE_ONE)

      END FUNCTION SET_NAN
C-----------------------------------------------------------------------
      END MODULE HYPER_DUAL
