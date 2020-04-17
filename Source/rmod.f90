!------------------------------------------------------------------------------
! Parts of several FDS modules are reproduced here in an effort to make
! rcal.f90 transferable between stand-alone RADCAL and FDS.
!------------------------------------------------------------------------------


MODULE PRECISION_PARAMETERS
! Set important parameters having to do with variable precision and array allocations
!------------------------------------------------------------------------------

IMPLICIT NONE

! Precision of "Four Byte" and "Eight Byte" reals

INTEGER, PARAMETER :: FB = SELECTED_REAL_KIND(6)
INTEGER, PARAMETER :: EB = SELECTED_REAL_KIND(12)

! Single- and double-precision complex

INTEGER, PARAMETER :: SPC = KIND((1._FB,1._FB))
INTEGER, PARAMETER :: DPC = KIND((1._EB,1._EB))

! Hardwired bounds for certain species arrays

INTEGER, PARAMETER :: MAX_SPECIES=20

! Hardwired length of most labels

INTEGER, PARAMETER :: LABEL_LENGTH=60, MESSAGE_LENGTH=200

! Special numbers

REAL(EB), PARAMETER :: EPSILON_EB=EPSILON(1._EB),ALMOST_ONE=1._EB-EPSILON(1._EB),MICRON=1.E-6_EB,&
                       TWO_EPSILON_EB=2._EB*EPSILON(1._EB),TINY_EB=TINY(1._EB),HUGE_EB=HUGE(1._EB)

! Often used numbers

REAL(EB), PARAMETER :: ONTH=1._EB/3._EB,THFO=3._EB/4._EB,FOTH=4._EB/3._EB,TWTH=2._EB/3._EB,ONSI=1._EB/6._EB,&
                       SR3=SQRT(3._EB),FTTOT=4._EB*(2._EB/3._EB)**(1._EB/3._EB),EIONTH=18._EB**(1._EB/3._EB)
REAL(EB), PARAMETER :: PI=4._EB*ATAN(1.0_EB), SQRTPI=SQRT(PI), RPI=1._EB/PI, TWOPI=2._EB*PI, PIO2=PI/2._EB, &
                       RFPI=1._EB/(4._EB*PI), FOTHPI = FOTH*PI

END MODULE PRECISION_PARAMETERS
!------------------------------------------------------------------------------


MODULE GLOBAL_CONSTANTS
! Module containing global constants, parameters, variables
!------------------------------------------------------------------------------

USE PRECISION_PARAMETERS
IMPLICIT NONE

! Logical units and output file names

INTEGER :: LU_ERR=0

! Miscellaneous logical constants

LOGICAL :: AEROSOL_AL2O3=.FALSE.

! Miscellaneous real constants

REAL(EB), PARAMETER :: SIGMA=5.670373E-8_EB

END MODULE GLOBAL_CONSTANTS
!------------------------------------------------------------------------------


MODULE COMP_FUNCTIONS
!------------------------------------------------------------------------------
! I/O + OS functions

IMPLICIT NONE

CONTAINS

SUBROUTINE SHUTDOWN(MESSAGE)
USE GLOBAL_CONSTANTS, ONLY: LU_ERR

! Stops the code gracefully after writing a message

CHARACTER(*) MESSAGE

WRITE(LU_ERR,'(/A)') TRIM(MESSAGE)

STOP

END SUBROUTINE SHUTDOWN


END MODULE COMP_FUNCTIONS
!------------------------------------------------------------------------------


MODULE MEMORY_FUNCTIONS
!------------------------------------------------------------------------------

USE PRECISION_PARAMETERS, ONLY: MESSAGE_LENGTH
USE COMP_FUNCTIONS, ONLY : SHUTDOWN
IMPLICIT NONE

PUBLIC CHKMEMERR

CONTAINS

SUBROUTINE ChkMemErr(CodeSect,VarName,IZERO)

! Output result of memory check (if fail)

CHARACTER(*), INTENT(IN) :: CodeSect, VarName
INTEGER IZERO
CHARACTER(MESSAGE_LENGTH) MESSAGE

IF (IZERO==0) RETURN

WRITE(MESSAGE,'(4A)') 'ERROR: Memory allocation failed for ', TRIM(VarName),' in the routine ',TRIM(CodeSect)
CALL SHUTDOWN(MESSAGE)

END SUBROUTINE ChkMemErr

END MODULE MEMORY_FUNCTIONS
!------------------------------------------------------------------------------


MODULE MATH_FUNCTIONS
!------------------------------------------------------------------------------

USE PRECISION_PARAMETERS
IMPLICIT NONE

CONTAINS

SUBROUTINE INTERPOLATE1D(X,Y,XI,ANS)

REAL(EB), INTENT(IN), DIMENSION(:) :: X, Y
REAL(EB), INTENT(IN) :: XI
REAL(EB), INTENT(OUT) :: ANS
INTEGER I, UX,LX

UX = UBOUND(X,1)
LX = LBOUND(X,1)

IF (XI <= X(LX)) THEN
  ANS = Y(LX)
ELSEIF (XI >= X(UX)) THEN
  ANS = Y(UX)
ELSE
  L1: DO I=LX,UX-1
    IF (ABS(XI -X(I)) <= SPACING(X(I))) THEN
      ANS = Y(I)
      EXIT L1
    ELSEIF (X(I+1)>XI) THEN
      ANS = Y(I)+(XI-X(I))/(X(I+1)-X(I)) * (Y(I+1)-Y(I))
      EXIT L1
    ENDIF
  ENDDO L1
ENDIF

END SUBROUTINE INTERPOLATE1D

END MODULE MATH_FUNCTIONS


MODULE RADCONS
USE PRECISION_PARAMETERS
USE GLOBAL_CONSTANTS, ONLY: SIGMA
REAL(EB):: RPI_SIGMA = SIGMA/PI

END MODULE RADCONS
!------------------------------------------------------------------------------
