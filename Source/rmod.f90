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

REAL(EB) FUNCTION ERF(X)

! Error function

REAL(EB),INTENT(IN)::X

ERF = 1._EB-ERFC(X)

END FUNCTION ERF


REAL(EB) FUNCTION ERFC(X)

! Complimentary error function

REAL(EB), INTENT(IN) :: X

REAL(EB) XSML,XMAX,SQEPS,Y
REAL(EB), PARAMETER, DIMENSION(13) :: ERFCS=(/&
 -.049046121234691808_EB,&
 -.14226120510371364_EB,&
  .010035582187599796_EB,&
 -.000576876469976748_EB,&
  .000027419931252196_EB,&
 -.000001104317550734_EB,&
  .000000038488755420_EB,&
 -.000000001180858253_EB,&
  .000000000032334215_EB,&
 -.000000000000799101_EB,&
  .000000000000017990_EB,&
 -.000000000000000371_EB,&
  .000000000000000007_EB/)

REAL(EB), PARAMETER, DIMENSION(23) :: ERC2CS=(/&
 -.069601346602309501_EB,&
 -.041101339362620893_EB,&
  .003914495866689626_EB,&
-.000490639565054897_EB,&
 .000071574790013770_EB,&
-.000011530716341312_EB,&
 .000001994670590201_EB,&
-.000000364266647159_EB,&
 .000000069443726100_EB,&
-.000000013712209021_EB,&
 .000000002788389661_EB,&
-.000000000581416472_EB,&
 .000000000123892049_EB,&
-.000000000026906391_EB,&
 .000000000005942614_EB,&
-.000000000001332386_EB,&
 .000000000000302804_EB,&
-.000000000000069666_EB,&
 .000000000000016208_EB,&
-.000000000000003809_EB,&
 .000000000000000904_EB,&
-.000000000000000216_EB,&
 .000000000000000052_EB /)

REAL(EB), PARAMETER, DIMENSION(24) :: ERFCCS=(/&
 0.0715179310202925_EB,&
 -.026532434337606719_EB,&
  .001711153977920853_EB,&
 -.000163751663458512_EB,&
  .000019871293500549_EB,&
 -.000002843712412769_EB,&
  .000000460616130901_EB,&
 -.000000082277530261_EB,&
  .000000015921418724_EB,&
 -.000000003295071356_EB,&
  .000000000722343973_EB,&
 -.000000000166485584_EB,&
  .000000000040103931_EB,&
 -.000000000010048164_EB,&
  .000000000002608272_EB,&
 -.000000000000699105_EB,&
  .000000000000192946_EB,&
 -.000000000000054704_EB,&
  .000000000000015901_EB,&
 -.000000000000004729_EB,&
  .000000000000001432_EB,&
 -.000000000000000439_EB,&
  .000000000000000138_EB,&
 -.000000000000000048_EB /)

XSML = -200._EB
XMAX = 200._EB
SQEPS = 0.001_EB

IF (X<=XSML) THEN
   ERFC = 2._EB
ELSE

   IF (X<=XMAX) THEN
      Y = ABS(X)
      IF (Y<=1.0_EB) THEN  ! ERFC(X) = 1.0 - ERF(X) FOR -1._EB <= X <= 1.
         IF (Y<SQEPS)  ERFC = 1.0_EB - 2.0_EB*X/SQRTPI
         IF (Y>=SQEPS) ERFC = 1.0_EB - X*(1.0_EB + CSEVL (2._EB*X*X-1._EB, ERFCS, 10) )
      ELSE  ! ERFC(X) = 1.0 - ERF(X) FOR 1._EB < ABS(X) <= XMAX
         Y = Y*Y
         IF (Y<=4._EB) ERFC = EXP(-Y)/ABS(X) * (0.5_EB + CSEVL ((8._EB/Y-5._EB)/3._EB,ERC2CS, 10) )
         IF (Y>4._EB) ERFC = EXP(-Y)/ABS(X) * (0.5_EB + CSEVL (8._EB/Y-1._EB,ERFCCS, 10) )
         IF (X<0._EB) ERFC = 2.0_EB - ERFC
      ENDIF
   ELSE
      ERFC = 0._EB
   ENDIF
ENDIF
RETURN

END FUNCTION ERFC

REAL(EB) FUNCTION CSEVL(X,CS,N)

REAL(EB), INTENT(IN) :: X
REAL(EB) CS(:),B1,B0,TWOX,B2
INTEGER NI,N,I

B1=0._EB
B0=0._EB
TWOX=2._EB*X
DO I=1,N
B2=B1
B1=B0
NI=N+1-I
B0=TWOX*B1-B2+CS(NI)
ENDDO

CSEVL = 0.5_EB*(B0-B2)

END FUNCTION CSEVL

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
!------------------------------------------------------------------------------
