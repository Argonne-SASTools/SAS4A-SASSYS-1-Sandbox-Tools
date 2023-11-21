!===============================================================================
!
! This file is part of the SAS4A-SASSYS-1-Sandbox-Tools.
!
! See LICENSE for full details.
!
!===============================================================================

!===============================================================================
!   MODULE DIFF                                                                $
!===============================================================================
!===============================================================================

!===============================================================================

MODULE DIFF
    IMPLICIT NONE

	! Module Data:
	! <none>

	real*8 :: MAX_REL_DIFF = 0
	real*8 :: MAX_ABS_DIFF = 0
    real*8, PRIVATE ::  REL_TOLERANCE, ABS_TOLERANCE

!===============================================================================
CONTAINS

!-------------------------------------------------------------------------------
!
!  SUBROUTINE SetTolerances
!
!-------------------------------------------------------------------------------
!!
!! Sets the absolute and relative tolerances to user input values
!!
!-------------------------------------------------------------------------------

SUBROUTINE SetTolerances(rel,abs)
    IMPLICIT NONE
    real*8 :: rel
    real*8 :: abs

    INTEGER IWARN, IERR
    COMMON /warn_err/ IWARN, IERR

	if (rel > 0.0) REL_TOLERANCE = rel
	if (abs > 0.0) ABS_TOLERANCE = abs


	write(6,8031) REL_TOLERANCE
	write(6,8032) ABS_TOLERANCE

	if (REL_TOLERANCE .eq. 0) then
        IERR = IERR + 1
        write (6,8051)
    endif

8031 format(/, '  RELATIVE TOLERANCE is: ' 1P,e15.8)
8032 format('  ABSOLUTE TOLERANCE is: ' 1P,e15.8)
8051 format(/,'ERROR: You must define a relative tolerance that is greater than zero',/)

END SUBROUTINE SetTolerances

!-------------------------------------------------------------------------------
!
!  FUNCTION Compare
!
!-------------------------------------------------------------------------------
!!
!! Compares the absolute and relative differences between two values REAL_1
!! and REAL_2 to maximum allowable absolute and relative differences
!!
!! COMP_RESULT =  0 is Good
!! COMP_RESULT =  1 is Bad
!! COMP_RESULT = -1 is Warning
!!
!-------------------------------------------------------------------------------

FUNCTION COMPARE(first, second) RESULT(COMP_RESULT)
	IMPLICIT NONE
	INTEGER	:: result

    INTEGER IWARN, IERR
    COMMON /warn_err/ IWARN, IERR

	! Local Variables:
	real*8 :: &
		REL_DIFF,    &
		ABS_DIFF
	real*8 :: &
	    first,       &
        second
	integer :: COMP_RESULT

	!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    REL_DIFF = CALCULATE_REL_DIFFERENCE(first, second)
    ABS_DIFF = CALCULATE_ABS_DIFFERENCE(first, second)

    COMP_RESULT = 0

    if (REL_DIFF .gt. REL_TOLERANCE) then
        if (ABS_DIFF .gt. ABS_TOLERANCE) then
            COMP_RESULT = -1
        elseif (ABS_DIFF .le. ABS_TOLERANCE) then
            COMP_RESULT = 1
         endif
    endif

	!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

END FUNCTION COMPARE

!-------------------------------------------------------------------------------
!
!  FUNCTION CALCULATE_REL_DIFFERENCE
!
!-------------------------------------------------------------------------------
!!
!! Calculate the relative difference between two values REAL_1 and REAL_2
!!
!-------------------------------------------------------------------------------

FUNCTION CALCULATE_REL_DIFFERENCE(REAL_1, REAL_2)

    implicit none

    real*8 :: &
        CALCULATE_REL_DIFFERENCE,    &
		REAL_1,    &
		REAL_2

    if (REAL_1 .ne. 0 .and. REAL_2 .ne. 0) then
        CALCULATE_REL_DIFFERENCE = abs(REAL_1 - REAL_2)/max(REAL_1, REAL_2)
    else
        CALCULATE_REL_DIFFERENCE = 0.0
    endif

    return

END FUNCTION CALCULATE_REL_DIFFERENCE

!-------------------------------------------------------------------------------
!
!  FUNCTION CALCULATE_ABS_DIFFERENCE
!
!-------------------------------------------------------------------------------
!!
!! Calculate the absolute difference between two values REAL_1 and REAL_2
!!
!-------------------------------------------------------------------------------

FUNCTION CALCULATE_ABS_DIFFERENCE(REAL_1, REAL_2)

    implicit none

    real*8 :: &
        CALCULATE_ABS_DIFFERENCE,    &
		REAL_1,    &
		REAL_2

    CALCULATE_ABS_DIFFERENCE = abs(REAL_1 - REAL_2)

    return

END FUNCTION CALCULATE_ABS_DIFFERENCE

!-------------------------------------------------------------------------------
END MODULE DIFF
!===============================================================================
