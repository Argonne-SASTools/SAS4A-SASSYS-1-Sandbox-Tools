!===============================================================================
!
! This file is part of the SAS4A-SASSYS-1-Sandbox-Tools.
!
! See LICENSE for full details.
!
!===============================================================================

!===============================================================================

MODULE PRIMAR4

    USE DIFF
    USE CHECKPRIMAR

    IMPLICIT NONE

	! Module Data:
	! <none>

    LOGICAL :: cont_after_error = .FALSE.

!===============================================================================
CONTAINS

!-------------------------------------------------------------------------------
!
!  SUBROUTINE INIT15
!
!-------------------------------------------------------------------------------
!!
!! Check that two PRIMAR4.dat files were given for comparison
!! Check # of arrays and # of time steps
!! Initialize tolerances and PRIMAR4.dat files to check for the comparison
!!
!-------------------------------------------------------------------------------

SUBROUTINE INIT15(base_file, new_file, char_rel_tol, char_abs_tol, num_args)
    IMPLICIT NONE
    CHARACTER*(*) base_file, new_file
    CHARACTER*(*) char_rel_tol, char_abs_tol
    CHARACTER*32 arg

    INTEGER num_args, i

    INTEGER IWARN, IERR
    COMMON /warn_err/ IWARN, IERR

    real*8 rel_tol
    real*8 abs_tol

    read (char_rel_tol,*) rel_tol
    read (char_abs_tol,*) abs_tol

    !call CheckInputArgs(base_file,new_file)
    call CheckNumArrays_Time(base_file,new_file)
    call SetTolerances(rel_tol,abs_tol)

    if (IWARN .gt. 0) then
        write (6,8059) IWARN
        !Reset IWARN so we can count number of warnings later
        IWARN = 0
    endif

    if (IERR .gt. 0) then
        write (6,8060) IERR
        stop
    endif

    if (command_argument_count() .gt. 4) then
        do i = 5, command_argument_count()
            CALL getarg(i, arg)
            select case (arg)
                case ('-c', '--continue')
                    cont_after_error = .TRUE.
                    write (6,8061)
            end select
        enddo
    endif

8059 format(/,'You have had ', i4, ' warning(s).',/,'Proceed with caution.',/)
8060 format(/,'You have had ', i4, ' error(s).',/,'Terminating comparison.',/)
8061 format(/'All values will be compared, even if a pair is found to be unacceptably different',/)

END SUBROUTINE


!-------------------------------------------------------------------------------
!
!  SUBROUTINE FORT15_COMPARE
!
!-------------------------------------------------------------------------------
!!
!! Perform the actual PRIMAR4.dat comparison
!!
!-------------------------------------------------------------------------------

SUBROUTINE FORT15_COMPARE(base_file, new_file)
    IMPLICIT NONE
    CHARACTER*(*) base_file, new_file
    INTEGER :: UNIT_FILE_BASE = 21, UNIT_FILE_NEW = 22
    INTEGER :: RESULT
    INTEGER :: index, j
    INTEGER :: ISTEP_B, IPRSTP_B, NBNTOT_B
    INTEGER :: ISTEP_N, IPRSTP_N, NBNTOT_N


    INTEGER IWARN, IERR
    COMMON /warn_err/ IWARN, IERR
    INTEGER BASE_Array, NEW_Array
    COMMON /arrays/ BASE_Array, NEW_Array

    real*8 ARRAY_B(BASE_Array), ARRAY_N(NEW_Array)

    open (unit = UNIT_FILE_BASE, file = base_file, status='old', form='UNFORMATTED')
    open (unit = UNIT_FILE_NEW, file = new_file, status='old', form='UNFORMATTED')

    rewind(UNIT_FILE_BASE)
    rewind(UNIT_FILE_NEW)

    do while (.true.)

		read(UNIT_FILE_BASE, end=996) ISTEP_B, IPRSTP_B, NBNTOT_B, (ARRAY_B(index),index=1,BASE_Array)
		read(UNIT_FILE_NEW, end=996) ISTEP_N, IPRSTP_N, NBNTOT_N, (ARRAY_N(index),index=1,NEW_Array)

		do j = 1,BASE_Array
		    RESULT = COMPARE(ARRAY_B(j),ARRAY_N(j))
		    if (RESULT .eq. 1) then
		        IWARN = IWARN + 1
		        write(6,8077) ARRAY_B(j),ARRAY_N(j),ISTEP_B
		    endif
		    if (RESULT .eq. -1) then
		        IERR = IERR + 1
		        write(6,8078) ARRAY_B(j),ARRAY_N(j),ISTEP_B
		        if (.NOT. (cont_after_error .eq. .TRUE.)) then
		            write (6,8082)
		            IERR = -1
		            call END_FORT15
		        endif
		        if ( IERR .eq. 999 ) then
		            write (6, 8083) IERR
		            stop
		        endif
		    endif
	    enddo

8077 format('WARNING for values',1P,e15.8,' and',1P,e15.8,' at time step ', i5)
8078 format('ERROR for values',1P,e15.8,' and',1P,e15.8,' at time step ', i5)
8082 format(/,'Stopping because a pair of values from your PRIMAR4.dat files were unacceptably different')
8083 format(/, i3, ' errors in your PRIMAR4.dat comparison.',/,'TOO MANY ERRORS. TERMINATING COMPARISON.',&
            & /,'See output for further details.')

    enddo
    996 continue

END SUBROUTINE

!-------------------------------------------------------------------------------
!
!  SUBROUTINE END_FORT15
!
!-------------------------------------------------------------------------------
!!
!! End the PRIMAR4.dat comparison
!!
!-------------------------------------------------------------------------------

SUBROUTINE END_FORT15
    IMPLICIT NONE

    INTEGER IWARN, IERR
    COMMON /warn_err/ IWARN, IERR

    if (IERR .eq. -1) then
        stop
    endif

    write (6,8080)
8080 format(/, &
    &//,'-------------------------------------------------------------------------------',&
    &/,'FINAL SUMMARY',/,&
    &'-------------------------------------------------------------------------------')

    if (IERR .eq. 0 .and. IWARN .eq. 0) then
        write (6,8082)
    elseif (IWARN .gt. 0 .and. IERR .eq. 0) then
        write (6,8084) IWARN
    elseif (IERR .gt. 0) then
        write (6,8088) IERR
    endif

8082    format(/,'YOUR TWO PRIMAR4.dat FILES COMPARE VERY WELL',/)
8084        format(/,i4 ' values in your PRIMAR4.dat files were different by more than the allowable',&
            &/,'       relative tolerance but less than the allowable absolute tolerance.',&
            &/,'       Please check the output carefully.'/'       COMPARISON TENTATIVELY PASSED.',/)
8088        format(/,i4 ' values in your PRIMAR4.dat files were unacceptably different.',&
            &/,'       Please check the output carefully.'/'       COMPARISON FAILED',/)

END SUBROUTINE

!-------------------------------------------------------------------------------
END MODULE
!===============================================================================
