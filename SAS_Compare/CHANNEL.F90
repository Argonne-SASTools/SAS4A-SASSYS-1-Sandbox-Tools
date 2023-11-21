!===============================================================================
!
! This file is part of the SAS4A-SASSYS-1-Sandbox-Tools.
!
! See LICENSE for full details.
!
!===============================================================================

!===============================================================================
!   MODULE FORT11                                                                $
!===============================================================================
!===============================================================================

!===============================================================================

MODULE CHANNEL

    USE DIFF
    USE CHECKCHANNEL

    IMPLICIT NONE

	! Module Data:
	! <none>

    INTEGER :: IERR_WC = 0, IERR_Ch = 0, IWARN_WC = 0, IWARN_Ch = 0
    LOGICAL :: cont_after_error = .FALSE.

!===============================================================================
CONTAINS

!-------------------------------------------------------------------------------
!
!  SUBROUTINE INIT11
!
!-------------------------------------------------------------------------------
!!
!! Check that two CHANNEL.dat files were given for comparison
!! Check # of channels and # of time steps
!! Initialize tolerances and CHANNEL.dat files to check for the comparison
!!
!-------------------------------------------------------------------------------

SUBROUTINE INIT11(base_file, new_file, char_rel_tol, char_abs_tol, num_args)
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
    call CheckNumChannels_Time(base_file,new_file)
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

END SUBROUTINE INIT11

!-------------------------------------------------------------------------------
!
!  SUBROUTINE FORT11_COMPARE
!
!-------------------------------------------------------------------------------
!!
!! Perform the actual CHANNEL.dat comparison
!!
!-------------------------------------------------------------------------------

SUBROUTINE FORT11_COMPARE(base_file, new_file)
    IMPLICIT NONE
    CHARACTER*(*) base_file, new_file
    INTEGER IWARN, IERR
    COMMON /warn_err/ IWARN, IERR

    call Whole_Core_Comp(base_file,new_file)

    IERR_WC = IERR
    IWARN_WC = IWARN

    call Channel_Comp(base_file,new_file)

    IERR_Ch = IERR - IERR_WC
    IWARN_Ch = IWARN - IWARN_WC

END SUBROUTINE FORT11_COMPARE

!-------------------------------------------------------------------------------
!
!  SUBROUTINE Whole_Core_Comp
!
!-------------------------------------------------------------------------------
!!
!! Compares the whole core portions of the two CHANNEL.dat files
!!
!-------------------------------------------------------------------------------

SUBROUTINE Whole_Core_Comp(base_file,new_file)
    IMPLICIT NONE
    INTEGER :: UNIT_FILE_BASE = 21, UNIT_FILE_NEW = 22
    CHARACTER*(*) base_file, new_file
    INTEGER :: RESULT
    INTEGER :: index, j, STEP = 1
	real*8 WC_B(15), WC_N(15)
	INTEGER :: WC_INT_B(2), WC_INT_N(2)

    INTEGER IWARN, IERR
    COMMON /warn_err/ IWARN, IERR
    INTEGER BASE_Chan, NEW_Chan
    COMMON /channels/ BASE_Chan, NEW_Chan

    open (unit = UNIT_FILE_BASE, file = base_file, status='old', form='UNFORMATTED')
    open (unit = UNIT_FILE_NEW, file = new_file, status='old', form='UNFORMATTED')

    rewind(UNIT_FILE_BASE)
    rewind(UNIT_FILE_NEW)

    STEP = 1

    ! Skip to the first step for core-wide data:
	do index = 1, BASE_Chan
		read(UNIT_FILE_BASE)
	enddo
	do index = 1, NEW_Chan
		read(UNIT_FILE_NEW)
	enddo

    do while (.true.)

		read(UNIT_FILE_BASE, end=996) (WC_INT_B(index),index=1,2), &
		&                             (WC_B(index),index=1,15)
		read(UNIT_FILE_NEW, end=996) (WC_INT_N(index),index=1,2), &
		&                             (WC_N(index),index=1,15)

		do j = 1,15
		    RESULT = COMPARE(WC_B(j),WC_N(j))
		    if (RESULT .eq. 1) then
		        IWARN = IWARN + 1
		        write(6,8077) WC_B(j),WC_N(j),STEP
		    endif
		    if (RESULT .eq. -1) then
		        IERR = IERR + 1
		        write(6,8078) WC_B(j),WC_N(j),STEP
		        if (.NOT. (cont_after_error .eq. .TRUE.)) then
		            write (6,8082)
		            IERR = -1
		            call END_FORT11
		        endif
		        if ( IERR .eq. 999 ) then
		            write (6, 8083) IERR
		            stop
		        endif
		    endif
	    enddo

8077 format('WARNING for Whole Core values',1P,e15.8,' and',1P,e15.8,' at time step ', i5)
8078 format('ERROR for Whole Core values',1P,e15.8,' and',1P,e15.8,' at time step ', i5)
8082 format(/,'Stopping because a pair of values from the Whole Core data were unacceptably different')
8083 format(/, i3, ' errors in your Whole Core comparison.',/,'TOO MANY ERRORS. TERMINATING COMPARISON.',&
            & /,'See output for further details.')

		! Skip to the next step
		do index = 1, BASE_Chan
			read(UNIT_FILE_BASE, end=996)
		enddo
		do index = 1, NEW_Chan
			read(UNIT_FILE_NEW, end=996)
		enddo
        STEP = STEP + 1

    enddo
    996 continue

END SUBROUTINE Whole_Core_Comp

!-------------------------------------------------------------------------------
!
!  SUBROUTINE Channel_Comp
!
!-------------------------------------------------------------------------------
!!
!! Compares the channel portions of the two CHANNEL.dat files
!!
!-------------------------------------------------------------------------------

SUBROUTINE Channel_Comp(base_file,new_file)
    IMPLICIT NONE
    INTEGER :: UNIT_FILE_BASE = 21, UNIT_FILE_NEW = 22
    CHARACTER*(*) base_file, new_file
    INTEGER :: RESULT
    INTEGER :: i, iChan, j, STEP = 1
	real*8 Chan_B_REAL(29), Chan_N_REAL(29)
	INTEGER :: Chan_B_INT(29), Chan_N_INT(29)

    INTEGER IWARN, IERR
    COMMON /warn_err/ IWARN, IERR
    INTEGER BASE_Chan, NEW_Chan
    COMMON /channels/ BASE_Chan, NEW_Chan

    open (unit = UNIT_FILE_BASE, file = base_file, status='old', form='UNFORMATTED')
    open (unit = UNIT_FILE_NEW, file = new_file, status='old', form='UNFORMATTED')

    rewind(UNIT_FILE_BASE)
    rewind(UNIT_FILE_NEW)

    STEP = 1

    do while (.true.)

        do iChan = 1,BASE_Chan

		    read(UNIT_FILE_BASE, end=996) (Chan_B_INT(i),i=1,3),(Chan_B_REAL(i),i=4,25),&
	                 & Chan_B_INT(26), Chan_B_REAL(27), Chan_B_INT(28), Chan_B_INT(29)
		    read(UNIT_FILE_NEW, end=996) (Chan_N_INT(i),i=1,3),(Chan_N_REAL(i),i=4,25),&
	                 & Chan_N_INT(26), Chan_N_REAL(27), Chan_N_INT(28), Chan_N_INT(29)

		    do j = 1,29
		        if (j .ge. 4 .and. j .le. 21) then
		            RESULT = COMPARE(Chan_B_REAL(j),Chan_N_REAL(j))
		            if (RESULT .eq. 1) then
		                IWARN = IWARN + 1
		                write(6,8079) iChan,Chan_B_REAL(j),Chan_N_REAL(j),STEP
		            endif
		            if (RESULT .eq. -1) then
		                IERR = IERR + 1
		                write(6,8080) iChan,Chan_B_REAL(j),Chan_N_REAL(j),STEP
		                if (.NOT. (cont_after_error .eq. .TRUE.)) then
		                    write (6,8081)
		                    IERR = -1
		                    call END_FORT11
		                endif
		            if ( IERR .eq. 999 ) then
		                IERR_Ch = IERR - IERR_WC
		                write (6, 8084) IERR_WC, IERR_Ch
		                stop
		            endif
		            endif
		        endif

8079 format('WARNING for Channel',i3,' values',1P,e15.8,' and',1P,e15.8,' at time step ',i5)
8080 format('ERROR for Channel',i3,' values',1P,e15.8,' and',1P,e15.8,' at time step ',i5)
8081 format(/,'Stopping because a pair of values from the Channel data were unacceptably different')
8084 format(/, i3, ' errors in your Whole Core comparison.',/,&
            & i3, ' errors in your Channel comparison',/,'TOO MANY ERRORS. TERMINATING COMPARISON.',&
            & /,'See output for further details.')

	        enddo

	        !Skip to the next step for this channel
	    	if (iChan .eq. BASE_Chan) then
	    	    STEP = STEP + 1
	    	    read(UNIT_FILE_BASE, end=996)
	    	    read(UNIT_FILE_NEW, end=996)
		    endif

		enddo

    enddo
    996 continue

END SUBROUTINE Channel_Comp

!-------------------------------------------------------------------------------
!
!  SUBROUTINE END_FORT11
!
!-------------------------------------------------------------------------------
!!
!! End the CHANNEL.dat comparison
!!
!-------------------------------------------------------------------------------

SUBROUTINE END_FORT11
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



    if (IERR_WC .eq. 0 .and. IERR_Ch .eq. 0 .and. IWARN_WC .eq. 0 .and. &
    &   IWARN_Ch .eq. 0) then
        write (6,8082)
    elseif (IERR_WC .eq. 0 .and. IERR_Ch .eq. 0) then
        if (IWARN_WC .gt. 0) then
            write (6,8084) IWARN_WC
        endif
        if (IWARN_Ch .gt. 0) then
            write (6,8086) IWARN_Ch
        endif
    elseif (IERR_WC .gt. 0 .or. IERR_Ch .gt. 0) then
        if (IERR_WC .gt. 0) then
            write (6,8088) IERR_WC
        endif
        if (IERR_Ch .gt. 0) then
            write (6,8090) IERR_Ch
        endif
    endif

8082    format(/,'YOUR TWO CHANNEL.dat FILES COMPARE VERY WELL',/)
8084        format(/,i4 ' values in the Whole Core portion of your CHANNEL.dat files were different by more',&
            &/,'       than the allowable relative tolerance but less than the allowable absolute tolerance.',&
            &/,'       Please check the output file carefully.'/'       COMPARISON TENTATIVELY PASSED.',/)
8086        format(/,i4 ' values in the Channel portion of your CHANNEL.dat files were different by more',&
            &/,'       than the allowable relative tolerance but less than the allowable absolute tolerance.',&
            &/,'       Please check the output file carefully.'/'       COMPARISON TENTATIVELY PASSED.',/)
8088        format(/,i4 ' values in the Whole Core portion of your CHANNEL.dat files were unacceptably different.',&
            &/,'       Please check the output file carefully.'/'       COMPARISON FAILED',/)
8090        format(/,i4 ' values in the Channel portion of your CHANNEL.dat files were unacceptably different.',&
            &/,'       Please check the output file carefully.'/'       COMPARISON FAILED',/)

END SUBROUTINE END_FORT11

!-------------------------------------------------------------------------------
END MODULE
!===============================================================================
