!===============================================================================
!
! This file is part of the SAS4A-SASSYS-1-Sandbox-Tools.
!
! See LICENSE for full details.
!
!===============================================================================

!===============================================================================
!   MODULE CHECK                                                                $
!===============================================================================
!===============================================================================

!===============================================================================

MODULE CHECKCHANNEL
    IMPLICIT NONE

	! Module Data:
	! <none>

!===============================================================================
CONTAINS

!-------------------------------------------------------------------------------
!
!  SUBROUTINE CheckInputArgs
!
!-------------------------------------------------------------------------------
!!
!! Confirms that the first two arguments given are both fort.11 files
!!
!-------------------------------------------------------------------------------

SUBROUTINE CheckInputArgs(base_file,new_file)
    IMPLICIT NONE
    CHARACTER*(*) base_file, new_file

    INTEGER Len_BF, Len_NF
    INTEGER IWARN, IERR
    COMMON /warn_err/ IWARN, IERR

    ! Find the string length of both file names
    Len_BF = len_trim(base_file)
    Len_NF = len_trim(new_file)

    if (base_file(Len_BF-11:Len_BF) .ne. 'CHANNEL.dat') then
        IERR = IERR + 1
        write (6,8040) trim(base_file)
    endif

    if (new_file(Len_NF-11:Len_NF) .ne. 'CHANNEL.dat') then
        IERR = IERR + 1
        write (6,8050) trim(new_file)
    endif

8040 format(/,'ERROR: Your base file ', A, ' is not a CHANNEL.dat file',/)
8050 format(/,'ERROR: Your new file ', A, ' is not a CHANNEL.dat file',/)

END SUBROUTINE CheckInputArgs

!-------------------------------------------------------------------------------
!
!  SUBROUTINE CheckNumChannels_Time
!
!-------------------------------------------------------------------------------
!!
!! Confirms that both files have the same number of channels and time steps
!!
!-------------------------------------------------------------------------------

SUBROUTINE CheckNumChannels_Time(base_file,new_file)
    IMPLICIT NONE
    CHARACTER*(*) base_file, new_file
    INTEGER BASE_Time_Steps, NEW_Time_Steps
    INTEGER :: UNIT_FILE_BASE = 21, UNIT_FILE_NEW = 22

    INTEGER IWARN, IERR
    COMMON /warn_err/ IWARN, IERR
    INTEGER BASE_Chan, NEW_Chan
    COMMON /channels/ BASE_Chan, NEW_Chan

    open (unit = UNIT_FILE_BASE, file = base_file, status='old', form='UNFORMATTED')
    open (unit = UNIT_FILE_NEW, file = new_file, status='old', form='UNFORMATTED')

    ! First check that the two fort.11 files have the same number of channels

    BASE_Chan = CountChannels(UNIT_FILE_BASE)
    NEW_Chan = CountChannels(UNIT_FILE_NEW)

    if (BASE_Chan .ne. NEW_Chan) then
    	IERR = IERR + 1
    	write (6,8040) NEW_Chan, BASE_Chan
    endif

    if (BASE_Chan .le. 0) then
    	IERR = IERR + 1
    	write (6,8041) BASE_Chan
    endif

    if (NEW_Chan .le. 0) then
    	IERR = IERR + 1
    	write (6,8042) NEW_Chan
    endif

    ! Second check that the two fort.11 files have the same number of time steps

    BASE_Time_Steps = COUNT_TIME_STEPS(UNIT_FILE_BASE, BASE_Chan)
    NEW_Time_Steps =  COUNT_TIME_STEPS(UNIT_FILE_NEW, NEW_Chan)

    if (BASE_Time_Steps .ne. NEW_Time_Steps) then
    	IWARN = IWARN + 1
    	write (6,8043) BASE_Time_Steps, NEW_Time_Steps
    	BASE_Time_Steps = min(BASE_Time_Steps, NEW_Time_Steps)
    	NEW_Time_Steps = min(BASE_Time_Steps, NEW_Time_Steps)
    endif

    if (BASE_Time_Steps .le. 0) then
    	IERR = IERR + 1
    	write (6,8044) BASE_Time_Steps
    endif

    if (NEW_Time_Steps .le. 0) then
    	IERR = IERR + 1
    	write (6,8045) NEW_Time_Steps
    endif

8040 format('ERROR: The number of channels in new file (', i3, &
    & ') does not equal the number of channels in your original file (', i3, ').',/)
8041 format('ERROR: Found', i3, ' channels in your base CHANNEL.dat file',/)
8042 format('ERROR: Found', i3, ' channels in your new CHANNEL.dat file',/)
8043 format(/,'WARNING: The number of time steps in your original file (', i3, &
    & ') does not equal the number of time steps in your base file (', i3, ').',/,&
    & '         Using the lesser number of time steps for the comparison'/)
8044 format(/,'ERROR: Found', i3, ' channels in your base CHANNEL.dat file',/)
8045 format(/,'ERROR: Found', i3, ' channels in your new CHANNEL.dat file',/)

END SUBROUTINE CheckNumChannels_Time

!-------------------------------------------------------------------------------
!
!  FUNCTION CountChannels
!
!-------------------------------------------------------------------------------
!!
!! Count the number of channels in a CHANNEL.dat file
!!
!-------------------------------------------------------------------------------

INTEGER FUNCTION CountChannels(inUnit)

	implicit none

	INTEGER iStep0, iStep, inUnit, theCount

    theCount = 0
	CountChannels = -1
	rewind(inUnit)
	read(inUnit, end=999) iStep0
	iStep = iStep0
	do while (iStep .eq. iStep0)
		theCount = theCount + 1
		read(inUnit, end=999) iStep
	enddo
	rewind(inUnit)
	CountChannels = theCount - 1

    999 continue
	return

END FUNCTION CountChannels

!-------------------------------------------------------------------------------
!
!  FUNCTION CountChannels
!
!-------------------------------------------------------------------------------
!!
!! Count the number of channels in a CHANNEL.dat file
!!
!-------------------------------------------------------------------------------

INTEGER function COUNT_TIME_STEPS(inUnit, nChan)

    implicit none

    INTEGER inUnit, ISTEP, index, nChan

    COUNT_TIME_STEPS = 0

    rewind(inUnit)

    do while (.true.)
        read(inUnit, end=998) ISTEP
        do index = 1, nChan
            read(inUnit, end=998)
        enddo
        COUNT_TIME_STEPS = COUNT_TIME_STEPS + 1
    enddo

    998 continue
    return

END FUNCTION COUNT_TIME_STEPS

!-------------------------------------------------------------------------------
END MODULE
!===============================================================================
