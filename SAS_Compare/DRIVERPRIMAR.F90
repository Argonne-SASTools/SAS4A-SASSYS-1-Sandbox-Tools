!===============================================================================
!
! This file is part of the SAS4A-SASSYS-1-Sandbox-Tools.
!
! See LICENSE for full details.
!
!===============================================================================

PROGRAM DRIVER

!The utility is intended to compare PRIMAR4.dat files that are either
!(a) from slightly different SAS4A/SASSYS-1 input files or (b) from
!the same input file but with different versions of SAS4A/SASSYS-1.
!The utility will compare PRIMAR4.dat files from these cases to determine
!if changes to the input or source have created significant differences
!in the results.

!The command requires four arguments:
!PRIMAR4_COMP.x file_1 file_2 max_rel_tol max_abs_tol [-c]

!file_1 and file_2 must be PRIMAR4.dat files for the utility to function
!properly. It is assumed that the same SAS4A/SASSYS-1 input was used
!to generate the two PRIMAR4.dat files. max_rel_tol is the user-defined
!maximum relative tolerance and max_abs_tol is the user-defined maximum
!absolute tolerance. The -c option allows the user to continue with
!the comparison after a pair of values are found to be unacceptably
!different.

!This utility will scan through both PRIMAR4.dat files and compare each
!pair of values. If the relative difference between the two values
!is less than the user-defined maximum relative tolerance, the values
!are considered acceptably similar and the comparison of the two
!PRIMAR4.dat files will continue.

!If, however, the relative difference between the two values is larger
!than the user-defined maximum relative tolerance, the values are
!considered different. If the absolute difference between the two
!values is larger than the user-defined maximum absolute tolerance,
!the values are considered unacceptably different and an error is
!generated. An example of two unacceptably different values would be
!temperatures of 588.8 K and 589.7 K. In this example, a change to
!the input file or SAS4A/SASSYS-1 source code has made a significant
!difference in the results that the user should be made aware of.

!If the relative difference is larger than the user-defined maximum
!relative tolerance, but the absolute difference is less than the
!user-defined maximum absolute tolerance, the values are considered
!acceptably different and a warning is generated. In general values
!printed to PRIMAR4.dat are large enough where the absolute tolerance
!should not be a factor. However, an example of two acceptably
!different values would be 1.342E-8 and 1.488E-8. Relative to each
!other these two values are significantly different. But the absolute
!difference between these two values is small enough that the final
!SAS4A/SASSYS-1 results will be not be significantly different.

!Note that suggestions are not made for appropriate maximum allowable
!relative or absolute tolerances. These are for the user to determine.

!By default the utility will stop after two values are found to be
!unacceptably different. However, with the -c option, the utility will
!continue until the end of the PRIMAR4.dat files or 999 pairs have been
!found unacceptably different.


    USE PRIMAR4

    implicit none
    integer, parameter :: stringLengthArgsMax = 256, numberOfCommandLineArgs=4
    CHARACTER(len=stringLengthArgsMax) :: args(numberOfCommandLineArgs)
    INTEGER NUM_ARGS, iarg, stringLenghArgs(numberOfCommandLineArgs), istat
    INTRINSIC COMMAND_ARGUMENT_COUNT, get_command_argument

    INTEGER :: IWARN = 0, IERR = 0
    COMMON /warn_err/ IWARN, IERR

    ! Get the number of  command line arguments that the user provided:
    NUM_ARGS = COMMAND_ARGUMENT_COUNT ()

    ! Check the number of command line arguments.
    ! Apparently a latter 5th argument (-c) is allowed, but don't advertise it here.
    if(NUM_ARGS /= numberOfCommandLineArgs .and. NUM_ARGS /= numberOfCommandLineArgs+1) then
    	write(*,'(a,i6,a,i6)') &
    		'Error: Illegal command line syntax. Expected ', numberOfCommandLineArgs, &
    		' arguments, but found ', NUM_ARGS
    	write(*,'(a)') &
    		'Syntax: PRIMAR4_COMP.x file_1 file_2 max_rel_tol max_abs_tol'
    	stop 'Errors'
    endif

    ! Get the first 4 command line arguments
    do iarg = 1, numberOfCommandLineArgs
    	call get_command_argument(iarg, args(iarg), stringLenghArgs(iarg), istat)
    	! check status returned from get_command_argument.
    	if( istat /= 0) then
    		if(istat > 0) then
    			write(*,'(a,i6,a,i6)') 'Error: Retrieving command line argument ', iarg, &
    				'. Returned status = ', istat
    		else
    			write(*,'(a)') 'Error: Command line argument truncated. Likely a string length issue.'
    		endif
    		stop 'Error: Getting command line arguments'
    	endif
    enddo ! iarg

    call INIT15(args(1),args(2),args(3),args(4),NUM_ARGS)
    call FORT15_COMPARE(args(1),args(2))
    call END_FORT15

END PROGRAM DRIVER
