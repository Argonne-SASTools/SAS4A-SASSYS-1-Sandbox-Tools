#===============================================================================
#
# This file is part of the SAS4A-SASSYS-1-Sandbox-Tools.
#
# See LICENSE for full details.
#
#===============================================================================

#===============================================================================
#
# make
# make ALL
#
#   Compiles both the CHANNEL_COMP and PRIMAR4_COMP utilities with
#   the Intel Fortran Compiler
#
# make CHANNEL_COMP
#
#	Compiles the CHANNEL_COMP utility with the Intel Fortran Compiler
#
# make PRIMAR4_COMP
#
#	Compiles the PRIMAR4_COMP utility with the Intel Fortran Compiler
#
################################################################################

OBJECTCHANNEL = CHECKCHANNEL.o DIFF.o CHANNEL.o DRIVERCHANNEL.o
OBJECTPRIMAR =  CHECKPRIMAR.o DIFF.o PRIMAR4.o DRIVERPRIMAR.o

ALL: CHANNEL_COMP PRIMAR4_COMP

CHANNEL_COMP: $(OBJECTCHANNEL)
	ifort -o $@ $(OBJECTCHANNEL)

PRIMAR4_COMP: $(OBJECTPRIMAR)
	ifort -o $@ $(OBJECTPRIMAR)

%.o: %.F90
	ifort -c $<

CHANNEL.o: CHECKCHANNEL.o DIFF.o
PRIMAR4.o: CHECKPRIMAR.o DIFF.o
DRIVERCHANNEL.o: CHANNEL.o
DRIVERPRIMAR.o: PRIMAR4.o

clean:
	rm CHANNEL_COMP  \
	PRIMAR4_COMP  \
	*.mod *.o

