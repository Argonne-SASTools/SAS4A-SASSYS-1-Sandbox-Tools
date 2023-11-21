#!/bin/bash
#===============================================================================
#
# This file is part of the SAS4A-SASSYS-1-Sandbox-Tools.
#
# See LICENSE for full details.
#
#===============================================================================

#
# Remove pagination and other garbage from SAS4A/SASSYS-1 output file.
# This may be useful for running diff to compare two output files.
# Assumes that the current user ($USER output) generated the output file.
# Reads standard input and prints standard ouput.
#

grep -v ^SAS - | grep -v ^CHANNEL | \
  grep -v $USER | grep -v "ELAPSED TIME" | \
  grep -v $HOSTNAME | \
  grep -v "TIME REQUIRED " | \
  grep -v "===================" | \
  grep -v "source"
