#!/bin/bash
#===============================================================================
#
# This file is part of the SAS4A-SASSYS-1-Sandbox-Tools.
#
# See LICENSE for full details.
#
#===============================================================================

for out; do

	txt="`dirname $out`/`basename $out .out`.txt"

	echo $out
	echo $txt

	sed -E \
		-e '/^(SAS4A\/SASSYS-1|CHANNEL)/s|[0-9]{8}  .{8}    (PAGE *[0-9]+)$|DATE: n/a TIME: n/a   \1|' \
		-e '/^JOB: /s|JOB: .{20} |JOB: n/a                  |' \
		-e '/^JOB: /s|USER: .*|USER: n/a|' \
		-e '/ADDRESS OF/s|= *[0-9]*$|= n/a|' \
		-e '/OFFSET/s|= *[0-9]* WORDS.$|= n/a|' \
		-e '/SAS4A\/SASSYS-1 TIMING INFORMATION/,/TOTAL/d' \
	"$out" > "$txt"

done
