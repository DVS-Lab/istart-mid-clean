#!/bin/bash

# ensure paths are correct irrespective from where user runs the script
scriptdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
basedir="$(dirname "$scriptdir")"

# Extract signal from zstat
TASK=mid		

for TYPE in "act" "ppi_seed-NAcc" "nppi-dmn"; do
	for ROI in "seed-VS" "target-vmpfc_bin"; do
		if [[ ("${TYPE}" == "act"  &&  "${ROI}" == "seed-VS") || ("${TYPE}" == "ppi_seed-NAcc" && "${ROI}" == "target-vmpfc_bin") || ("${TYPE}" == "nppi-dmn" && "${ROI}" == "seed-VS") ]]; then
			MASK=${basedir}/masks/${ROI}.nii.gz
			for COPEINFO in "1 LargeGain" "3 LargeLoss" "7 Neut" "11 Salience" "12 Hit-Miss" "13 LG-SG" "14 LL-SL"; do
				set -- $COPEINFO
				COPENUM=$1
				COPENAME=$2
			
				# Define vars & execute fslmeants command			
				INPUT=${basedir}/derivatives/imaging_plots/merged_type-${TYPE}_cnum-${COPENUM}_cname-${COPENAME}.nii.gz
				if [ -e ${INPUT} ]; then
					OUTPUT=${basedir}/derivatives/imaging_plots/zstat_type-${TYPE}_cnum-${COPENUM}_cname-${COPENAME}_mask-${ROI}.txt
					echo "Extracting zstat ${MASK} from ${TYPE}: ${COPENUM} ${COPENAME}"	
					echo "fslmeants: input = ${INPUT}, mask = ${MASK}, output = ${OUTPUT}"		
					fslmeants -i $INPUT -m $MASK -o $OUTPUT
				else
					echo "skipping ${INPUT}"
				fi
			done
		else
			echo "Skipping extraction for TYPE=${TYPE}, ROI=${ROI}"
		fi
	done
done