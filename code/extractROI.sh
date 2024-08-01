#!/usr/bin/env bash
# This code extracts ROI from L3 results.

# ensure paths are correct irrespective from where user runs the script
scriptdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
maindir="$(dirname "$scriptdir")"

# COPE Nums:

# 1.  Large Gain (USE)
# 2.  Small Gain
# 3.  Large Loss
# 4.  Small Loss
# 5.  Hit
# 6.  Miss
# 7.  Neutral (Neut)
# 8.  Gain-Loss
# 9.  Gain-Neutral
# 10. Loss-Neutral
# 11. Salience (USE)
# 12. Hit-Miss (USE)
# 13. Large Gain-Small Gain (LG-SG)
# 14. Large Loss-Small Loss (LL-SL)

TASK=mid

for TYPE in "type-ppi_seed-NAcc"; do # "act" "type-nppi-dmn" "type-ppi_seed-NAcc"; do
	for ROI in "target-vmpfc_bin"; do # "seed-VS" "target-vmpfc_bin"; do
		MASK=${maindir}/masks/${ROI}.nii.gz
		for COPEINFO in "01 LargeGain" "03 LargeLoss" "05 Hit" "06 Miss" "07 Neut" "11 Salience" "12 Hit-Miss" "13 LG-SG" "14 LL-SL"; do
			set -- $COPEINFO
			COPENUM=$1
			COPENAME=$2
			
			# Define vars & execute fslmeants command			
			INPUT=${maindir}/derivatives/fsl/L3_model-4A_task-mid_n48_flame1/L3_task-${TASK}_${TYPE}_cnum-${COPENUM}_cname-${COPENAME}_onegroup_INTs-4A.gfeat/cope1.feat/filtered_func_data.nii.gz
			OUTPUT=${maindir}/derivatives/imaging_plots/signal_type-${TYPE}_cnum-${COPENUM}_cname-${COPENAME}_mask-${ROI}.txt
			echo "Extracting ${MASK} from ${TYPE}: ${COPENUM} ${COPENAME}"			
			fslmeants -i $INPUT -m $MASK -o $OUTPUT
			
		done
	done
done

