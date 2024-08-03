#!/bin/bash

# ensure paths are correct irrespective from where user runs the script
scriptdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
basedir="$(dirname "$scriptdir")"

for TYPE in "act" "ppi_seed-NAcc" "nppi-dmn"; do
	for COPEINFO in "1 LargeGain" "3 LargeLoss" "5 Hit" "6 Miss" "7 Neut" "11 Salience" "12 Hit-Miss" "13 LG-SG" "14 LL-SL"; do
		set -- $COPEINFO
		COPENUM=$1
		COPENAME=$2
		
		images=(
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1001/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1003/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1004/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1006/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1009/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1010/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1011/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1012/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1013/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1015/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1016/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1019/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1021/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1242/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1243/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1244/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1245/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1247/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1248/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1249/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1255/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1276/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1282/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1286/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1294/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1300/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1301/L2_task-mid_model-4_type-${TYPE}.gfeat/cope${COPENUM}.feat/stats/zstat1.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1302/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-1303/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-3116/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-3125/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-3140/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-3143/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-3166/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-3167/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-3170/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-3173/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-3175/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-3176/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-3189/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-3190/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-3199/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-3200/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-3206/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-3212/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-3218/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		/ZPOOL/data/projects/istart-mid-clean/derivatives/fsl/sub-3220/L1_task-mid_model-4_type-${TYPE}_run-1_sm-5.feat/stats/zstat${COPENUM}.nii.gz
		)
	
		#for img in "${images[@]}"; do
		#	echo $img
		#done
		
		# Create merged zstat image
		merged_image=merged_type-${TYPE}_cnum-${COPENUM}_cname-${COPENAME}.nii.gz
		echo "fslmerge: ${merged_image}"
		fslmerge -t ${basedir}/derivatives/imaging_plots/${merged_image} "${images[@]}"	
		
		# Extract signal from zstat
		TASK=mid		
		for TYPE in "act" "type-nppi-dmn" "type-ppi_seed-NAcc"; do
			for ROI in "seed-VS" "target-vmpfc_bin"; do
			MASK=${maindir}/masks/${ROI}.nii.gz
				for COPEINFO in "01 LargeGain" "03 LargeLoss" "05 Hit" "06 Miss" "07 Neut" "11 Salience" "12 Hit-Miss" "13 LG-SG" "14 LL-SL"; do
					set -- $COPEINFO
					COPENUM=$1
					COPENAME=$2
			
					# Define vars & execute fslmeants command			
					INPUT=${basedir}/derivatives/imaging_plots/${merged_image}
					OUTPUT=${maindir}/derivatives/imaging_plots/zstat_type-${TYPE}_cnum-${COPENUM}_cname-${COPENAME}_mask-${ROI}.txt
					echo "Extracting zstat ${MASK} from ${TYPE}: ${COPENUM} ${COPENAME}"			
					fslmeants -i $INPUT -m $MASK -o $OUTPUT
			
		done
	done
done
	
	done
done
