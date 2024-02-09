#!/bin/bash

#SBATCH --account=bgmp
#SBATCH --partition=compute
#SBATCH --cpus-per-task=12
#SBATCH --mem=64G

conda activate iSeg

/usr/bin/time -v python demo2.py --input /projects/bgmp/shared/groups/2023/mass-spec/shared/Data/MyAnnotations/KPMP_uS-X002Y011.png --minLabels 3 --visualize 0 --output_image KPMP_uS-X002Y011.3_out.png
