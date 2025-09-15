# Unsupervised Semantic Segmentation of H&E Images

### Description

The goal of this work is to compare open-source unsupervised semantic segmentation algorithms for tissue feature unit identification in histology (H&E) images. Each algorithm had to have a way to set a number of clusters, and had to be semantic, which means it groups similar things together as opposed to distinguishing them, which is instance segmentation. In this work, we also tested the effect of blurring images on the performance of segmentation algorithms.

### Repo Structure 

| Folder | Files | Description |
|--------|-------|-------------|
|Algorithms/ | -- | Contains all scripts to run unsupervised segmentation algorithms |
|Algorithms/ | binning.py | A simple clustering approach that divides pixels into "k" bins |
|Algorithms/ | clara.R | The "Clustering Large Applications" method from the cluster package |
|Algorithms/ | kcc.R | The "K-Centroids Cluster" Analysis method from the flexclust package |
|Algorithms/ | kmeans.R | The K-Means clustering method from the stats package |
|Algorithms/ | multiotsu.py | The Multi-Otsu clustering method from the python package ski_image |
|Algorithms/ | pytorchtip.py | The implementation of the pytorch method from [here](https://github.com/kanezaki/pytorch-unsupervised-segmentation-tip/tree/master) |
|Algorithms/ | recolorize.R | The recolorize method from the recolorize package expanded to produce a set number of clusters |
|Algorithms/ | supercells.R | An implementation of the SLIC Superpixel method from the supercells package |
|Algorithms/ | Dimension_Reduction/ | Contains the R scripts with dimension reduction methods paired with clustering algorithms |
|Data_Processing/ | -- | Contains all scripts to process images |
|Data_Processing/ | Run_Python_Algorithms.ipynb | Run binning, multiotsu, and pytorchtip |
|Data_Processing/ | Run_R_Algorithms.R | Script to run the R functions in the Algorithms folder |
|Data_Processing/ | SVG_to_CSV.R | Converts QuPath SVG annotations to CSV, with a cluster assigned to each pixel |
|Figures/ | -- | Contains figures and tables |
|Metadata/ | -- | Contains all manual annotations of clusters |
|Performance/ | -- | Contains code and results from balanced accuracy calculations |
|Visualization/ | -- | Contains a function to convert the segmentation mask txt files to images in png format |

### Images

A trelliscope display of the dimension reductionm, blur, and full studies can be found [here](https://pnnl-hubmap-proteoform-suite.github.io/unsupervisedsegmentation.io/)

### Citation 

If any code or data is used, please cite [Link]
