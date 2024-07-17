# Unsupervised Semantic Segmentation of H&E Images

### Description

The goal of this work is to characterize unsupervised semantic segmentation algorithms for tissue feature unit identification in histology (H&E) images. Each algorithm had to have a way to set a number of clusters, and had to be semantic, which means groups like things as opposed to distinguishing them, which is instance segmentation. In this work, we also tested the effect of blurring images on the performance of segmentation algorithms.

### Repo Structure 

| Folder | Files | Description |
|--------|-------|-------------|
|Algorithms/ | -- | Contains all scripts to run unsupervised segmentation algorithms |
|Algorithms/ | clara.R | The "Clustering Large Applications" method from the cluster package |
|Algorithms/ | kcc.R | The "K-Centroids Cluster" Analysis method from the flexclust package |
|Algorithms/ | kmeans.R | The K-Means clustering method from the stas package |
|Algorithms/ | pyimseg.py | The imsegm method from here | 
|Algorithms/ | pytorch.py | The implementation of the pytorch methods from here |
|Algorithms/ | recolorize.R | The recolorize method from the recolorize package expanded to produce a set number of clusters |
|Algorithms/ | supercells.R | An implementation of the SLIC Superpixel method from the supercells package |
|Data_Processing/ | -- | Contains all scripts to process images |
|Data_Processing/ | Run_R_Algorithms.R | Script to run the R functions in the Algorithms folder |
|Data_Processing/ | SVG_to_CSV.R | Converts QuPath SVG annotations to CSV, with a cluster assigned to each pixel |
|Figures/ | -- | Contains figures and tables |
|Metadata/ | -- | Contains all manual annotations of clusters |
|Performance/ | -- | Contains code and results from balanced accuracy calculations |
|Performance/ | BA_Performance.R | Takes the files with counts of positives and negatives and calculate balanced accuracy |
|Performance/ | Blur_Counts/ | Contains the the positive and negative counts for the blur study |
|Performance/ | Full_Counts/ | Contains the positive and negative counts for the full study |
|Performance/ | Truth_Counts/ | Generates the count files with the number of true positives, true negatives, false positives, and false negatives using the predicted and manual annotations of clusters |
|Visualization/ | -- | Contains a function to convert the segmentation mask txt files to images in png format |

### Images

A trelliscope display of the blur and full studies can be found here: 

### Citation 

If any code or data is used, please cite [Link]
