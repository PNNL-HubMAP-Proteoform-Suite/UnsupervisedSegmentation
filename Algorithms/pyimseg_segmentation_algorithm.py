#!/usr/bin/env python

# import packages
import os, sys, glob, time
import numpy as np
from PIL import Image
import matplotlib.pyplot as plt
from skimage.segmentation import mark_boundaries
sys.path += [os.path.abspath('.'), os.path.abspath('..')] # Add path to root
import imsegm.utilities.data_io as tl_data
import imsegm.pipelines as segm_pipe
import pandas as pd

# make pyimseg algorithm a function that processes multiple images at a time
def seg(directory, clust_num):
    for hist_img in os.listdir(directory):
        path_img = directory + hist_img
        img = np.array(Image.open(path_img))

        FIG_SIZE = (8. * np.array(img.shape[:2]) / np.max(img.shape))[::-1]
        _= plt.imshow(img)
        print(FIG_SIZE)

        nb_classes = int(clust_num) # this sets cluster number
        sp_size = 50 # this sets limit for cluster size
        sp_regul = 0.15 # this sets cluster border smoothness
        dict_features = {'color': ['mean', 'std', 'median']}

        model, _ = segm_pipe.estim_model_classes_group([img], nb_classes, sp_size=sp_size, sp_regul=sp_regul, 
                                                dict_features=dict_features, pca_coef=None, model_type='GMM')

        dict_debug = {}
        seg, _ = segm_pipe.segment_color2d_slic_features_model_graphcut(img, model, sp_size=sp_size, sp_regul=sp_regul, 
                        dict_features=dict_features, gc_regul=5., gc_edge_type='color', debug_visual=dict_debug)

        # create segmentation png
        fig = plt.figure(figsize=FIG_SIZE)
        plt.imshow(img)
        plt.imshow(seg, alpha=0.6, cmap=plt.cm.jet)
        plt.contour(seg, levels=np.unique(seg), colors='w')
        plt.xticks([])
        plt.yticks([])
        plt.savefig("seg_" + hist_img[:-4] + '_' + clust_num + '.png') # file name can be changed as user sees fit

        # saves segmentation mask in a dataframe
        df = pd.DataFrame(np.hstack((np.indices(seg.shape).reshape(2, seg.size).T,\
                        seg.reshape(-1, 1))), columns=['y_pos', 'x_pos', 'cluster'])
        df['y_pos'] += 1
        df['x_pos'] += 1

        # reorder columns to x, y, cluster 
        df = df[['x_pos', 'y_pos', 'cluster']]

        df.to_csv("mask_" + hist_img[:-4] + '_' + clust_num + '.csv', sep=',', index=False) # file name can be changed as user sees fit