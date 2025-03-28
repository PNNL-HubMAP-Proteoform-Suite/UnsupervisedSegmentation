from PIL import Image
import skimage as ski
import numpy as np
import pandas as pd
import os

def run_multi_otsu(in_path, k, out_path):
    '''
    Apply multi_otsu threshold clustering to an image

    Args:
        in_path (string): Path to the input image 
        k (integer): Number of clusters
        out_path (string): Path to folder where outputs will be stored
    '''

    # Read the image 
    img = Image.open(in_path)

    # Convert the image to black and white
    img = img.convert("L")

    # Make the image a numpy array
    img_array = np.array(img)

    # Define thresholds     
    thresholds = ski.filters.threshold_multiotsu(img_array, classes = k)

    # Determine regions
    regions = np.digitize(img_array, bins = thresholds)

    # Write image as pandas data.frame
    end_string = in_path.split("/")[-1].re(".png", "_multiotsu.txt")
    pd.DataFrame(regions).to_csv(os.path.join(out_path, end_string), sep = "\t")
    

