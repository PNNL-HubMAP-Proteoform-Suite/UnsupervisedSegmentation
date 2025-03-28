from PIL import Image
import numpy as np
import pandas as pd
import os

def run_binning(in_path, k, out_path):
    '''
    Apply bin threshold clustering to an image

    Args:
        in_path (string): Path to the input image 
        k (integer): Number of clusters
        out_path (string): Path to folder where outputs will be stored
    '''

    # Read the image 
    img = Image.open(in_path)

    # Convert the image to black and white
    img = img.convert("L")

    # Make a pandas dataframe of the image
    image_array = np.array(img)

    # Calculate thresholds
    minimum = np.min(image_array)
    maximum = np.max(image_array)
    diff = maximum - minimum
    multiplier = int(diff / k)
    thresholds = [multiplier * (x + 1) + minimum for x in range(k)]

    # Initialize a dataframe to hold clusters
    clusters = np.zeros(image_array.shape) + 1

    # Get indices where condition happens
    for thresh in thresholds[:-1]:
        index1, index2 = np.where(image_array > thresh)
        clusters[index1, index2] += 1

    # Write image as pandas data.frame. Reverse image
    end_string = in_path.split("/")[-1].replace(".png", "_binning.txt")
    pd.DataFrame(clusters.astype(int)).iloc[::-1].reset_index(drop = True).to_csv(os.path.join(out_path, end_string), sep = "\t")
    return None