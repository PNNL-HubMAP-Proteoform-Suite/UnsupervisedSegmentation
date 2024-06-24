# Script for convering pyimseg output into a mask

# Example command: python get_mask.py  --input "/Users/lewi052/Imaging_3D/Segmentation/Pytorch_output/KPMP_uS-X001Y006.png" --output_file /Users/lewi052/Imaging_3D/Segmentation/Pytorch_mask/KPMP_uS-X001Y006.csv 

import numpy as np
import matplotlib.pyplot as plt
from PIL import Image 
import argparse

def get_args():
    parser = argparse.ArgumentParser(description="A program to get the masking information from an imaghe")
    parser.add_argument("-i", "--input", help="eter image ", required=True)
    parser.add_argument("-o", "--output_file", help="output file")#, required=True)
    return parser.parse_args()

args=get_args()
input_file =args.input
output_file =args.output_file

image = Image.open(input_file, 'r')
data = image.getdata()
width, height = image.size
pixelList = []

for i in range(height):
    for j in range(width):
        stride = (width*i) + j
        pixelList.append((j, i, data[stride]))
        
rgb_val = [t[2] for t in pixelList]
                            #print(set(rgb_val))

replace_dict= {}
#make a dictionary to give a cluster number to each rgb value
for val in set(rgb_val):
    replace_dict[val]=None

count =0
for key,value in replace_dict.items():
    count+=1
    replace_dict[key] = count

                                                #print(counters)
#loop through all the rgb values and replace them with the corresponding cluster numbers
rgb_cluster = [replace_dict[i] for i in rgb_val]
                                                #print(
#write out masking information to file
with open(output_file, 'w') as mask:
    mask.write(f'x\ty\tcluster\n')
    for coord in range(len(pixelList)):
        mask.write(f'{pixelList[coord][0]}\t{pixelList[coord][1]}\t{rgb_cluster[coord]}\n')

#mask.close()
