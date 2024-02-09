library(SuperpixelImageSegmentation) # required for slic and slico
library(OpenImageR)                  # used by the above package
library(ClusterR)
library(tidyverse)
library(glue)


main = function(image_path){
  
  image_name = substr(image_path, 92, 107)
  
  im = OpenImageR::readImage(image_path)
  
  init = Image_Segmentation$new()
  
  # Generate segmentation matrices
  slic = init$spixel_segmentation(input_image = im, #pretrained forest structure model
                                  kmeans_method = "",
                                  superpixel = 35, 
                                  AP_data = TRUE,
                                  use_median = FALSE, 
                                  sim_wL = 3, 
                                  sim_wA = 1, 
                                  sim_wB = 1,
                                  sim_color_radius = 1, 
                                  verbose = TRUE, 
                                  return_labels_2_dimensionsional = TRUE)
  
  slicKM = init$spixel_segmentation(input_image = im, #pretrained forest structure model
                                    kmeans_method = "kmeans",
                                    superpixel = 35,
                                    AP_data = TRUE,
                                    use_median = FALSE,
                                    sim_wL = 3,
                                    sim_wA = 1,
                                    sim_wB = 1,
                                    sim_color_radius = 1,
                                    verbose = TRUE,
                                    return_labels_2_dimensionsional = TRUE)

  slico = init$spixel_segmentation(input_image = im, #pretrained forest structure model
                                   method = "slico",
                                   kmeans_method = "",
                                   superpixel = 50, 
                                   AP_data = TRUE,
                                   use_median = FALSE, 
                                   sim_wL = 3, 
                                   sim_wA = 10, 
                                   sim_wB = 10,
                                   sim_color_radius = 1, 
                                   verbose = TRUE, 
                                   return_labels_2_dimensionsional = TRUE)

  slicoKM = init$spixel_segmentation(input_image = im, #pretrained forest structure model
                                     method = "slico",
                                     kmeans_method = "kmeans",
                                     superpixel = 50, 
                                     AP_data = TRUE,
                                     use_median = FALSE, 
                                     sim_wL = 3, 
                                     sim_wA = 10, 
                                     sim_wB = 10,
                                     sim_color_radius = 1, 
                                     verbose = TRUE, 
                                     return_labels_2_dimensionsional = TRUE)
  
  # Save CSV files to their respective directories
  slicDF = image_dataframe(slic$spix_labels)
  write.csv(slicDF, glue("/file/path/to/png"), row.names = FALSE)
  
  slicoDF = image_dataframe(slico$spix_labels)
  write.csv(slicoDF, glue("/file/path/to/png"), row.names = FALSE)

  slicKMDF = image_dataframe(slicKM$spix_labels)
  write.csv(slicKMDF, glue("/file/path/to/png"), row.names = FALSE)

  slicoKMDF = image_dataframe(slicoKM$spix_labels)
  write.csv(slicoKMDF, glue("/file/path/to/png"), row.names = FALSE)

}


directory = "/file/path/to/png/folder"
setwd(directory)

file.png =
  list.files(
    directory,
    pattern = "*.png",
    ignore.case = TRUE,
    full.names = TRUE)

# main(file.png[1])
for (path in file.png) {
  main(path)
}

