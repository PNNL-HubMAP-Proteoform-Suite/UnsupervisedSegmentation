#' @param in_path Path to the input image 
#' @param k Number of clusters 
#' @param out_path Path to place the segmented image data.frame
#' @param blur A boolean (TRUE/FALSE) to indicate whether the image should be blurred or not 
apply_supercells <- function(in_path, k, out_path, blur) {
  
  # Image processing libraries
  library(flexclust)
  library(magick)
  library(png)
  
  # Data processing libraries 
  library(tidyverse)
  library(data.table)
  library(uuid)
  
  # Load clustering library
  library(sf)
  library(terra)
  library(supercells)
  library(recolorize)
  
  # If blur, make and read the blurred image 
  if (blur) {
    
    # Generate unique name
    id <- UUIDgenerate()
    
    # Blur the image
    img <- image_read(in_path)
    blurred <- image_blur(img, radius = 100, sigma = 10)
    path <- tempdir()
    image_write(blurred, file.path(path, id), format = "png")
    
    # Read blurred image
    imgRead <- readPNG(file.path(path, id))
    
  } else {
    imgRead <- readPNG(in_path)
  }
  
  # Make raster
  the_rast <- terra::rast(imgRead)
  
  # Run supercells and cluster
  SCELLS <- supercells(the_rast, k = 100000, compactness = 1e-20) %>%
    mutate(
      Cluster = as.factor(kmeans(data.frame(lyr.1, lyr.2, lyr.3), centers = k)$cluster)
    )
  
  # Make a plot object and save
  png("temp_file.png", height = dim(imgRead)[1], width = dim(imgRead)[2], bg = "transparent")
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), xpd = NA)
  plot(sf::st_geometry(SCELLS), col = SCELLS$Cluster, border = SCELLS$Cluster)
  dev.off()

  # Read in the plot object
  imgRead <- readPNG("temp_file.png")
  unlink("temp_file.png")
  clusters <- recolorize::recolorize(imgRead, bins = k)$pixel_assignments
  
  # Make output matrix
  Smaller <- clusters %>% data.frame()
  colnames(Smaller) <- paste0("V", gsub("X", "", colnames(Smaller)))
  Smaller <- Smaller[nrow(Smaller):1, ]
  
  # Write file
  end_string <- strsplit(in_path, "/") %>% unlist() %>% tail(1) %>% gsub(pattern = ".png", replacement = "_supercells.txt", fixed = T)
  fwrite(Smaller, file.path(out_path, end_string), quote = F, row.names = F, sep = "\t")
  
}