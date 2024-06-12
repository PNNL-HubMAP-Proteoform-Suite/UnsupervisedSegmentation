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
  SCELLS <- supercells(the_rast, k = 100000, compactness = 1e-20)
  
  # Extract each region
  coordinates <- do.call(rbind, st_geometry(SCELLS)) %>%
    as_tibble() %>% 
    rename(Coords = V1) %>%
    mutate(Regions = 1:nrow(.)) %>%
    select(Coords, Regions) %>%
    group_by(Regions) %>%
    unnest(cols = c(Coords)) 
  coordinates <- data.frame(
    X = coordinates$Coords[,1],
    Y = coordinates$Coords[,2],
    Regions = coordinates$Regions
  ) %>%
    mutate(X = ifelse(X == 0, 1, X), Y = ifelse(Y == 0, 1, Y))
  
  # Convert regions to groups
  regions <- data.frame(Regions = unique(coordinates$Regions),
             Cluster = as.factor(kmeans(data.frame(SCELLS$lyr.1, SCELLS$lyr.2, SCELLS$lyr.3), centers = k)$cluster))
  regions <- left_join(regions, coordinates)
  
  # Fill a matrix with edges
  val_mat <- matrix(NA, ncol = max(coordinates$X), nrow = max(coordinates$Y))
  colnames(val_mat) <- 1:max(coordinates$X)
  row.names(val_mat) <- 1:max(coordinates$Y)
  fast_fill <- lapply(1:nrow(regions), function(row) {
    val_mat[regions$Y[row], regions$X[row]] <<- regions$Cluster[row]
  })
  rm(fast_fill)
  
  # Fill edges line by line 
  fast_fill <- lapply(1:nrow(val_mat), function(theRow) {
    closest = 1
    lapply(1:ncol(val_mat), function(theCol) {
      if (!is.na(val_mat[theRow, theCol])) {closest <<- val_mat[theRow, theCol]} else {
        val_mat[theRow, theCol] <<- closest
      }
    })
    return(NULL)
  })
  rm(fast_fill)
  
  # Make output matrix
  Smaller <- val_mat %>% data.frame()
  colnames(Smaller) <- paste0("V", gsub("X", "", colnames(Smaller)))
  
  # Write file
  end_string <- strsplit(in_path, "/") %>% unlist() %>% tail(1) %>% gsub(pattern = ".png", replacement = "_supercells.txt", fixed = T)
  fwrite(Smaller, file.path(out_path, end_string), quote = F, row.names = F, sep = "\t")
  
}



