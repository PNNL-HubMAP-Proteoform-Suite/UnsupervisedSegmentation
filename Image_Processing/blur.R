library(magick)

blur <- function(in_path, out_path, radius = 100, sigma = 5) {
  img <- image_read(in_path)
  blurred <- image_blur(img, radius = radius, sigma = sigma)
  image_write(blurred, out_path)
}

blur(
  in_path = "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney/Original/KPMP_uS-X002Y010.png",
  out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney/Blur/KPMP_uS-X002Y010_blur.png",
  sigma = 10
)
