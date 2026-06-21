# runme.R
# Quick script to test out the VisionSimR package functions!
# It runs a few examples and then launches the Shiny app automatically.

library(VisionSimR)
library(imager)

# We'll just use the built-in parrots image from the imager package
test_image <- system.file("extdata/parrots.png", package = "imager")

cat("\n--- 1. Testing simulate_vision() ---\n")
# Simulating a mix of myopia and astigmatism for a 30-year-old.
# This calculates the defocus and applies the 1D directional blur.
res <- simulate_vision(
  image_path = test_image,
  condition = "astigmatism",
  Rx_D = -1.0,           
  cylinder_D = -1.5,     
  axis_deg = 90,         
  age = 30,              
  show_plot = TRUE       
)
Sys.sleep(1.5)

cat("\n--- 2. Testing show_svd_ranks() ---\n")
# Visualising the SVD rank truncation math behind the blur engine.
# We'll see the image at ranks 5, 20, 50, and 100.
show_svd_ranks(
  image_path = test_image,
  ranks = c(5, 20, 50, 100)
)
Sys.sleep(1.5)

cat("\n--- 3. Testing apply_cvd() ---\n")
# Let's apply a colour vision deficiency filter directly (Deuteranopia)
img <- load.image(test_image)
img_array <- as.array(img)

cvd_array <- apply_cvd(img_array, type = "deuteranopia")
cvd_image <- as.cimg(cvd_array)

par(mfrow = c(1, 2))
plot(img, main = "Original", axes = FALSE)
plot(cvd_image, main = "Deuteranopia Simulation", axes = FALSE)
par(mfrow = c(1, 1))

Sys.sleep(1.5)

cat("\n--- 4. Launching the Shiny Dashboard! ---\n")
# Starting up the UI
shiny::runApp(system.file("shiny", package = "VisionSimR"))
