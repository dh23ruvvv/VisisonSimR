# simulate.R
# Main simulation function + SVD rank visualizer

#' Simulate Vision Impairment on Images
#'
#' Takes an input image and applies physics-based optical blurring and
#' color transformations to simulate how the image appears to a person with
#' a specific refractive error or color vision deficiency.
#'
#' The simulation calculates the required optical power and compares it against
#' the eye's effective power (based on the provided Rx_D prescription).
#' The resulting defocus is converted into a physical blur radius.
#'
#' Accommodation amplitude is computed from age using Hofstetter's formula
#' (1950): Acc_max = max(0, 18.5 - 0.3 * age).
#'
#' Astigmatism is modelled via the Conoid of Sturm: two orthogonal meridians
#' with different focal powers, producing directional blur via 1D SVD
#' rank truncation.
#'
#' Blurring uses SVD truncation (Eckart-Young theorem). Each image channel
#' is a matrix, and lower-rank approximations simulate the loss of
#' high-frequency spatial detail.
#'
#' @param image_path Character. Path to a JPEG, PNG, BMP, or TIFF image file.
#' @param condition Character. One of "myopia", "hyperopia", "presbyopia",
#'   or "astigmatism". Default is "myopia".
#' @param Rx_D Numeric. Spectacle prescription in diopters (spherical).
#'   Negative for myopia, positive for hyperopia. Default is 0.
#' @param age Numeric. Patient age in years (10-80). Used for Hofstetter
#'   accommodation. Default is 25.
#' @param cylinder_D Numeric. Cylinder power in diopters for astigmatism
#'   (typically negative). Default is 0.
#' @param axis_deg Numeric. Cylinder axis in degrees (0-180). Default is 180.
#' @param distance_m Numeric. Viewing distance in metres. Must be > 0.
#' @param scene_width_m Numeric. Physical width the image represents (metres).
#' @param pupil_mm Numeric. Pupil diameter in mm. Default 4.
#' @param max_accommodation_D Numeric or NULL. If NULL (default), computed
#'   from age. If numeric, overrides the age-based calculation.
#' @param color_deficiency Character or NULL. One of "protanopia",
#'   "deuteranopia", or "tritanopia". Default is NULL.
#' @param max_dim Integer. Max pixel dimension for resizing before SVD.
#' @param show_plot Logical. If TRUE, shows a side-by-side comparison plot.
#'
#' @return A list with components: original, simulated (cimg objects),
#'   defocus_D, blur_px, far_point_m, near_point_m, age, accommodation_D,
#'   cylinder_D, axis_deg.
#'
#' @import imager
#' @importFrom graphics par plot abline
#' @importFrom tools toTitleCase
#' @export
#'
#' @examples
#' \dontrun{
#' img <- system.file("extdata/parrots.png", package = "imager")
#'
#' # Myopia at age 30
#' res <- simulate_vision(img, condition = "myopia", Rx_D = -2.5, age = 30)
#'
#' # Presbyopia at age 55
#' res <- simulate_vision(img, condition = "presbyopia", Rx_D = 0,
#'                        age = 55, distance_m = 0.3)
#'
#' # Astigmatism with cylinder -1.5 D at axis 90
#' res <- simulate_vision(img, condition = "astigmatism",
#'                        Rx_D = -1.0, cylinder_D = -1.5, axis_deg = 90)
#'
#' # Hyperopia + protanopia at age 65
#' res <- simulate_vision(img, condition = "hyperopia",
#'                        Rx_D = +3.0, age = 65,
#'                        color_deficiency = "protanopia")
#' }
simulate_vision <- function(image_path,
                            condition = "myopia",
                            Rx_D = 0,
                            age = 25,
                            cylinder_D = 0,
                            axis_deg = 180,
                            distance_m = 2,
                            scene_width_m = 5,
                            pupil_mm = 4,
                            max_accommodation_D = NULL,
                            color_deficiency = NULL,
                            max_dim = 400L,
                            show_plot = TRUE) {

  stopifnot(file.exists(image_path))
  stopifnot(distance_m > 0, scene_width_m > 0, pupil_mm > 0)
  stopifnot(age >= 1, age <= 120)
  stopifnot(axis_deg >= 0, axis_deg <= 180)

  condition <- tolower(trimws(condition))
  valid_conditions <- c("myopia", "hyperopia", "presbyopia", "astigmatism")
  if (!condition %in% valid_conditions) stop("Invalid condition")

  if (!is.null(color_deficiency)) {
    color_deficiency <- tolower(trimws(color_deficiency))
    if (!color_deficiency %in% names(CVD_MATRICES)) stop("Invalid color deficiency")
  }

  # Accommodation from Hofstetter (or manual override)
  if (is.null(max_accommodation_D)) {
    max_accommodation_D <- hofstetter_accommodation(age)
  }

  # Load and resize image
  img <- load.image(image_path)
  if (spectrum(img) == 4L) img <- rm.alpha(img)
  if (spectrum(img) == 1L) img <- add.color(img)

  W <- width(img)
  H <- height(img)
  scale <- min(1.0, max_dim / max(W, H))
  if (scale < 1.0) {
    img <- resize(img, round(W * scale), round(H * scale), interpolation_type = 5L)
  }

  W_px <- width(img)
  H_px <- height(img)
  img_arr <- as.array(img)

  # Calculate optical defocus
  P_req <- required_power(distance_m)
  P_eye <- eye_power(Rx_D)
  P_eff <- effective_power(P_eye, P_req, max_accommodation_D)
  D_def <- compute_defocus(P_eff, P_req)

  r_defocus <- blur_radius_px(D_def, pupil_mm, distance_m, scene_width_m, W_px)
  r_floor <- floor_blur_px(distance_m, scene_width_m, W_px, pupil_mm)
  r_total <- sqrt(r_defocus^2 + r_floor^2)

  # Apply blur (directional for astigmatism, uniform otherwise)
  sim_arr <- img_arr

  if (condition == "astigmatism") {
    D_cyl_defocus <- abs(cylinder_D)
    r_cyl <- blur_radius_px(D_cyl_defocus, pupil_mm, distance_m, scene_width_m, W_px)

    sim_arr <- apply_astigmatism_blur(img_arr,
                                      blur_r_sphere = r_total,
                                      blur_r_cyl = r_cyl,
                                      axis_deg = axis_deg)
  } else {
    sim_arr <- apply_svd_blur(img_arr, r_total)
  }

  # Apply colour vision deficiency if requested
  if (!is.null(color_deficiency)) {
    sim_arr <- apply_cvd(sim_arr, color_deficiency)
  }

  img_sim <- as.cimg(sim_arr)

  # Print clinical summary
  fp <- far_point(Rx_D, max_accommodation_D)
  np <- near_point(Rx_D, max_accommodation_D)

  cat("\n-- VisionSimR --------------------------------------\n")
  cat(sprintf("  Condition        : %s\n", condition))
  cat(sprintf("  Prescription     : %+.1f D\n", Rx_D))
  cat(sprintf("  Age              : %d yrs\n", as.integer(age)))
  cat(sprintf("  Accommodation    : %.1f D (Hofstetter)\n", max_accommodation_D))
  cat(sprintf("  Required power   : %.2f D\n", P_req))
  cat(sprintf("  Eye power        : %.2f D\n", P_eye))
  cat(sprintf("  Effective power  : %.2f D\n", P_eff))
  cat(sprintf("  Defocus          : %+.3f D\n", D_def))
  cat(sprintf("  Blur (defocus)   : %.2f px\n", r_defocus))
  cat(sprintf("  Blur (floor)     : %.2f px\n", r_floor))
  cat(sprintf("  Blur (total)     : %.2f px\n", r_total))

  if (condition == "astigmatism") {
    cat(sprintf("  Cylinder         : %+.2f D\n", cylinder_D))
    cat(sprintf("  Axis             : %d deg\n", as.integer(axis_deg)))
  }

  fp_str <- "infinity"
  if (is.nan(fp)) {
    fp_str <- "NONE (Absolute Hyperopia)"
  } else if (!is.infinite(fp)) {
    fp_str <- sprintf("%.2f m", fp)
  }

  np_str <- "infinity"
  if (!is.infinite(np)) np_str <- sprintf("%.2f m", np)

  cat(sprintf("  Far point        : %s\n", fp_str))
  cat(sprintf("  Near point       : %s\n", np_str))

  if (!is.null(color_deficiency)) {
    cat(sprintf("  Colour sim       : %s\n", color_deficiency))
  }
  cat("----------------------------------------------------\n\n")

  if (show_plot) {
    sim_label <- paste0(tools::toTitleCase(condition),
                        sprintf(" (Rx = %+.1f D, age %d)", Rx_D, as.integer(age)))
    if (condition == "astigmatism") {
      sim_label <- paste0(sim_label, sprintf("\nCyl %+.2f D x %d", cylinder_D, as.integer(axis_deg)))
    }
    if (!is.null(color_deficiency)) {
      sim_label <- paste0(sim_label, "\n+ ", tools::toTitleCase(color_deficiency))
    }

    par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))
    plot(img,     main = "Original",   axes = FALSE)
    plot(img_sim, main = sim_label,    axes = FALSE)
    par(mfrow = c(1, 1))
  }

  invisible(list(
    original = img,
    simulated = img_sim,
    defocus_D = D_def,
    blur_px = r_total,
    far_point_m = fp,
    near_point_m = np,
    age = age,
    accommodation_D = max_accommodation_D,
    cylinder_D = cylinder_D,
    axis_deg = axis_deg
  ))
}

#' Visualize SVD Rank Truncation
#'
#' Plots an image reconstructed at various SVD ranks. Useful for
#' understanding how rank truncation relates to blur/information loss.
#'
#' @param image_path Character. Path to an image file.
#' @param ranks Integer vector. SVD ranks to display. Default c(5, 15, 40, 100).
#' @param max_dim Integer. Max pixel dimension for resizing.
#'
#' @return Invisible NULL. Generates a multi-panel plot.
#' @export
#'
#' @examples
#' \dontrun{
#' img <- system.file("extdata/parrots.png", package = "imager")
#' show_svd_ranks(img, ranks = c(10, 50))
#' }
show_svd_ranks <- function(image_path, ranks = c(5L, 15L, 40L, 100L), max_dim = 300L) {
  img <- load.image(image_path)
  if (spectrum(img) == 4L) img <- rm.alpha(img)

  W <- width(img); H <- height(img)
  s <- min(1.0, max_dim / max(W, H))
  if (s < 1.0) {
    img <- resize(img, round(W * s), round(H * s), interpolation_type = 5L)
  }
  W_px <- width(img); H_px <- height(img)

  n <- length(ranks) + 1L
  par(mfrow = c(1, n), mar = c(1, 0.5, 3, 0.5))

  plot(img, main = "Original", axes = FALSE)

  for (k in ranks) {
    k <- min(k, min(W_px, H_px))
    ch_r <- matrix(as.numeric(img[,,1,1]), W_px, H_px)
    ch_g <- matrix(as.numeric(img[,,1,2]), W_px, H_px)
    ch_b <- matrix(as.numeric(img[,,1,3]), W_px, H_px)

    recon_ch <- function(ch) {
      sv <- svd(ch, nu = k, nv = k)
      r  <- sv$u %*% diag(sv$d[seq_len(k)], nrow = k) %*% t(sv$v)
      matrix(pmax(0, pmin(1, as.numeric(r))), W_px, H_px)
    }

    out <- array(0.0, dim = c(W_px, H_px, 1L, 3L))
    out[,,1,1] <- recon_ch(ch_r)
    out[,,1,2] <- recon_ch(ch_g)
    out[,,1,3] <- recon_ch(ch_b)

    pct <- round(100 * k / min(W_px, H_px), 1)
    plot(as.cimg(out),
         main = sprintf("Rank %d  (%.0f%%)", k, pct),
         axes = FALSE)
  }
  par(mfrow = c(1, 1))
  invisible(NULL)
}