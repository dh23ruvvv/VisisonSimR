# color.R
# Colour vision deficiency (CVD) simulation
#
# Each pixel is an [R, G, B] vector. We stack all pixels into
# an N x 3 matrix P, then apply: P_new = P %*% t(M_cvd)
# where M_cvd is a 3x3 matrix encoding the specific dichromacy.

# RGB <-> LMS colour space conversion matrices
M_rgb2lms <- matrix(c(
  0.31399022,  0.63951294,  0.04649755,
  0.15537241,  0.75789446,  0.08670142,
  0.01775239,  0.10944209,  0.87256922
), nrow = 3, byrow = TRUE)

M_lms2rgb <- solve(M_rgb2lms)

# Dichromacy projection matrices (LMS space)
M_protan <- matrix(c(
  0.0,      2.02344, -2.52581,
  0.0,      1.0,      0.0,
  0.0,      0.0,      1.0
), nrow = 3, byrow = TRUE)

M_deutan <- matrix(c(
  1.0,      0.0,      0.0,
  0.494207, 0.0,      1.24827,
  0.0,      0.0,      1.0
), nrow = 3, byrow = TRUE)

M_tritan <- matrix(c(
  1.0,       0.0,       0.0,
  0.0,       1.0,       0.0,
  -0.395913,  0.801109,  0.0
), nrow = 3, byrow = TRUE)

CVD_MATRICES <- list(
  protanopia   = M_protan,
  deuteranopia = M_deutan,
  tritanopia   = M_tritan
)

# sRGB gamma linearisation / compression
gamma_expand <- function(x) {
  out <- x / 12.92
  idx <- x > 0.04045
  out[idx] <- ((x[idx] + 0.055) / 1.055)^2.4
  out
}

gamma_compress <- function(x) {
  x <- pmax(0, pmin(1, x))
  out <- 12.92 * x
  idx <- x > 0.0031308
  out[idx] <- 1.055 * x[idx]^(1/2.4) - 0.055
  out
}


#' Simulate colour vision deficiency on an image array
#'
#' Applies a linear transform (matrix multiplication) to simulate how
#' someone with protanopia, deuteranopia, or tritanopia would see an image.
#' The full pipeline is: linearise -> RGB to LMS -> project -> LMS to RGB -> compress.
#'
#' @param img_array 4D array [W, H, 1, 3] from imager (values 0-1).
#' @param type One of "protanopia", "deuteranopia", "tritanopia".
#' @return 4D array, same shape, with CVD simulation applied.
#' @export
apply_cvd <- function(img_array, type) {
  type <- tolower(trimws(type))
  if (!type %in% names(CVD_MATRICES))
    stop("type must be one of: ", paste(names(CVD_MATRICES), collapse = ", "))

  W <- dim(img_array)[1]
  H <- dim(img_array)[2]
  N <- W * H

  M_cvd <- CVD_MATRICES[[type]]

  R <- as.numeric(img_array[,,1,1])
  G <- as.numeric(img_array[,,1,2])
  B <- as.numeric(img_array[,,1,3])
  P <- matrix(c(R, G, B), nrow = N, ncol = 3)

  # Full pipeline: linearise -> LMS -> project -> back to RGB -> compress
  P_lin <- gamma_expand(P)
  P_lms <- P_lin %*% t(M_rgb2lms)
  P_lms_sim <- P_lms %*% t(M_cvd)
  P_rgb_lin <- P_lms_sim %*% t(M_lms2rgb)
  P_out <- gamma_compress(P_rgb_lin)

  if (!is.matrix(P_out)) P_out <- matrix(P_out, nrow = N, ncol = 3)

  out <- array(0.0, dim = c(W, H, 1L, 3L))
  out[,,1,1] <- matrix(P_out[,1], W, H)
  out[,,1,2] <- matrix(P_out[,2], W, H)
  out[,,1,3] <- matrix(P_out[,3], W, H)
  out
}