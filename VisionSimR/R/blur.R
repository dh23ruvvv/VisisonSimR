# blur.R
# SVD-based image blur (Eckart-Young theorem)
#
# Each image channel is a matrix M. We decompose it as M = U D V^T
# and keep only the top k singular values to get a rank-k approximation.
# Lower rank = fewer details = blurry image.
#
# The mapping from physical blur radius to SVD rank is:
#   rank = max(1, floor(min(nrow, ncol) / (2 * blur_radius)))
#
# Blur radius comes from the Circle of Confusion formula:
#   r = (pupil/2) * |defocus| * distance * (pixels / scene_width)


# Compute blur radius in pixels from optical defocus
#
# @param D_defocus Defocus in diopters
# @param pupil_mm Pupil diameter in mm
# @param distance_m Viewing distance in metres
# @param scene_width_m Physical width of scene (metres)
# @param img_width_px Image pixel width
# @return Blur radius in pixels
# @noRd
blur_radius_px <- function(D_defocus, pupil_mm,
                           distance_m, scene_width_m, img_width_px) {
  pupil_m <- pupil_mm / 1000
  ang_scale <- img_width_px / scene_width_m
  r <- (pupil_m / 2) * abs(D_defocus) * distance_m * ang_scale
  max(0, r)
}


# Blur one channel via SVD rank truncation
#
# @param channel Matrix (one colour channel, values 0-1)
# @param blur_r Blur radius in pixels
# @return Blurred channel matrix, clipped to [0, 1]
# @noRd
svd_blur_channel <- function(channel, blur_r) {
  nr <- nrow(channel)
  nc <- ncol(channel)

  # Convert blur radius to rank (more blur = lower rank)
  rank_k <- max(1L, floor(min(nr, nc) / (2 * blur_r)))
  rank_k <- min(rank_k, min(nr, nc))

  s <- svd(channel, nu = rank_k, nv = rank_k)

  # Rank-k reconstruction: U_k %*% diag(d_k) %*% t(V_k)
  reconstructed <- s$u %*% diag(s$d[seq_len(rank_k)], nrow = rank_k) %*% t(s$v)

  matrix(pmax(0, pmin(1, as.numeric(reconstructed))), nrow = nr, ncol = nc)
}


# Apply SVD blur to all 3 colour channels, then smooth with a Gaussian PSF
#
# The Gaussian post-smoothing gets rid of the banding artifacts that
# come from hard SVD truncation. The sigma is proportional to blur_r.
#
# @param img_array 4D array [W, H, 1, 3] from imager
# @param blur_r Blur radius in pixels
# @return 4D array, same dimensions, blurred
# @noRd
apply_svd_blur <- function(img_array, blur_r) {
  if (blur_r < 0.5) return(img_array)

  W <- dim(img_array)[1]
  H <- dim(img_array)[2]

  ch_r <- svd_blur_channel(matrix(img_array[,,1,1], W, H), blur_r)
  ch_g <- svd_blur_channel(matrix(img_array[,,1,2], W, H), blur_r)
  ch_b <- svd_blur_channel(matrix(img_array[,,1,3], W, H), blur_r)

  out <- array(0.0, dim = c(W, H, 1L, 3L))
  out[,,1,1] <- ch_r
  out[,,1,2] <- ch_g
  out[,,1,3] <- ch_b

  # Gaussian PSF smoothing to reduce SVD banding
  sigma_psf <- blur_r * 0.35
  if (sigma_psf >= 0.3) {
    smoothed <- isoblur(as.cimg(out), sigma_psf)
    out <- as.array(smoothed)
    out[] <- pmax(0, pmin(1, out))
  }
  out
}


# Directional SVD blur for one channel (Conoid of Sturm model)
#
# SVD decomposes M into row-space (U) and column-space (V).
# By using different ranks for U and V, we can blur more along one
# axis than the other, which is how astigmatism works optically.
#
# We blend two "focal line" reconstructions (one sharp horizontally,
# one sharp vertically) to approximate the circle of least confusion.
#
# @param channel Matrix [W x H]
# @param blur_r_meridian1 Blur along first meridian (px)
# @param blur_r_meridian2 Blur along second meridian (px)
# @return Directionally blurred matrix, clipped to [0, 1]
# @noRd
svd_blur_directional <- function(channel, blur_r_meridian1, blur_r_meridian2) {
  nr <- nrow(channel)
  nc <- ncol(channel)

  k1 <- max(1L, floor(min(nr, nc) / (2 * max(0.5, blur_r_meridian1))))
  k2 <- max(1L, floor(min(nr, nc) / (2 * max(0.5, blur_r_meridian2))))
  k1 <- min(k1, min(nr, nc))
  k2 <- min(k2, min(nr, nc))

  k_compute <- max(k1, k2)
  s <- svd(channel, nu = k_compute, nv = k_compute)

  # Focal line 1: full U rank (k1), limited V rank (k2)
  u1 <- s$u[, seq_len(k1), drop = FALSE]
  d1 <- s$d[seq_len(k1)]
  v1 <- s$v[, seq_len(min(k1, k2)), drop = FALSE]

  if (k1 > k2) {
    recon1 <- u1[, seq_len(k2), drop = FALSE] %*%
              diag(d1[seq_len(k2)], nrow = k2) %*% t(v1)
  } else {
    recon1 <- u1 %*% diag(d1, nrow = k1) %*%
              t(s$v[, seq_len(k1), drop = FALSE])
  }

  # Focal line 2: limited U rank, full V rank (k2)
  u2 <- s$u[, seq_len(min(k1, k2)), drop = FALSE]
  v2 <- s$v[, seq_len(k2), drop = FALSE]

  if (k2 > k1) {
    recon2 <- u2 %*% diag(s$d[seq_len(k1)], nrow = k1) %*%
              t(v2[, seq_len(k1), drop = FALSE])
  } else {
    recon2 <- s$u[, seq_len(k2), drop = FALSE] %*%
              diag(s$d[seq_len(k2)], nrow = k2) %*% t(v2)
  }

  # Average the two focal lines (circle of least confusion)
  reconstructed <- (recon1 + recon2) / 2

  matrix(pmax(0, pmin(1, as.numeric(reconstructed))), nrow = nr, ncol = nc)
}


# Apply astigmatism blur via Conoid of Sturm model
#
# For cardinal axes (near 0/90/180 degrees), we use true directional
# SVD with independent row/column ranks.
# For oblique axes, we fall back to spherical equivalent (SE) to
# avoid rotation artifacts.
#
# @param img_array 4D array [W, H, 1, 3]
# @param blur_r_sphere Blur from spherical component (px)
# @param blur_r_cyl Additional blur from cylinder (px)
# @param axis_deg Cylinder axis (0-180)
# @return 4D array, astigmatically blurred
# @noRd
apply_astigmatism_blur <- function(img_array, blur_r_sphere, blur_r_cyl,
                                    axis_deg) {
  W <- dim(img_array)[1]
  H <- dim(img_array)[2]

  # Check if axis is close to cardinal (within 15 deg of 0/90/180)
  axis_norm <- axis_deg %% 180
  is_cardinal <- (axis_norm <= 15) || (axis_norm >= 165) ||
                 (abs(axis_norm - 90) <= 15)

  r_along_axis <- max(0.5, blur_r_sphere)
  r_perp_axis  <- max(0.5, blur_r_sphere + blur_r_cyl)

  if (is_cardinal) {
    # Cardinal axis: use directional SVD
    if (abs(axis_norm - 90) <= 15) {
      # With-the-rule astigmatism
      r_rows <- r_along_axis
      r_cols <- r_perp_axis
    } else {
      # Against-the-rule astigmatism
      r_rows <- r_perp_axis
      r_cols <- r_along_axis
    }

    blur_fn <- function(ch) svd_blur_directional(ch, r_rows, r_cols)
  } else {
    # Oblique axis: use spherical equivalent
    r_se <- blur_r_sphere + blur_r_cyl / 2
    if (r_se < 0.5) return(img_array)

    blur_fn <- function(ch) svd_blur_channel(ch, r_se)
  }

  ch_r <- matrix(img_array[,,1,1], W, H)
  ch_g <- matrix(img_array[,,1,2], W, H)
  ch_b <- matrix(img_array[,,1,3], W, H)

  out <- array(0.0, dim = c(W, H, 1L, 3L))
  out[,,1,1] <- blur_fn(ch_r)
  out[,,1,2] <- blur_fn(ch_g)
  out[,,1,3] <- blur_fn(ch_b)

  # Gaussian PSF smoothing
  sigma_psf <- max(blur_r_sphere, blur_r_sphere + blur_r_cyl) * 0.35
  if (sigma_psf >= 0.3) {
    smoothed <- isoblur(as.cimg(out), sigma_psf)
    out <- as.array(smoothed)
    out[] <- pmax(0, pmin(1, out))
  }
  out
}


# Baseline blur from acuity limit + diffraction (affects everyone)
#
# Even a perfect eye can't beat physics:
#   1. Acuity limit: ~1 arcminute minimum resolvable angle (20/20 vision)
#   2. Diffraction (Airy disk): r = 1.22 * lambda / pupil_diameter
# These combine in quadrature with the refractive blur.
#
# @param distance_m Viewing distance (m)
# @param scene_width_m Scene width (m)
# @param img_width_px Image width (px)
# @param pupil_mm Pupil diameter (mm)
# @param acuity_arcmin Minimum resolvable angle (default 1 arcmin)
# @return Combined acuity + diffraction blur radius in pixels
# @noRd
floor_blur_px <- function(distance_m, scene_width_m, img_width_px,
                          pupil_mm, acuity_arcmin = 1.0) {
  pix_per_rad <- img_width_px * distance_m / scene_width_m

  r_acuity <- (acuity_arcmin * pi / (180 * 60)) * pix_per_rad
  r_acuity <- max(0, r_acuity - 0.5)

  lambda_m <- 550e-9
  r_diff <- 1.22 * lambda_m / (pupil_mm / 1000) * pix_per_rad
  r_diff <- max(0, r_diff - 0.5)

  sqrt(r_acuity^2 + r_diff^2)
}