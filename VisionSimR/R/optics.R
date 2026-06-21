# optics.R
# Thin-lens eye model: converts a spectacle prescription into
# a defocus value (in diopters). Also computes far/near points.

V_RET <- 0.017   # retinal distance in metres (axial length)


# Hofstetter's accommodation formula (1950)
# Acc_max = max(0, 18.5 - 0.3 * age)
# Gold-standard clinical estimate. At ~62 yrs, accommodation = 0.
hofstetter_accommodation <- function(age) {
  max(0, 18.5 - 0.3 * age)
}


# Convert spectacle Rx to eye power (diopters)
eye_power <- function(Rx_D) {
  (1 / V_RET) - Rx_D
}


# Power needed to focus at a given distance
required_power <- function(distance_m) {
  stopifnot(distance_m > 0)
  (1 / V_RET) + (1 / distance_m)
}


# How much the eye's power misses the target
compute_defocus <- function(P_eye, P_req) {
  P_eye - P_req
}


# Effective eye power after accommodation
# Clamps accommodation effort to [0, max_acc]
effective_power <- function(P_eye, P_req, max_accommodation_D) {
  extra_needed <- P_req - P_eye
  extra_used <- min(max(extra_needed, 0), max_accommodation_D)
  P_eye + extra_used
}


# Far point: farthest distance of clear vision (no glasses)
#   Myopia (Rx < 0): far point = -1/Rx
#   Facultative hyperopia (Rx <= Acc_max): far point = Inf
#   Absolute hyperopia (Rx > Acc_max): far point = NaN
far_point <- function(Rx_D, max_accommodation_D) {
  if (Rx_D < 0) return(-1 / Rx_D)
  if (Rx_D <= max_accommodation_D) return(Inf)
  return(NaN)
}


# Near point: closest readable distance (using full accommodation)
near_point <- function(Rx_D, max_accommodation_D) {
  total_power <- eye_power(Rx_D) + max_accommodation_D
  vergence_obj <- total_power - (1 / V_RET)
  if (vergence_obj <= 0) return(Inf)
  1 / vergence_obj
}