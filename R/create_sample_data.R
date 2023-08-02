#' Title
#'
#' @param treatment
#' @param light_isotope
#' @param heavy_isotope
#' @param replicate
#' @param timepoint
#'
#' @return
#' @export
#'
#' @examples
create_sample_data <- function(treatment = c("TRT1", "TRT2"),
                               light_isotope = c("12C"),
                               heavy_isotope = c("13C"),
                               replicate = c("A", "B", "C"),
                               timepoint = c("early")) {
  # check if isotopes are allowed
  allowed_light <- c("12C", "14N", "16O", "unlabeled")
  allowed_heavy <- c("13C", "15N", "18O")

  if (length(setdiff(light_isotope, allowed_light)) > 0) {
    stop("light_isotope is not an allowed value")
  } else if (length(setdiff(heavy_isotope, allowed_heavy)) > 0) {
    stop("heavy_isotope is not an allowed value")
  }

  isotopes <- c(light_isotope, heavy_isotope)

  tidyr::expand_grid(treatment, isotopes, timepoint, replicate) %>%
    dplyr::mutate(tube = dplyr::row_number()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      material_g = rnorm(1, mean = 5, sd = 0.5),
      dna_ug = rnorm(1, mean = 20, sd = 5)
    )
}
