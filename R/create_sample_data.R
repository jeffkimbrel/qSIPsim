#' Create Sample Dataframe
#'
#' @param treatment A vector with names of biological treatments (e.g. drought)
#' @param isotope A vector of isotopes either explicit (c("12C", "13C")) or implicit ("C")
#' @param replicate
#' @param timepoint
#'
#' @return
#' @export
#'
#' @examples
create_sample_data <- function(treatment = c("TRT1", "TRT2"),
                               isotope = "C",
                               replicate = c("A", "B", "C"),
                               timepoint = c("early")) {

  # populate isotopes
  isotopes = vector()
  for (i in isotope) {
    if (i %in% c("C", "12C", "13C")) {
      isotopes = c(isotopes, "unlabeled", "13C")
    } else if (i %in% c("N", "14N", "15N")) {
      isotopes = c(isotopes, "unlabeled", "15N")
    } else if (i %in% c("O", "16O", "18O")) {
      isotopes = c(isotopes, "unlabeled", "18O")
    } else {
      stop(paste0("Unknown isotope \"", i, "\" provided"))
    }
  }

  isotopes = unique(isotopes)

  tidyr::expand_grid(treatment, isotopes, timepoint, replicate) %>%
    dplyr::mutate(tube = dplyr::row_number()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      material_g = abs(rnorm(1, mean = 5, sd = 0.5)),
      dna_ug = abs(rnorm(1, mean = 20, sd = 5)),
      qPCR_copies_ul = abs(rnorm(1, mean = dna_ug*5000000, sd = 10000000))
    )
}
