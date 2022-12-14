#' Classify each patient's adverse event grade as "less than 2" or
#' "greater or equal to 2".
#'
#' Based on patients' adverse event grade/severity grade, each of their grade is
#' classified as "less than 2" or "greater or equal to 2".
#' @param x an object inherited from a data.frame which records patients'
#' adverse events situations.
#' @param adverse the adverse event that want to be classified on.
#' @return a data frame records patients' adverse event classification, as well
#' as their baseline characteristics, treatments and survival status.
#' @importFrom dplyr left_join filter select mutate
#' @export
ae_surv <- function(x, adverse) {
  ae <- x
  ae$bind <- paste(ae$SUBJID, ae$AEPT, sep = ", ")
  most_serious_ae <- ae[order(ae$AESEVCD, decreasing = TRUE), ] |>
    distinct(bind, .keep_all = TRUE)
  most_serious_ae |> filter(AEPT == adverse)
  rash_surv <- left_join(kras(dl$biomark, 1), most_serious_ae |>
                          filter(AEPT == "Rash"), by = "SUBJID") |>
    select(SUBJID, ATRT, BMK, DTHDY, DTH, PFSDYCR, PFSCR, AEPT, AESOC, AESEVCD)
  rash_surv <- rash_surv |>
    mutate(AESEVCD = ifelse(is.na(AESEVCD), 0, AESEVCD)) |>
    mutate(RASH_GRADE = ifelse(AESEVCD < 2, "less than 2",
                               "greater or equal to 2"))
  rash_surv
}
