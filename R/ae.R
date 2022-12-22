#' Return a data frame that records the adverse events and their counts and
#' percent for 2 treatments.
#'
#' Based on the given data, only adverse events that are statistically
#' significant different between 2 treatments are preserved. For each adverse
#' event, their count and percent for each treatment and the p-value is
#' recorded.
#' @param x a data.frame records patients' adverse events situations.
#' @param biomarker Must be one of "Wild-type" and "Mutant". If "Wild-type", the
#' function will return the adverse event data frame for Wild-type KRAS. If
#' "Mutant", the function will return the adverse event data frame for Mutant
#' KRAS.
#' @return a dataframe records adverse events' count and percent for each
#' treatment and the p-value.
#' @importFrom dplyr left_join select filter distinct count pull
#' @importFrom stats prop.test
#' @importFrom scales label_percent
#' @export
adverse_event <- function(x, biomarker = c("Wild-type", "Mutant")) {
  ae <- x
  ae$bind <- paste(ae$SUBJID, ae$AEPT, sep = ", ")
  most_serious_ae <- ae[order(ae$AESEVCD, decreasing = TRUE), ] |>
    distinct(bind, .keep_all = TRUE)
  most_serious_ae <- most_serious_ae[order(most_serious_ae$SUBJID), ]
  ae <- left_join(kras(dl$biomark, 1), most_serious_ae, by = "SUBJID") |>
    select(SUBJID, ATRT, BMK, AEPT, AESOC, AESEVCD)
  ae$arm <- paste(ae$ATRT, ae$BMK, sep = ", ")
  ae_list <- ae$AEPT |> unique()
  p_arm <- paste0("Panitumumab + FOLFOX, ", biomarker)
  f_arm <- paste0("FOLFOX alone, ", biomarker)
  p_w_all <- ae |>
    filter(arm == p_arm) |>
    distinct(SUBJID) |>
    count() |>
    pull()
  f_w_all <- ae |>
    filter(arm == f_arm) |>
    distinct(SUBJID) |>
    count() |>
    pull()
  df <- data.frame()
  for (i in ae_list){
    p_w <- ae |>
      filter(AEPT == i) |>
      filter(arm == p_arm) |>
      count() |>
      pull()
    f_w <- ae |>
      filter(AEPT == i) |>
      filter(arm == f_arm) |>
      count() |>
      pull()
    if (f_w / f_w_all >= 0.05 || p_w / p_w_all >= 0.05) {
      twosample_prop <- prop.test(c(p_w, f_w), n = c(p_w_all, f_w_all),
                                  correct = FALSE)
      if (is.nan(twosample_prop$p.value) == FALSE) {
        if (twosample_prop$p.value < 0.05) {
          p_w_per <- paste0(p_w, " (", label_percent()(p_w / p_w_all), ")")
          f_w_per <- paste0(f_w, " (", label_percent()(f_w / f_w_all), ")")
          row <- c(i, p_w_per, f_w_per,
                   signif(as.double(twosample_prop$p.value), 3))
          df <- rbind(df, row)
        }
      }
    }
  }
  pf <- paste0("Panitumumab+FOLFOX (n=", p_w_all, ")")
  f <- paste0("FOLFOX (n=", f_w_all, ")")
  colnames(df) <- c("Adverse event", pf, f, "p-value")
  df
}
