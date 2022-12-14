#' Run a log_rank test.
#'
#' Based on the given data, run a log_rank test to find out if there is
#' statistically significant difference.
#' @param x a data.frame records patients' mutant situation, as well as
#' their baseline characteristics, treatments and survival status.
#' @param measure Must be one of "PFS"and "OS". If "PFS", the
#' function will run log_rank test on Progression-free Survival(PFS). If "OS",
#' the function will run log_rank test on Overall Survival(OS).
#' @param arm Must be one of "Treatment" and "Rash Grade". If "Treatment",
#' "Treatment" will be predictor of the survival model. If "Rash Grade",
#' "Rash Grade" will be predictor of the survival model.
#' @param filter Must be one of "Wild-type" and "Mutant". If "Wild-type", the
#' function will run log_rank test for Wild-type KRAS. If "Mutant", the
#' function will run log_rank test for Mutant KRAS.
#' @return the median(95% CI) and p-value of the log-rank test.
#' @importFrom dplyr filter
#' @importFrom survival survfit survdiff Surv
#' @importFrom gtsummary tbl_survfit
#' @importFrom stats as.formula pchisq
#' @export
log_rank <- function(x, measure = c("PFS", "OS"), arm = c("Treatment",
                    "Rash Grade"), filter = c("Wild-type", "Mutant")) {
  filter_marker_death <- x |> filter(BMK == filter)
  if (arm == "Treatment") {
    predictor <- "ATRT"
  } else if (arm == "Rash Grade") {
    predictor <- "RASH_GRADE"
  } else {
        stop("Please enter a valid arm")
    }
  if (measure == "PFS" && exists("predictor")) {
    surv <- paste0("Surv(PFSDYCR, PFSCR) ~ ", predictor) |> as.formula()
    fit <- survfit(surv, data = filter_marker_death) |>
      tbl_survfit(prob = 0.5, label_header = "**Median (95% CI) days**")
    logr <- survdiff(surv, data = filter_marker_death)
    result <- list()
    result$median <- fit
    result$p <- 1 - pchisq(logr$chisq, length(logr$n) - 1)
    return(result)
  } else if (measure == "OS" && exists("predictor")) {
    surv <- paste0("Surv(DTHDY, DTH) ~ ", predictor) |> as.formula()
    fit <- survfit(surv, data = filter_marker_death) |>
      tbl_survfit(prob = 0.5, label_header = "**Median (95% CI) days**")
    logr <- survdiff(surv, data = filter_marker_death)
    result <- list()
    result$median <- fit
    result$p <- 1 - pchisq(logr$chisq, length(logr$n) - 1)
    return(result)
  } else {
    stop("Please enter a valid measure.")
  }
}
