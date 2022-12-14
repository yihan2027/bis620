#' Draw the survival plot
#'
#' Based on given measure and filter, draw the survival plot of given data.
#' @param x a data.frame records patients' mutant situation, as well as
#' their baseline characteristics, treatments and survival status.
#' @param measure Must be one of "PFS"and "OS". If "PFS", the
#' function will plot on Progression-free Survival(PFS). If "OS", the function
#' will plot on Overall Survival(OS).
#' @param arm Must be one of "Treatment" and "Rash Grade". If "Treatment",
#' "Treatment" will be predictor of the survival model. If "Rash Grade",
#' "Rash Grade" will be predictor of the survival model.
#' @param filter Must be one of "Wild-type" and "Mutant". If "Wild-type", the
#' function will plot for Wild-type KRAS. If "Mutant", the function will plot
#' for Mutant KRAS.
#' @return a survival plot of panitumumab+FOLFOX4 and FOLFOX4 alone arms.
#' @importFrom dplyr filter rename tibble
#' @importFrom survival survfit Surv
#' @importFrom survminer ggsurvplot
#' @importFrom stats as.formula
#' @export
survival_plot <- function(x, measure = c("PFS", "OS"), arm = c("Treatment",
                         "Rash Grade"), filter = c("Wild-type", "Mutant")) {
  filter_marker_death <- x |> filter(BMK == filter)
  if (arm == "Treatment") {
    predictor <- "ATRT"
  } else if (arm == "Rash Grade") {
    predictor <- "RASH_GRADE"
  } else {
        stop("Please enter a valid arm")
    }
  title_1 <- paste("Survival plot of", measure, "(", filter, "KRAS )")
  if (measure == "PFS" && exists("predictor")) {
    surv <- paste0("Surv(PFSDYCR, PFSCR) ~ ", predictor) |> as.formula()
    fit <- do.call(survfit, args = list(formula = surv,
                                        data = filter_marker_death))
    ggsurvplot(fit,
               pval = TRUE, conf.int = TRUE, data = filter_marker_death,
               title = title_1)
  } else if (measure == "OS" && exists("predictor")) {
    surv <- paste0("Surv(DTHDY, DTH) ~ ", predictor) |> as.formula()
    fit <- do.call(survfit, args = list(formula = surv,
                                     data = filter_marker_death))
    ggsurvplot(fit,
               pval = TRUE, conf.int = TRUE, data = filter_marker_death,
               title = title_1)
  } else {
    stop("Please enter a valid measure.")
    }
}
