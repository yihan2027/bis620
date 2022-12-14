test_that(
  "The log_rank() returns a list of gtsummary and numeric.",
  {
    data(dl)
    p <-  log_rank(kras(dl$biomark, 1), "PFS", "Treatment", "Wild-type")
    expect_true(inherits(p$median, "gtsummary"))
    expect_true(inherits(p$p, "numeric"))
  }
)
test_that(
  "The log_rank() uses RASH_GRADE as arm when arm=Rash Grade.",
  {
    data(dl)
    p <-  log_rank(ae_surv(dl$adae, "Rash"), "PFS", "Rash Grade", "Wild-type")
    logr <- survdiff(Surv(PFSDYCR, PFSCR) ~ RASH_GRADE, data =
                       ae_surv(dl$adae, "Rash") |>
               filter(BMK == "Wild-type"))
    p1 <- 1 - pchisq(logr$chisq, length(logr$n) - 1)
    expect_equal(p$p, p1)
  }
)
test_that(
  "The log_rank() errors when arm is not one of Treatment and Rash Grade.",
  {
    data(dl)
    expect_error(log_rank(ae_surv(dl$adae, "Rash"), "PFS", "test error",
                          "Wild-type"))
  }
)
test_that(
  "The log_rank() uses OS as measure when measure=OS.",
  {
    data(dl)
    p <-  log_rank(ae_surv(dl$adae, "Rash"), "OS", "Rash Grade", "Wild-type")
    logr <- survdiff(Surv(DTHDY, DTH) ~ RASH_GRADE,
                     data = ae_surv(dl$adae, "Rash") |>
                       filter(BMK == "Wild-type"))
    p1 <- 1 - pchisq(logr$chisq, length(logr$n) - 1)
    expect_equal(p$p, p1)
  }
)
test_that(
  "The log_rank() errors when measure is not one of PFS and OS.",
  {
    data(dl)
    expect_error(log_rank(ae_surv(dl$adae, "Rash"),
                          "test error", "Rash Grade", "Wild-type"))
  }
)
