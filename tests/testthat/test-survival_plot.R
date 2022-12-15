test_that(
  "The survival_plot() returns a ggsurv.",
  {
    data(dl)
    p <-  survival_plot(ae_surv(dl$adae, "Rash"),
                        "PFS", "Rash Grade", "Wild-type")
    expect_true(inherits(p, "ggsurv"))
  }
)
test_that(
  "The survival_plot() uses ATRT as arm when arm=Treatment.",
  {
    data(dl)
    p <-  survival_plot(ae_surv(dl$adae, "Rash"),
                        "PFS", "Treatment", "Wild-type")
    vdiffr::expect_doppelganger("survival_plot_pfs_treatment", p)
  }
)
test_that(
  "The survival_plot() errors when arm is not one of Treatment and Rash Grade.",
  {
    data(dl)
    expect_error(survival_plot(ae_surv(dl$adae, "Rash"),
                               "PFS", "test error", "Wild-type"))
  }
)
test_that(
  "The survival_plot() uses OS as measure when measure=OS.",
  {
    data(dl)
    p <-  survival_plot(ae_surv(dl$adae, "Rash"),
                        "OS", "Treatment", "Wild-type")
    vdiffr::expect_doppelganger("survival_plot_os_treatment", p)
  }
)
test_that(
  "The survival_plot() errors when measure is not one of PFS and OS.",
  {
    data(dl)
    expect_error(survival_plot(ae_surv(dl$adae, "Rash"),
                               "test error", "Treatment", "Wild-type"))
  }
)
