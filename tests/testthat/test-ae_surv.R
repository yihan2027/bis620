test_that(
  "The ae_surv() returns a dataframe.",
  {
    data(dl)
    p <-  ae_surv(dl$adae, "Rash")
    expect_s3_class(p, "data.frame")
  }
)
