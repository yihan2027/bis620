test_that(
  "The adverse_event() returns a dataframe.",
  {
    data(dl)
    p <-  adverse_event(dl$adae, "Wild-type")
    expect_s3_class(p, "data.frame")
  }
)
