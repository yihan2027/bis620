test_that(
  "The adverse_event() returns a dataframe.",
  {
    data(dl)
    p <-  adverse_event(dl$adae, "Wild-type")$table
    expect_s3_class(p, "data.frame")
  }
)
test_that(
  "The adverse_event() returns a ggplot",
  {
    data(dl)
    p <-  adverse_event(dl$adae, "Wild-type")$plot
    expect_true(inherits(p, "gg"))
  }
)
