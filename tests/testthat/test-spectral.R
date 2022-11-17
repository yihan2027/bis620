test_that(
  "The spectral_signature returns a dataframe.",
  {
    data(ukb_accel)
    p <-  spec_sig(ukb_accel[1:100, ])
    expect_s3_class(p, "data.frame")
  }
)
test_that(
  "The spectral_signature takes log when take_log=TRUE.",
  {
    data(ukb_accel)
    p <-  spec_sig(ukb_accel[1:100, ], take_log = TRUE)|>select(X, Y, Z)
    p1 <- spec_sig(ukb_accel[1:100, ])|>select(X, Y, Z)|>log()
    expect_equal(p, p1)
  }
)
