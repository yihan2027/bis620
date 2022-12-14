test_that(
  "The kras function returns a dataframe.",
  {
    data(dl)
    p <-  kras(dl$biomark, 1)
    expect_s3_class(p, "data.frame")
  }
)
test_that(
  "The kras considers patients as Mutant if his/her KRAS exon 2
  is Mutant when mutant_standard=2.",
  {
    data(dl)
    p <-  kras(dl$biomark, 2)
    p1 <- left_join(dl$adsl, dl$biomark, by = "SUBJID") |>
      rename(BMK = BMMTR1) |>
      select(SUBJID, ATRT, DTHDY, DTH, PFSDYCR, PFSCR, RACE, B_ECOG, BMK)|>
      filter(BMK != "")
    p1 <- data.frame(p1)
    expect_equal(p, p1)
  }
)
test_that(
  "The kras errors when mutant_standard is not one of 1 or 2",
  {
    data(dl)
    expect_error(kras(dl$biomark, 3))
  }
)
