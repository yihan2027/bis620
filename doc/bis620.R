## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bis620.2022)

## ---- include = FALSE---------------------------------------------------------
library(knitr)
library(dplyr)
library(gtsummary)

## -----------------------------------------------------------------------------
#load in data
data(dl)
dl$adsl |>
  select(ATRT, DTHDY, DTH, PFSDYCR, PFSCR, AGE, SEX) |>
  tbl_summary(by = ATRT)

## -----------------------------------------------------------------------------
kras(dl$biomark, mutant_standard = 1) |> group_by(arm) |> summarize(n = n()) |>
  kable()

## -----------------------------------------------------------------------------
kras(dl$biomark, mutant_standard = 2) |> group_by(arm) |> summarize(n = n()) |>
  kable()

## ---- fig.align='center', fig.dim = c(6, 3.5)---------------------------------
survival_plot(kras(dl$biomark, 1), "PFS", "Treatment", "Wild-type")
log_rank(kras(dl$biomark, 1), "PFS", "Treatment", "Wild-type")$median

## -----------------------------------------------------------------------------
log_rank(kras(dl$biomark, 1), "PFS", "Treatment", "Wild-type")$p

## ---- fig.align='center', fig.dim = c(6, 3.5)---------------------------------
survival_plot(kras(dl$biomark, 2), "PFS", "Treatment", "Wild-type")
log_rank(kras(dl$biomark, 2), "PFS", "Treatment", "Wild-type")$median

## -----------------------------------------------------------------------------
log_rank(kras(dl$biomark, 2), "PFS", "Treatment", "Wild-type")$p

## ---- fig.align='center', fig.dim = c(6, 3.5)---------------------------------
survival_plot(kras(dl$biomark, 1), "PFS", "Treatment", "Mutant")
log_rank(kras(dl$biomark, 1), "PFS", "Treatment", "Mutant")$median

## -----------------------------------------------------------------------------
log_rank(kras(dl$biomark, 1), "PFS", "Treatment", "Mutant")$p

## -----------------------------------------------------------------------------
adverse_event(dl$adae, "Wild-type")$table |> kable(format = "simple")

## ---- fig.align='center', fig.dim = c(7, 4)-----------------------------------
adverse_event(dl$adae, "Wild-type")$plot

## -----------------------------------------------------------------------------
ae_surv(dl$adae, "Rash") |>
  select(SUBJID, ATRT, BMK, DTHDY, DTH, PFSDYCR, PFSCR, RASH_GRADE) |>
  head(5) |>
  kable()

## ---- fig.align='center', fig.dim = c(6, 3.5)---------------------------------
survival_plot(ae_surv(dl$adae, "Rash") |>
                filter(ATRT == "Panitumumab + FOLFOX"),
              "PFS", "Rash Grade", "Wild-type")
log_rank(ae_surv(dl$adae, "Rash") |>
           filter(ATRT == "Panitumumab + FOLFOX"),
         "PFS", "Rash Grade", "Wild-type")$median

## -----------------------------------------------------------------------------
log_rank(ae_surv(dl$adae, "Rash")|> filter(ATRT == "Panitumumab + FOLFOX"),
         "PFS", "Rash Grade", "Wild-type")$p

