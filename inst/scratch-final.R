library(devtools)
library(lubridate)
library(dplyr)
library(ggplot2)

document()
document()

data(dl)

#label each patient as mutant/wild: mutant if exon2,3,4 have at least one mutant
biomarker <- dl$biomark
mutant <- list()
notmutant <- list()
wild <- list()
unknown <- list()
for (i in biomarker$SUBJID) {
  row <- biomarker[biomarker$SUBJID == i, ]
  if (row$BMMTR1 == "Mutant" || row$BMMTR2 == "Mutant" || row$BMMTR3 == "Mutant"
      || row$BMMTR15 == "Mutant") {
    mutant <- append(mutant, c(i))
  } else {
    notmutant <- append(notmutant, i)
    }
  }
for (i in notmutant){
  row <- biomarker[biomarker$SUBJID == i, ]
  row <- unlist(row)
  wildnum <- sum(row == "Wild-type")
  unknownnum <- sum(row == "Unknown") + sum(row == "Failure")
  if (wildnum > unknownnum) {
    wild <- append(wild, i)
  } else {
    unknown <- append(unknown, i)
    }
}
biomarker_list <- tibble(SUBJID = as.character(mutant), BMK = "Mutant")
biomarker_list <- biomarker_list|>
  add_row(SUBJID = as.character(wild), BMK = "Wild-type") |>
  add_row(SUBJID = as.character(unknown), BMK = "Unknown")
marker_death <- left_join(dl$adsl, biomarker_list, by = "SUBJID") |>
  select(SUBJID, ATRT, DTHDY, DTH, PFSDYCR, PFSCR, RACE, B_ECOG, BMK)
marker_death <- marker_death |> filter(BMK != "Unknown")

#survival plot
library(survival)
library(survminer)
#for wild-type group
wild_marker_death <- marker_death|>filter(BMK == "Wild-type")
ggsurvplot(survfit(Surv(DTHDY, DTH) ~ ATRT, data = wild_marker_death),
           data = wild_marker_death)
ggsurvplot(survfit(Surv(PFSDYCR, PFSCR) ~ ATRT, data = wild_marker_death),
           data = wild_marker_death)
#for mutant group
mutant_marker_death <- marker_death|>filter(BMK == "Mutant")
ggsurvplot(survfit(Surv(DTHDY, DTH) ~ ATRT, data = mutant_marker_death),
           data = mutant_marker_death)
ggsurvplot(survfit(Surv(PFSDYCR, PFSCR) ~ ATRT, data = mutant_marker_death),
           data = mutant_marker_death)

#log-rank test
logr <- survdiff(Surv(DTHDY, DTH) ~ ATRT,
                 data = wild_marker_death)
logr
logr <- survdiff(Surv(PFSDYCR, PFSCR) ~ ATRT,
                 data = wild_marker_death)
logr
marker_death |> filter(BMK == "Wild-type")|>
  select(-SUBJID) |> tbl_summary(by = ATRT)


#mutant/wild by exon2
marker_death_2  <-  left_join(dl$adsl, dl$biomark, by = "SUBJID")
wild_marker_death_2 <- marker_death_2 |> filter(BMMTR1 == "Wild-type")
logr <- survdiff(Surv(DTHDY, DTH) ~ ATRT,
                 data = wild_marker_death_2)
logr
logr <- survdiff(Surv(PFSDYCR, PFSCR) ~ ATRT,
                 data = wild_marker_death_2)
logr

#PFS, if progressive disease, replace DTHDY with VISITDY, DTH=1
resp <- dl$adrsp|>
  filter(RSRESP == "Progressive disease")|>
  select(SUBJID, RSRESP, VISITDY) |>
  distinct(SUBJID, .keep_all = TRUE)
marker_death_prog <- left_join(marker_death_2, resp, by = "SUBJID") |>
  rename(PROGDY = VISITDY)
marker_death_prog <- marker_death_prog |>
  mutate(PROG = ifelse(is.na(PROGDY), NA, 1)) |>
  mutate(PROGDY = ifelse(is.na(PROGDY), DTHDY, PROGDY)) |>
  mutate(PROG = ifelse(is.na(PROG), DTH, PROG))
marker_death_prog |> filter(BMMTR1 == "Wild-type")|>
  select(-SUBJID) |> tbl_summary(by = ATRT)

#log rank test
logr <- survdiff(Surv(PROGDY, PROG) ~ ATRT,
                 data = marker_death_prog |> filter(BMMTR1 == "Wild-type"))
logr
logr <- survdiff(Surv(DTHDY, DTH) ~ ATRT,
                 data = marker_death_prog |> filter(BMMTR1 == "Wild-type"))
logr
1 - pchisq(logr$chisq, length(logr$n) - 1)


#PFS for exon2,3,4
marker_death_prog <- left_join(marker_death, resp, by = "SUBJID") |>
  rename(PROGDY = VISITDY)
marker_death_prog <- marker_death_prog |>
  mutate(PROGDY = ifelse(is.na(PROGDY), DTHDY, PROGDY))
marker_death_prog |>
  filter(BMK == "Wild_type") |>
  select(-SUBJID) |>
  tbl_summary(by = ATRT)

#log rank test for PFS, DTH
logr <- survdiff(Surv(PROGDY, DTH) ~ ATRT,
                 data = marker_death_prog |> filter(BMK == "Wild_type"))
logr
1 - pchisq(logr$chisq, length(logr$n) - 1)
logr <- survdiff(Surv(PFSDYCR, PFSCR) ~ ATRT,
                 data = marker_death_prog |> filter(BMK == "Wild_type"))
logr
1 - pchisq(logr$chisq, length(logr$n) - 1)

#stratified log rank test
marker_death_prog$B_ECOG <- as.factor(marker_death_prog$B_ECOG)
marker_death_2$B_ECOG <- as.factor(marker_death_2$B_ECOG)
logr <- survdiff(Surv(PFSDYCR, PFSCR) ~ ATRT + strata(B_ECOG),
                 data = marker_death_prog |> filter(BMK == "Wild_type"))
logr
cox <- coxph(Surv(PROGDY, PROG) ~ ATRT + strata(RACE) + strata(B_ECOG),
             data = marker_death_prog |> filter(BMMTR1 == "Wild-type"))
cox

#test KRAS
kras(dl$biomark, 1) |> filter(BMK == "Wild-type")
kras(dl$biomark, 2)
kras(dl$biomark, 3)

#test survival plot
survival_plot(kras(dl$biomark, 1), "PFS", "Treatment", "Wild-type")
survival_plot(kras(dl$biomark, 1), "PFS", "Treatment", "Mutant")
survival_plot(kras(dl$biomark, 1), "OS", "Treatment", "Mutant")
survival_plot(kras(dl$biomark, 1), "OS", "Treatment", "Wild-type")

#test log-rank
log_rank(kras(dl$biomark, 1), "PFS", "Treatment", "Wild-type")
log_rank(kras(dl$biomark, 2), "PFS", "Treatment", "Wild-type")


#adverse event
ae <- dl$adae
ae$bind <- paste(ae$SUBJID, ae$AEPT, sep = ", ")
most_serious_ae <- ae[order(ae$AESEVCD, decreasing = TRUE), ] |>
  distinct(bind, .keep_all = TRUE)
most_serious_ae <- most_serious_ae[order(most_serious_ae$SUBJID), ]
ae <- left_join(KRAS(dl$biomark, 1), most_serious_ae, by = "SUBJID") |>
  select(SUBJID, ATRT, BMK, AEPT, AESOC, AESEVCD)
ae$arm <- paste(ae$ATRT, ae$BMK, sep = ", ")
ae_list <- ae$AEPT |> unique()
p_w_all <- ae |>
  filter(arm == "Panitumumab + FOLFOX, Wild-type") |>
  distinct(SUBJID) |>
  count() |>
  pull()
f_w_all <- ae |>
  filter(arm == "FOLFOX alone, Wild-type") |>
  distinct(SUBJID) |>
  count() |>
  pull()
df <- data.frame()
for (i in ae_list){
  p_w <- ae |>
    filter(AEPT == i) |>
    filter(arm == "Panitumumab + FOLFOX, Wild-type") |>
    count() |>
    pull()
  f_w <- ae |>
    filter(AEPT == i) |>
    filter(arm == "FOLFOX alone, Wild-type") |>
    count() |>
    pull()
  twosample_prop <- prop.test(c(p_w, f_w), n = c(p_w_all, f_w_all),
                              correct = FALSE)
  if (is.nan(twosample_prop$p.value) == FALSE) {
    if (twosample_prop$p.value < 0.05) {
      p_w_per <- paste0(P_W, " (", label_percent()(p_w / p_w_all), ")")
      f_w_per <- paste0(F_W, " (", label_percent()(f_w / f_w_all), ")")
      row <- c(i, p_w_per, f_w_per, as.double(twosample_prop$p.value))
      df <- rbind(df, row)
    }
  }
}
pf <- paste0("Panitumumab+FOLFOX (n=", p_w_all, ")")
f <- paste0("FOLFOX (n=", f_w_all, ")")
colnames(df) <- c("Adverse event", pf, f, "p-value")
df

#test adverse-event func
adverse_event(dl$adae, "Wild-type")
adverse_event(dl$adae, "Mutant")

#rash lead to better survival?
ae <- dl$adae
ae$bind <- paste(ae$SUBJID, ae$AEPT, sep = ", ")
most_serious_ae <- ae[order(ae$AESEVCD, decreasing = TRUE), ] |>
  distinct(bind, .keep_all = TRUE)
most_serious_ae |> filter(AEPT == "Rash")
rash_surv <- left_join(KRAS(dl$biomark, 1), most_serious_ae |>
                         filter(AEPT == "Rash"), by = "SUBJID") |>
  select(SUBJID, ATRT, BMK, DTHDY, DTH, PFSDYCR, PFSCR, AEPT, AESOC, AESEVCD)
rash_surv <- rash_surv |>
  mutate(AESEVCD = ifelse(is.na(AESEVCD), 0, AESEVCD)) |>
  mutate(RASH_GRADE = ifelse(AESEVCD < 2, "less than 2",
                             "greater or equal to 2"))
fit <- survfit(Surv(PFSDYCR, PFSCR) ~ RASH_GRADE, data = rash_surv |>
                 filter(BMK == "Wild-type")) |>
  tbl_survfit(prob = 0.5, label_header = "**Median (95% CI) days**")
fit
logr <- survdiff(Surv(PFSDYCR, PFSCR) ~ RASH_GRADE,
                 data = rash_surv |> filter(BMK == "Wild-type"))
logr

#test updated log_rank
log_rank(rash_surv, "PFS", "Rash Grade", "Wild-type")
log_rank(rash_surv, "OS", "Rash Grade", "Wild-type")

#test ae_surv
ae_surv(dl$adae, "Rash")
log_rank(ae_surv(dl$adae, "Rash"), "PFS", "Rash Grade", "Wild-type")

#test updated survival_plot
survival_plot(ae_surv(dl$adae, "Rash"), "PFS", "Rash Grade", "Wild-type")

lint_package(path = ".", linters = linters_with_defaults(cyclocomp_linter =
                                               cyclocomp_linter(25)))
