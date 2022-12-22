#' Label each patients as wild-type or mutant.
#'
#' Based on patients' KRAS testing result, each of them is labelled as
#' "Wild-type" or "Mutant".
#' @param x an object inherited from a data.frame which records tested gene
#' loci and its mutation.
#' @param mutant_standard If mutant_standard=1, A patient will be considered
#' “Mutant” if there is at least one “Mutant” biomarker in KRAS exons 2, 3, 4.
#' A patient will be considered “Wild-type” if he/she is not “Mutant” and he/she
#' has more “Wild-type” markers than “Unknown” or “Failure”. If
#' mutant_standard=2, A patient will be considered "Mutant" if his/her KRAS
#' exon 2 is "Mutant".
#' @return a data frame records patients' mutant situation, as well as
#' their baseline characteristics, treatments and survival status.
#' @importFrom dplyr left_join filter rename
#' @importFrom tibble tibble add_row
#' @export
kras <- function(x, mutant_standard = 1) {
  biomarker <- x
  if (mutant_standard == 1) {
    mutant <- list()
    notmutant <- list()
    wild <- list()
    unknown <- list()
    for (i in biomarker$SUBJID) {
      row <- biomarker[biomarker$SUBJID == i, ]
      if (row$BMMTR1 == "Mutant" || row$BMMTR2 == "Mutant" ||
          row$BMMTR3 == "Mutant" || row$BMMTR15 == "Mutant") {
        mutant <- append(mutant, c(i))
        } else {
        notmutant <- append(notmutant, i)
      }
      }
    for (i in notmutant) {
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
    biomarker_list <- biomarker_list |>
      add_row(SUBJID = as.character(wild), BMK = "Wild-type") |>
      add_row(SUBJID = as.character(unknown), BMK = "Unknown")
    marker_death <- left_join(dl$adsl, biomarker_list, by = "SUBJID") |>
      select(SUBJID, ATRT, DTHDY, DTH, PFSDYCR, PFSCR, AGE, SEX, RACE, BMK)
    marker_death <- marker_death |> filter(BMK != "Unknown")
    marker_death$arm <- paste(marker_death$BMK, marker_death$ATRT, sep = ", ")
    data.frame(marker_death)
  } else if (mutant_standard == 2) {
    marker_death <- left_join(dl$adsl, x, by = "SUBJID") |>
      rename(BMK = BMMTR1) |>
      select(SUBJID, ATRT, DTHDY, DTH, PFSDYCR, PFSCR, AGE, SEX, RACE, BMK)
    marker_death <- marker_death |>
      filter(BMK != "") |>
      filter(BMK != "Failure")
    marker_death$arm <- paste(marker_death$BMK, marker_death$ATRT, sep = ", ")
    data.frame(marker_death)
  } else {
    stop("Please enter a valid mutant stardard.")
  }
}
