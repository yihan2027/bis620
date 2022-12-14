% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KRAS.R
\name{kras}
\alias{kras}
\title{Label each patients as wild-type or mutant.}
\usage{
kras(x, mutant_standard = 1)
}
\arguments{
\item{x}{an object inherited from a data.frame which records tested gene
loci and its mutation.}

\item{mutant_standard}{If mutant_standard=1, A patient will be considered
“Mutant” if there is at least one “Mutant” biomarker in KRAS exons 2, 3, 4.
A patient will be considered “Wild-type” if he/she is not “Mutant” and he/she
has more “Wild-type” markers than “Unknown” or “Failure”. If
mutant_standard=2, A patient will be considered "Mutant" if his/her KRAS
exon 2 is "Mutant".}
}
\value{
a data frame records patients' mutant situation, as well as
their baseline characteristics, treatments and survival status.
}
\description{
Based on patients' KRAS testing result, each of them is labelled as
"Wild-type" or "Mutant".
}
