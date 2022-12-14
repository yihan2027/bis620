% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survival_plot.R
\name{survival_plot}
\alias{survival_plot}
\title{Draw the survival plot}
\usage{
survival_plot(
  x,
  measure = c("PFS", "OS"),
  arm = c("Treatment", "Rash Grade"),
  filter = c("Wild-type", "Mutant")
)
}
\arguments{
\item{x}{a data.frame records patients' mutant situation, as well as
their baseline characteristics, treatments and survival status.}

\item{measure}{Must be one of "PFS"and "OS". If "PFS", the
function will plot on Progression-free Survival(PFS). If "OS", the function
will plot on Overall Survival(OS).}

\item{arm}{Must be one of "Treatment" and "Rash Grade". If "Treatment",
"Treatment" will be predictor of the survival model. If "Rash Grade",
"Rash Grade" will be predictor of the survival model.}

\item{filter}{Must be one of "Wild-type" and "Mutant". If "Wild-type", the
function will plot for Wild-type KRAS. If "Mutant", the function will plot
for Mutant KRAS.}
}
\value{
a survival plot of panitumumab+FOLFOX4 and FOLFOX4 alone arms.
}
\description{
Based on given measure and filter, draw the survival plot of given data.
}
