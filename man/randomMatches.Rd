% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reproduce.R
\name{randomMatches}
\alias{randomMatches}
\title{Random Mating}
\usage{
randomMatches(individuals, number_of_couples)
}
\arguments{
\item{individuals}{Data frame containing id, sex, warner-status,
mother id and father id.}

\item{number_of_couples}{Number of couples to form in the current
generation.}
}
\value{
A named list with two elements:
\describe{
  \item{females}{character vector of id numbers of females}
  \item{males}{character vector of id numbers of females}
}
The two vectors are of the same length.  Corresponding elements
represent a couple.
}
\description{
Default function to govern mating.  Each fertile female mates
with a randomly-selected male.  If the user chooses to modify
this a as custom function for the \code{mating_behavior}, in
\code{simulate} it shold be noted that the current values of
of \code{individuals} and \code{number_of_couples} will be
provided by the program.
}
