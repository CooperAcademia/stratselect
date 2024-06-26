% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extraction.R
\name{extract_cov}
\alias{extract_cov}
\title{Extract group level covariances from the samples}
\usage{
extract_cov(
  sampler,
  par_names = sampler$par_names,
  filter = unique(sampler$samples$stage)
)
}
\arguments{
\item{sampler}{The pmwgs sampler object}

\item{par_names}{The names of the parameters to extract - defaults to all
parameters estimated.}

\item{filter}{The sampling stages to extract - defaults to all stages run.
Can also be a vector of sample indices.}
}
\value{
A 3D array with (M, M, N) where M is parameters and N is samples
}
\description{
This function taks a pmwgs sampler object and extracts the group level
covariance samples for the specified parameter estimates.
}
