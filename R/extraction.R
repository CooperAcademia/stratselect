#' Extract group level parameters from the samples
#'
#' This function taks a pmwgs sampler object and extracts the group level
#' samples for the specified parameter estimates.
#'
#' @param sampler The pmwgs sampler object
#' @param par_names The names of the parameters to extract - defaults to all
#'   parameters estimated.
#' @param filter The sampling stages to extract - defaults to all stages run.
#'   Can also be a vector of sample indices.
#'
#' @return A tibble with the parameter samples and a sampleid column
#'
#' @import dplyr
#' @export
extract_tmu <- function(sampler,
                        par_names = sampler$par_names,
                        filter = unique(sampler$samples$stage)) {
  filter <- consolidate_filter(sampler, filter)
  stages <- sampler$samples$stage[filter]

  sampler %>%
    pmwg::as_mcmc(filter = filter) %>%
    as_tibble() %>%
    select(all_of(par_names)) %>%
    mutate(sampleid = row_number()) %>%
    mutate(stageid = stages)
}

#' Extract subject level parameters from the samples
#'
#' This function taks a pmwgs sampler object and extracts the subject level
#' samples for the specified parameter estimates.
#'
#' @inheritParams extract_tmu
#'
#' @return A tibble with the parameter samples and a sampleid + subjectid column
#'
#' @import dplyr
#' @export
extract_alpha <- function(sampler,
                          par_names = sampler$par_names,
                          filter = unique(sampler$samples$stage)) {
  filter <- consolidate_filter(sampler, filter)
  stages <- sampler$samples$stage[filter]

  pmwg::as_mcmc(sampler, selection = "alpha", filter = filter) %>%
    lapply(FUN = function(x) {
      x %>%
        as_tibble() %>%
        select(all_of(par_names)) %>%
        mutate(sampleid = row_number()) %>%
        mutate(stageid = stages)
    }) %>%
    bind_rows(.id = "subjectid")
}

#' Extract group level covariances from the samples
#'
#' This function taks a pmwgs sampler object and extracts the group level
#' covariance samples for the specified parameter estimates.
#'
#' @inheritParams extract_tmu
#'
#' @return A 3D array with (M, M, N) where M is parameters and N is samples
#'
#' @import checkmate
#' @export
extract_cov <- function(sampler,
                        par_names = sampler$par_names,
                        filter = unique(sampler$samples$stage)) {
  samples <- sampler$samples
  filter <- consolidate_filter(sampler, filter)

  assert_subset(par_names, sampler$par_names)

  samples$theta_sig[par_names, par_names, filter]
}


#' Extract parameters from the samples
#'
#' This function taks a pmwgs sampler object and extracts both the group level
#' and the individual subject level samples for the specified parameter
#' estimates. The resulting tibble will bein long format
#'
#' @param sampler The pmwgs sampler object
#' @param par_names The names of the parameters to extract - defaults to all
#'   parameters estimated.
#' @inheritParams pmwg::as_mcmc
#'
#' @return A tibble with the parameter samples and a subjectid column
#'
#' @import dplyr
#' @export
extract_parameters <- function(sampler,
                               par_names = sampler$par_names,
                               filter = unique(sampler$samples$stage)) {
  tmu <- extract_tmu(sampler, par_names, filter) %>%
    mutate(subjectid = "theta_mu")

  extract_alpha(sampler, par_names, filter) %>%
    bind_rows(tmu) %>%
    tidyr::pivot_longer(cols = -c(subjectid, sampleid, stageid),
                        names_to = "parameter")
}
