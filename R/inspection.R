#' Extract parameters from the samples
#'
#' This function taks a pmwgs sampler object and extracts both the group level
#' and the individual subject level samples for the specified parameter estimates.
#'
#' @param sampler The pmwgs sampler object
#' @param par_names The names of the parameters to extract
#'
#' @return A tibble with the parameter samples and a subjectid column
#'
#' @import dplyr
#' @export
extract_parameters <- function(sampler, par_names) {
  tmu <- sampler %>%
    pmwg::as_mcmc(filter = "sample") %>%
    data.frame() %>%
    tibble() %>%
    select(all_of(par_names)) %>%
    mutate(subjectid = "theta_mu")

  pmwg::as_mcmc(sampler, selection = "alpha", filter = "sample") %>%
    lapply(FUN = function(x) {
      x %>%
        data.frame() %>%
        tibble() %>%
        select(all_of(par_names))
    }) %>%
    bind_rows(.id = "subjectid") %>%
    tibble() %>%
    bind_rows(tmu)
}


#' Return median values for  parameters (from extract_parameters)
#'
#' @param pars The result of extract_parameters, tibble containing samples for
#'   the selected parameter estimates.
#'
#' @return A tibble containing the medians of the samples for each subject
#'
#' @import dplyr
#' @export
get_medians <- function(pars, alpha = TRUE) {
  if (alpha) {
    pars %>%
      group_by(subjectid) %>%
      summarise(across(.fns = stats::median)) %>%
      tidyr::pivot_longer(-subjectid,
                          names_to = c("drop", "Parameter"),
                          names_sep = "_",
                          names_transform = list(Parameter = as.factor)) %>%
      mutate(value = exp(value)) %>%
      select(-drop)
  } else {
    pars %>%
      group_by(subjectid) %>%
      summarise(across(.fns = stats::median)) %>%
      tidyr::pivot_longer(-subjectid, names_to = "Parameter") %>%
      mutate(value = exp(value))
  }
}