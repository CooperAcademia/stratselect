consolidate_filter <- function(sampler, filter) {
  samples <- sampler$samples

  if (test_character(filter)) {
    assert_subset(filter, c("init", "burn", "adapt", "sample"))
    filter <- which(samples$stage %in% filter)
  } else {
    assert_subset(filter, 1:samples$idx)
  }
  filter
}


#' Return summary values for parameters (from extract_parameters)
#'
#' @param pars The result of extract_parameters, tibble containing samples for
#'   the selected parameter estimates.
#' @param tform Any transformation necessary to the data prior to summarising
#' @param sfunc A summary function (one value), default is median
#'
#' @return A tibble containing the medians of the samples for each subject
#'
#' @import dplyr
#' @importFrom rlang .data
#' @export
get_summary <- function(pars, tform = base::identity, sfunc = stats::median) {
  pars %>%
    mutate(value = tform(.data$value)) %>%
    group_by(.data$subjectid, .data$parameter) %>%
    summarise(value = sfunc(value))
}


#' Takes a sample tibble and rearranges it to a long df with just drift rates.
#'
#' @param sample_df A dataframe with columns for each parameter and sampleid
#'
#' @return A long tibble containing the drift rate type and values
#'
#' @import dplyr
#' @importFrom readr parse_factor
#' @importFrom rlang .data
#' @export
get_drifts <- function(sample_df) {
  sample_df %>%
  select(c(.data$sampleid, starts_with("v_"))) %>%
  pivot_longer(
    -.data$sampleid,
    names_to = c("drift", "response", "attribute", "salience"),
    names_transform = list(
      response = ~ parse_factor(.x, levels = c("acc", "rej")),
      attribute = ~ parse_factor(.x, levels = c("p", "r")),
      salience = ~ parse_factor(.x, levels = c("H", "L", "D"))
    ),
    names_sep = "_"
  ) %>%
  select(-.data$drift) %>%
  rename(drift = .data$value) %>%
  mutate(
    response = fct_recode(.data$response, Accept = "acc", Reject = "rej"),
    attribute = fct_recode(.data$attribute, Price = "p", Rating = "r")
  )
}
