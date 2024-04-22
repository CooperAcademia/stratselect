#' Create an output basename from envars list
#'
#' From a list extracted from environment variable for a modelling job
#' extract the necessary element for a basename for output files.
#'
#' @param vars The list of object extracted from the environment
#'
#' @return a character vector with 1 element, the basename
#'
#' @export
get_base_filename <- function(vars) {
  exp <- vars$experiment
  id <- vars$jobid
  tag <- vars$tag

  if (vars$experiment == "SymbolicVDCE") {
    tag <- paste(vars$displaytype, tag, sep = "_")
  }

  if (vars$method == "recovery") {
    tag <- paste(vars$test_model, tag, sep = "_")
  }

  paste(exp, id, tag, sep = "_")
}
