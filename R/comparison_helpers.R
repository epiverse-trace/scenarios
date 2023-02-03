#' Get scenario names from a comparison object
#'
#' @param x A comparison object.
#'
#' @return A vector of scenario names.
#' @export
#'
#' @examples
#' # create some scenarios
#' pandemic_flu <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(r0 = 1.5),
#'   replicates = 1L
#' )
#'
#' covid19 <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(r0 = 5.0),
#'   replicates = 1L
#' )
#'
#' # create comparison, passing prepared scenarios with some names
#' outbreak_comparison <- comparison(
#'   pandemic_flu = pandemic_flu, covid19 = covid19,
#'   baseline = "pandemic_flu"
#' )
sce_get_scenario_names <- function(x) {
  # input checking
  stopifnot(
    "Input 'x' must be a `comparison` object" =
      is_comparison(x)
  )

  names(x$data)
}

#' Set scenario names in a comparison object.
#'
#' @param x A `comparison` object.
#' @param old_names A character vector of the names of the scenarios in this
#' comparison object, which are to be replaced.
#' @param new_names A character vector of the names with which to replace the
#' names given in `old_names`.
#' @param new_baseline Optional. The name of the new baseline scenario, if this
#' has been changed. The old baseline is retained if this argument is missing.
#'
#' @return A `comparison` object with new scenario names.
#' @export
#'
#' @examples
#' # create some scenarios
#' pandemic_flu <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(r0 = 1.5),
#'   replicates = 1L
#' )
#'
#' covid19 <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(r0 = 5.0),
#'   replicates = 1L
#' )
#'
#' # create comparison, passing prepared scenarios with some names
#' outbreak_comparison <- comparison(
#'   pandemic_flu = pandemic_flu, covid19 = covid19,
#'   baseline = "pandemic_flu"
#' )
#'
#' # change all names to upper case
#' outbreak_comparison <- sce_set_scenario_names(
#'   x = outbreak_comparison,
#'   old_names = "pandemic_flu",
#'   new_names = "Influenza",
#'   new_baseline = "Influenza"
#' )
#'
sce_set_scenario_names <- function(x, old_names, new_names, new_baseline) {
  # input checking
  stopifnot(
    "Input 'x' must be a `comparison` object" =
      is_comparison(x),
    "Old names must be passed as a character vector" =
      is.character(old_names),
    "New names must be passed as a character vector" =
      is.character(new_names),
    "Lengths of old and new names must match" =
      (length(old_names) == length(new_names)),
    "Length of 'old_names' must be less than or equal to" =
      (length(old_names) <= length(x$data)),
    "Old scenario names not found in comparison object" =
      all(old_names %in% sce_get_scenario_names(x)),
    "New baseline name is not among new scenario names" =
      new_baseline %in% new_names
    # other checks for new baseline in the sce_set_baseline_function
  )

  # swap names: make new names named, match names to data, assign new names
  names(new_names) <- old_names
  new_names_ <- names(x$data)
  names(new_names_) <- new_names_
  new_names_[old_names] <- new_names[old_names]
  names(x$data) <- new_names_

  # set new baseline if needed
  if (!missing(new_baseline)) {
    x <- sce_set_baseline(x, new_baseline = new_baseline)
  }

  # validate comparison and return
  validate_comparison(x)
  x
}

#' Change the baseline scenario in a comparison
#'
#' @param x A `comparison` object.
#' @param new_baseline The name of a `scenario` object contained in the
#' `comparison` which is to be considered the new baseline.
#'
#' @return The `comparison` object with the baseline tag set to the new baseline
#' .
#' @export
#'
#' @examples
#' # create some scenarios
#' pandemic_flu <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(r0 = 1.5),
#'   replicates = 1L
#' )
#'
#' covid19 <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(r0 = 5.0),
#'   replicates = 1L
#' )
#'
#' # create comparison, passing prepared scenarios with some names
#' outbreak_comparison <- comparison(
#'   pandemic_flu = pandemic_flu, covid19 = covid19,
#'   baseline = "pandemic_flu"
#' )
#'
#' # change baseline
#' sce_set_baseline(
#'   outbreak_comparison, "covid19"
#' )
#'
sce_set_baseline <- function(x, new_baseline) {
  # input checking
  stopifnot(
    "Input 'x' must be a `comparison` object" =
      is_comparison(x),
    "New baseline must be a single string" =
      is.character(new_baseline) && length(new_baseline) == 1L,
    "New baseline must be among scenario names" =
      (new_baseline) %in% sce_get_scenario_names(x)
  )

  # check whether multiple scenarios have the same name as baseline
  if (length(which(sce_get_scenario_names(x) == new_baseline)) > 1L) {
    stop(
      "Error: Multiple scenario names match new baseline!"
    )
  } else {
    x$baseline <- new_baseline
  }

  # validate and return
  validate_comparison(x)
  x
}

#' Get the name of the baseline scenario
#'
#' @param x A `comparison` object.
#'
#' @return The name of the baseline scenario of the comparison.
#' @export
#'
#' @examples
#' # create some scenarios
#' pandemic_flu <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(r0 = 1.5),
#'   replicates = 1L
#' )
#'
#' covid19 <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(r0 = 5.0),
#'   replicates = 1L
#' )
#'
#' # create comparison, passing prepared scenarios with some names
#' outbreak_comparison <- comparison(
#'   pandemic_flu = pandemic_flu, covid19 = covid19,
#'   baseline = "pandemic_flu"
#' )
#'
#' # change baseline
#' sce_set_baseline(
#'   outbreak_comparison, "covid19"
#' )
#'
#' # get baseline
#' sce_get_baseline(outbreak_comparison)
#'
sce_get_baseline <- function(x) {
  # input checking
  checkmate::assert_class(x, "comparison")

  x$baseline
}
