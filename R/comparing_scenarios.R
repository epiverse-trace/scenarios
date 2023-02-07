#' Check for scenario matching variables
#'
#' @param x A `scenario` object.
#' @param variables A character vector of `scenario` parameter names or extra
#' information names for which to search in `x`.
#' @keywords internal
#' @return A logical indicating whether the variables correspond to parameter
#' names in `x`.
sce_has_match_variables <- function(x, variables) {
  stopifnot(
    "Object must be of the `scenario` class" =
      is_scenario(x)
  )
  # informative error message about which variable(s) is/are missing
  if (all(variables %in% c(names(x$parameters), names(x$extra_info)))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Check for variables in scenario outcomes
#'
#' @param x A `scenario` object.
#' @param variables A character vector of variables that are expected to be
#' found in the `data` list of `x`.
#' @keywords internal
#' @return A logical indicating whether the variables correspond to column names
#' in `x$data`.
sce_has_comparison_variables <- function(x, variables) {
  stopifnot(
    "Object must be of the `scenario` class" =
      is_scenario(x),
    "Scenario object must have data to check for comparison variables" =
      sce_has_data(x)
  )
  data_ <- sce_get_outcomes(x)

  # informative error message about which variable(s) is/are missing
  if (all(variables %in% colnames(data_))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Check whether a pair of scenarios is comparable
#'
#' @param baseline A `scenario` object.
#' @param compare A second `scenario` object.
#' @param match_variables A character string of scenario parameter names that is
#' used to check whether the two scenarios have identical parameters or other
#' characteristics (stored in `extra_information`).
#' @param comparison_variables A character string of column names expected in
#' the scenarios' outcome data. This is used to check whether the scenarios both
#' have the required columns in their data.
#'
#' @return A logical value indicating whether a pair of scenarios is comparable.
#' @export
#'
#' @examples
#' # prepare two scenarios of the final size of an epidemic
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
#' # run scenarios to generate data
#' pandemic_flu <- run_scenario(pandemic_flu)
#' covid19 <- run_scenario(covid19)
#'
#' # check whether scenarios are comparable
#' sce_are_comparable(
#'   baseline = pandemic_flu,
#'   compare = covid19,
#'   match_variables = "demography_vector",
#'   comparison_variables = "p_infected"
#' )
sce_are_comparable <- function(baseline, compare, match_variables,
                               comparison_variables) {
  # check inputs
  stopifnot(
    "Baseline or comparator must be `scenario` objects" =
      (all(is_scenario(baseline), is_scenario(compare))),
    "Matching variables are missing, pass a string of variables" =
      !missing(match_variables),
    "Comparison variables are missing, pass a string of variables" =
      !missing(comparison_variables),
    "Matching variables must be character vectors or single string" =
      is.character(match_variables),
    "Comparison variables must be character vectors or single string" =
      is.character(comparison_variables),
    # check whether scenarios have the same model function
    # no allowances made for differences in namespacing
    "Scenarios have different model functions and cannot be compared" =
      (baseline$model_function == compare$model_function)
  )

  # check whether both scenarios have matching and comparison variables at all
  have_match_variables <- sce_has_match_variables(baseline, match_variables) &&
    sce_has_match_variables(compare, match_variables)
  have_comparison_variables <- sce_has_comparison_variables(
    baseline,
    comparison_variables
  ) &&
    sce_has_comparison_variables(compare, comparison_variables)

  # continue processing for different cases of match and comparison variables
  if (all(have_match_variables, have_comparison_variables)) {
    # if scenarios have variables check whether matching variables are identical
    # only if expect_identical_match is TRUE

    # get baseline and comparator scenario parameters
    baseline_parameters <- sce_get_information(
      baseline,
      which = match_variables
    )
    compare_parameters <- sce_get_information(
      compare,
      which = match_variables
    )

    # check whether all matching parameters are identical (equivalent)
    can_match <- mapply(
      baseline_parameters, compare_parameters,
      FUN = function(x, y, expectation) {
        stopifnot(
          "Matching variables must refer to atomic parameters" =
            all(is.atomic(x), is.atomic(y))
        )
        # return whether all elements of x and y match
        all(x == y)
      }
    )

    # if all matching variables are identical
    if (all(can_match)) {
      return(TRUE)
    } else {
      non_match_variables <- match_variables[!can_match]
      non_match_variables <- glue::glue_collapse(
        glue::glue("'{non_match_variables}'"),
        sep = ", ", last = " and "
      )
      message(
        glue::glue(
          "Scenario parameters do not match, scenarios are not comparable.
          These parameters do not match: {non_match_variables}"
        )
      )
      return(FALSE)
    }
  } else { # case where either matching or comparison variables are missing
    message(
      glue::glue(
        "Scenarios do not share matching or comparison variables, and cannot \\
        be compared."
      )
    )
    return(FALSE)
  }
}

#' Filter for comparable scenarios
#'
#' @param x A 'comparison' object comprised of 'scenario' objects, each of which
#' must have data prepared.
#' @param match_variables A character string of scenario parameter names that is
#' used to check whether the two scenarios have identical parameters.
#' @param comparison_variables A character string of column names expected in
#' the scenarios' outcome data. This is used to check whether the scenarios both
#' have the required columns in their data.
#'
#' @return A 'comparison' object retaining only those 'scenario' objects in the
#' data list that are comparable with the baseline scenario.
#' @export
#'
#' @examples
#' pandemic_flu <- scenario(
#'   name = "pandemic_flu",
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(r0 = 1.5),
#'   extra_info = list(country = "UK", pathogen = "flu")
#' )
#'
#' covid19 <- scenario(
#'   name = "covid19",
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(r0 = 5.0),
#'   extra_info = list(country = "UK", pathogen = "SARS-CoV-2")
#' )
#'
#' # run scenarios to generate data
#' pandemic_flu <- run_scenario(pandemic_flu)
#' covid19 <- run_scenario(covid19)
#'
#' # check whether scenarios are comparable
#' outbreak_comparison <- comparison(
#'   pandemic_flu, covid19,
#'   baseline = "pandemic_flu"
#' )
#'
#' # filter on pathogen
#' sce_filter_comparable(
#'   outbreak_comparison,
#'   match_variables = "pathogen",
#'   comparison_variables = "p_infected"
#' )
sce_filter_comparable <- function(x, match_variables,
                                  comparison_variables) {
  stopifnot(
    "Error: 'x' must be a 'comparison' object" = is_comparison(x),
    "Matching variables must be a string" =
      (is.character(match_variables)),
    "Comparison variables must be a string" =
      (is.character(comparison_variables))
  )
  # get baseline scenario
  baseline_ <- x$data[[Position(
    f = function(sc) sc$name == x$baseline, x$data
  )]]
  # get which scenarios match
  # suppress messages from sce_are_comparable
  suppressMessages(
    does_match <- vapply(
      x$data, sce_are_comparable,
      FUN.VALUE = TRUE,
      baseline = baseline_,
      match_variables = match_variables,
      comparison_variables = comparison_variables
    )
  )
  # filter for matches. the baseline is always returned as a match
  x$data <- x$data[does_match]

  # set comparison match and comparison variables
  x$match_variables <- if (is.na(x$match_variables)) {
    match_variables
  } else {
    union(x$match_variables, match_variables)
  }
  x$comparison_variables <- if (is.na(x$comparison_variables)) {
    comparison_variables
  } else {
    union(x$comparison_variables, comparison_variables)
  }

  # return comparison object
  x
}
