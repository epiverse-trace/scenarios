#' Constructor for the `scenario` class
#'
#' @description Create a scenario object check inputs.
#'
#' @param r0 The basic reproductive number \eqn{R_0} of the infection.
#' @param replicates The number of scenario replicates.
#' @return scenario object
#' @keywords internal
#'
#' @examples
#' scenarios:::new_scenario(
#'   r0 = 1.5, replicates = 10
#' )
new_scenario <- function(r0 = numeric(1), replicates = integer(1)) {
  # check input
  checkmate::assert_number(r0, lower = 0.0, finite = TRUE, null.ok = FALSE)
  checkmate::assert_int(replicates, lower = 1, null.ok = FALSE)

  # create and return epidist class
  structure(
    list(
      r0 = r0,
      replicates = replicates,
      data = vector(mode = "list", length = replicates),
      data_prepared = FALSE # whether the data list is populated with results
    ),
    class = "scenario"
  )
}

#' Create a `scenario` object
#'
#' @description The `scenario` class is intended to store the outcomes of a
#' number of runs of an epidemic simulation. This is a work in progress, and is
#' initially targeted for compatibility with outputs from
#' [finalsize::final_size()].
#'
#' @param r0 The basic reproductive number \eqn{R_0} of the infection.
#' @param replicates The number of scenario replicates.
#'
#' @return A `scenario` object
#' @export
#'
#' @examples
#' pandemic_influenza <- scenario(
#'   r0 = 1.5, replicates = 10
#' )
scenario <- function(r0, replicates = 1L) {
  # check input
  checkmate::assert_number(r0, lower = 0.0, finite = TRUE, null.ok = FALSE)
  checkmate::assert_int(replicates, lower = 1, null.ok = FALSE)

  # call scenario constructor
  scenario <- new_scenario(
    r0 = r0,
    replicates = replicates
  )

  # call scenario validator
  validate_scenario(scenario = scenario)

  # return epidist object
  scenario
}

#' Validator for the `scenario` class
#'
#' @param scenario A `scenario` object
#'
#' @return None. Errors when an invalid `scenario` object is provided.
#' @export
validate_scenario <- function(scenario) {
  if (!inherits(scenario, "scenario")) {
    stop("Object should be of class scenario")
  }

  # check for class invariants
  stopifnot(
    "scenario object does not contain the correct attributes" =
      (c("r0", "replicates", "data") %in%
        attributes(scenario)$names),
    "Scenario R0 must be a single number > 0.0" =
      (is.numeric(scenario$r0) && length(scenario$r0) == 1 && scenario$r0 > 0),
    "Scenario data list must be the same length as the number of replicates" =
      (length(scenario$data) == scenario$replicates),
    "Scenario data list should not be initialised" =
      (all(vapply(scenario$data, is.null, FUN.VALUE = TRUE)))
  )
  invisible(scenario)
}

#' @export
print.scenario <- function(x, ...) {
  writeLines(
    c(
      sprintf("Epidemic scenario object"),
      ifelse(
        cli::is_utf8_output(),
        sprintf(" R\u2080: %s", x$r0),
        sprintf(" R_0: %s", x$r0)
      ),
      sprintf(" Scenario replicates: %s", x$replicates),
      sprintf(" Scenario outcomes are %s", ifelse(
        x$data_prepared, "prepared", "not prepared"
      ))
    )
  )

  invisible(x)
}
