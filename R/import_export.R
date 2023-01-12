
#' Write scenario to JSON
#'
#' @param x A `scenario` object.
#' @param file The file to which to write the JSON.
#'
#' @return Writes the `scenario` object to JSON.
#' @export
#'
#' @examples
#' scenario_pandemic_flu <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(),
#'   replicates = 1
#' )
#'
#' sce_to_json(scenario_pandemic_flu, file = tempfile())
sce_to_json <- function(x, file) {
  # input checking
  checkmate::assert_class(x, "scenario")
  validate_scenario(x)
  checkmate::assert_named(x)
  checkmate::assert_named(x$parameters)

  jsonlite::write_json(
    lapply(x, `[`),
    path = file,
    pretty = TRUE
  )
}

#' Read scenario from JSON
#'
#' @param file A JSON file
#'
#' @return A `scenario` object.
#' @export
#'
#' @examples
#' scenario_pandemic_flu <- scenario(
#'   model_function = "finalsize::final_size",
#'   parameters = make_parameters_finalsize_UK(),
#'   replicates = 1
#' )
#' # create temporary filepath
#' tmpfile <- tempfile(fileext = ".json")
#' sce_to_json(scenario_pandemic_flu, file = tmpfile)
#' sce_from_json(tmpfile)
sce_from_json <- function(file) {
  stopifnot(
    "File may not be a JSON file" =
      (grepl(pattern = "(?i)json", file))
  )
  input <- jsonlite::read_json(
    path = file,
    simplifyVector = TRUE
  )

  # check input
  checkmate::assert_named(
    input
  )
  checkmate::assert_names(
    names(input),
    must.include = c("model_function", "parameters", "replicates", "data")
  )

  # get model function
  model_function <- input[["model_function"]]

  # get parameter list
  parameters <- input[["parameters"]]

  # get replicates
  replicates <- input[["replicates"]]

  # get scenario data
  data <- input[["data"]]
  if ("output" %in% colnames(data) &&
    all(vapply(data$output, is.null, FUN.VALUE = TRUE))) {
    data$output <- vector("list", length = replicates)
  }
  data <- data.table::as.data.table(data)

  # create scenario object
  scenario_ <- structure(
    list(
      model_function = model_function,
      parameters = parameters,
      replicates = replicates,
      data = data
    ),
    class = "scenario"
  )
  validate_scenario(scenario_, data_ok = TRUE)

  # return scenario
  scenario_
}
