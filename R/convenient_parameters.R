#' Make useful default arguments to `finalsize::final_size()`
#'
#' Helper function to make useful arguments to [finalsize::final_size()],
#' with the possibility to specify \eqn{R_0}, which is set to 1.5 by default.
#' Draws on the default U.K. POLYMOD social contacts dataset included in
#' `{finalsize}`, with three age groups: 0 -- 20, 20 -- 40, and > 40.
#' Assumes a single susceptibility group, with full susceptibility.
#' Selects the Newton solver.
#'
#' @param r0 The basic reproductive number of an infection. Default is 1.5 for
#' pandemic influenza.
#'
#' @return A list of arguments to [finalsize::final_size()].
#' @export
#'
#' @examples
#' pandemic_flu_args <- make_parameters_finalsize_UK(r0 = 1.5)
#' covid_args <- make_parameters_finalsize_UK(r0 = 5.0)
make_parameters_finalsize_UK <- function(r0 = 1.5) {
  # check inputs
  checkmate::assert_number(r0, lower = 0.0, finite = TRUE)

  # get POLYMOD data from finalsize
  polymod_uk <- finalsize::polymod_uk

  # get contact matrix and demography vector
  contact_matrix <- polymod_uk$contact_matrix
  demography_vector <- polymod_uk$demography_vector

  # define the number of age groups
  n_demo_grps <- length(demography_vector)

  # prepare p_susceptibility and susceptibility
  p_susceptibility <- matrix(
    data = 1, nrow = n_demo_grps, ncol = 1L
  )
  # all individuals are fully susceptible
  susceptibility <- p_susceptibility

  # Pick solver function and R0
  solver <- "newton"

  # return list
  list(
    r0 = r0,
    contact_matrix = contact_matrix,
    demography_vector = demography_vector,
    p_susceptibility = p_susceptibility,
    susceptibility = susceptibility,
    solver = solver
  )
}

#' Make useful default arguments to `epidemics::sir_desolve()`
#'
#' @description Function to make convenient default parameters for an SIR
#' epidemic model as implemented in [epidemics::sir_desolve()].
#'
#' @param beta The rate of transmission \eqn{\beta}.
#' @param gamma The rate of recovery of infected individuals \eqn{\gamma}.
#'
#' @return A list of SIR model parameters, which are suitable as arguments to
#' [epidemics::sir_desolve()].
#' @export
#'
#' @examples
#' make_parameters_SIR_epidemic()
make_parameters_SIR_epidemic <- function(beta = 1.0, gamma = 0.1) {
  # check input
  checkmate::assert_number(beta, lower = 0.0, finite = TRUE)
  checkmate::assert_number(gamma, lower = 0.0, finite = TRUE)

  # return list
  list(
    times = seq(0, 100, length.out = 101),
    init = c(
      S = 0.99,
      I = 0.01,
      R = 0.0
    ),
    parms = c(beta = beta, gamma = gamma)
  )
}
