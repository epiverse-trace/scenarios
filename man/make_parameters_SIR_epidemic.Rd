% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convenient_parameters.R
\name{make_parameters_SIR_epidemic}
\alias{make_parameters_SIR_epidemic}
\title{Make useful default arguments to \code{epidemics::sir_desolve()}}
\usage{
make_parameters_SIR_epidemic(beta = 1, gamma = 0.1)
}
\arguments{
\item{beta}{The rate of transmission \eqn{\beta}.}

\item{gamma}{The rate of recovery of infected individuals \eqn{\gamma}.}
}
\value{
A list of SIR model parameters, which are suitable as arguments to
\code{\link[epidemics:sir_desolve]{epidemics::sir_desolve()}}.
}
\description{
Function to make convenient default parameters for an SIR
epidemic model as implemented in \code{\link[epidemics:sir_desolve]{epidemics::sir_desolve()}}.
}
\examples{
make_parameters_SIR_epidemic()
}
