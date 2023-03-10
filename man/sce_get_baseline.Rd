% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparison_helpers.R
\name{sce_get_baseline}
\alias{sce_get_baseline}
\title{Get the name of the baseline scenario}
\usage{
sce_get_baseline(x)
}
\arguments{
\item{x}{A \code{comparison} object.}
}
\value{
The name of the baseline scenario of the comparison.
}
\description{
Get the name of the baseline scenario
}
\examples{
# create some scenarios
pandemic_flu <- scenario(
  name = "pandemic_flu",
  model_function = "finalsize::final_size",
  parameters = make_parameters_finalsize_UK(r0 = 1.5),
  replicates = 1L
)

covid19 <- scenario(
  name = "covid19",
  model_function = "finalsize::final_size",
  parameters = make_parameters_finalsize_UK(r0 = 5.0),
  replicates = 1L
)

# create comparison, passing prepared scenarios with some names
outbreak_comparison <- comparison(
  pandemic_flu, covid19,
  baseline = "pandemic_flu"
)

# change baseline
sce_set_baseline(
  outbreak_comparison, "covid19"
)

# get baseline
sce_get_baseline(outbreak_comparison)

}
