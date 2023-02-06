# Tests for the comparison filtering function
# Prepare data
pandemic_flu <- scenario(
  name = "pandemic_flu",
  model_function = "finalsize::final_size",
  parameters = make_parameters_finalsize_UK(r0 = 1.5),
  extra_info = list(country = "UK", pathogen = "flu")
)

covid19 <- scenario(
  name = "covid19",
  model_function = "finalsize::final_size",
  parameters = make_parameters_finalsize_UK(r0 = 5.0),
  extra_info = list(country = "UK", pathogen = "SARS-CoV-2")
)

# check whether scenarios are comparable
outbreak_comparison <- comparison(
  pandemic_flu, covid19,
  baseline = "pandemic_flu"
)

# run scenarios from the comparison
outbreak_comparison <- run_scenario(outbreak_comparison)

test_that("Filtering comparison for matching scenarios", {
  # filter on pathogen alone
  filtered_comparison <- sce_filter_comparable(
    outbreak_comparison,
    match_variables = "pathogen",
    comparison_variables = "p_infected"
  )
  expect_s3_class(filtered_comparison, "comparison")
  expect_identical(
    sce_get_scenario_names(filtered_comparison),
    "pandemic_flu"
  )

  # filter on two variables, pathogen and country
  filtered_comparison <- sce_filter_comparable(
    outbreak_comparison,
    match_variables = c("pathogen", "country"),
    comparison_variables = "p_infected"
  )
  expect_s3_class(filtered_comparison, "comparison")
  expect_identical(
    sce_get_scenario_names(filtered_comparison),
    "pandemic_flu"
  )

  # filter on country
  filtered_comparison <- sce_filter_comparable(
    outbreak_comparison,
    match_variables = "country",
    comparison_variables = "p_infected"
  )
  expect_s3_class(filtered_comparison, "comparison")
  expect_identical(
    sce_get_scenario_names(filtered_comparison),
    c("pandemic_flu", "covid19")
  )

  # check sequential comparison filtering logs all
  # matching variables
  filtered_comparison <- sce_filter_comparable(
    outbreak_comparison,
    match_variables = "country",
    comparison_variables = "p_infected"
  )
  filtered_comparison <- sce_filter_comparable(
    filtered_comparison,
    match_variables = "pathogen",
    comparison_variables = "p_infected"
  )
  expect_identical(
    sce_get_scenario_names(filtered_comparison),
    "pandemic_flu"
  )
  # check for matching variables
  expect_identical(
    filtered_comparison$match_variables,
    c("country", "pathogen")
  )
})
