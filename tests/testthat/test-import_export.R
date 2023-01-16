
# create a scenario object
scenario_pandemic_flu <- scenario(
  model_function = "finalsize::final_size",
  parameters = make_parameters_finalsize_UK(),
  replicates = 1
)

# create temporary filepath
tmpfile <- tempfile(fileext = ".json")

test_that("Import and export from JSON works", {
  expect_silent(
    sce_to_json(scenario_pandemic_flu, file = tmpfile)
  )
  expect_silent(
    scenario_ <- sce_from_json(tmpfile)
  )
  expect_s3_class(
    scenario_, "scenario"
  )
})

# run the scenario
scenario_pandemic_flu <- run_scenario(scenario_pandemic_flu)
test_that("Import and export from JSON works with data", {
  expect_silent(
    sce_to_json(scenario_pandemic_flu, file = tmpfile)
  )
  expect_silent(
    scenario_ <- sce_from_json(tmpfile)
  )
  expect_s3_class(
    scenario_, "scenario"
  )
  expect_true(
    sce_has_data(scenario_)
  )
  expect_s3_class(
    sce_get_outcomes(scenario_),
    "data.table"
  )
})
