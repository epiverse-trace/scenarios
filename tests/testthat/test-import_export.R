
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
