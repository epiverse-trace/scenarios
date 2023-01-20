
# Prepare a comparison of two scenarios of the final size of an epidemic
pandemic_flu <- scenario(
  model_function = "finalsize::final_size",
  parameters = make_parameters_finalsize_UK(r0 = 1.5),
  replicates = 1L
)

covid19 <- scenario(
  model_function = "finalsize::final_size",
  parameters = make_parameters_finalsize_UK(r0 = 5.0),
  replicates = 1L
)

# create a comparison object without matching or comparison variables
x1 <- comparison(
  pandemic_flu = pandemic_flu, covid19 = covid19,
  baseline = "pandemic_flu"
)
# create an identical comparison using a list of scenarios
x2 <- comparison(
  list(pandemic_flu = pandemic_flu, covid19 = covid19),
  baseline = "pandemic_flu"
)

test_that("comparison class is initialised correctly", {
  expect_s3_class(x1, class = "comparison")
  expect_length(x2, 4)
  expect_named(
    x1,
    c("data", "baseline", "match_variables", "comparison_variables")
  )
  expect_type(x2$data, "list")
  expect_type(x1$baseline, "character")
  expect_type(x2$match_variables, "character")
  expect_type(x1$comparison_variables, "character")
})

test_that("Correct printing of comparison class", {
  # save a snapshot
  expect_snapshot(
    print(x1) # conflicts with method from 'testthat'
  )
})
