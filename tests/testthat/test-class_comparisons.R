
# Prepare a comparison of two scenarios of the final size of an epidemic
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

# create a comparison object without matching or comparison variables
x <- comparison(
  pandemic_flu, covid19,
  baseline = "pandemic_flu"
)

test_that("comparison class is initialised correctly", {
  expect_s3_class(x, class = "comparison")
  expect_length(x, 4)
  expect_named(
    x,
    c("data", "baseline", "match_variables", "comparison_variables")
  )
  expect_type(x$data, "list")
  expect_type(x$baseline, "character")
  expect_type(x$match_variables, "character")
  expect_type(x$comparison_variables, "character")
})

test_that("Correct printing of comparison class", {
  # save a snapshot
  expect_snapshot(
    print(x) # conflicts with method from 'testthat'
  )
})

test_that("comparison object is initialised correctly in alternative ways", {
  # with a single scenario object
  x <- comparison(
    pandemic_flu,
    baseline = "pandemic_flu"
  )
  expect_s3_class(x, "comparison")

  # with a single list of scenarios
  x <- comparison(
    list(pandemic_flu, covid19),
    baseline = "pandemic_flu"
  )
  expect_s3_class(x, "comparison")
})
