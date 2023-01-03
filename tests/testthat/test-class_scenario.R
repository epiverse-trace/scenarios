test_that("scenario class is initialised correctly", {
  
  # simple scenario
  pandemic_influenza <- scenario(r0 = 1.5, replicates = 10L)
  
  expect_s3_class(pandemic_influenza, class = "scenario")
  expect_length(pandemic_influenza, 4)
  expect_named(
    pandemic_influenza,
    c("r0", "replicates", "data", "data_prepared")
  )
  expect_type(pandemic_influenza$r0, "double")
  expect_type(pandemic_influenza$replicates, "integer")
  expect_type(pandemic_influenza$data, "list")
  expect_type(pandemic_influenza$data_prepared, "logical")
})

test_that("Correct printing of scenario class", {
  # save a snapshot
  expect_snapshot(
    scenario(r0 = 1.5, replicates = 10L)
  )
})
