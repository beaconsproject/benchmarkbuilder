# neighbours
test_that("neighbours are as expected", {
  expect_snapshot_output(
    as.data.frame(neighbours(catchments_sample))
  )
})
