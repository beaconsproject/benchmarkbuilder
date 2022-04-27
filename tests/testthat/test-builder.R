# seeds
test_that("area target method: none provided", {
  expect_error(seeds(catchments_sf = catchments_sample),
               "No valid area target method provided")
})

test_that("area target method: non-matching col", {
  expect_warning(
    expect_error(seeds(catchments_sf = catchments_sample, areatarget_col = "ifl"),
               "No valid area target method provided"),
    "areatarget_col provided but"
    )
})

test_that("area target method: non-matching column in areatarget_polygon", {
  test_poly <- catchments_sample[1:10,]
  expect_warning(
    expect_error(seeds(catchments_sf = catchments_sample, areatarget_polygon = test_poly, areatarget_polygon_col = ""),
               "No valid area target method provided"),
    "areatarget_polygon provided but"
  )
})

test_that("threshold value missing", {
  expect_error(seeds(catchments_sf = catchments_sample, filter_intactness_col = "intact", filter_intactness_threshold = "x", areatarget_value = 1000),
               "filter_intactness_threshold must be provided")
})

test_that("intactness filter works", {
  expect_snapshot_output(as.data.frame(seeds(catchments_sf = catchments_sample, filter_intactness_col = "intact", filter_intactness_threshold = 1, areatarget_value = 1000)))
})

test_that("no filter grabs all catchments", {
  expect_equal(length(seeds(catchments_sf = catchments_sample, areatarget_value = 1000)$CATCHNUM),
               nrow(catchments_sample))
})

test_that("polygon filter works", {
  test_poly <- catchments_sample[1:10,]
  expect_equal(seeds(catchments_sf = catchments_sample, filter_polygon = test_poly, areatarget_value = 1000)$CATCHNUM,
             catchments_sample$CATCHNUM[1:10]
             )
})

test_that("polygon filter works with intactness col", {
  test_poly <- catchments_sample[1:10,]
  expect_equal(seeds(catchments_sf = catchments_sample, filter_intactness_col = "intact", filter_intactness_threshold = 1, filter_polygon = test_poly, areatarget_value = 1000)$CATCHNUM,
               catchments_sample$CATCHNUM[1:10][catchments_sample$intact[1:10] == 1]
  )
})

test_that("Zero filter catches", {
  expect_error(seeds(catchments_sf = catchments_sample, filter_intactness_col = "intact", filter_intactness_threshold = 1.1, areatarget_value = 1000),
               "No catchments selected"
               )
})

# test if filter has zero results, either by non-overlapping polygon or no intactness matches
# test error for when areatarget_polygon_col containing a non-numeirc value


# BUILDER
# test and out_dir with spaces
