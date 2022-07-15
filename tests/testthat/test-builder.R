# setup
nghbrs <- neighbours(builder_catchments_sample)
existing_reserves_sample$Areatarget <- 10000000

# neighbours
test_that("neighbours are as expected", {
  expect_snapshot_output(
    as.data.frame(nghbrs)
  )
})

# seeds
test_that("area target method: none provided", {
  expect_error(seeds(catchments_sf = builder_catchments_sample),
               "No valid area target method provided")
})

test_that("area target method: non-matching col", {
  expect_warning(
    expect_error(seeds(catchments_sf = builder_catchments_sample, areatarget_col = "ifl"),
               "No valid area target method provided"),
    "areatarget_col provided but"
    )
})

test_that("area target method: non-matching column in areatarget_polygon", {
  test_poly <- builder_catchments_sample[1:10,]
  expect_warning(
    expect_error(seeds(catchments_sf = builder_catchments_sample, areatarget_polygon = test_poly, areatarget_polygon_col = ""),
               "No valid area target method provided"),
    "areatarget_polygon provided but"
  )
})

test_that("threshold value missing", {
  expect_error(seeds(catchments_sf = builder_catchments_sample, filter_intactness_col = "intact", filter_intactness_threshold = "x", areatarget_value = 1000),
               "filter_intactness_threshold must be provided")
})

test_that("intactness filter works", {
  expect_snapshot_output(as.data.frame(seeds(catchments_sf = builder_catchments_sample, filter_intactness_col = "intact", filter_intactness_threshold = 1, areatarget_value = 1000)))
})

test_that("no filter grabs all catchments", {
  expect_equal(length(seeds(catchments_sf = builder_catchments_sample, areatarget_value = 1000)$CATCHNUM),
               nrow(builder_catchments_sample))
})

test_that("polygon filter works", {
  test_poly <- builder_catchments_sample[1:10,]
  expect_equal(seeds(catchments_sf = builder_catchments_sample, filter_polygon = test_poly, areatarget_value = 1000)$CATCHNUM,
             builder_catchments_sample$CATCHNUM[1:10]
             )
})

test_that("polygon filter works with intactness col", {
  test_poly <- builder_catchments_sample[1:10,]
  expect_equal(seeds(catchments_sf = builder_catchments_sample, filter_intactness_col = "intact", filter_intactness_threshold = 1, filter_polygon = test_poly, areatarget_value = 1000)$CATCHNUM,
               builder_catchments_sample$CATCHNUM[1:10][builder_catchments_sample$intact[1:10] == 1]
  )
})

test_that("polygon filter works with areatarget_polygon_col", {
  test_poly <- builder_catchments_sample[1:10,]
  test_poly$areatarget <- 5000
  expect_equal(seeds(catchments_sf = builder_catchments_sample, filter_polygon = test_poly, areatarget_polygon = test_poly, areatarget_polygon_col = "areatarget"),
             dplyr::tibble(CATCHNUM=builder_catchments_sample$CATCHNUM[1:10], Areatarget = as.integer(rep(5000, 10)))
             )
})

test_that("error when areatarget_polygon_col contains a non-numeirc value", {
  test_poly <- builder_catchments_sample[1:10,]
  test_poly$areatarget <- "x"
  expect_error(seeds(catchments_sf = builder_catchments_sample, filter_polygon = test_poly, areatarget_polygon = test_poly, areatarget_polygon_col = "areatarget"),
             "values in areatarget_polygon must be numeric")
})

test_that("Zero filter catches", {
  expect_error(seeds(catchments_sf = builder_catchments_sample, filter_intactness_col = "intact", filter_intactness_threshold = 1.1, areatarget_value = 1000),
               "No catchments selected"
               )
})


# BUILDER
test_that("check seeds colname error", {
  test_poly <- builder_catchments_sample[1:10,]
  seedz <- seeds(catchments_sf = builder_catchments_sample, filter_polygon = test_poly, areatarget_value = 100000000)
  names(seedz) <- c("CATCHNUM", "areatarget")
  expect_error(builder(catchments_sf = builder_catchments_sample, seeds = seedz, neighbours = nghbrs),
               "Column 'Areatarget' not in table"
               )
})

test_that("check neighbours colname error", {
  test_poly <- builder_catchments_sample[1:10,]
  seedz <- seeds(catchments_sf = builder_catchments_sample, filter_polygon = test_poly, areatarget_value = 100000000)
  nghbrs2 <- nghbrs
  names(nghbrs2) <- c("CATCHNUM", "ngh", "key")
  expect_error(builder(catchments_sf = builder_catchments_sample, seeds = seedz, neighbours = nghbrs2),
               "Column 'neighbours' not in table"
               )
})

test_that("check catchments colname error", {
  test_poly <- builder_catchments_sample[1:10,]
  seedz <- seeds(catchments_sf = builder_catchments_sample, filter_polygon = test_poly, areatarget_value = 100000000)
  builder_catchments_sample2 <- builder_catchments_sample
  names(builder_catchments_sample2)[names(builder_catchments_sample2) == "ORDER1"] <- "ORDER_1"
  expect_error(builder(catchments_sf = builder_catchments_sample2, seeds = seedz, neighbours = nghbrs),
               "Column 'ORDER1' not in table"
               )
})

test_that("check seeds in catchments warning", {
  test_poly <- builder_catchments_sample[1:10,]
  seedz <- seeds(catchments_sf = builder_catchments_sample, filter_polygon = test_poly, areatarget_value = 100000000)
  seedz <- rbind(seedz, dplyr::tibble(CATCHNUM = as.integer(1), Areatarget = as.integer(100000000)))
  expect_warning(builder(catchments_sf = builder_catchments_sample, seeds = seedz, neighbours = nghbrs),
               "Not all seeds are in catchments_sf"
               )
})

test_that("check seeds in catchments error", {
  test_poly <- builder_catchments_sample[1:10,]
  seedz <- dplyr::tibble(CATCHNUM = as.integer(1), Areatarget = as.integer(100000000))
  expect_error(builder(catchments_sf = builder_catchments_sample, seeds = seedz, neighbours = nghbrs),
               "None of the seeds are in catchments_sf"
               )
})

test_that("check area targets error", {
  test_poly <- builder_catchments_sample[1:10,]
  seedz <- seeds(catchments_sf = builder_catchments_sample, filter_polygon = test_poly, areatarget_value = 0)
  expect_error(builder(catchments_sf = builder_catchments_sample, seeds = seedz, neighbours = nghbrs),
               "All Areatarget values must be > 0"
               )
})

test_that("test outdir", {
  test_poly <- builder_catchments_sample[1:10,]
  seedz <- seeds(catchments_sf = builder_catchments_sample, filter_polygon = test_poly, areatarget_value = 100000000)

  unlink(file.path(tempdir(), "builder2"), recursive = TRUE)
  builder_out1 <- builder(catchments_sf = builder_catchments_sample, seeds = seedz, neighbours = nghbrs, out_dir = file.path(tempdir(), "builder2"))
  builder_out2 <- builder(catchments_sf = builder_catchments_sample, seeds = seedz, neighbours = nghbrs)
  expect_equal(builder_out1, builder_out2)
  unlink(file.path(tempdir(), "builder2"), recursive = TRUE)
})

test_that("test an out_dir with spaces", {
  test_poly <- builder_catchments_sample[1:10,]
  seedz <- seeds(catchments_sf = builder_catchments_sample, filter_polygon = test_poly, areatarget_value = 100000000)

  expect_error(
    builder(catchments_sf = builder_catchments_sample, seeds = seedz, neighbours = nghbrs, out_dir = file.path(tempdir(), "builder 2")),
    "out_dir cannot have spaces"
    )
})

test_that("basic builder with defaults", {
  test_poly <- builder_catchments_sample[1:10,]
  seedz <- seeds(catchments_sf = builder_catchments_sample, filter_polygon = test_poly, areatarget_value = 100000000)
  builder_out <- builder(catchments_sf = builder_catchments_sample, seeds = seedz, neighbours = nghbrs)
  expect_snapshot(builder_out)
})

test_that("test an empty returned table, should return NULL", {
  test_poly <- builder_catchments_sample[1,]
  seedz <- seeds(catchments_sf = builder_catchments_sample, filter_polygon = test_poly, areatarget_value = 1000000000)

  expect_warning(
    expect_equal(builder(catchments_sf = builder_catchments_sample, seeds = seedz, neighbours = nghbrs), NULL),
    "No benchmarks to return"
  )
})

