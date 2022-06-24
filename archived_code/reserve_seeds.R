### seeds_reserve ###
#
#' Create a reserves seeds table.
#'
#' The reserves seeds table is an input to BUILDER that lists seed catchments and area targets for a set of reserves.
#' Reserve seed catchments are aggregations of catchments to be built into benchmarks. Typically an aggregation represents
#' and existing protected area. This function takes a polygon labelled with area target values and extracts the intersecting catchments
#' to create the reserve seeds table. Reserve seeds can optionally be filtered by their intactness values.
#'
#' ### Aggregating reserve catchments
#' A list of seed catchments for each reserve in \code{reserve_polygons} is selected based on their centroid (more specifically,
#' \code{sf::st_point_on_surface()}) falling inside the reserve. Reserve names are pulled from the \code{name_col} and area
#' targets from the \code{areatarget_col}.
#'
#' ### Filtering
#' If \code{filter_intactness_col} and \code{filter_intactness_threshold} are provided, catchments will be filtered to include those
#' with an intactness value >= the threshold. If no filter arguments are provided, all aggregated catchments in the reserve will be
#' added to the seeds table.
#'
#' ### Area targets
#' Area targets should be provided in m2 in a \code{areatarget_col} in the \code{reserve_polygons}.
#'
#' ### Passing to builder()
#' The BUILDER software requires reserve seeds in a wide format csv or txt table. The output of \code{seeds_reserve()} can be converted
#' to the wide format using \code{reserve_seeds_to_builder()}. This is done automatically by \code{builder_reserve()}.
#'
#' @param catchments_sf sf object of the catchments dataset with unique identifier column: CATCHNUM.
#' @param reserve_polygons sf object of reserves used to select reserve seed catchments.
#' @param name_col Name of column in \code{reserve_polygons} holding unique reserve names.
#' @param areatarget_col Name of column in \code{reserve_polygons} holding area target in m2 for each reserve.
#' @param filter_intactness_col Optional intactness column in \code{catchments_sf} to filter on.
#' @param filter_intactness_threshold If \code{filter_intactness_col} provided, the minimum intactness value to filter on.
#'
#' @return A long tibble of seed catchments and their area targets grouped by reserve names.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' existing_reserves_sample$areatarget_m2 <- 100000000
#' seeds_reserve(builder_catchments_sample, existing_reserves_sample, "reserve", "areatarget_m2")
#' seeds_reserve(builder_catchments_sample, existing_reserves_sample, "reserve",
#'               "areatarget_m2", "intact", 1)
#'
#' # Save as wide format required by BUILDER and save
#' seeds_reserve_wide <-
#'   reserve_seeds_to_builder(
#'     seeds_reserve(builder_catchments_sample, existing_reserves_sample, "reserve", "areatarget_m2")
#'     )
#' #write.csv(seeds_reserve_wide, "reserve_seeds.csv", quote = FALSE, row.names = FALSE)
seeds_reserve <- function(catchments_sf, reserve_polygons, name_col, areatarget_col, filter_intactness_col = NULL, filter_intactness_threshold = NULL){

  # CHECKS
  # check geometry
  check_for_geometry(catchments_sf)
  check_for_geometry(reserve_polygons)

  # check catchnum and convert to integer
  catchments_sf <- make_catchnum_integer(catchments_sf)

  # check columns
  check_colnames(reserve_polygons, "reserve_polygons", c(name_col, areatarget_col))

  if(!is.null(filter_intactness_col)){
    check_colnames(catchments_sf, "catchments_sf", filter_intactness_col)
  }

  # if filter_intactness_col, filter_intactness_threshold must be provided
  if(!is.null(filter_intactness_col)){
    if(!is.numeric(filter_intactness_threshold)){
      stop("filter_intactness_threshold must be provided with a filter_intactness_col")
    }
  }

  # force output to use reserve and Areatarget
  names(reserve_polygons)[names(reserve_polygons) == name_col] <- "Name"
  names(reserve_polygons)[names(reserve_polygons) == areatarget_col] <- "Areatarget"

  # FILTER
  filtered_catchments <- catchments_sf
  sf::st_agr(filtered_catchments) = "constant"

  # filter by intactness
  if(!is.null(filter_intactness_col)){

    filtered_catchments <- filtered_catchments %>%
      dplyr::filter(.data[[filter_intactness_col]] >= as.numeric(filter_intactness_threshold))
  }

  # AGGREGATE
  # get catchnums in each reserve polygon
  catch_within <- filtered_catchments %>%
    sf::st_point_on_surface() %>% # get catchment centroids.
    sf::st_within(reserve_polygons) %>% # test within for all centroids, returns a row for each match. row.id is a catchnum index, col.id is a reserve index.
    as.data.frame()
  filtered_catchments$key <- 1:nrow(filtered_catchments) # add a key column to sf table. Must be an index to match st_within output
  sf_catch_key <- sf::st_drop_geometry(filtered_catchments[c("key","CATCHNUM")])
  reserve_polygons$key <- 1:nrow(reserve_polygons) # add key to reserve_polygons
  reserve_key <- sf::st_drop_geometry(reserve_polygons[c("key", "Name", "Areatarget")])

  # convert indexes from st_within to catchnums using the keys to join
  out_tab <- catch_within %>%
    dplyr::left_join(sf_catch_key, by = c("row.id" = "key")) %>%
    dplyr::left_join(reserve_key, by = c("col.id" = "key")) %>%
    dplyr::select(.data$Name, .data$Areatarget, .data$CATCHNUM) %>%
    dplyr::mutate(Areatarget = as.integer(ceiling(.data$Areatarget))) %>%
    dplyr::arrange(.data$Name, "CATCHNUM") %>%
    dplyr::tibble()

  return(out_tab)
}

### reserve_seeds_to_builder ###
#
#' Prepares a reserve seed table for BUILDER.
#'
#' Takes a long reserve seeds table output by \code{seeds_reserve()} and converts it to the comma separated format required
#' by BUILDER.
#'
#' Output of \code{reserve_seeds_to_builder} must be saved using \code{write.csv(..., row.names = FALSE, quote = FALSE)} to
#' be passed to BUILDER.
#'
#' \code{reserve_seeds_to_builder} is called internally by \code{builder_reserve()} so is only needed if preparing a reserve seed
#' table to pass directly to the BUILDER software GUI (outside of R).
#'
#' @param seeds_reserve_long Tibble or data frame output by \code{seeds_reserve()} with columns \code{Name},
#' \code{Areatarget} and \code{CATCHNUM}.
#'
#' @return A wide tibble with one row per reserve and CATCHNUMS separated by commas.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' existing_reserves_sample$areatarget_m2 <- 100000000
#' seeds_reserve_wide <-
#'   reserve_seeds_to_builder(
#'     seeds_reserve(builder_catchments_sample, existing_reserves_sample, "reserve", "areatarget_m2")
#'     )
#' #write.csv(seeds_reserve_wide, "reserve_seeds.csv", quote = FALSE, row.names = FALSE)
reserve_seeds_to_builder <- function(seeds_reserve_long){

  out_tab <- seeds_reserve_long %>%
    dplyr::group_by(.data$Name, .data$Areatarget) %>%
    dplyr::summarise(CATCHNUMS = paste(.data$CATCHNUM, collapse=","))

  return(out_tab)
}

### builder_reserve ###
#
#' Call the BUILDER software from R in reserve mode.
#'
#' Prepares and passes all input tables and variables to the BUIDLER executable and returns the benchmark table to R.
#'
#' This function prepares the input tables (neighbours, seeds, catchments) required by BUILDER in a temp file, then creates
#' a string of parameters that is passed to the BUILDER executable using \code{system()}. The BUILDER output tables are saved to the
#' temp file, and the output table listing benchmark names and catchment lists is returned to the R session.
#'
#' This function only returns the BUILDER output table describing the lists of catchments making up each benchmark. We typically recommend
#' using the \code{beaconstools} package to calculate hydrology metrics, but hydrology information as well as other summary information is
#' produced by BUILDER.
#' If users want access to the complete set of BUILDER output tables (e.g. hydrology and summary information), a valid output directory
#' can be provided in which case all input and output tables will be saved to that directory. If no output directory is provided a temp
#' folder will be used and deleted after the function completes. As well as providing access to the full list of BUILDER output tables,
#' an output directory is useful in a looped workflow with multiple calls to BUILDER producing multiple sets of output files. In this
#' workflow a different output directory can be used to save each set of output files which can later be processed using the \code{beaconstools} package.
#'
#' The \code{builder_reserve()} function calls BUILDER in 'reserve' mode where each seed is an aggregation of catchments that will be built into a
#' benchmark. See \code{builder()} for calling BUILDER in 'catchment' mode.
#'
#' The main parameters to adjust when building benchmarks are:
#' \itemize{
#'   \item{Area target: Set in the seeds table, this defines the minimum size for each benchmark.}
#'   \item{Catchment-level intactness: Only catchments with intactness values greater than this value will be added to the benchmark.}
#'   \item{Benchmark-level intactness: Only benchmarks with an area-weighted intactness greater than or equal to this value will be returned.}
#'   \item{Area-target proportion: The proportion of the Area target that benchmarks need to meet. Allows output of benchmarks smaller than
#'   the area target (e.g. if no benchmarks meet 100% of the area target, area target multiplier can be lowered to 0.9 to search for smaller
#'   benchmarks).}
#'   }
#'
#' @param catchments_sf sf object of the catchments dataset with unique identifier column: CATCHNUM.
#' @param seeds Seeds table from \code{seeds_reserve()} listing reserve names, seed catchments and area targets.
#' @param neighbours Neighbours table from \code{neighbours()} listing all neighbouring pairs of catchments.
#' @param out_dir If provided, input (seeds, neighbours and catchments) files and output BUILDER tables will be saved to this directory. Otherwise
#' a temp directory will be used. Function will attempt to create the directory if it doesn't already exist.
#' @param catchment_level_intactness Minimum intactness value for catchment inclusion (between 0-1). i.e. if value of 1 is used, only 100% intact
#' catchments will be used to build benchmarks.
#' @param benchmark_level_intactness Minimum area-weighted intactness of final benchmarks. Only benchmarks meeting this value will be returned.
#' @param area_target_proportion The proportion of the Area target that benchmarks need to meet (0-1) to be returned by the function.
#' @param area_type If 'land', only terrestrial areas are counted towards the area target. If 'water', only water areas are counter. If 'landwater',
#' all areas are counted.
#' @param construct_benchmarks Should builder build benchmarks?
#' @param area_target_multiplier Multiplier applied to the area target in the seed list that allows for adjustments to the area target without having
#' to remake the seeds table.
#' @param handle_isolated_catchments Should small isolated catchments be merged into the benchmarks?
#' @param output_upstream Should upstream catchments be calculated?
#' @param output_downstream Should downstream catchments be calculated?
#' @param output_hydrology_metrics Should hydrology metrics be calculated?
#' @param area_land Catchments column listing the area of land in each catchment.
#' @param area_water Catchments column listing the area of water in each catchment.
#' @param skeluid Catchments column listing the skeluid field.
#' @param catchnum Catchments column listing the unique catchments ID.
#' @param subzone Catchments column listing the hydrological subzone.
#' @param zone Catchments column listing the hydrological zone.
#' @param basin Catchments column listing the basin field.
#' @param order1 Catchments column listing the order 1 field.
#' @param order2 Catchments column listing the order 2 field.
#' @param order3 Catchments column listing the order 3 field.
#' @param stream_length Catchments column listing stream length.
#' @param intactness Catchments column listing the intactness value.
#' @param isolated Catchments column identifying catchments isolated from the stream network.
#' @param benchmark_identifier Prefix for benchmark names.
#' @param handler_summary Should a summary listing counts of available benchmarks at different intactness and area target proportions be created?
#' @param benchmark_level_intactness_props If a summary is created, provide the intactness proportions to be summarised in the format '0.7,0.8,0.9'.
#' @param area_target_props If a summary is created, provide the area target proportions to be summarised in the format '0.7,0.8,0.9'.
#'
#' @return A tibble with column names representing benchmarks, and rows of catchments making up the benchmarks.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Build reserves out to 1000km using all catchments at least 50% intact.
#' # Return any options with an area-weighted intactness of at least 80%.
#' nghbrs <- neighbours(builder_catchments_sample)
#' existing_reserves_sample$Areatarget <- 1000000000
#' seed <- seeds_reserve(catchments_sf = builder_catchments_sample,
#'                       reserve_polygons = existing_reserves_sample,
#'                       name_col = "reserve", areatarget_col = "Areatarget")
#' builder_reserve(catchments_sf = builder_catchments_sample, seeds = seed, neighbours = nghbrs,
#'                 catchment_level_intactness = 0.5, benchmark_level_intactness = 0.8)
builder_reserve <- function(catchments_sf, seeds, neighbours, # input tables
                            out_dir = NULL, # output dir
                            catchment_level_intactness = 1, benchmark_level_intactness = 1, area_target_proportion = 1,
                            area_type = "land", construct_benchmarks = TRUE,  area_target_multiplier = 1,
                            handle_isolated_catchments = TRUE, output_upstream = FALSE, output_downstream = FALSE, output_hydrology_metrics = FALSE,
                            area_land = "Area_Land", area_water = "Area_Water",
                            skeluid = "SKELUID", catchnum = "CATCHNUM", subzone = "FDAHUC8", zone = "ZONE", basin = "BASIN", order1 = "ORDER1",
                            order2 = "ORDER2", order3 = "ORDER3", stream_length = "STRMLEN", intactness = "intact", isolated = "Isolated",
                            benchmark_identifier = "PB", handler_summary = FALSE, benchmark_level_intactness_props = '""', area_target_props = '""'
){
  # builder path
  builder_cmd <- system.file("exec", "BenchmarkBuilder_cmd.exe", package = "benchmarkbuilder")

  # set up output directory. Default is a temp folder. out_dir can be used if builder output should be saved, e.g. in a looped analysis.
  if(is.null(out_dir)){
    tmpdir <- file.path(tempdir(), "builder")
    if(!dir.exists(tmpdir)){
      dir.create(tmpdir)
    }
    outdir <- tmpdir
  } else{
    outdir <- out_dir
    # check no spaces
    if(grepl(" ", outdir)){
      stop("out_dir cannot have spaces")
    }
    if(!dir.exists(outdir)){
      dir.create(outdir, recursive = TRUE)
    }
  }

  # save seeds table to outdir
  message("checking seeds table")
  check_colnames(seeds, "seeds", c("Name", "CATCHNUM", "Areatarget"))
  seeds <- make_all_integer(seeds, c("CATCHNUM", "Areatarget"))
  seeds <- make_all_character(seeds, c("Name"))
  check_seeds_areatargets(seeds)

  # For reserve seeds, builder throws error if all catchnums are not present
  if(!all(seeds$CATCHNUM %in% catchments_sf$CATCHNUM)){
    stop("All reserve seeds must be in catchments_sf")
  }

  utils::write.csv(reserve_seeds_to_builder(seeds), file.path(outdir, "seeds.txt"), row.names = FALSE, quote = FALSE)

  # save neighbours table to outdir
  message("checking neighbours table")
  check_colnames(neighbours, "neighbours", c("CATCHNUM", "neighbours", "key"))
  neighbours <- make_all_integer(neighbours)
  utils::write.csv(neighbours, file.path(outdir, "neighbours.csv"), row.names = FALSE)

  # save catchment attributes
  catchments_csv <- sf::st_drop_geometry(catchments_sf)
  check_colnames(catchments_csv, "catchments_sf", c(area_land, area_water, skeluid, catchnum, subzone, zone, basin, order1, order2, order3, stream_length, intactness, isolated)) # check all provided columns are present
  catchments_csv <- make_all_character(catchments_csv, c(order1, order3, zone, subzone, basin)) # check type
  catchments_csv <- make_all_integer(catchments_csv, c(catchnum, skeluid, isolated))
  catchments_csv <- make_all_numeric(catchments_csv, c(order2, area_land, area_water, intactness))

  catchments_file <- file.path(outdir, "catchments.csv")
  catchments_table_name <- "catchments"
  utils::write.csv(catchments_csv, catchments_file, row.names = FALSE)

  # convert TRUE FALSE to 1 0
  construct_benchmarks <- logical_to_integer(construct_benchmarks)
  handle_isolated_catchments <- logical_to_integer(handle_isolated_catchments)
  output_upstream <- logical_to_integer(output_upstream)
  output_downstream <- logical_to_integer(output_downstream)
  output_hydrology_metrics <- logical_to_integer(output_hydrology_metrics)
  handler_summary <- logical_to_integer(handler_summary)

  # set system variables
  seeds_file <- file.path(outdir, "seeds.txt")
  seeds_table_name <- "seeds"
  seeds_reserve_field <- "Name"
  seeds_catchnum_field <- "CATCHNUMS"
  seeds_areatarget_field <- "Areatarget"
  neighbours_file <- file.path(outdir, "neighbours.csv")
  neighbours_file_name <- "neighbours"
  neighbours_catchnum_field <- "CATCHNUM"
  neighbours_neighbours_field <- "neighbours"

  # construct system call
  system_string <- paste(sep = " ",
                         builder_cmd,
                         "builder", "1",
                         outdir,
                         "reserve",
                         seeds_file,
                         seeds_table_name,
                         seeds_reserve_field,
                         seeds_catchnum_field,
                         seeds_areatarget_field,
                         area_type,
                         construct_benchmarks,
                         catchment_level_intactness,
                         area_target_multiplier,
                         handle_isolated_catchments,
                         output_upstream,
                         output_downstream,
                         output_hydrology_metrics,
                         catchments_file,
                         catchments_table_name,
                         area_land,
                         area_water,
                         skeluid,
                         catchnum,
                         subzone,
                         basin,
                         order1,
                         order2,
                         order3,
                         stream_length,
                         intactness,
                         zone,
                         isolated,
                         neighbours_file,
                         neighbours_file_name,
                         neighbours_catchnum_field,
                         neighbours_neighbours_field
  )

  # call BUILDER
  system(system_string)

  # prep for HANDLER
  h1 <- list.files(outdir, pattern = "_ROW.csv", full.names = TRUE)
  h1 <- ifelse(length(h1) > 0, h1[[length(h1)]], '""') # if multiple, return the last one which should be most recent
  h2 <- list.files(outdir, pattern = "_ROW_AREALAND.csv", full.names = TRUE)
  h2 <- ifelse(length(h2) > 0, h2[[length(h2)]], '""') # if multiple, return the last one which should be most recent
  h3 <- list.files(outdir, pattern = "_ROW_AREALAND.csv", full.names = TRUE)
  h3 <- ifelse(length(h3) > 0, h3[[length(h3)]], '""') # if multiple, return the last one which should be most recent
  h4 <- list.files(outdir, pattern = "_ROW_UPSTREAM_CATCHMENTS.csv", full.names = TRUE)
  h4 <- ifelse(length(h4) > 0, h4[[length(h4)]], '""') # if multiple, return the last one which should be most recent
  h5 <- list.files(outdir, pattern = "_ROW_DOWNSTREAM_CATCHMENTS.csv", full.names = TRUE)
  h5 <- ifelse(length(h5) > 0, h5[[length(h5)]], '""') # if multiple, return the last one which should be most recent

  # prep HANDLER system string
  handler_string <- paste(sep = " ",
                          builder_cmd,
                          "handler",
                          h1, h2, h3, h4, h5,
                          benchmark_level_intactness,
                          area_target_proportion,
                          benchmark_identifier,
                          handler_summary,
                          benchmark_level_intactness_props,
                          area_target_props
  )
  # call HANDLER
  system(handler_string)

  # Load output tables
  benchmarks_out <- list.files(outdir, pattern = "COLUMN_All_Unique_BAs.csv")
  if(length(benchmarks_out) > 0){
    if(length(benchmarks_out) > 1){
      benchmarks_out <- benchmarks_out[[length(benchmarks_out)]]
      warning("Multiple benchmark tables in output folder")
    }
  } else{
    # clean up before throwing error
    if(is.null(out_dir)){
      unlink(outdir, recursive = TRUE)
    }
    stop("Can't find benchmarks table")
  }

  message(paste0("Returning BUILDER table: ", benchmarks_out))

  out_tab <- utils::read.csv(file.path(outdir, benchmarks_out))

  # delete temp directory. If out_dir was provided, don't delete
  if(is.null(out_dir)){
    unlink(outdir, recursive = TRUE)
  }

  # make sure BUILDER made some benchmarks
  if(nrow(out_tab) == 0){
    warning("No benchmarks to return, try changing the BUILDER parameters by decreasing intactness and/or area requirements")
    return(NULL)
  }

  # return
  out_tab$OID <- NULL
  return(dplyr::as_tibble(out_tab))
}



#### vignette code ####
## Example using reserve seeds

```{r message=FALSE, fig.width=7, fig.height=5}

# Add an area target column to three existing reserves with a value of 1000 km2
existing_reserves_sample$Areatarget <- 1000000000

# Prepare the reserve seeds table for builder
seed <- seeds_reserve(catchments_sf = builder_catchments_sample,
                      reserve_polygons = existing_reserves_sample,
                      name_col = "reserve", areatarget_col = "Areatarget")

# Run builder_reserve() using the catchments, the seeds table defining the 1000 km2 area target, and the neighbours table
# Use all catchments regardless of intactness to build benchmarks, but only return benchmarks with an area-weighted intactness of at least 80%
reserve_benchmarks_tab <- builder_reserve(catchments_sf = builder_catchments_sample, seeds = seed, neighbours = nghbrs,
                                          catchment_level_intactness = 0, benchmark_level_intactness = 0.8)


# BUILDER created three benchmarks, one for each existing reserve.
# View the output table listing the catchments making up the three returned benchmarks
head(reserve_benchmarks_tab)

# Use beaconstools to convert benchmarks to polygons
reserve_benchmarks_poly <- dissolve_catchments_from_table(catchments_sf = builder_catchments_sample,
                                                          input_table = reserve_benchmarks_tab,
                                                          out_feature_id = "network")

# Make map of the catchments, coloured green if intactness > 80%
# Add the outlines of the existing reserves (thick black line), and the benchmarks building off the reserves (thin black line)

# Plot benchmark 1
# Note how the the benchmark includes the <80% intact catchments because we set catchment level intactness to 0
plot(builder_catchments_sample$geometry[builder_catchments_sample$intact>=0.8], lty = 0, col = 'seagreen3')
plot(builder_catchments_sample$geometry[builder_catchments_sample$intact<0.8], lty = 0, col = 'grey', add = T)
plot(existing_reserves_sample$geometry[1], add=T, lwd = 2)
plot(reserve_benchmarks_poly$geometry[1], add = T)

# Plot benchmark 2
plot(builder_catchments_sample$geometry[builder_catchments_sample$intact>=0.8], lty = 0, col = 'seagreen3')
plot(builder_catchments_sample$geometry[builder_catchments_sample$intact<0.8], lty = 0, col = 'grey', add = T)
plot(existing_reserves_sample$geometry[2], add=T, lwd = 2)
plot(reserve_benchmarks_poly$geometry[2], add = T)

# Plot benchmark 3
plot(builder_catchments_sample$geometry[builder_catchments_sample$intact>=0.8], lty = 0, col = 'seagreen3')
plot(builder_catchments_sample$geometry[builder_catchments_sample$intact<0.8], lty = 0, col = 'grey', add = T)
plot(existing_reserves_sample$geometry[3], add=T, lwd = 2)
plot(reserve_benchmarks_poly$geometry[3], add = T)

# Show the individual catchments in benchmark 3
plot(reserve_benchmarks_poly$geometry[3])
plot(builder_catchments_sample$geometry[builder_catchments_sample$intact>=0.8], col = 'seagreen3', border = 'grey40', add = T)
plot(builder_catchments_sample$geometry[builder_catchments_sample$intact<0.8], col = 'grey', border = 'grey40', add = T)
plot(existing_reserves_sample$geometry[3], add=T, lwd = 3)
plot(reserve_benchmarks_poly$geometry[3], add = T, lwd = 2)
```
