### neighbours ###
#
#' Create a neighbours table listing neighbours for each catchment.
#'
#' For an sf object of catchments with unique CATCHNUM id's, calculates a list of neighbouring CATCHNUM pairs
#' and returns them in a long tibble. Neighbours are defined as having at least on point in common (within 0.1m).
#'
#'
#' @param catchments_sf sf object of the catchments dataset with unique identifier column: CATCHNUM .
#'
#' @return A tibble of neighbouring pairs with columns \code{CATCHNUM} and \code{neighbours}.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' neighbours(catchments_sample)
#'
neighbours <- function(catchments_sf){
  # Generate neighbours where single point is shared
  # this was originally done in Python using the GenerateSpatialWeightsMatrix() function
  # here we use the SF package
  # pattern = "****T****" in the st_relate() function matches any intersecting polygons.
  # More info on st_relate at:
  # https://www.rdocumentation.org/packages/sf/versions/0.7-7/topics/st_relate
  # Queen pattern found here: https://github.com/r-spatial/sf/issues/234

  # check catchnum and convert to integer
  catchments_sf <- make_catchnum_integer(catchments_sf)

  st_queen <- function(a, b = a) sf::st_relate(a, b, pattern = "****T****") # this tests for an intersect of at least one shared point between a and b
  nbr_df <- as.data.frame(st_queen(sf::st_buffer(catchments_sf, dist=0.1)))

  # replace index values with catchnum values using a key
  catchments_sf$key <- 1:nrow(catchments_sf) # add a key column to sf table. Must be an index so it matches the index assigned to the NB_QUEEN column
  sf_catch_key <- sf::st_drop_geometry(catchments_sf[c("key","CATCHNUM")])

  nbr_df <- nbr_df %>%
    dplyr::left_join(sf_catch_key, by = c("row.id" = "key")) %>%
    dplyr::left_join(sf_catch_key, by = c("col.id" = "key")) %>%
    dplyr::select(.data$CATCHNUM.x, .data$CATCHNUM.y)

  names(nbr_df) <- c("CATCHNUM", "neighbours") # rename output columns

  # remove cases where CATCHNUM is its own NEIGHBOUR
  nbr_df <- nbr_df %>%
    dplyr::filter(.data$CATCHNUM != .data$neighbours) %>%
    dplyr::as_tibble()

  # add key needed by BUILDER
  nbr_df$key <- as.integer(0:(nrow(nbr_df)-1))

  return(nbr_df)
}


### seeds ###
#
#' Create a seeds table.
#'
#' The seeds table is an input to BUILDER that lists seed catchments and area targets. Seed catchments are the
#' starting catchments for constructing benchmarks. This function provides various
#' methods for filtering the catchments dataset to create seeds. Each seed requires an area target detailing
#' the minimum area in m2 for the constructed benchmark. Various methods are provided for adding an area target
#' for each seed catchment.
#'
#' ### Filtering
#' Two filtering methods are provided, one or both can be used. If no filter arguments are provided, all catchments in
#'  \code{catchments_sf} will be added to the seeds table:
#'  \itemize{
#'    \item{If \code{filter_intactness_col} and \code{filter_intactness_threshold} are provided, catchments will be filtered
#'    to include those with an intactness value >= the threshold.}
#'    \item{A \code{filter_polygon} can be provided to filter the catchments based on their centroid (more
#'    specifically, \code{sf::st_point_on_surface()}) falling inside the \code{filter_polygon}.}
#'    }
#' ### Area targets
#' Area targets should be provided in m2.
#' Arguments to satisfy one of the following area target methods must be provided. If multiple arguments are provided, only one
#' will be used, with the priority following the order below:
#' \itemize{
#'   \item{Single-value. If \code{areatarget_value} is provided, all seeds will be assigned the area target value.}
#'   \item{Column. If \code{areatarget_col} is provided, area targets will be pulled from the column of that
#'   name in \code{catcments_sf}.}
#'   \item{Spatial join. An \code{areatarget_polygon} will be spatially joined to the catchments. Area targets
#'    will be extracted from the \code{areatarget_polygon_col} column in \code{areatarget_polygon}. If multiple polygons in
#'    \code{areatarget_polygon} intersect a catchment, the value in \code{areatarget_polygon_col} with the most overlap will be
#'    selected.}
#'   }
#'
#' @param catchments_sf sf object of the catchments dataset with unique identifier column: CATCHNUM.
#' @param filter_intactness_col Optional intactness column in \code{catchments_sf} to filter on.
#' @param filter_intactness_threshold If \code{filter_intactness_col} provided, the minimum intactness value to filter on.
#' @param filter_polygon Optional sf polygon object to filter catchments. Only catchments inside the polygon are kept.
#' @param areatarget_value Optional single area target value to apply to all seeds.
#' @param areatarget_col Optional column in \code{catchments_sf} holding area target values.
#' @param areatarget_polygon Optional sf polygon object holding area target values that are applied to seeds using a spatial join.
#' @param areatarget_polygon_col If \code{areatarget_polygon} provided, the column containing the area target values to join to the catchments.
#'
#' @return A tibble of seed catchments and their area targets.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Use all catchments as seeds, single area target
#' seeds(catchments_sf = catchments_sample, areatarget_value = 1000000000)
#'
#' # Use all catchments as seeds, column area target
#' catchments_sample$area_target <- 1000000000
#' seeds(catchments_sf = catchments_sample, areatarget_col = "area_target")
#'
#' # Filter based on intactness
#' seeds(catchments_sf = catchments_sample,
#'       filter_intactness_col = "intact", filter_intactness_threshold = 1,
#'       areatarget_value = 1000000000)
#'
#'library(dplyr)
#'library(sf)
#' # Filter based on polygon, assign area target via spatial join
#' ref_poly <- data.frame(
#'              lon = c(-138.4, -138.1, -138.1, -138.4,  -138.1, -138.1, -138, -138),
#'              lat = c(64.3, 64.3, 64.1, 64.1,  64.3, 64.1, 64.1, 64.3),
#'              Areatarget = c(rep(1000000000, 4),rep(2000000000,4))) %>%
#'            st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
#'            group_by(Areatarget) %>%
#'            summarise(geometry = st_combine(geometry)) %>%
#'            st_cast("POLYGON") %>%
#'            st_transform(st_crs(catchments_sample))
#' seeds(catchments_sf = catchments_sample,
#'       filter_polygon = ref_poly,
#'       areatarget_polygon = ref_poly, areatarget_polygon_col = "Areatarget")
#'
seeds <- function(catchments_sf, filter_intactness_col = NULL, filter_intactness_threshold = NULL, filter_polygon = NULL,
                  areatarget_value = NULL, areatarget_col = NULL, areatarget_polygon = NULL, areatarget_polygon_col = NULL){

  # SET UP
  # determine area target method.
  # priority: single value > column > polygon
  areatarget_method <- ""
  if(!is.null(areatarget_value) & is.numeric(areatarget_value)){

    areatarget_method <- "single_value"

  } else if(!is.null(areatarget_col)){
    if(areatarget_col %in% colnames(catchments_sf)){

      areatarget_method <- "column"

    } else{
      warning(paste0("areatarget_col provided but '", areatarget_col, "' not in catchments_sf"))
    }
  } else if(!is.null(areatarget_polygon)){
    if(areatarget_polygon_col %in% colnames(areatarget_polygon)){

      areatarget_method <- "polygon"

    } else{
      warning(paste0("areatarget_polygon provided but '", areatarget_polygon_col, "' not in areatarget_polygon"))
    }
  }
  if(areatarget_method == ""){
    stop("No valid area target method provided")
  }

  # CHECKS
  # if filter_polygon is provided, check for geometry in catchments and polygon
  if(!is.null(filter_polygon)){
    check_for_geometry(filter_polygon)
    check_for_geometry(catchments_sf)
  }

  # if area target method is polygon, check for geometry in catchments and polygon
  if(areatarget_method == "polygon"){
    check_for_geometry(catchments_sf)
    check_for_geometry(areatarget_polygon)
  }

  # if filter_intactness_col, filter_intactness_threshold must be provided
  if(!is.null(filter_intactness_col)){
    if(!is.numeric(filter_intactness_threshold)){
      stop("filter_intactness_threshold must be provided with a filter_intactness_col")
    }
  }

  # check catchnum and convert to integer
  catchments_sf <- make_catchnum_integer(catchments_sf)

  # FILTER
  filtered_catchments <- catchments_sf
  sf::st_agr(filtered_catchments) = "constant"

  # filter by intactness
  if(!is.null(filter_intactness_col)){

    filtered_catchments <- filtered_catchments %>%
    dplyr::filter(.data[[filter_intactness_col]] >= as.numeric(filter_intactness_threshold))
  }

  # filter by polygon
  if(!is.null(filter_polygon)){

    catchnum_indexes <- filtered_catchments %>%
      sf::st_point_on_surface() %>% # get catchment centroids.
      sf::st_within(filter_polygon) %>% # test within for all centroids, returns a row for each match. row.id is a catchnum index, col.id is a PA index.
      as.data.frame() %>%
      dplyr::pull(.data$row.id)

    filtered_catchments <- filtered_catchments[catchnum_indexes,] # filter using indexes from st_within
  }

  # Error if no catchments selected
  if(nrow(filtered_catchments) == 0){
    stop("No catchments selected")
  }

  # AREA TARGET
  if(areatarget_method == "single_value"){

    filtered_catchments$Areatarget <- as.integer(areatarget_value)

  } else if(areatarget_method == "column"){

    filtered_catchments$Areatarget <- as.integer(filtered_catchments[[areatarget_col]])

  } else if(areatarget_method == "polygon"){

    # Check all area targets are valid numerics
    if(!all(!is.na(as.numeric(areatarget_polygon[[areatarget_polygon_col]])))){
      stop(paste0("All '", areatarget_polygon_col, "' values in '", areatarget_polygon, "' must be numeric"))
    }

    sf::st_agr(areatarget_polygon) = "constant"
    sf::st_agr(filtered_catchments) = "constant"

    filtered_catchments <- filtered_catchments %>%
      sf::st_join(areatarget_polygon, left = FALSE, largest = TRUE) %>% # inner join, assign catchnum area target to polygon value with largest overlap
      dplyr::mutate(Areatarget = as.integer(ceiling(.data[[areatarget_polygon_col]]))) # round up to next m2

  } else{
    stop("Catchments filtered but no valid area target method provided")
  }

  out_tab <- filtered_catchments %>%
    sf::st_drop_geometry() %>%
    dplyr::as_tibble() %>%
    dplyr::select(.data$CATCHNUM, .data$Areatarget)

  return(out_tab)
}

### seeds_reserve ###
#
#' Create a reserves seeds table.
#'
#' The reserves seeds table is an input to BUILDER that lists seed catchments and area targets for a set of reserves.
#' Reserve seed catchments are aggregations of catchments to be built into benchmarks. Typically an aggregation represents
#' and existing protected area. This function takes a polygon labelled with area target values and join it to the catchments
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
#' @param reserve_polygons
#' @param name_col
#' @param areatarget_col
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
#' # Convert to wide format required by BUILDER and save
seeds_reserve <- function(catchments_sf, reserve_polygons, name_col, areatarget_col, filter_intactness_col = NULL, filter_intactness_threshold = NULL){

  # CHECKS
  # check geometry
  check_for_geometry(catchments_sf)
  check_for_geometry(reserve_polygons)

  # check catchnum and convert to integer
  catchments_sf <- make_catchnum_integer(catchments_sf)

  # check reserve_polygons columns
  if(!all(c(name_col, areatarget_col) %in% colnames(reserve_polygons))){
    stop(paste0("'reserve_polygons' must have columns '", name_col, "' and '", areatarget_col, "'"))
  }

  # if filter_intactness_col, filter_intactness_threshold must be provided
  if(!is.null(filter_intactness_col)){
    if(!is.numeric(filter_intactness_threshold)){
      stop("filter_intactness_threshold must be provided with a filter_intactness_col")
    }
  }

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
  reserve_key <- sf::st_drop_geometry(reserve_polygons[c("key", name_col, areatarget_col)])

  # convert indexes from st_within to catchnums using the keys to join
  out_tab <- catch_within %>%
    dplyr::left_join(sf_catch_key, by = c("row.id" = "key")) %>%
    dplyr::left_join(reserve_key, by = c("col.id" = "key")) %>%
    dplyr::select(.data[[name_col]], .data[[areatarget_col]], .data$CATCHNUM) %>%
    dplyr::arrange(.data[[name_col]], "CATCHNUM") %>%
    dplyr::tibble()

  return(out_tab)
}

### reserve_seeds_to_builder
reserve_seeds_to_builder <- function(seeds_reserve_long, name_col, areatarget_col){

  out_tab <- seeds_reserve_long %>%
    group_by(.data[[name_col]], .data[[areatarget_col]]) %>%
    summarise(CATCHNUMS = paste(.data$CATCHNUM, collapse=","))

  return(out_tab)
}

### builder
builder <- function(catchments_sf, out_dir = NULL, seeds, neighbours,
                    area_type = "land", construct_benchmarks = TRUE, catchment_level_intactness = 1, area_target_multiplier = 1,
                    handle_isolated_catchments = TRUE, output_upstream = FALSE, output_downstream = FALSE, output_hydrology_metrics = FALSE,
                    area_land = "Area_Land", area_water = "Area_Water",
                    skeluid = "SKELUID", catchnum = "CATCHNUM",
                    subzone = "FDAHUC8", zone = "ZONE",
                    basin = "BASIN", order1 = "ORDER1", order2 = "ORDER2", order3 = "ORDER3", stream_length = "STRMLEN",
                    intactness = "intact", isolated = "Isolated",
                    benchmark_level_intactness = 1, area_target_proportion = 1, benchmark_identifier = "PB",
                    handler_summary = FALSE, benchmark_level_intactness_props = '""', area_target_props = '""'
                    ){

  # builder path
  builder_cmd <- system.file("exec", "BenchmarkBuilder_cmd.exe", package = "benchmarkbuilder")

  # set up output directory. Default is a temp folder. out_dir can be used if builder output should be saved, e.g. in a looped analysis.
  if(is.null(out_dir)){
    tmpdir <- file.path(tempdir(), "builder")
    dir.create(tmpdir)
    outdir <- tmpdir
  } else{
    outdir <- out_dir
    dir.create(outdir, recursive = TRUE)
  }

  # save seeds table to outdir
  message("checking seeds table")
  seeds <- make_all_integer(seeds)
  check_colnames(seeds, "seeds", c("CATCHNUM", "Areatarget"))
  check_seeds_areatargets(seeds)
  check_seeds_in_catchments(seeds, catchments_sf)
  write.csv(seeds, file.path(outdir, "seeds.txt"), row.names = FALSE)

  # save neighbours table to outdir
  message("checking neighbours table")
  check_colnames(neighbours, "neighbours", c("CATCHNUM", "neighbours", "key"))
  neighbours <- make_all_integer(neighbours)
  write.csv(neighbours, file.path(outdir, "neighbours.csv"), row.names = FALSE)

  # save catchment attributes
  catchments_csv <- sf::st_drop_geometry(catchments_sf)
  check_colnames(catchments_csv, "catchments_sf", c(area_land, area_water, skeluid, catchnum, subzone, zone, basin, order1, order2, order3, stream_length, intactness, isolated)) # check all provided columns are present
  catchments_csv <- make_all_character(catchments_csv, c(order1, order3, zone, subzone, basin)) # check type
  catchments_csv <- make_all_integer(catchments_csv, c(catchnum, skeluid, isolated))
  catchments_csv <- make_all_numeric(catchments_csv, c(order2, area_land, area_water, intactness))

  catchments_file <- file.path(outdir, "catchments.csv")
  catchments_table_name <- "catchments"
  write.csv(catchments_csv, catchments_file, row.names = FALSE)

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
  seeds_catchnum_field <- "CATCHNUM"
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
    "catchment",
    seeds_file,
    seeds_table_name,
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

  out_tab <- read.csv(file.path(outdir, benchmarks_out))

  # delete temp directory. If out_dir was provided, don't delete
  if(is.null(out_dir)){
    unlink(outdir, recursive = TRUE)
  }

  # make sure BUILDER made some benchmarks
  if(nrow(out_tab) == 0){
    message("No benchmarks to return, try changing the BUILDER parameters by decreasing intactness and/or area requirements")
  }

  # return
  out_tab$OID <- NULL
  return(out_tab)
}

### builder_reserves
