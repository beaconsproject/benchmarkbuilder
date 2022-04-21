check_catchnum <- function(catchments_sf){

  # check CATCHNUM exists
  if(!"CATCHNUM" %in% names(catchments_sf)){
    stop("Catchments must contain column 'CATCHNUM'")
  }

  # make sure it's an integer then convert to character
  catchments_sf$CATCHNUM <- as.integer(catchments_sf$CATCHNUM)

  return(catchments_sf)
}

check_for_geometry <- function(in_sf){

  if(!"geometry" %in% names(in_sf)){
    stop("Benchmark/network object must contain column: geometry")
  }
}
