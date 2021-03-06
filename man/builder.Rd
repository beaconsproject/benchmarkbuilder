% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/builder.R
\name{builder}
\alias{builder}
\title{Call the BUILDER software from R.}
\usage{
builder(
  catchments_sf,
  seeds,
  neighbours,
  out_dir = NULL,
  catchment_level_intactness = 1,
  benchmark_level_intactness = 1,
  area_target_proportion = 1,
  area_type = "land",
  construct_benchmarks = TRUE,
  area_target_multiplier = 1,
  handle_isolated_catchments = TRUE,
  output_upstream = FALSE,
  output_downstream = FALSE,
  output_hydrology_metrics = FALSE,
  area_land = "Area_Land",
  area_water = "Area_Water",
  skeluid = "SKELUID",
  catchnum = "CATCHNUM",
  subzone = "FDA",
  zone = "MDA",
  basin = "BASIN",
  order1 = "ORDER1",
  order2 = "ORDER2",
  order3 = "ORDER3",
  stream_length = "STRMLEN",
  intactness = "intact",
  isolated = "Isolated",
  benchmark_identifier = "PB",
  handler_summary = FALSE,
  benchmark_level_intactness_props = "\\"\\"",
  area_target_props = "\\"\\""
)
}
\arguments{
\item{catchments_sf}{sf object of the catchments dataset with unique identifier column: CATCHNUM.}

\item{seeds}{Seeds table from \code{seeds()} listing seed catchments and area targets.}

\item{neighbours}{Neighbours table from \code{neighbours()} listing all neighbouring pairs of catchments.}

\item{out_dir}{If provided, input (seeds, neighbours and catchments) files and output BUILDER tables will be saved to this directory. Otherwise
a temp directory will be used. Function will attempt to create the directory if it doesn't already exist.}

\item{catchment_level_intactness}{Minimum intactness value for catchment inclusion (between 0-1). i.e. if value of 1 is used, only 100\% intact
catchments will be used to build benchmarks.}

\item{benchmark_level_intactness}{Minimum area-weighted intactness of final benchmarks. Only benchmarks meeting this value will be returned.}

\item{area_target_proportion}{The proportion of the Area target that benchmarks need to meet (0-1) to be returned by the function.}

\item{area_type}{If 'land', only terrestrial areas are counted towards the area target. If 'water', only water areas are counter. If 'landwater',
all areas are counted.}

\item{construct_benchmarks}{Should builder build benchmarks?}

\item{area_target_multiplier}{Multiplier applied to the area target in the seed list that allows for adjustments to the area target without having
to remake the seeds table.}

\item{handle_isolated_catchments}{Should small isolated catchments be merged into the benchmarks?}

\item{output_upstream}{Should upstream catchments be calculated?}

\item{output_downstream}{Should downstream catchments be calculated?}

\item{output_hydrology_metrics}{Should hydrology metrics be calculated?}

\item{area_land}{Catchments column listing the area of land in each catchment.}

\item{area_water}{Catchments column listing the area of water in each catchment.}

\item{skeluid}{Catchments column listing the skeluid field.}

\item{catchnum}{Catchments column listing the unique catchments ID.}

\item{subzone}{Catchments column listing the hydrological subzone.}

\item{zone}{Catchments column listing the hydrological zone.}

\item{basin}{Catchments column listing the basin field.}

\item{order1}{Catchments column listing the order 1 field.}

\item{order2}{Catchments column listing the order 2 field.}

\item{order3}{Catchments column listing the order 3 field.}

\item{stream_length}{Catchments column listing stream length.}

\item{intactness}{Catchments column listing the intactness value.}

\item{isolated}{Catchments column identifying catchments isolated from the stream network.}

\item{benchmark_identifier}{Prefix for benchmark names.}

\item{handler_summary}{Should a summary listing counts of available benchmarks at different intactness and area target proportions be created?}

\item{benchmark_level_intactness_props}{If a summary is created, provide the intactness proportions to be summarised in the format '0.7,0.8,0.9'.}

\item{area_target_props}{If a summary is created, provide the area target proportions to be summarised in the format '0.7,0.8,0.9'.}
}
\value{
A tibble with column names representing benchmarks, and rows of catchments making up the benchmarks.
}
\description{
Prepares and passes all input tables and variables to the BUIDLER executable and returns the benchmark table to R.
}
\details{
This function prepares the input tables (neighbours, seeds, catchments) required by BUILDER in a temp file, then creates
a string of parameters that is passed to the BUILDER executable using \code{system()}. The BUILDER output tables are saved to the
temp file, and the output table listing benchmark names and catchment lists is returned to the R session.

This function only returns the BUILDER output table describing the lists of catchments making up each benchmark. We typically recommend
using the \code{beaconstools} package to calculate hydrology metrics, but hydrology information as well as other summary information is
produced by BUILDER.
If users want access to the complete set of BUILDER output tables (e.g. hydrology and summary information), a valid output directory
can be provided in which case all input and output tables will be saved to that directory. If no output directory is provided a temp
folder will be used and deleted after the function completes. As well as providing access to the full list of BUILDER output tables,
an output directory is useful in a looped workflow with multiple calls to BUILDER producing multiple sets of output files. In this
workflow a different output directory can be used to save each set of output files which can later be processed using the \code{beaconstools} package.

The main parameters to adjust when building benchmarks are:
\itemize{
\item{Area target: Set in the seeds table, this defines the minimum size for each benchmark.}
\item{Catchment-level intactness: Only catchments with intactness values greater than this value will be added to the benchmark.}
\item{Benchmark-level intactness: Only benchmarks with an area-weighted intactness greater than or equal to this value will be returned.}
\item{Area-target proportion: The proportion of the Area target that benchmarks need to meet. Allows output of benchmarks smaller than
the area target (e.g. if no benchmarks meet 100\% of the area target, area target multiplier can be lowered to 0.9 to search for smaller
benchmarks).}
}
}
\examples{
nghbrs <- neighbours(builder_catchments_sample)
# select 10 seeds with 100\% intactness for the example. Build benchmarks to 500km2.
seed <- seeds(catchments_sf = builder_catchments_sample,
              filter_intactness_col = "intact", filter_intactness_threshold = 1,
              areatarget_value = 500000000)
seed <- seed[1:10,]
builder(catchments_sf = builder_catchments_sample, seeds = seed, neighbours = nghbrs)
}
