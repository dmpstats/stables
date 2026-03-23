#' segmentTracks
#'
# nolint start
#' Splits survey tracks into regular segments.
#'
#' @param trackdata An sf dataframe of survey tracks (LINESTRING or MULTILINESTRING).
#' @param seg_length Numeric. Intended length of each segment in metres.
#' @param output_type Character: either "points" (default) to return segment centroids as points, or "lines" to return segment linestrings.
#'
#' @return An sf dataframe of segmented tracks.
# nolint end
#' @details
#' - Validates input types and required columns.
#' - Segments tracks into regular lengths.
#' @examples
#' \dontrun{
#' segments <- segmentTracks(trackdata, 1000)
#' segment_points <- segmentTracks(trackdata, 1000, output_type = "points")
#' }
#' @export
segmentTracks <- function(
  # nolint
  trackdata,
  seg_length,
  output_type = c("points", "lines")
) {
  cli::cli_h3("Segmenting {nrow(trackdata)} survey tracks into {seg_length} metre segments") # nolint
  # Input validation
  if (!inherits(trackdata, "sf") || nrow(trackdata) == 0) {
    stop("trackdata must be a non-empty sf dataframe.")
  }
  geom_type <- unique(sf::st_geometry_type(trackdata))
  if (!all(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
    stop("trackdata must have LINESTRING or MULTILINESTRING geometries.")
  }
  if (!is.numeric(seg_length) || length(seg_length) != 1 || seg_length <= 0) {
    stop("seg_length must be a single positive numeric value (metres).")
  }

  # If output_type is not valid, abort
  output_type <- match.arg(output_type)

  # Split MULTILINESTRING into individual LINESTRING rows
  if (any(sf::st_geometry_type(trackdata) == "MULTILINESTRING")) {
    trackdata <- trackdata %>%
      sf::st_cast("LINESTRING")
    cli::cli_inform("Converted MULTILINESTRING geometries to {nrow(trackdata)} LINESTRING transects.") # nolint
  }

  # Segment tracks
  # segmented_tracks <- sf::st_segmentize(
  #   trackdata,
  #   dfMaxLength = units::set_units(seg_length, "m")
  # )

  # If transect_id is not present, create an artificial one
  if (!"transect_id" %in% colnames(trackdata)) {
    trackdata <- trackdata %>%
      dplyr::mutate(transect_id = dplyr::row_number())
  }

  cli::cli_inform("Splitting {nrow(trackdata)} tracks into segments of approximately {seg_length} metres...") # nolint
  segmented_tracks <- split_lines(
    trackdata,
    max_length = seg_length,
    id = "transect_id"
  ) %>%
    dplyr::mutate(segment_id = dplyr::row_number())
  segmented_tracks$seg_length_m <- sf::st_length(sf::st_geometry(segmented_tracks)) %>%
    units::set_units("m") %>%
    units::drop_units()

  # Report how many segments are created, and their mean length
  cli::cli_inform("Created {nrow(segmented_tracks)} segments with mean length {round(mean(segmented_tracks$seg_length_m), 2)} metres.") # nolint
  # If the mean length is belowq 75% of the intended seg_length, warn the user
  if (mean(segmented_tracks$seg_length_m) < 0.75 * seg_length) {
    cli::cli_warn("Mean segment length ({round(mean(segmented_tracks$seg_length_m), 2)} metres) is below 75% of intended seg_length ({seg_length} metres). Consider increasing seg_length to reduce fragmentation.") # nolint
  }

  # If output_type is "points", convert segments to centroids
  if (output_type == "points") {
    sf::st_geometry(segmented_tracks) <- sf::st_centroid(sf::st_geometry(segmented_tracks)) # nolint
  }

  cli::cli_alert_success("Done!")

  segmented_tracks
}

utils::globalVariables(c(
  "%>%"
))
