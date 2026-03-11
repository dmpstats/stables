#' segmentTracks
#'
#' Splits survey tracks into regular segments and optionally associates observations with segments.
#'
#' @param trackdata An sf dataframe of survey tracks (LINESTRING or MULTILINESTRING).
#' @param seg_length Numeric. Intended length of each segment in metres.
#' @param obsdata Optional. An sf dataframe of observation points, must contain a 'Camera' column. This should be provided if a different number of cameras were reviewed on each survey, as transect widths will differ. This should be provided BEFORE filtering to survey-level, i.e. not the output of prepObsPPM.
#' @param output_type Character: either "points" (default) to return segment centroids as points, or "lines" to return segment linestrings.
#' @param camera_width Numeric: width of each camera's field of view in metres. Only used if obsdata is not provided. Default is 125 metres.
#' @param transect_width Numeric: width of each transect in metres. Only used if obsdata is not provided. If both obsdata and transect_width are provided, obsdata takes precedence.
#' @param polygonize Logical: if TRUE, segments will be converted to polygons representing the transect area. Default is FALSE.
#'
#' @return An sf dataframe of segmented tracks, optionally with associated observations.
#' @details
#' - Validates input types and required columns.
#' - Segments tracks into regular lengths.
#' - Associates observations with segments if provided.
#' @examples
#' \dontrun{
#' segments <- segmentTracks(trackdata, 1000)
#' segments_with_obs <- segmentTracks(trackdata, 1000, obsdata)
#' }
#' @export
segmentTracks <- function(trackdata, 
seg_length, 
obsdata = NULL,
                          output_type = c("points", "lines"),
                          camera_width = 125,
                          transect_width = NULL,
                          polygonize = FALSE) {
  cli::cli_h3("Segmenting {nrow(trackdata)} survey tracks into {seg_length} metre segments")
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
  if (!is.null(obsdata)) {
    if (!inherits(obsdata, "sf") || nrow(obsdata) == 0) {
      stop("obsdata must be a non-empty sf dataframe if provided.")
    }
    obs_geom_type <- unique(sf::st_geometry_type(obsdata))
    if (!all(obs_geom_type %in% c("POINT", "MULTIPOINT"))) {
      stop("obsdata must have POINT or MULTIPOINT geometries.")
    }
    if (!"Camera" %in% colnames(obsdata)) {
      stop("obsdata must contain a 'Camera' column.")
    }
  }

  # If 'obsdata' is provided, check that both datasets have a 'date' column
  # and that all dates in obsdata are present in trackdata, but the reverse is not necessary
  if (!is.null(obsdata)) {
    if (!"date" %in% colnames(trackdata) || !"date" %in% colnames(obsdata)) {
      cli::cli_abort("Both trackdata and obsdata must contain a 'date' column to associate observations with segments.")
    }
    missing_dates <- setdiff(unique(obsdata$date), unique(trackdata$date))
    if (length(missing_dates) > 0) {
      cli::cli_abort("The following dates are present in obsdata but missing from trackdata: ", paste(missing_dates, collapse = ", "))
    }
  }

  # If obsdata is provided but transect_width is also provided, warn that obsdata takes precedence
  if (!is.null(obsdata) && !is.null(transect_width)) {
    cli::cli_alert_warning("Both obsdata and transect_width are provided. Transect widths will be calculated from obsdata; transect_width will be ignored.")
  }

  # If no obsdata is provided but nor is transect_width, abort
  if (is.null(obsdata) && is.null(transect_width)) {
    cli::cli_abort("Either obsdata or transect_width must be provided to determine transect widths.")
  }

  # If output_type is not valid, abort
  output_type <- match.arg(output_type)

  # Split MULTILINESTRING into individual LINESTRING rows
  if (any(sf::st_geometry_type(trackdata) == "MULTILINESTRING")) {
    trackdata <- trackdata %>%
      sf::st_cast("LINESTRING")
    cli::cli_innform("Converted MULTILINESTRING geometries to {nrow(trackdata)} LINESTRING transects.")
  }

  # If obsdata is provided, split MULTIPOINT into individual POINT rows
  if (!is.null(obsdata) && any(sf::st_geometry_type(obsdata) == "MULTIPOINT")) {
    obsdata <- obsdata %>%
      sf::st_cast("POINT")
    cli::cli_inform("Converted MULTIPOINT geometries to {nrow(obsdata)} POINT observations.")
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

  cli::cli_inform("Splitting {nrow(trackdata)} tracks into segments of approximately {seg_length} metres...")
  segmented_tracks <- split_lines(
    trackdata,
    max_length = seg_length,
    id = "transect_id"
  ) %>%
    dplyr::mutate(
      segment_id = dplyr::row_number(),
      seg_length_m = sf::st_length(geometry) %>%
        units::set_units("m") %>%
        units::drop_units()
    )

  # Report how many segments are created, and their mean length
  cli::cli_inform("Created {nrow(segmented_tracks)} segments with mean length {round(mean(segmented_tracks$seg_length_m), 2)} metres.")
  # If the mean length is belowq 75% of the intended seg_length, warn the user
  if (mean(segmented_tracks$seg_length_m) < 0.75 * seg_length) {
    cli::cli_warn("Mean segment length ({round(mean(segmented_tracks$seg_length_m), 2)} metres) is below 75% of intended seg_length ({seg_length} metres). Consider increasing seg_length to reduce fragmentation.")
  }

  # If obsdata is provided, determine how many cameras were reviewed
  # on each survey-date
  if (!is.null(obsdata)) {
    cameras_per_date <- obsdata %>%
      as.data.frame() %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(n_cameras = dplyr::n_distinct(Camera)) %>%
      dplyr::ungroup()
    # Join camera counts to segmented_tracks
    segmented_tracks <- segmented_tracks %>%
      dplyr::left_join(cameras_per_date, by = "date")
    # Calculate transect width for each segment
    segmented_tracks <- segmented_tracks %>%
      dplyr::mutate(
        transect_width_m = n_cameras * camera_width
      )
    # Report how often each number of cameras was used
    camera_summary <- cameras_per_date %>%
      dplyr::group_by(n_cameras) %>%
      dplyr::summarise(n_dates = dplyr::n()) %>%
      dplyr::mutate(summary = paste0(
        n_cameras, " camera",
        ifelse(n_cameras > 1, "s", ""), ": ", n_dates, " day",
        ifelse(n_dates > 1, "s", "")
      )) %>%
      dplyr::pull(summary)
    cli::cli_inform("Summary of cameras used per survey date:")
    cli::cli_ul(camera_summary)
  } else {
    # If obsdata is not provided, use transect_width for all segments
    segmented_tracks <- segmented_tracks %>%
      dplyr::mutate(
        transect_width_m = transect_width
      )
  }

  # And derive spatial effort, in km
  segmented_tracks <- segmented_tracks %>%
    dplyr::mutate(
      effort_km = (seg_length_m / 1000) * (transect_width_m / 1000)
    )

  # If output_type is "points", convert segments to centroids
  if (output_type == "points") {
    segmented_tracks <- segmented_tracks %>%
      dplyr::mutate(geometry = sf::st_centroid(geometry)) %>%
      sf::st_set_geometry("geometry")
  }

  # If polygonize is TRUE, convert segments to polygons, ONLY if output_type is "lines"
  if (output_type == "lines" && polygonize) {
    segmented_tracks <- segmented_tracks %>%
      dplyr::mutate(
        geometry = sf::st_buffer(geometry, dist = transect_width_m / 2, endCapStyle = "FLAT")
      ) %>%
      sf::st_set_geometry("geometry")
  }

  cli::cli_alert_success("Done!")

  return(segmented_tracks)
}

utils::globalVariables(c(
  "date", "Camera", "n_cameras", "%>%",
  "transect_width_m", "seg_length_m", "effort_km"
))
