#' prepTracks
#'
#' Prepare HiDef Track Data
#'
#' Processes raw HiDef track data to identify transects, calculate bearings, and clean records for further analysis.
#'
#' This function takes an sf dataframe of track points, assigns transect IDs, calculates step bearings, and applies quality checks. It is designed to work with output from \code{readHiDef()} and expects two columns: \code{date} and \code{geometry}.
#'
#' @param trackdata A spatial dataframe (sf object) containing HiDef track points. Must include columns \code{date} (survey date) and \code{geometry} (spatial points).
#' @param survey_polygon An sf polygon object representing the survey area. Used to filter track points within the survey boundary. If unavailable, either set to NULL (no filtering) or create a convex hull around observation points.
#' @param assign_transects Logical; if TRUE, transect IDs will be assigned based on step distances and bearings. Default is TRUE.
#' @param method Character; method for transect assignment. Options are "distance" (based on step length), "bearing" (based on bearing changes), or "both" (combination of both). Default is "distance".
#' @param angle_threshold Numeric; angle in degrees to define significant bearing changes for transect breaks. Default is 0. This should be an angle perpendicular to the main survey direction.
#' @param transect_step Numeric; step distance in metres to define transect breaks. Default is 1000 metres.
#' @param make_linestrings Logical; if TRUE, the output will be converted from points to linestrings representing transects. Default is TRUE.
#'
#' @return A cleaned sf dataframe of track points with added transect IDs and bearing information.
#' @details
#' - Assigns transect IDs based on step distances and bearings.
#' - Calculates bearings between consecutive points.
#' - Removes points outside the survey polygon.
#' - Handles missing or invalid coordinates.
#'
#' @examples
#' \dontrun{
#' cleaned_tracks <- prepTracks(trackdata, survey_polygon)
#' }
#' @export
prepTracks <- function(
  trackdata,
  survey_polygon = NULL,
  assign_transects = TRUE,
  method = c("distance", "bearing", "both"),
  angle_threshold = 0,
  transect_step = 1000,
  make_linestrings = TRUE
) {
  cli::cli_h3("Preparing HiDef track data")
  if (assign_transects) {
    cli::cli_inform("Assigning transect IDs to track data with step size of {transect_step} metres.")
  }

  # Ensure inputs are reasonable
  method <- match.arg(method)
  # Check angle_threshold is between -180 and 180
  if (angle_threshold < -180 || angle_threshold > 180) {
    cli::cli_abort("angle_threshold must be between -180 and 180 degrees.")
  }
  # Check transect_step is positive
  if (transect_step <= 0) {
    cli::cli_abort("transect_step must be a positive number.")
  }

  # If survey_polygon is provided, transform its CRS to match trackdata
  if (!is.null(survey_polygon)) {
    survey_polygon <- sf::st_transform(survey_polygon, sf::st_crs(trackdata))
  }

  # Summarise the input data
  cli::cli_inform("Input track data contains {nrow(trackdata)} records across {length(unique(trackdata$date))} survey-dates.")

  # If survey_polygon is provided, check its validity
  if (!is.null(survey_polygon)) {
    cli::cli_inform("A survey polygon has been provided. Checking track data against polygon extent...")
    # Check which track points fall within the survey polygon
    within_polygon <- sf::st_within(trackdata, survey_polygon, sparse = FALSE)
    n_within <- sum(within_polygon)
    cli::cli_inform("{n_within} out of {nrow(trackdata)} track records fall within the survey polygon. Removing outliers...")
    # Filter trackdata to only those within the polygon
    trackdata <- trackdata[within_polygon, ]
  }

  # If assign_transects is TRUE, assign transect IDs
  if (assign_transects) {
    # Calculate distance between each point
    trackdata <- trackdata %>%
      dplyr::group_by(date) %>%
      dplyr::mutate(
        steplength = sf::st_distance(dplyr::lag(geometry), geometry, by_element = TRUE) %>%
          units::set_units("m") %>%
          units::drop_units(),
        # Calculate bearing angle
        stepbearing = geosphere::bearing(
          p1 = sf::st_coordinates(dplyr::lag(geometry %>% sf::st_transform(4326))),
          p2 = sf::st_coordinates(geometry %>% sf::st_transform(4326))
        ),
        bigstep = ifelse(steplength > transect_step, 1, 0) %>%
          # Handle NA values in steplength
          tidyr::replace_na(0),
        # Whenever the bearing crosses the angle threshold, we consider it a new transect
        # e.g. if the previous bearing was -5, the next is 5, and the threshold is 0, this counts as a new transect
        anglechange = ifelse(
          (dplyr::lag(stepbearing) < angle_threshold & stepbearing > angle_threshold) |
            (dplyr::lag(stepbearing) > angle_threshold & stepbearing < angle_threshold),
          1, 0
        ) %>%
          # Handle NA values in lagged stepbearing
          tidyr::replace_na(0)
      )
    # Depending on method, define transect breaks
    if (method == "distance") {
      trackdata <- trackdata %>%
        dplyr::mutate(
          transect_id = cumsum(bigstep) + 1
        )
    } else if (method == "bearing") {
      trackdata <- trackdata %>%
        dplyr::mutate(
          transect_id = cumsum(anglechange) + 1
        )
    } else if (method == "both") {
      trackdata <- trackdata %>%
        dplyr::mutate(
          transect_id = cumsum(bigstep | anglechange) + 1
        )
    } else {
      cli::cli_abort("Invalid method specified. Choose from 'distance', 'bearing', or 'both'.")
    }
    # How many transects identified, counting across survey-dates?
    n_transects <- paste0(trackdata$date, trackdata$transect_id) %>%
      unique() %>%
      length()
    cli::cli_inform("Assigned {n_transects} transects across all survey-dates.")
  } else {
    cli::cli_inform("Skipping transect assignment as assign_transects is FALSE.")
  }

  # Remove single-point transects
  if (assign_transects) {
    trackdata <- trackdata %>%
      dplyr::group_by(date, transect_id) %>%
      dplyr::filter(dplyr::n() > 1) %>%
      dplyr::ungroup()
    # Report how many transects were removed
    n_transects_after <- paste0(trackdata$date, trackdata$transect_id) %>%
      unique() %>%
      length()
    cli::cli_inform("After removing single-point transects, {n_transects_after} transects remain.")
  }

  if (make_linestrings) {
    cli::cli_inform("Summarising track points into linestrings...")
    # Now, summarise into linestrings
    tracklines <- trackdata %>%
      dplyr::group_by(date, transect_id = if (assign_transects) transect_id else NULL) %>%
      dplyr::summarise(do_union = FALSE) %>%
      sf::st_cast("LINESTRING") %>%
      dplyr::ungroup()
  } else {
    cli::cli_inform("Skipping linestring conversion as make_linestrings is FALSE.")
    tracklines <- trackdata
  }

  cli::cli_alert_success("Done!")

  return(tracklines)
}
