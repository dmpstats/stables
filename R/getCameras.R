#' getCameras
#'
#' Count distinct cameras reviewed per survey flight.
#'
#' Uses the survey string in observations (for example
#' \code{Zone274_M09_S01_D03_24}) to extract a flight token (\code{D03}),
#' derive a survey identifier without that flight token, and count distinct
#' cameras in each survey-flight combination.
#'
#' @param observations Observation records as an \code{sf} object or
#' \code{data.frame}, typically \code{data$observations} from
#' \code{readHiDef()}.
#' @param broad_category Optional character scalar used to filter
#' observations by \code{Broad_Category} before counting cameras. Use
#' \code{NULL} (default) to keep all categories.
#' @param survey_col Character scalar. Name of the survey column in
#' observations. Default is \code{"Survey"}.
#' @param camera_col Character scalar. Name of the camera column in
#' observations. Default is \code{"Camera"}.
#' @param flight_regex Character scalar. Regular expression used to extract
#' flight tokens from survey strings. Default is \code{"D\\\\d{2}"}.
#'
#' @return A data.frame with one row per survey-flight combination and columns:
#' \code{date}, \code{Survey_ID}, \code{Flight}, and \code{n_cameras}.
#' @details
#' - Drops geometry before summarising if input observations are \code{sf}.
#' - Cleans camera IDs using \code{stringr::str_squish()}.
#' - Removes rows with missing or empty survey IDs, flights, camera values,
#'   or dates.
#' - Throws an error if a date has differing camera counts across flights.
#' @examples
#' \dontrun{
#' data <- readHiDef("path/to/hidef")
#' cameras <- getCameras(data$observations, broad_category = "Bird")
#' }
#' @export
getCameras <- function(observations,  # nolint
                       broad_category = NULL,
                       survey_col = "Survey",
                       camera_col = "Camera",
                       flight_regex = "D\\d{2}") {
  cli::cli_h3("Extracting camera counts from survey strings")

  obsdata <- observations

  if (!inherits(obsdata, "sf") && !is.data.frame(obsdata)) {
    cli::cli_abort("{.arg observations} must be an observation sf/data.frame.") # nolint
  }

  if (!is.character(survey_col) || length(survey_col) != 1 ||
    is.na(survey_col) || !nzchar(survey_col)) {
    cli::cli_abort("{.arg survey_col} must be a single non-empty character value.") # nolint
  }

  if (!is.character(camera_col) || length(camera_col) != 1 ||
    is.na(camera_col) || !nzchar(camera_col)) {
    cli::cli_abort("{.arg camera_col} must be a single non-empty character value.") # nolint
  }

  if (!is.character(flight_regex) || length(flight_regex) != 1 ||
    is.na(flight_regex) || !nzchar(flight_regex)) {
    cli::cli_abort("{.arg flight_regex} must be a single non-empty character value.") # nolint
  }

  required_cols <- c("date", survey_col, camera_col)
  missing_cols <- setdiff(required_cols, colnames(obsdata))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing required columns in observations: {paste(missing_cols, collapse = ', ')}") # nolint
  }

  if (!is.null(broad_category)) {
    if (!is.character(broad_category) || length(broad_category) != 1 ||
      is.na(broad_category) || !nzchar(broad_category)) {
      cli::cli_abort("{.arg broad_category} must be NULL or a single non-empty character value.") # nolint
    }
    if (!"Broad_Category" %in% colnames(obsdata)) {
      cli::cli_abort("{.field Broad_Category} column is required when {.arg broad_category} is provided.") # nolint
    }
    obsdata <- obsdata[obsdata$Broad_Category == broad_category, , drop = FALSE]
    cli::cli_inform("Filtered observations to broad category '{broad_category}'.") # nolint
  }

  if (inherits(obsdata, "sf")) {
    obsdata <- sf::st_drop_geometry(obsdata)
  }

  obs_tbl <- data.frame(
    date = obsdata[["date"]],
    Survey = as.character(obsdata[[survey_col]]),
    Camera = stringr::str_squish(as.character(obsdata[[camera_col]])),
    stringsAsFactors = FALSE
  )

  obs_tbl$Flight <- stringr::str_extract(obs_tbl$Survey, flight_regex)

  # Remove the flight token from survey strings to create a shared survey ID.
  obs_tbl$Survey_ID <- stringr::str_replace(
    obs_tbl$Survey,
    paste0("_", flight_regex, "_"),
    "_"
  )
  obs_tbl$Survey_ID <- stringr::str_replace(
    obs_tbl$Survey_ID,
    paste0("_", flight_regex, "$"),
    ""
  )
  obs_tbl$Survey_ID <- stringr::str_replace(obs_tbl$Survey_ID, "_$", "")

  keep <- !is.na(obs_tbl$Camera) & obs_tbl$Camera != "" &
    !is.na(obs_tbl$date) &
    !is.na(obs_tbl$Flight) & obs_tbl$Flight != "" &
    !is.na(obs_tbl$Survey_ID) & obs_tbl$Survey_ID != ""

  n_removed <- sum(!keep)
  if (n_removed > 0) {
    cli::cli_alert_warning("Removed {n_removed} records with missing camera/flight/survey/date values.") # nolint
  }
  obs_tbl <- obs_tbl[keep, , drop = FALSE]

  if (nrow(obs_tbl) == 0) {
    cli::cli_abort("No valid observation rows remain after cleaning.")
  }

  cameras_per_survey_flight <- stats::aggregate(
    Camera ~ date + Survey_ID + Flight,
    data = obs_tbl,
    FUN = function(x) length(unique(x))
  )

  names(cameras_per_survey_flight)[
    names(cameras_per_survey_flight) == "Camera"
  ] <- "n_cameras"
  cameras_per_survey_flight <- cameras_per_survey_flight[
      order(cameras_per_survey_flight$date,
        cameras_per_survey_flight$Survey_ID,
          cameras_per_survey_flight$Flight),
  ]

  camera_count_per_date <- stats::aggregate(
    n_cameras ~ date,
    data = cameras_per_survey_flight,
    FUN = function(x) length(unique(x))
  )

  inconsistent_dates <- camera_count_per_date$date[
    camera_count_per_date$n_cameras > 1
  ]

  if (length(inconsistent_dates) > 0) {
    inconsistent_rows <- cameras_per_survey_flight[
      cameras_per_survey_flight$date %in% inconsistent_dates,
    ]
    inconsistent_summary <- stats::aggregate(
      n_cameras ~ date,
      data = inconsistent_rows,
      FUN = function(x) paste(sort(unique(x)), collapse = ", ")
    )
    diagnostic_lines <- paste0(
      as.character(inconsistent_summary$date),
      ": ",
      inconsistent_summary$n_cameras,
      " cameras across flights"
    )
    cli::cli_alert_warning("Detected dates with inconsistent numbers of cameras across flights:") # nolint
    cli::cli_ul(diagnostic_lines)
    cli::cli_abort("Each date must have a single camera count across flights.")
  }

  cli::cli_alert_success("Done! Found camera counts for {nrow(cameras_per_survey_flight)} survey-flight combinations.") # nolint

  cameras_per_survey_flight
}
