#' Prepare Observations for PPM Analysis
#'
#' Filters and processes observation data for point process modelling (PPM).
#'
#' This function validates input data, filters observations by species and behaviour, checks spatial proximity to survey tracks/polygons, and applies jitter to duplicated coordinates. It is designed for use with output from readHiDef and related survey data workflows.
#'
#' @param observations An sf dataframe of observation points, with columns including 'Species', 'Behaviour', and 'geometry'. Preferably the output of readHiDef().
#' @param tracks An sf dataframe of survey tracks or polygons. Ideally the output of prepTracks().
#' @param targetSpecies Character. The species to filter observations for. Case sensitive.
#' @param targetBehaviour Character. The behaviour to filter observations for (default: "All" for no filtering). Case insensitive, supports partial matching.
#' @param survey_tolerance Numeric. The buffer distance (in metres) around tracks/polygons to include observations (default: 500).
#' @param jitter Numeric. The amount of spatial jitter (in metres) to apply to duplicated observation coordinates (default: 5).
#' @return An sf dataframe of filtered and processed observation points, ready for PPM analysis.
#' @details
#' - Validates input data types and required columns.
#' - Filters by species and behaviour.
#' - Removes observations outside the survey tolerance.
#' - Applies spatial jitter to duplicated coordinates.
#' - Reports filtering and cleaning steps via cli messages.
#' @examples
#' \dontrun{
#' result <- prepObsPPM(observations, tracks, "Gannet", "Flying", 500)
#' }
#' @export
#'
#' TODO: Assign transect widths based on observation data cameras
#'
#' @examples
#' # Example usage
#' result <- function_name(param1, param2)
prepObsPPM <- function(
  observations,
  tracks,
  targetSpecies,
  targetBehaviour = "All",
  survey_tolerance = 500,
  jitter = 5,
  remove_dead = TRUE
) {
  cli::cli_h3("Preparing observations for PPM analysis")
  cli::cli_inform("Filtering observations  species '{targetSpecies}' and behaviour '{targetBehaviour}'.")  # nolint
  cli::cli_alert_info("Beginning with {nrow(observations)} total observations.")

  # Input validation
  # Ensure that observations is an sf point dataframe
  if (!inherits(observations, "sf") || 
  !all(sf::st_geometry_type(observations) %in% c("POINT", "MULTIPOINT"))) { 
    cli::cli_abort("observations must be an sf dataframe with POINT or MULTIPOINT geometries.")  # nolint
  }
  # Ensure that tracks is an sf linestring or polygon dataframe
  if (!inherits(tracks, "sf") || !all(sf::st_geometry_type(tracks) %in% c("LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON"))) {
    cli::cli_abort("tracks must be an sf dataframe with LINESTRING or POLYGON geometries.")  # nolint
  }
  # If no Species column is provided, abort
  if (!"Species" %in% colnames(observations)) {
    cli::cli_abort("The observations data must contain a 'Species' column.")
  }
  # If no Behaviour column is provided but one is requested, abort
  if (targetBehaviour != "All" && !"Behaviour" %in% colnames(observations)) {
    cli::cli_abort("The observations data must contain a 'Behaviour' column to filter by behaviour.")  # nolint
  }
  # Check that the provided species is within the observations data
  if (!targetSpecies %in% unique(observations$Species)) {
    cli::cli_abort("The specified species '{targetSpecies}' is not found in the observations data.")  # nolint
  }
  # If behaviour is not "All", check it's in the data
  if (targetBehaviour != "All" &&
    !any(stringr::str_detect(tolower(observations$Behaviour), 
    tolower(targetBehaviour)))) {
    cli::cli_abort("The specified behaviour '{targetBehaviour}' is not found in the observations data.")  # nolint
  }

  # If Behaviour is provided, check if any observations
  # match 'dead'. If they do, remove them with a warning
  if (remove_dead && "Behaviour" %in% colnames(observations)) {
    n_dead <- sum(stringr::str_detect(tolower(observations$Behaviour), "dead"))
    if (n_dead > 0) {
      cli::cli_warn("Removing {n_dead} observations with 'dead' behaviour. Set remove_dead = FALSE to keep them.") # nolint
      observations <- observations[!stringr::str_detect(tolower(observations$Behaviour), "dead"), ] # nolint
    }
  }

  # If Behaviour is provided, we list all the behaviours that string-match
  # so that the user can identify errors
  if (targetBehaviour != "All") {
    matching_behaviours <- unique(observations$Behaviour[stringr::str_detect(tolower(observations$Behaviour), tolower(targetBehaviour))]) # nolint
    cli::cli_inform("The following behaviours match the target behaviour:")
    cli::cli_ul(matching_behaviours)
  }
  # Filter observations for target species and behaviour (if provided)
  filtered_obs <- observations %>%
    dplyr::filter(Species == targetSpecies) %>% {
      if (targetBehaviour != "All") dplyr::filter(., stringr::str_detect(tolower(Behaviour), tolower(targetBehaviour))) else .  # nolint
    }
  # Report how many observations remain after filtering
  n_filtered <- nrow(filtered_obs)
  cli::cli_alert_info("{n_filtered} observations remain after filtering for species '{targetSpecies}' and behaviour '{targetBehaviour}'.")  # nolint
  if (n_filtered == 0) {
    cli::cli_abort("No observations remain after filtering. Please check your species and behaviour filters.")  # nolint
  }

  # Check that all observations are within survey 
  # tolerance of tracks or polygons
  track_buffer <- sf::st_buffer(tracks, dist = units::set_units(survey_tolerance, "meters"))  # nolint
  obs_within_buffer <- sf::st_within(filtered_obs, sf::st_union(track_buffer), sparse = FALSE)  # nolint
  if (any(!obs_within_buffer)) {
    n_outside <- sum(!obs_within_buffer)
    percent_outside <- round((n_outside / nrow(filtered_obs)) * 100, 2)
    cli::cli_warn("{n_outside} observations ({percent_outside}% of total) are outside the survey tolerance of {survey_tolerance} metres from the tracks/polygons. They will be removed.")  # nolint
    filtered_obs <- filtered_obs[obs_within_buffer, ]
  }

  # If any coordinates are duplicated, apply a small jitter
  coords <- sf::st_coordinates(filtered_obs)
  duplicated_coords <- duplicated(coords) | duplicated(coords, fromLast = TRUE)
  # Jitter only the duplicated coordinates with st_jitter
  if (any(duplicated_coords)) {
    cli::cli_inform("Applying jitter to {sum(duplicated_coords)}
    duplicated observation coordinates.")  # nolint
    filtered_obs[duplicated_coords, ] <- sf::st_jitter(
      filtered_obs[duplicated_coords, ],
      amount = jitter
    )  # nolint
  }
  # Check again for duplicated coordinates, to ensure this worked
  coords_after <- sf::st_coordinates(filtered_obs)
  if (any(duplicated(coords_after))) {
    n_still_duplicated <- sum(duplicated(coords_after) ||
      duplicated(coords_after, fromLast = TRUE))
    cli::cli_warn("{n_still_duplicated} observation coordinates
    are still duplicated after jittering.")
  }

  cli::cli_alert_success("Done!")
  filtered_obs
}
