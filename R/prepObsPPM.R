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
#' @param remove_dead Logical. If TRUE, observations with 'dead' behaviour will be removed (default: TRUE).
#' @param expect_empty Logical. If TRUE, allows the function to return an empty sf dataframe without error if no observations remain after filtering (default: TRUE).
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
  remove_dead = TRUE,
  expect_empty = TRUE
) {
  cli::cli_h3("Preparing observations for PPM analysis")

  cli::cli_inform(
    "Filtering observations for species '{targetSpecies}' and behaviour '{targetBehaviour}'."
  )

  cli::cli_alert_info(
    "Beginning with {nrow(observations)} total observations."
  )

  # ------------------------------------------------------------------
  # 1. Input validation
  # ------------------------------------------------------------------

  if (
    !inherits(observations, "sf") ||
      !all(sf::st_geometry_type(observations) %in% c("POINT", "MULTIPOINT"))
  ) {
    cli::cli_abort(
      "observations must be an sf dataframe with POINT or MULTIPOINT geometries."
    )
  }

  if (
    !inherits(tracks, "sf") ||
      !all(
        sf::st_geometry_type(tracks) %in%
          c(
            "LINESTRING",
            "MULTILINESTRING",
            "POLYGON",
            "MULTIPOLYGON"
          )
      )
  ) {
    cli::cli_abort(
      "tracks must be an sf dataframe with LINESTRING, MULTILINESTRING, POLYGON, or MULTIPOLYGON geometries."
    )
  }

  if (!"Species" %in% colnames(observations)) {
    cli::cli_abort(
      "The observations data must contain a 'Species' column."
    )
  }

  if (
    targetBehaviour != "All" &&
      !"Behaviour" %in% colnames(observations)
  ) {
    cli::cli_abort(
      "The observations data must contain a 'Behaviour' column to filter by behaviour."
    )
  }

  if (!targetSpecies %in% unique(observations$Species)) {
    if (expect_empty) {
      cli::cli_alert_warning(
        "The specified species '{targetSpecies}' is not found in the observations data, but expect_empty = TRUE, so returning an empty sf dataframe."
      )
      return(
        data.frame(
          Species = character(0),
          Behaviour = character(0),
          geometry = sf::st_sfc(),
          stringsAsFactors = FALSE
        ) |>
          sf::st_as_sf() |>
          sf::st_set_crs(sf::st_crs(observations))
      )
    } else {
      cli::cli_abort(
        "The specified species '{targetSpecies}' is not found in the observations data."
      )
    }
  }

  if (
    targetBehaviour != "All" &&
      !any(
        stringr::str_detect(
          tolower(observations$Behaviour),
          tolower(targetBehaviour)
        )
      )
  ) {
    if (expect_empty) {
      cli::cli_alert_warning(
        "The specified behaviour '{targetBehaviour}' is not found in the observations data, but expect_empty = TRUE, so returning an empty sf dataframe."
      )
      return(
        data.frame(
          Species = character(0),
          Behaviour = character(0),
          geometry = sf::st_sfc(),
          stringsAsFactors = FALSE
        ) |>
          sf::st_as_sf() |>
          sf::st_set_crs(sf::st_crs(observations))
      )
    } else {
      cli::cli_abort(
        "The specified behaviour '{targetBehaviour}' is not found in the observations data."
      )
    }
  }

  # ------------------------------------------------------------------
  # 2. Check CRS before doing ANY spatial operations
  # ------------------------------------------------------------------

  obs_crs <- sf::st_crs(observations)
  track_crs <- sf::st_crs(tracks)

  if (is.na(obs_crs)) {
    cli::cli_abort(
      "observations does not have a valid CRS."
    )
  }

  if (is.na(track_crs)) {
    cli::cli_abort(
      "tracks does not have a valid CRS."
    )
  }

  # We expect observations and tracks to already be in the same CRS.
  # Do NOT silently transform one here because that can hide an upstream
  # coordinate/CRS problem.
  if (obs_crs != track_crs) {
    cli::cli_abort(
      paste0(
        "observations and tracks have different CRSs.\n",
        "observations: ",
        sf::st_crs(observations)$input,
        "\n",
        "tracks: ",
        sf::st_crs(tracks)$input,
        "\n",
        "Transform them to the same CRS before calling prepObsPPM()."
      )
    )
  }

  cli::cli_inform(
    "Observations and tracks both use {obs_crs$input}."
  )

  # ------------------------------------------------------------------
  # 3. Remove dead observations
  # ------------------------------------------------------------------

  if (remove_dead && "Behaviour" %in% colnames(observations)) {
    n_dead <- sum(
      stringr::str_detect(
        tolower(observations$Behaviour),
        "dead"
      ),
      na.rm = TRUE
    )

    if (n_dead > 0) {
      cli::cli_warn(
        "Removing {n_dead} observations with 'dead' behaviour. Set remove_dead = FALSE to keep them."
      )

      observations <- observations[
        !stringr::str_detect(
          tolower(observations$Behaviour),
          "dead"
        ),
        ,
        drop = FALSE
      ]
    }
  }

  # ------------------------------------------------------------------
  # 4. Report behaviours matching requested behaviour
  # ------------------------------------------------------------------

  if (targetBehaviour != "All") {
    matching_behaviours <- unique(
      observations$Behaviour[
        stringr::str_detect(
          tolower(observations$Behaviour),
          tolower(targetBehaviour)
        )
      ]
    )

    cli::cli_inform(
      "The following behaviours match the target behaviour:"
    )

    cli::cli_ul(matching_behaviours)
  }

  # ------------------------------------------------------------------
  # 5. Filter observations by species and behaviour
  # ------------------------------------------------------------------

  filtered_obs <- observations |>
    dplyr::filter(Species == targetSpecies)

  if (targetBehaviour != "All") {
    filtered_obs <- filtered_obs |>
      dplyr::filter(
        stringr::str_detect(
          tolower(Behaviour),
          tolower(targetBehaviour)
        )
      )
  }

  n_filtered <- nrow(filtered_obs)

  cli::cli_alert_info(
    "{n_filtered} observations remain after filtering for species '{targetSpecies}' and behaviour '{targetBehaviour}'."
  )

  if (n_filtered == 0) {
    if (expect_empty) {
      cli::cli_alert_warning(
        "No observations remain after filtering, but expect_empty = TRUE, so this is expected."
      )
      return(
        data.frame(
          Species = character(0),
          Behaviour = character(0),
          geometry = sf::st_sfc(),
          stringsAsFactors = FALSE
        ) |>
          sf::st_as_sf() |>
          sf::st_set_crs(obs_crs)
      )
    } else {
      cli::cli_abort(
        "No observations remain after filtering. Please check your species and behaviour filters."
      )
    }
  }

  # ------------------------------------------------------------------
  # 6. Check observations against survey tracks
  # ------------------------------------------------------------------
  #
  # observations and tracks are both EPSG:4326 in your current data.
  #
  # Because survey_tolerance is in metres, temporarily transform BOTH
  # datasets to a projected CRS before buffering the tracks.
  #
  # This is important: st_buffer(..., 1000) on EPSG:4326 is NOT a
  # 1000-metre buffer.
  # ------------------------------------------------------------------

  spatial_crs <- 32630

  observations_proj <- sf::st_transform(
    filtered_obs,
    crs = spatial_crs
  )

  tracks_proj <- sf::st_transform(
    tracks,
    crs = spatial_crs
  )

  track_buffer <- sf::st_buffer(
    tracks_proj,
    dist = survey_tolerance
  )

  sf::sf_use_s2(FALSE)

  track_union <- track_buffer |>
    sf::st_make_valid() |>
    sf::st_union() |>
    sf::st_make_valid()

  obs_within_buffer <- sf::st_within(
    observations_proj,
    track_union,
    sparse = FALSE
  )

  sf::sf_use_s2(TRUE)

  # st_within() returns a matrix. Because track_union has been unioned,
  # reduce it to one logical value per observation.
  obs_within_buffer <- apply(
    obs_within_buffer,
    1,
    any
  )

  if (any(!obs_within_buffer)) {
    n_outside <- sum(!obs_within_buffer)

    percent_outside <- round(
      (n_outside / nrow(filtered_obs)) * 100,
      2
    )

    cli::cli_warn(
      paste0(
        "{n_outside} observations (",
        "{percent_outside}% of total) are outside the survey tolerance ",
        "of {survey_tolerance} metres from the tracks/polygons. ",
        "They will be removed."
      )
    )

    filtered_obs <- filtered_obs[obs_within_buffer, , drop = FALSE]
  }

  # ------------------------------------------------------------------
  # 7. Check that observations remain after track filtering
  # ------------------------------------------------------------------

  if (nrow(filtered_obs) == 0) {
    if (expect_empty) {
      cli::cli_alert_warning(
        "No observations remain after applying the {survey_tolerance} metre survey tolerance, but expect_empty = TRUE, so this is expected."
      )
      return(
        data.frame(
          Species = character(0),
          Behaviour = character(0),
          geometry = sf::st_sfc(),
          stringsAsFactors = FALSE
        ) |>
          sf::st_as_sf() |>
          sf::st_set_crs(obs_crs)
      )
    } else {
      cli::cli_abort(
        paste0(
          "No observations remain after applying the ",
          "{survey_tolerance} metre survey tolerance."
        )
      )
    }
  }

  cli::cli_alert_info(
    "{nrow(filtered_obs)} observations remain after the survey-track check."
  )

  # ------------------------------------------------------------------
  # 8. Handle duplicated coordinates
  # ------------------------------------------------------------------
  #
  # IMPORTANT:
  #
  # Observations are normally stored in EPSG:4326 (degrees).
  # Therefore jitter = 5 cannot be applied directly in EPSG:4326,
  # because that would mean approximately 5 degrees.
  #
  # We transform ONLY the duplicated observations to EPSG:32630,
  # apply a 5-metre jitter, then transform them back to their original
  # CRS.
  #
  # This does NOT alter the location of non-duplicated observations.
  # ------------------------------------------------------------------

  coords <- sf::st_coordinates(filtered_obs)

  duplicated_coords <- duplicated(coords) |
    duplicated(coords, fromLast = TRUE)

  if (any(duplicated_coords)) {
    n_duplicate <- sum(duplicated_coords)

    cli::cli_inform(
      "Applying {jitter} m jitter to {n_duplicate} duplicated observation coordinates."
    )

    original_crs <- sf::st_crs(filtered_obs)

    if (is.na(original_crs)) {
      cli::cli_abort(
        "Observations do not have a valid CRS, so duplicated coordinates cannot be safely jittered."
      )
    }

    # Transform to metres
    filtered_obs_proj <- sf::st_transform(
      filtered_obs,
      crs = spatial_crs
    )

    # Jitter ONLY duplicated observations
    filtered_obs_proj[duplicated_coords, ] <-
      sf::st_jitter(
        filtered_obs_proj[duplicated_coords, ],
        amount = jitter
      )

    # Transform back to original CRS
    filtered_obs <- sf::st_transform(
      filtered_obs_proj,
      crs = original_crs
    )
  }

  # ------------------------------------------------------------------
  # 9. Check for remaining duplicates
  # ------------------------------------------------------------------

  coords_after <- sf::st_coordinates(filtered_obs)

  duplicated_after <- duplicated(coords_after) |
    duplicated(coords_after, fromLast = TRUE)

  if (any(duplicated_after)) {
    n_still_duplicated <- sum(duplicated_after)

    cli::cli_warn(
      "{n_still_duplicated} observation coordinates are still duplicated after jittering."
    )
  }

  # ------------------------------------------------------------------
  # 10. Final CRS diagnostic
  # ------------------------------------------------------------------

  output_crs <- sf::st_crs(filtered_obs)

  if (is.na(output_crs)) {
    cli::cli_abort(
      "The prepared observations have lost their CRS."
    )
  }

  if (output_crs != obs_crs) {
    cli::cli_abort(
      paste0(
        "The CRS of the prepared observations does not match the input CRS.\n",
        "Input CRS:  ",
        obs_crs$input,
        "\n",
        "Output CRS: ",
        output_crs$input
      )
    )
  }

  cli::cli_inform(
    "Output CRS retained: {output_crs$input}"
  )

  # ------------------------------------------------------------------
  # 11. Final message
  # ------------------------------------------------------------------

  cli::cli_alert_success(
    "Done! Returning {nrow(filtered_obs)} prepared observations."
  )

  filtered_obs
}
