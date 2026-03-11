#' Snap Observations to Nearest Effort Nodes and Summarize by Group
#'
#' This function associates observation points (e.g., from transect surveys) with their nearest effort nodes or segments (e.g., trackpoints from survey effort), and summarizes the number of observations snapped to each node within specified groups (such as survey dates).
#'
#' @param obsdata An `sf` object containing observation points. Must have geometry of type `POINT` or `MULTIPOINT`, and include a grouping column (e.g., "date").
#' @param trackpoints An `sf` object containing effort nodes or segments (e.g., trackpoints or lines). Must have geometry of type `POINT`, `MULTIPOINT`, or `LINESTRING`, and include the same grouping column as `obsdata`.
#' @param group_col Character. The name of the column used to group data (default is `"date"`). This column must be present in both `obsdata` and `trackpoints`.
#' @param sparse_format Logical. If 'TRUE', returns a dataframe output with few columns (rather than an sf-object). This is useful to prepare data for MRSea analysis. Default is 'FALSE'.
#'
#' @details
#' For each group (e.g., each survey date), the function:
#' \itemize{
#'   \item Checks that the grouping column exists in both datasets and that all groups in `obsdata` are present in `trackpoints`.
#'   \item Ensures geometries are of the correct type, casting `MULTIPOINT` to `POINT` if necessary.
#'   \item For each observation, finds the nearest effort node within the same group.
#'   \item Summarizes the number of observations snapped to each node.
#'   \item Returns a combined `sf` object with a `count` column indicating the number of observations associated with each node.
#' }
#'
#' @return An `sf` object with the same features as `trackpoints`, grouped by `group_col`, and a `count` column indicating the number of observations snapped to each node (0 if none).
#'
#' @examples
#' # Example usage:
#' # snapped <- snapObs(obsdata, trackpoints, group_col = "date")
#'
#' @export
snapObs <- function(obsdata, 
trackpoints, 
group_col = "date",
sparse_format = FALSE) {
  cli::cli_h3("Snapping observations to nearest effort nodes and summarizing by group")

  # Check group_col exists in both datasets
  if (!(group_col %in% colnames(obsdata))) {
    cli::cli_abort("'{group_col}' not found in obsdata.")
  }
  if (!(group_col %in% colnames(trackpoints))) {
    cli::cli_abort("'{group_col}' not found in trackpoints.")
  }

  # Check all levels in obsdata are present in trackpoints
  obs_groups <- unique(obsdata[[group_col]])
  track_groups <- unique(trackpoints[[group_col]])
  missing_groups <- setdiff(obs_groups, track_groups)
  if (length(missing_groups) > 0) {
    cli::cli_abort("The following '{group_col}' values in obsdata are missing from trackpoints: {missing_groups}")
  }

  # Geometry checks and casting
  if (!inherits(obsdata, "sf") || !all(sf::st_geometry_type(obsdata) %in% c("POINT", "MULTIPOINT"))) {
    cli::cli_abort("obsdata must be an sf object with POINT or MULTIPOINT geometries.")
  }
  if (!inherits(trackpoints, "sf") || !all(sf::st_geometry_type(trackpoints) %in% c("POINT", "MULTIPOINT", "LINESTRING"))) {
    cli::cli_abort("trackpoints must be an sf object with POINT, MULTIPOINT or LINESTRING geometries.")
  }
  if (any(sf::st_geometry_type(obsdata) == "MULTIPOINT")) {
    obsdata <- sf::st_cast(obsdata, "POINT")
  }
  if (any(sf::st_geometry_type(trackpoints) == "MULTIPOINT")) {
    trackpoints <- sf::st_cast(trackpoints, "POINT")
  }

  # Snap and summarize by group
  result_list <- lapply(obs_groups, function(g) {
    obs_g <- obsdata[obsdata[[group_col]] == g, ]
    track_g <- trackpoints[trackpoints[[group_col]] == g, ]
    nearest_indices <- sf::st_nearest_feature(obs_g, track_g)
    obs_g$nearest_id <- nearest_indices
    summarized_obs <- obs_g %>%
      as.data.frame() %>% # remove sf geometry for counting
      dplyr::group_by(nearest_id) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")
    track_g <- track_g %>%
      dplyr::mutate(row_id = dplyr::row_number())
    joined <- track_g %>%
      dplyr::left_join(summarized_obs, by = c("row_id" = "nearest_id")) %>%
      dplyr::mutate(count = ifelse(is.na(count), 0, count)) %>%
      dplyr::select(-row_id)
    joined
  })

  # Report maximum and minimum counts per node
  cli::cli_inform(
    "Observation counts per snapped node range from {min(sapply(result_list, function(x) min(x$count)))} to {max(sapply(result_list, function(x) max(x$count)))}."
  )

  result <- dplyr::bind_rows(result_list)

  if (sparse_format) {
    cli::cli_inform("Returning sparse dataframe format...")
    cols_to_keep <- c("X", "Y", group_col, "transect_id", "effort_km", "count")
    # Bind sf cols to dataframe
    result <- result %>%
      cbind(sf::st_coordinates(result)) %>%
      as.data.frame() %>%
      dplyr::select(dplyr::any_of(cols_to_keep))
  }

  cli::cli_alert_success("Done!")
  result
}
