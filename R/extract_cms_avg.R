#' extract_cms_avg
#'
#' Extract CopernicusMarine data averaged over a date range at point locations
#'
#' Downloads gridded ocean or sea ice data from the CopernicusMarine repository
#' for a date range, averages each grid cell over time, and extracts the mean
#' values at the point locations in `coords`. Unlike
#' \code{\link{extract_cms_daily}}, this function makes a single download call
#' and does not use the `date` column to match points to data.
#'
#' @param coords An \code{sf} object of point locations.
#' @param product_id A character string giving the CopernicusMarine product ID,
#'   for example \code{"GLOBAL_MULTIYEAR_PHY_001_030"}.
#' @param layer_id A character string giving the layer ID within the product.
#'   Must belong to \code{product_id}.
#' @param variables A character vector of variable names to extract. Each must
#'   be available in the chosen layer.
#' @param min_date A character string or date giving the start of the time
#'   range, for example \code{"2023-01-01"}.
#' @param max_date A character string or date giving the end of the time
#'   range, for example \code{"2023-01-31"}.
#' @param cms_username A character string giving your CopernicusMarine
#'   username.
#' @param cms_pw A character string giving your CopernicusMarine password.
#' @param bbox_buffer A numeric value giving the distance in metres to buffer
#'   the bounding box of \code{coords} before downloading. Defaults to
#'   \code{5000}.
#'
#' @return An \code{sf} object with one row per input point, containing the
#'   original columns of \code{coords} plus the mean value of each variable
#'   over the date range. Missing values are reported as a warning message but
#'   are not removed.
#'
#' @details
#' The function checks that \code{product_id}, \code{layer_id}, and
#' \code{variables} are valid before downloading any data. It aborts with an
#' informative error if any check fails.
#'
#' The download is restricted to the surface (\code{verticalrange = c(0, 0)}),
#' and the function expects a single elevation value. Averaging is done over
#' the time dimension with \code{na.rm = TRUE}, so missing timesteps are
#' ignored rather than propagated.
#'
#' @seealso \code{\link{extract_cms_daily}} for extracting values matched to
#'   the date of each point.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' pts <- st_as_sf(
#'   data.frame(lon = c(-30, -28), lat = c(45, 46)),
#'   coords = c("lon", "lat"),
#'   crs = 4326
#' )
#'
#' result <- extract_cms_avg(
#'   coords = pts,
#'   product_id = "GLOBAL_MULTIYEAR_PHY_001_030",
#'   layer_id = "cmems_mod_glo_phy_my_0.083deg_P1D-m_202311",
#'   variables = c("thetao", "so"),
#'   min_date = "2023-01-01",
#'   max_date = "2023-01-31",
#'   cms_username = Sys.getenv("CMS_USERNAME"),
#'   cms_pw = Sys.getenv("CMS_PASSWORD")
#' )
#' }
#'
#' @export
extract_cms_avg <- function(
  coords,
  product_id,
  layer_id,
  variables,
  min_date,
  max_date,
  cms_username,
  cms_pw,
  bbox_buffer = 5000
) {
  cli::cli_h2(
    "Extracting {product_id} Data from CopernicusMarine Repository..."
  )

  # Validate that data is sf
  if (!inherits(coords, "sf")) {
    cli::cli_abort("coords must be an sf object")
  }

  # Check details of the product
  det <- CopernicusMarine::cms_product_details(product_id)
  cli::cli_alert_info("Selected Product: {det$title}")

  # Check the validity of the layer
  layers <- CopernicusMarine::cms_product_metadata(product_id)
  this_layer <- layers |>
    dplyr::filter(id == layer_id)
  if (nrow(this_layer) != 1) {
    cli::cli_abort("Layer ID {layer_id} is not valid for product {product_id}")
  } else {
    cli::cli_alert_info("Selected Layer: {this_layer$properties[[1]]$title}")
  }

  # Check taht the variables are valid
  vars <- this_layer$properties[[1]]$`cube:variables` |> names()
  if (!all(variables %in% vars)) {
    cli::cli_abort(
      "Some variables are not valid for layer {layer_id}. Valid variables are: {paste(vars, collapse = ', ')}"
    )
  } else {
    cli::cli_alert_info(
      "Selected Variables: {paste(variables, collapse = ', ')}"
    )
  }

  # Get the bbox for the data
  bbox <- coords |>
    sf::st_transform(4326) |>
    sf::st_union() |>
    sf::st_buffer(
      units::set_units(bbox_buffer, "m")
    ) |>
    sf::st_bbox()
  bbox_vect <- bbox |>
    as.vector()

  # Call the data
  sample_data <- CopernicusMarine::cms_download_subset(
    product = product_id,
    layer = layer_id,
    variable = variables,
    region = bbox_vect,
    timerange = c(min_date, max_date),
    verticalrange = c(0, 0),
    username = cms_username,
    password = cms_pw
  )

  # Report how many timesteps we have
  timesteps <- stars::st_get_dimension_values(sample_data, "time")
  cli::cli_inform(
    "Downloaded data has {length(timesteps)} timesteps from {min(timesteps)} to {max(timesteps)}"
  )

  # Check that there is only 1x elevation dimension
  elev <- stars::st_get_dimension_values(sample_data, "elevation")
  if (length(elev) != 1) {
    cli::cli_abort("There should only be 1 elevation value in the data")
  }

  # We want to average over these values to get a mean.
  sample_avg <- stars::st_apply(
    sample_data,
    MARGIN = c("longitude", "latitude", "elevation"),
    FUN = mean,
    na.rm = TRUE
  )

  # Now extract the values at the coordinates
  coords <- coords |>
    sf::st_transform(4326)
  values <- stars::st_extract(sample_avg, sf::st_coordinates(coords))

  out <- cbind(
    coords,
    values
  )

  cli::cli_alert_success(
    "Done extracting {product_id} data from CopernicusMarine Repository!"
  )

  # Report on NAs
  outcols <- out[, variables]
  if (any(is.na(outcols))) {
    cli::cli_alert_warning(
      "There are {sum(is.na(outcols))} NA values in the extracted data"
    )
  }

  out
}
