#' extract_cms_daily
#'
#' Extract daily CopernicusMarine data at point locations
#'
#' Downloads daily gridded ocean or sea ice data from the CopernicusMarine
#' repository for each unique date in `coords`, then extracts the values at
#' the corresponding point locations. Each date is requested separately, so
#' the function makes one download call per unique date. Points are matched
#' to data by their `date` column.
#'
#' Two example products are referenced in the function's internal comments:
#' \itemize{
#'   \item \strong{GOPR} (Global Ocean Physics Reanalysis): product ID
#'     \code{GLOBAL_MULTIYEAR_PHY_001_030}, layer ID
#'     \code{cmems_mod_glo_phy_my_0.083deg_P1D-m_202311}. Variables include
#'     \code{bottomT}, \code{mlotst}, \code{siconc}, \code{sithick},
#'     \code{so}, \code{thetao}, \code{uo}, \code{usi}, \code{vo},
#'     \code{vsi}, and \code{zos}.
#'   \item \strong{GOBH} (Global Ocean Biogeochemistry Hindcast): product ID
#'     \code{GLOBAL_MULTIYEAR_BGC_001_029}, layer ID
#'     \code{cmems_mod_glo_bgc_my_0.25deg_P1D-m_202406}. Variables include
#'     \code{chl}, \code{no3}, \code{nppv}, \code{o2}, \code{po4}, and
#'     \code{si}.
#' }
#'
#' @param coords An \code{sf} object of point locations. Must contain a
#'   \code{date} column giving the date of each observation.
#' @param product_id A character string giving the CopernicusMarine product ID,
#'   for example \code{"GLOBAL_MULTIYEAR_PHY_001_030"}.
#' @param layer_id A character string giving the layer ID within the product.
#'   Must belong to \code{product_id}.
#' @param variables A character vector of variable names to extract. Each must
#'   be available in the chosen layer.
#' @param cms_username A character string giving your CopernicusMarine
#'   username.
#' @param cms_pw A character string giving your CopernicusMarine password.
#' @param bbox_buffer A numeric value giving the distance in metres to buffer
#'   the bounding box of \code{coords} before downloading. Defaults to
#'   \code{5000}.
#'
#' @return An \code{sf} object with one row per input point, containing the
#'   original columns of \code{coords} plus the extracted values for each
#'   variable. Rows are ordered by date. Missing values are reported as a
#'   warning message but are not removed.
#'
#' @details
#' The function checks that \code{product_id}, \code{layer_id}, and
#' \code{variables} are valid before downloading any data. It aborts with an
#' informative error if any check fails.
#'
#' Each download is restricted to a single timestep at the surface
#' (\code{verticalrange = c(0, 0)}). If a download returns more than one time
#' or elevation value, the function stops with an error.
#'
#' @seealso \code{\link{extract_cms_avg}} for extracting a temporal average
#'   over a date range instead of matching points to individual dates.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' pts <- st_as_sf(
#'   data.frame(
#'     lon = c(-30, -28),
#'     lat = c(45, 46),
#'     date = as.Date(c("2023-06-01", "2023-06-02"))
#'   ),
#'   coords = c("lon", "lat"),
#'   crs = 4326
#' )
#'
#' result <- extract_cms_daily(
#'   coords = pts,
#'   product_id = "GLOBAL_MULTIYEAR_PHY_001_030",
#'   layer_id = "cmems_mod_glo_phy_my_0.083deg_P1D-m_202311",
#'   variables = c("thetao", "so"),
#'   cms_username = Sys.getenv("CMS_USERNAME"),
#'   cms_pw = Sys.getenv("CMS_PASSWORD")
#' )
#' }
#'
#' @export
extract_cms_daily <- function(
  coords,
  product_id,
  layer_id,
  variables,
  cms_username,
  cms_pw,
  bbox_buffer = 5000
) {
  cli::cli_h2(
    "Extracting {product_id} Data from CopernicusMarine Repository..."
  )

  #' We need to determine the product. Here are some example daily products:
  #' - GOPR: Global Ocean Physics Reanalysis
  #'   - Product ID: GLOBAL_MULTIYEAR_PHY_001_030
  #'   - Layer ID: cmems_mod_glo_phy_my_0.083deg_P1D-m_202311 (daily)
  #'   - Available variables:
  #'      - bottomT: Sea floor potential temperature
  #'      - mlotst: Density ocean mixed layer thickness
  #'      - siconc: Ice concentration
  #'      - sithick: Sea ice thickness
  #'      - so: Salinity
  #'      - thetao: Temperature
  #'      - uo: Eastward velocity
  #'      - usi: Sea ice eastward velocity
  #'      - vo: Northward velocity
  #'      - vsi: Sea ice northward velocity
  #'      - zos: Sea surface height
  #' - GOBH: Global Ocean Biogeochemistry Hindcast
  #'   - Product ID: GLOBAL_MULTIYEAR_BGC_001_029
  #'   - Layer ID: cmems_mod_glo_bgc_my_0.25deg_P1D-m_202406 (daily)
  #'   - Available variables:
  #'      - chl: Total Chlorophyll
  #'      - no3: Nitrate
  #'      - nppv: Total Primary Production of Phyto
  #'      - o2: Dissolved Oxygen
  #'      - po4: Phosphate
  #'      - si: Dissolved Silicate

  # Validate that data is sf
  if (!inherits(coords, "sf")) {
    cli::cli_abort("coords must be an sf object")
  }

  # Validate that there is a "date" col in coords
  if (!"date" %in% colnames(coords)) {
    cli::cli_abort("coords must have a 'date' column")
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

  # Get the unique dates
  dates <- unique(coords$date)

  # Iterate over each date and extract the data
  out <- lapply(dates, function(this_date) {
    cli::cli_inform("Extracting data for {this_date}...")

    # Get the end date
    this_date <- this_date |> as.character() |> paste("UTC")

    # Download the data
    sample_data <- cms_download_subset(
      product = product_id,
      layer = layer_id,
      variable = variables,
      region = bbox_vect,
      timerange = c(this_date, this_date),
      verticalrange = c(0, 0),
      username = cms_username,
      password = cms_pw
    )

    # Check that there is only 1 element in the time and elevation dimensions
    if (length(stars::st_get_dimension_values(sample_data, "time")) != 1) {
      stop("There should only be 1 time value in the data")
    }
    if (length(stars::st_get_dimension_values(sample_data, "elevation")) != 1) {
      stop("There should only be 1 elevation value in the data")
    }

    # Get the coordinates for extraction
    ext_coords <- coords |>
      dplyr::filter(date == this_date) |>
      sf::st_transform(4326) |>
      sf::st_coordinates()

    # Extract these coordinates from the stars object
    ext <- stars::st_extract(sample_data, ext_coords)

    out_sample <- coords |>
      dplyr::filter(date == this_date) |>
      cbind(ext)

    return(out_sample)
  })

  cli::cli_alert_success(
    "Done extracting GOPR data from CopernicusMarine Repository!"
  )

  outdat <- dplyr::bind_rows(out)

  # Report on number of NAs, if any
  outcols <- outdat[, variables]
  if (any(is.na(outcols))) {
    cli::cli_alert_warning(
      "There are {sum(is.na(outcols))} NA values in the extracted data"
    )
  }

  return(outdat)
}
