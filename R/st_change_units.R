#' Transform sf object to specified length unit
#'
#' This function takes a non-empty sf dataframe and transforms its CRS so that the unit of measurement matches the specified unit (e.g., "kilometre", "metre", "foot").
#' The function modifies the WKT string of the CRS to set the desired LENGTHUNIT, and reprojects the sf object accordingly.
#'
#' @param sf_obj A non-empty sf dataframe.
#' @param unit Character. Either 'm' (metre) or 'km' (kilometre).
#' @return sf dataframe with CRS updated to use the specified unit.
#' @examples
#' obs_km <- st_change_units(obs, unit = "kilometre", unit_factor = 0.001)
#' obs_ft <- st_change_units(obs, unit = "foot", unit_factor = 0.3048)
#' @export
st_change_units <- function(sf_obj, unit = "km") {
	stopifnot(requireNamespace("sf", quietly = TRUE))
	if (!inherits(sf_obj, "sf")) stop("Input must be an sf dataframe.")
	if (nrow(sf_obj) == 0) stop("Input sf dataframe is empty.")
 cli::cli_inform("Changing spatial units to {unit}...")
 
 crs_wkt <- sf::st_crs(sf_obj)$proj4string

crs_wkt_new <- gsub(
  pattern = ifelse(unit == "km", "units=m", "units=km"),
  replacement = ifelse(unit == "km", "units=km", "units=m"),
  x = crs_wkt
)

	# Transform using new CRS
	sf_obj_new <- sf::st_transform(sf_obj, crs_wkt_new)

	return(sf_obj_new)
}

