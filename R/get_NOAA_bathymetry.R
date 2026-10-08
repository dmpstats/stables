#' get_NOAA_bathymetry
#'
#' Extract bathymetry at point locations from NOAA data
#'
#' Downloads a NOAA bathymetry grid covering the input features (plus a
#' buffer), then looks up the depth at each feature's location using the
#' nearest grid cell. Results are checked for `NA` values, and a warning is
#' issued if any are found.
#'
#' Input geometries that are not points are reduced to a point on the surface
#' before extraction, so the output always has one row per input feature.
#'
#' @param points An `sf` object of point (or polygon) features in any CRS.
#' @param resolution Grid resolution passed to [marmap::getNOAA.bathy()]. The
#'   function passes this value through unchanged, and marmap documents its
#'   units as arc-minutes, so check that `0.25` is what you intend. Default
#'   `0.25`.
#' @param buffer_m Buffer distance in metres added around the input features
#'   before the download bounding box is calculated. Default `10000`.
#' @return The input `sf` object with an added numeric column `depth`
#'   (negative below sea level, following the marmap convention).
#' @examples
#' \dontrun{
#' pts <- sf::st_as_sf(
#'   data.frame(lon = c(-5.2, -3.1), lat = c(52.4, 55.0)),
#'   coords = c("lon", "lat"), crs = 4326
#' )
#' out <- extract_bathymetry(pts, resolution = 0.25)
#' }
get_NOAA_bathymetry <- function(points, resolution = 0.25, buffer_m = 10000) {
  if (!inherits(points, "sf")) {
    stop("`points` must be an sf object.", call. = FALSE)
  }
  if (
    !is.numeric(resolution) ||
      length(resolution) != 1 ||
      is.na(resolution) ||
      resolution <= 0
  ) {
    stop("`resolution` must be a single positive number.", call. = FALSE)
  }

  bbox <- bathy_bbox(points, buffer_m = buffer_m)
  bathy <- fetch_bathy(bbox, resolution = resolution)
  coords <- point_coords(points)
  depth <- extract_depths(bathy, coords)

  check_depth_na(depth)

  points$depth <- depth
  points
}


#' Bounding box of a set of geometries, in EPSG:4236
#'
#' Unions the geometries, applies a buffer, and returns the bounding box
#' in geographic coordinates (EPSG:4236).
#'
#' @param x An `sf` object.
#' @param buffer_m Buffer distance in metres applied before the bounding box
#'   is taken. Default `10000`.
#' @return A named numeric vector with elements `xmin`, `ymin`, `xmax`, `ymax`.
#' @keywords internal
bathy_bbox <- function(x, buffer_m = 10000) {
  x |>
    sf::st_transform(4236) |>
    sf::st_union() |>
    sf::st_buffer(units::set_units(buffer_m, "m")) |>
    sf::st_bbox()
}

#' Geographic (WGS84) coordinates of point locations
#'
#' Returns a two-column matrix of longitude and latitude. Non-point geometries
#' are reduced to a point on the surface before extraction, so that each
#' feature yields exactly one coordinate pair.
#'
#' @param x An `sf` object.
#' @return A numeric matrix with columns `X` (longitude) and `Y` (latitude),
#'   one row per feature.
#' @keywords internal
point_coords <- function(x) {
  if (!all(sf::st_geometry_type(x) == "POINT")) {
    message("Non-point geometries detected; using a point on each surface.")
    x <- sf::st_point_on_surface(x)
  }
  coords <- sf::st_coordinates(sf::st_transform(x, 4326))
  coords[, c("X", "Y"), drop = FALSE]
}

#' Download a bathymetry grid from NOAA
#'
#' Thin wrapper around [marmap::getNOAA.bathy()].
#'
#' @param bbox A named numeric vector with `xmin`, `ymin`, `xmax`, `ymax`,
#'   as returned by [bathy_bbox()].
#' @param resolution Grid resolution passed to [marmap::getNOAA.bathy()].
#' @return A `bathy` object (a matrix of depths with grid metadata).
#' @keywords internal
fetch_bathy <- function(bbox, resolution) {
  marmap::getNOAA.bathy(
    lon1 = bbox[["xmin"]],
    lon2 = bbox[["xmax"]],
    lat1 = bbox[["ymin"]],
    lat2 = bbox[["ymax"]],
    resolution = resolution
  )
}

#' Look up depth at coordinates in a bathymetry grid
#'
#' Uses the nearest grid cell to each coordinate pair.
#'
#' @param bathy A `bathy` object from [fetch_bathy()].
#' @param coords A two-column matrix of longitude and latitude, as returned by
#'   [point_coords()].
#' @return A numeric vector of depths, in the same order as the rows of
#'   `coords`. Depths are negative below sea level.
#' @keywords internal
extract_depths <- function(bathy, coords) {
  res <- marmap::get.depth(
    mat = bathy,
    x = coords[, "X"],
    y = coords[, "Y"],
    locator = FALSE
  )
  res$depth
}

#' Check extracted depths for missing values
#'
#' Issues a warning listing the affected row indices if any depths are `NA`.
#' Otherwise, reports that all values are present.
#'
#' @param depth A numeric vector of extracted depths.
#' @param max_report Maximum number of row indices to list in the warning.
#'   Default `20`.
#' @return Invisibly, an integer vector of the indices where `depth` is `NA`.
#' @keywords internal
check_depth_na <- function(depth, max_report = 20) {
  na_idx <- which(is.na(depth))
  n <- length(depth)

  if (length(na_idx) > 0) {
    shown <- utils::head(na_idx, max_report)
    more <- if (length(na_idx) > max_report) {
      sprintf(" (first %d shown)", max_report)
    } else {
      ""
    }
    warning(
      sprintf(
        "%d of %d points (%.1f%%) have NA depth. Row indices%s: %s",
        length(na_idx),
        n,
        100 * length(na_idx) / n,
        more,
        paste(shown, collapse = ", ")
      ),
      call. = FALSE
    )
  } else {
    message(sprintf("Depth check passed: all %d points have a depth value.", n))
  }

  invisible(na_idx)
}
