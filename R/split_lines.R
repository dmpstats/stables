#' Split Linestrings into Substrings of Maximum Length
#'
#' Splits linestring geometries in an sf object into smaller segments, each no longer than a specified maximum length.
#' This is useful for processing or visualizing long linestrings in manageable pieces.
#'
#' @param input_lines An sf object containing linestring geometries to be split.
#' @param max_length Numeric. The maximum allowed length for each segment. Units should match the CRS of `input_lines`.
#' @param id Character. Name of the identifier column for the input features. Default is "ID".
#'
#' @return An sf object with the split linestrings as geometry. All original columns are retained, with additional columns:
#'   \describe{
#'     \item{fID}{Original feature ID.}
#'     \item{piece}{Piece number within the original feature.}
#'     \item{start}{Normalized start position (0-1) of the substring.}
#'     \item{end}{Normalized end position (0-1) of the substring.}
#'   }
#'
#' @details
#' Linestrings shorter than `max_length` are not split. A warning is issued if `max_length` is suspiciously small.
#' Requires the `lwgeom` and `progress` packages.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' lines <- st_read("lines.shp")
#' split <- split_lines(lines, max_length = 1000)
#' }
#'
#' @export
split_lines <- function(input_lines, max_length, id = "ID") {
  if (max_length < 50) warning("short max length detected, do you have your units right?")

  geom_column <- attr(input_lines, "sf_column")
  input_crs <- sf::st_crs(input_lines)
  input_lines[["geom_len"]] <- sf::st_length(input_lines[[geom_column]])
  attr(input_lines[["geom_len"]], "units") <- NULL
  input_lines[["geom_len"]] <- as.numeric(input_lines[["geom_len"]])

  too_long <- dplyr::filter(input_lines, geom_len >= max_length)
  rm(input_lines) # just to control memory usage in case this is big.

  too_long <- dplyr::mutate(too_long,
    pieces = ceiling(geom_len / max_length),
    fID = 1:nrow(too_long)
  ) %>%
    dplyr::select(-geom_len)

  # Replicate all columns except geometry for each split
  split_points <- sf::st_set_geometry(too_long, NULL)[rep(seq_len(nrow(too_long)), too_long[["pieces"]]), ]
  split_points <- dplyr::mutate(split_points, split_fID = row.names(split_points)) %>%
    dplyr::group_by(fID) %>%
    dplyr::mutate(piece = 1:dplyr::n()) %>%
    dplyr::mutate(
      start = (piece - 1) / dplyr::n(),
      end = piece / dplyr::n()
    ) %>%
    dplyr::ungroup()

  new_line <- function(i, f, t) {
    lwgeom::st_linesubstring(x = too_long[[geom_column]][i], from = f, to = t)[[1]]
  }

  # Add progress bar
  if (!requireNamespace("progress", quietly = TRUE)) {
    cli::cli_abort("Please install the 'progress' package.")
  }
  pb <- progress::progress_bar$new(
    format = "  Splitting [:bar] :percent eta: :eta",
    total = nrow(split_points),
    clear = FALSE, width = 60
  )

  split_geoms <- vector("list", nrow(split_points))
  for (i in seq_len(nrow(split_points))) {
    x <- split_points[i, ]
    split_geoms[[i]] <- new_line(i = x[["fID"]], f = x[["start"]], t = x[["end"]])
    pb$tick()
  }

  rm(too_long)

  # Recombine all columns, add geometry
  split_lines_sf <- sf::st_sf(split_points, geometry = sf::st_sfc(split_geoms, crs = input_crs))

  # Diagnostic check for column retention
  orig_cols <- setdiff(names(split_points), c("piece", "start", "end", "split_fID"))
  result_cols <- setdiff(names(split_lines_sf), c("piece", "start", "end", "split_fID", attr(split_lines_sf, "sf_column")))
  lost_cols <- setdiff(orig_cols, result_cols)
  if (length(lost_cols) > 0) {
    warning("The following columns were lost during splitting: ", paste(lost_cols, collapse = ", "))
  }

  return(split_lines_sf)
}
