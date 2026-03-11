#'
#' @title addImpact.R
#'
#' @description This function adds two columns (Impact and Dist) to a 
#' given dataframe based on the proximity of points to offshore wind 
#' farm polygons.
#'
#' @param data An sf dataframe of points or segments representing observations.
#' @param owf_poly An sf or sfc object representing offshore wind farm polygons.
#' @param owf_construc_year An integer representing the year the 
#' offshore wind farm was constructed.
#' @param datecol Character. The name of the date column in 'data' to 
#' determine before/after construction year. Default is "date".
#'
#' @return An sf dataframe with two new columns:
#' #' \item{Impact}{Binary column indicating if the observation 
#' is before (0) or after (1) 
#' the construction year of the offshore wind farm.}
#' #' \item{owf_dist}{Numeric column indicating the nearest distance 
#' (in km) from each observation to the offshore wind farm polygons.}
#'
#' @examples
#' @export
addImpact <- function(
  data,
  owf_poly,
  owf_construc_year,
  datecol = "date"
) {

  # Check that owf_poly is an sf or sfc object
  stopifnot(requireNamespace("sf", quietly = TRUE))
  if (!inherits(owf_poly, c("sf", "sfc"))) {
    stop("owf_poly must be an sf or sfc object.")
  }
  
  # Check that data is an sf POINT or segment dataframe and non-empty
  if (!inherits(data, "sf")) {
    stop("data must be an sf dataframe.")
  }

  if (nrow(data) == 0) {
    stop("data sf dataframe is empty.")
  }

  # Check that owf_construc_year is an integer year
  if (!is.numeric(owf_construc_year) || length(owf_construc_year) != 1) {
    stop("owf_construc_year must be a single numeric value representing the construction year.") # nolint
  }

  # If 'datecol' is not a column in data, throw error
  if (!(datecol %in% colnames(data))) {
    stop(paste0("'", datecol, "' is not a column in data."))
  }

  # Convert OWF polygon to same CRS as data
  owf_poly <- sf::st_transform(owf_poly, sf::st_crs(data))

  # First, generate dist column - nearest distance to any OWF polygon
  cli::cli_inform("Calculating nearest distance to offshore wind farms...")
  dist_vec <- sf::st_nearest_points(data, owf_poly) %>%
    sf::st_length() %>%
    units::set_units("km") %>%
    units::drop_units()
  data$owf_dist <- dist_vec

  # Next, generate Impact column based on before/after construction year 
  cli::cli_inform("Calculating impact based on construction year {owf_construc_year}...") # nolint
  data$Impact <- ifelse(
    lubridate::year(data[[datecol]]) >= owf_construc_year,
    1, 0
  )

  # If all data is before/after impact, throw warning
  if (all(data$Impact == 0) || all(data$Impact == 1)) {
    cli::cli_warn("All data points are either before or after the construction year {owf_construc_year}. Check your date column '{datecol}'.") # nolint
  }

  data
}
