# nolint start
#' @title Get Design-Based Abundance Estimates
#'
#' @description Computes design-based density and abundance estimates from
#'   line-transect survey data using a non-parametric bootstrap. Transect-level
#'   counts and effort are summarised, mean density is bootstrapped across
#'   transects, and the resulting distribution is scaled by the survey area to
#'   produce abundance estimates with associated uncertainty.
#'
#' @param transect_data A data frame containing transect-level survey records.
#'   Must include the columns \code{transect_id} (character or factor transect
#'   identifier), \code{effort_km} (numeric transect effort in kilometres), and
#'   \code{count} (numeric animal count per record). Must have at least one row
#'   and no \code{NA} values in \code{transect_id}.
#' @param nsamp Integer. Number of bootstrap resamples used to estimate
#'   uncertainty. Defaults to \code{500}.
#' @param survey_area Numeric scalar. Total survey area (in consistent units,
#'   e.g. km²) used to convert density estimates to abundance. Must be a
#'   single, non-\code{NA} numeric value.
#'
#' @return A one-row data frame with the following columns:
#'   \describe{
#'     \item{preds}{Mean bootstrapped density (animals per km).}
#'     \item{SD}{Standard deviation of the bootstrapped density distribution.}
#'     \item{lc}{Lower 2.5th percentile of the bootstrapped density distribution.}
#'     \item{uc}{Upper 97.5th percentile of the bootstrapped density distribution.}
#'     \item{predAbund}{Predicted abundance (\code{preds * survey_area}).}
#'     \item{lowerAbund}{Lower 95\% abundance estimate (\code{lc * survey_area}).}
#'     \item{upperAbund}{Upper 95\% abundance estimate (\code{uc * survey_area}).}
#'   }
#'
#' @examples
#' \dontrun{
#' transect_df <- data.frame(
#'   transect_id = rep(c("T1", "T2", "T3"), each = 2),
#'   effort_km   = c(2.1, 1.8, 3.0, 2.5, 1.5, 2.0),
#'   count       = c(4, 2, 7, 5, 1, 3)
#' )
#' getDesignEsts(transect_df, nsamp = 1000, survey_area = 500)
#' }
# nolint end
#' @export
getDesignEsts <- function( # nolint
  transect_data,
  nsamp = 500,
  survey_area = NA
) {
  # Validate that the data is a dataframe and/or sf
  if (!is.data.frame(transect_data)) {
    stop("transect_data must be a dataframe.")
  }

  # If the input data is empty, throw an error
  if (nrow(transect_data) == 0) {
    stop("transect_data is empty.")
  }

  # Check that the required columns are present
  required_cols <- c("transect_id", "effort_km", "count")
  missing_cols <- setdiff(required_cols, colnames(transect_data))
  if (length(missing_cols) > 0) {
    stop(paste("transect_data is missing required columns:", paste(missing_cols, collapse = ", "))) # nolint
  }

  if (any(is.na(transect_data$transect_id))) {
    stop("transect_data contains NA values in transect_id column.")
  }

  # survey_area must be a single, non-NA numeric
  if (!is.numeric(survey_area) ||
    length(survey_area) != 1 || # nolint
    is.na(survey_area)
  ) {
    stop("survey_area must be a single, non-NA numeric value.")
  }

  if (any(transect_data$effort_km == 0 | is.na(transect_data$effort_km))) {
    stop("transect_data contains zero or NA values in effort_km column, which would lead to infinite density estimates.") # nolint
  }

  # Condense data to transect level, summing counts and effort
  transect_summary <- transect_data |>
    dplyr::filter(!is.na(transect_id)) |>
    dplyr::group_by(transect_id) |>
    dplyr::summarise(
      total_effort_km = sum(effort_km, na.rm = TRUE),
      total_count = sum(count, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(density = total_count / total_effort_km)

  # Iterate
  boots <- lapply(
    1:nsamp,
    function(i) {
      # Sample the transect IDs
      sample_transects <- sample(
        transect_summary$transect_id,
        nrow(transect_summary),
        replace = TRUE
      )

      # Take the matching row numbers, including duplicates
      sampled_data <- transect_summary[
        match(sample_transects, transect_summary$transect_id),
      ]

      # Calculate the mean density for this sample
      mean_density <- mean(sampled_data$density, na.rm = TRUE)

      mean_density
    }
  )
  boots_vec <- unlist(boots)

  outdat <- data.frame(
    preds = mean(boots_vec),
    SD = sd(boots_vec),
    lc = quantile(boots_vec, 0.025),
    uc = quantile(boots_vec, 0.975)
  ) |>
    dplyr::mutate(
      predAbund = preds * survey_area,
      lowerAbund = lc * survey_area,
      upperAbund = uc * survey_area
    )
  outdat
}