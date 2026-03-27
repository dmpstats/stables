#' readHiDef
#'
#' Calls HiDef data from a defined filepath and compiles data into track and
#' observation objects.
#'
#' TODO:
#' - Resolve timestamp formatting in both track and observations
#'
#' @param filepath The filepath containing relevant HiDef datafiles.
#' @param overwrite_dates Logical; if TRUE, dates in the data will be replaced with dates extracted from the filepaths. Default is FALSE.
#' @param force_crs Optional; If provided, all spatial data will be forced to this CRS. This should be used if the source datasets have inconsistent CRS, as the stacking will fail.
#'
#' @return A list containing two sf dataframes: 'tracks' and 'observations'.
#' @export
readHiDef <- function(
  filepath,
  overwrite_dates = FALSE,
  force_crs = NULL
) {
  cli::cli_h3("Reading HiDef data from {.file {filepath}}")

  # From the defined filepath, identify all filepaths ending in .gdb
  gdb_dirs <- list.dirs(filepath,
    recursive = TRUE,
    full.names = TRUE
  )
  gdb_dirs <- gdb_dirs[grepl(".gdb$", gdb_dirs)]

  # Handle re-issued data by checking for duplicates
  dirs <- data.frame(
    dir = gdb_dirs,
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(
      # Extract year and month from directory name format:
      # "YYYY - Month MM - Survey SS - Issue I - YYYYMMDD"
      # (the trailing date is the review date, not the collection date)
      year = as.integer(stringr::str_extract(dir, "\\d{4}(?= - Month )")),
      month = as.integer(stringr::str_extract(dir, "(?<=Month )\\d{2}")),
      date = lubridate::make_date(year, month, 1),
      survey = stringr::str_extract(dir, "(?<=Survey )\\d{2}"),
      issue = as.integer(stringr::str_extract(dir, "(?<=Issue )\\d+"))
    ) |>
    dplyr::group_by(date, survey) |>
    dplyr::arrange(-issue) |>
    dplyr::slice(1)
  
  gdb_dirs <- dirs$dir
  if (is.null(gdb_dirs) || length(gdb_dirs) == 0) {
    cli::cli_abort("No HiDef-format .gdb files detected.")
  } else {
    cli::cli_inform("{length(gdb_dirs)} files identified.")
  }

  # If overwrite_dates, report
  if (overwrite_dates) {
    cli::cli_inform("overwrite_dates is TRUE: Dates in data will be replaced with file dates.")
  }

  # If forcing CRS, report
  if (!is.null(force_crs)) {
    cli::cli_inform("force_crs is set to {force_crs}: All spatial data will be forced to this CRS.")
  }

  # Over all directories, check for available layers
  # and select tracks and observations
  cli::cli_inform("Reading track data...")
  
  trackdata <- lapply(gdb_dirs, function(dirpath) {
    layers <- sf::st_layers(dirpath)$name
    tracklayers <- layers[grepl("track", tolower(layers))]
    tracks <- lapply(tracklayers, function(layername) {
      temptracks <- sf::st_read(dirpath,
        layer = layername,
        quiet = TRUE
      ) |>
      # Remove all underscores from column names
      dplyr::rename_with(~gsub("_", "", .x, fixed = TRUE)) |>
      # Ensure DATE is formatted correctly
      dplyr::mutate(DATE = lubridate::date(DATE)) |>
      # Select only essential cols
      dplyr::select(DATE, HEIGHT, Shape) |>
      # rename column DATE to date
      dplyr::rename(date = DATE)
      # If overwrite_dates is TRUE, replace DATE with file date
      if (overwrite_dates) {
        file_date <- stringr::str_extract(dirpath, "(?<=_)\\d{8}(?=\\.gdb$)") |>
          lubridate::ymd()
        temptracks <- temptracks |> dplyr::mutate(date = file_date)
      }
      if (!is.null(force_crs)) {
        temptracks <- temptracks |> sf::st_transform(crs = force_crs)
      }
      return(temptracks)
    }) |> dplyr::bind_rows()
  }) |> 
    dplyr::bind_rows() |>
    dplyr::rename(geometry = Shape)

  cli::cli_inform("Reading observation data...")
  obsdata <- lapply(gdb_dirs, function(dirpath) {
    layers <- sf::st_layers(dirpath)$name
    obslayers <- layers[!grepl("track", tolower(layers))]
    obs <- lapply(obslayers, function(layername) {
      tempobs <- sf::st_read(dirpath,
        layer = layername,
        quiet = TRUE
      ) |>
        dplyr::mutate(Survey_Date = lubridate::date(Survey_Date)) |>
        dplyr::select(
          Survey,
          Species,
          Survey_Date,
          Broad_Category, Behaviour,
          Camera,
          Shape
        ) |>
        dplyr::rename(date = Survey_Date, geometry = Shape)
      # If overwrite_dates is TRUE, replace Survey_Date with file date
      if (overwrite_dates) {
        file_date <- stringr::str_extract(dirpath, "(?<=_)\\d{8}(?=\\.gdb$)") |>
          lubridate::ymd()
        tempobs <- tempobs |> dplyr::mutate(date = file_date)
      }
      if( !is.null(force_crs)) {
        tempobs <- tempobs |> sf::st_transform(crs = force_crs)
      }
      return(tempobs)
    }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows()

  cli::cli_alert_success("Done!")
  # Ensure date columns are in lubridate format
  trackdata$date <- lubridate::date(trackdata$date)
  obsdata$date <- lubridate::date(obsdata$date)

  # Diagnostic checks
  # 1. Check for missing dates in tracks
  if (any(is.na(trackdata$date))) {
    cli::cli_warn("Some track records have missing date values.")
  }
  # 2. Check for missing dates in observations
  if (any(is.na(obsdata$date))) {
    cli::cli_warn("Some observation records have missing date values.")
  }
  if (
    !all(trackdata$date %in% obsdata$date) ||
      !all(obsdata$date %in% trackdata$date)
  ) {
    missing_in_obs <- setdiff(trackdata$date, obsdata$date)
    missing_in_tracks <- setdiff(obsdata$date, trackdata$date)
    if (length(missing_in_obs) > 0) {
      cli::cli_warn("Dates present in tracks but missing from observations: {paste(missing_in_obs, collapse = ', ')}")
    }
    if (length(missing_in_tracks) > 0) {
      cli::cli_warn("Dates present in observations but missing from tracks: {paste(missing_in_tracks, collapse = ', ')}")
    }
    if (length(missing_in_obs) == 0 && length(missing_in_tracks) == 0) {
      cli::cli_warn("Mismatch between dates in track and observation data.")
    }
  }
  # 3. Check for missing or invalid coordinates in tracks
  if (any(is.na(sf::st_coordinates(trackdata)))) {
    cli::cli_warn("sum(!complete.cases(sf::st_coordinates(trackdata))) track records have missing or invalid coordinates. These will be removed.")
    trackdata <- trackdata[complete.cases(sf::st_coordinates(trackdata)), ]
  }
  # 4. Check for missing or invalid coordinates in observations
  if (any(is.na(sf::st_coordinates(obsdata)))) {
    cli::cli_warn(
      "{sum(!complete.cases(sf::st_coordinates(obsdata)))} observation records have missing or invalid coordinates. These will be removed."
    )
    obsdata <- obsdata[complete.cases(sf::st_coordinates(obsdata)), ]
  }
  # 5. Check for missing Survey values in observations
  if (any(is.na(obsdata$Survey))) {
    cli::cli_warn("{sum(is.na(obsdata$Survey))} observation records have missing Survey values. These will be removed.")
    obsdata <- obsdata[!is.na(obsdata$Survey), ]
  }

  cli::cli_alert_success("Done!")

  return(
    list(
      observations = obsdata,
      tracks = trackdata
    )
  )
}