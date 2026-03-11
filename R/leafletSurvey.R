#' leafletSurvey
#'
#' Plot survey data using Leaflet
#'
#' This function creates an interactive map using the leaflet
#' package to visualize survey tracks and observations.
#'
#' @param trackdata An sf dataframe containing track points or lines
#' @param obsdata An sf dataframe containing observation points.
#' @param boundary An optional sf polygon representing the survey boundary
#' @param obsColour Character; column name in obsdata to colour points by.
#'    Options are "Behaviour", "species", or "none" for no colouring.
#'
#' @return A leaflet map object displaying the tracks.
#'
#' @details
#' This function is designed to help visualize spatial
#' track data interactively. It can be customized with
#' additional leaflet layers and options.
#'
#' @examples
#' \dontrun{
#' plotTracks.leaflet(track_data)
#' }
#'
#' @importFrom magrittr %>%
#' @export
leafletSurvey <- function(
  trackdata = NULL,
  obsdata = NULL,
  boundary = NULL,
  footprint = NULL,
  # Two options for trackColour: "transect" or a provided colour
  trackColour = "transect",
  # Three options for obsColour: "behaviour", "species", or a provided colour
  obsColour = "behaviour"
) {
  cli::cli_h3("Plotting survey data with Leaflet")

  # Convert all input data to WGS84 for Leaflet
  if (!is.null(trackdata)) {
    trackdata <- sf::st_transform(trackdata, crs = 4326)
  }
  if (!is.null(obsdata)) {
    obsdata <- sf::st_transform(obsdata, crs = 4326)
  }
  if (!is.null(boundary)) {
    boundary <- sf::st_transform(boundary, crs = 4326)
  }
  if (!is.null(footprint)) {
    footprint <- sf::st_transform(footprint, crs = 4326)
  }

  # Create basemap
  map <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addScaleBar(position = "bottomleft") %>%
    # Add measure tool
    leaflet::addMeasure(
      position = "topright",
      primaryLengthUnit = "meters",
      primaryAreaUnit = "sqmeters",
      activeColor = "#3D535D",
      completedColor = "#7D4479"
    )

  # If footprint is provided, add to map as a grey polygon
  if (!is.null(footprint)) {
    cli::cli_inform("Adding survey footprint to map...")
    map <- map %>%
      leaflet::addPolygons(
        data = footprint,
        color = "grey",
        weight = 1,
        opacity = 0.5,
        fillOpacity = 0.2,
        group = "Installation Footprint"
      )
  }

  # If trackdata is provided and trackColour is 'transect',
  # derive colour from transect_id
  if (
    !is.null(trackdata) && trackColour == "transect" &&
      "transect_id" %in% colnames(trackdata)
  ) {
    cli::cli_inform("Adding track lines to map, coloured by transect ID...")
    track_pal <- leaflet::colorFactor(
      palette = "Dark2",
      domain = trackdata$transect_id
    )
  }

  # Plot trackdata, if LINESTRINGS are provided
  if (
    !is.null(trackdata) &&
      all(sf::st_geometry_type(trackdata) == "LINESTRING")
  ) {
    cli::cli_inform("Adding track lines to map...")
    map <- map %>%
      leaflet::addPolylines(
        data = trackdata,
        # If trackColour is 'transect', use the palette
        color = if (
          trackColour == "transect" && "transect_id" %in% colnames(trackdata)
        ) {
          ~ track_pal(transect_id)
        } else {
          trackColour
        },
        weight = 2,
        opacity = 0.7,
        # If colour is transect, provide transect ID as popup
        label = ~ paste(
          "Date:", if ("date" %in% colnames(trackdata)) {
            as.character(date)
          } else {
            "N/A"
          },
          if (
            trackColour == "transect" && "transect_id" %in% colnames(trackdata)
          ) {
            paste("Transect ID:", transect_id)
          } else {
            ""
          },
          "Length:", if ("seg_length_m" %in% colnames(trackdata)) {
            paste0(round(seg_length_m, 0), " m")
          } else {
            "N/A"
          }
        ),
        # If 'date' column exists, this will be our group.
        # Otherwise, all in one group.
        group = if ("date" %in% colnames(trackdata)) {
          as.character(trackdata$date)
        } else {
          "Tracks"
        }
      )
  }

  # Plot trackdata, if POLYGONS are provided
  if (!is.null(trackdata) &&
    all(sf::st_geometry_type(trackdata) == "POLYGON")) {
    cli::cli_inform("Adding track polygons to map...")
    map <- map %>%
      leaflet::addPolygons(
        data = trackdata,
        # If trackColour is 'transect', use the palette
        color = if (
          trackColour == "transect" &
            "transect_id" %in% colnames(trackdata)
        ) {
          ~ track_pal(transect_id)
        } else {
          trackColour
        },
        weight = 2,
        opacity = 0.7,
        fillOpacity = 0.3,
        # If colour is transect, provide transect ID as popup.
        # Otherwise, no popup
        label = ~ paste(
          "Date:", if ("date" %in% colnames(trackdata)) {
            as.character(date)
          } else {
            "N/A"
          },
          if (
            trackColour == "transect" &
              "transect_id" %in% colnames(trackdata)
          ) {
            paste("Transect ID:", transect_id)
          } else {
            ""
          },
          "Effort:", if ("effort_km" %in% colnames(trackdata)) {
            paste0(round(effort_km, 2), " km^2")
          } else {
            "N/A"
          }
        ),
        # If 'date' column exists, this will be our group.
        # Otherwise, all in one group.
        group = if ("date" %in% colnames(trackdata)) {
          as.character(trackdata$date)
        } else {
          "Tracks"
        }
      )
  }

  # Plot trackdata, if POINTS are provided
  if (!is.null(trackdata) && all(sf::st_geometry_type(trackdata) == "POINT")) {
    cli::cli_inform("Adding track points to map...")
    map <- map %>%
      leaflet::addCircleMarkers(
        data = trackdata,
        radius = 2,
        color = if (
          trackColour == "transect" &
            "transect_id" %in% colnames(trackdata)
        ) {
          ~ track_pal(transect_id)
        } else {
          trackColour
        },
        stroke = FALSE,
        fillOpacity = 0.8,
        label = ~ paste(
          "Date:", if ("date" %in% colnames(trackdata)) {
            as.character(date)
          } else {
            "N/A"
          },
          if (
            trackColour == "transect" &
              "transect_id" %in% colnames(trackdata)
          ) {
            paste("Transect ID:", transect_id)
          } else {
            ""
          },
          "Effort:", if ("effort_km" %in% colnames(trackdata)) {
            paste0(round(effort_km, 2), " km")
          } else {
            "N/A"
          }
        ),
        group = if ("date" %in% colnames(trackdata)) {
          as.character(trackdata$date)
        } else {
          "Tracks"
        }
      )
  }


  # Plot obsdata, if provided
  if (!is.null(obsdata)) {
    cli::cli_inform("Adding observation points to map...")
    # Define colour palette based on obsColour
    if (obsColour == "behaviour" && "Behaviour" %in% colnames(obsdata)) {
      obs_pal <- leaflet::colorFactor(
        palette = "Set1",
        domain = obsdata$Behaviour
      )
      color_mapping <- ~ obs_pal(Behaviour)
      label_mapping <- ~ paste(
        "Species:", if ("Species" %in% colnames(obsdata)) {
          as.character(Species)
        } else {
          "N/A"
        },
        "| Behaviour:", Behaviour,
        "| Date:", if ("date" %in% colnames(obsdata)) {
          as.character(date)
        } else {
          "N/A"
        }
      )
    } else if (obsColour == "species" && "species" %in% colnames(obsdata)) {
      obs_pal <- leaflet::colorFactor(
        palette = "Set1",
        domain = obsdata$Species
      )
      color_mapping <- ~ obs_pal(Species)
      label_mapping <- ~ paste(
        "Species:", Species,
        "| Behaviour:", if ("Behaviour" %in% colnames(obsdata)) {
          as.character(Behaviour)
        } else {
          "N/A"
        },
        "| Date:", if ("date" %in% colnames(obsdata)) {
          as.character(date)
        } else {
          "N/A"
        }
      )
    } else {
      color_mapping <- obsColour
      label_mapping <- ~ paste(
        "Species:", if ("Species" %in% colnames(obsdata)) {
          as.character(Species)
        } else {
          "N/A"
        },
        "| Behaviour:", if ("Behaviour" %in% colnames(obsdata)) {
          as.character(Behaviour)
        } else {
          "N/A"
        },
        "| Date:", if ("date" %in% colnames(obsdata)) {
          as.character(date)
        } else {
          "N/A"
        }
      )
    }

    map <- map %>%
      leaflet::addCircleMarkers(
        data = obsdata,
        radius = 3,
        color = color_mapping,
        stroke = FALSE,
        fillOpacity = 0.8,
        label = label_mapping,
        group = if ("date" %in% colnames(obsdata)) {
          as.character(obsdata$date)
        } else {
          "Observations"
        }
      )
  }

  # Identify layer groups to add to map. If date column exists, use unique dates
  layer_groups <- if (!is.null(trackdata) && "date" %in% colnames(trackdata)) {
    unique(as.character(trackdata$date))
  } else if (!is.null(trackdata)) {
    "Tracks"
  } else {
    NULL
  }

  # If obsdata is provided and has date column, add those dates to layer groups
  if (!is.null(obsdata) && "date" %in% colnames(obsdata)) {
    obs_dates <- unique(as.character(obsdata$date))
    layer_groups <- unique(c(layer_groups, obs_dates))
  } else if (!is.null(obsdata)) {
    layer_groups <- unique(c(layer_groups, "Observations"))
  }

  # And add a layers control
  if (!is.null(layer_groups)) {
    map <- map %>%
      leaflet::addLayersControl(
        baseGroups = layer_groups,
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
  }

  # If boundary is provided, add to map last as a static black line
  if (!is.null(boundary)) {
    cli::cli_inform("Adding survey boundary to map...")
    map <- map %>%
      leaflet::addPolygons(
        data = boundary,
        color = "black",
        weight = 2,
        dashArray = "3,5",
        opacity = 1,
        fill = FALSE,
        group = "Survey Boundary"
      )
  }

  cli::cli_alert_success("Done!")

  map
}

utils::globalVariables(
  c("%>%")
)
