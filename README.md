# STAC Image Viewer

## Overview
The STAC Image Viewer is a Shiny application that allows users to visualize satellite imagery using the SpatioTemporal Asset Catalog (STAC) API. This app provides an interactive interface for selecting satellite collections, viewing available bands, and loading assets onto a map.

## Features
- **Collection Selection**: Users can select from a variety of satellite collections available through the STAC API.
- **Dynamic Band Selection**: Depending on the selected collection, users can choose specific bands or view the imagery in true color (RGB).
- **Date Range Filtering**: Users can specify a date range to filter the available satellite assets.
- **Map Visualization**: The app integrates with Leaflet to display the selected satellite imagery on an interactive map.
- **Asset Loading**: Users can load selected assets onto the map for visualization.

## Requirements
- R
- R packages: `shiny`, `leaflet`, `leaflet.extras`, `rstac`, `terra`, `sf`, `shinydashboard`, `shinyjs`, `logger`, `leafem`, `raster`

## How to Run
1. Clone or download this repository.
2. Open the `stac_viewer_app.R` file in RStudio.
3. Run the application by executing the script.

## Logging
The application logs its activities in a `logs` directory, which is created automatically upon startup. This includes information about user actions and any errors encountered during execution.
