# ff_smarteR

This repository contains code for analyzing and evaluating SMART patrol data. It provides a framework for both manual analysis and interactive exploration through a Shiny app.

## Overview

-   **Evaluating Patrol Efforts**: Quantify patrol coverage, frequency, and efficiency.

<!-- -->

-   **Assessing the Deterrent Effect**: Analyze the relationship between patrol efforts and the reduction in illegal activities.

-   **Visualizing Patrol Data**: Predict threats map from historical patrol using occupancy modelling framework

## Data Requirements

To perform the analysis using this repository, you will need the following data:

1.  **Patrol Sector Shapefile** (`Patrol_Sector.shp`)

    -   A shapefile representing the patrol sectors.

    -   Must include all necessary components (`.shp`, `.dbf`, `.shx`, `.prj`).

2.  **Covariates Raster TIFFs**

    -   Environmental covariates in `.tif` format.

    -   Examples include elevation, distance to roads, etc.

    -   Must have same dimension between covariates

3.  **SMART Patrol Data**

    -   **Query Data** (`Aktivitas.csv`)

        -   CSV file exported from SMART using the POKJA SMART data model.

    -   **Patrol Tracks Shapefile** (`Jalur Patroli.shp`)

        -   Shapefile of patrol tracks for CPUE (Catch Per Unit Effort) analysis, exported from SMART using the POKJA SMART data model.

        -   Must include all necessary components (`.shp`, `.dbf`, `.shx`, `.prj`).

Ensure that all spatial data are in the same Coordinate Reference System (CRS) to avoid issues during analysis.

## Manual Analysis

The `scripts/` folder contains R scripts for conducting manual analysis of SMART patrol data.

### Scripts

-   **`01_sim_data.R`**: For simulating dataset

-   **`02_real_data_CPUE.R`**: Analyzes the deterrent effect of patrols on illegal activities using real data from specific query.

-   **`03_processingcovariate.R`**: Example of processing common covariates for occupancy modelling

-   **`04_bayes_occu_by_monthly_detection.R`**: Occupancy model under bayesian framework, plot effect and predicted map

## Shiny App

The Shiny app provides an interactive interface for analyzing SMART patrol data without the need to write code.

### Location

-   The app is located in the `app/` directory.

### App Features

1.  **Covariate Processing**

    -   Upload raster layers and shapefiles to process environmental covariates.

    -   Visualize raster summaries, correlation matrices, and distribution plots.

    -   Generate maps of covariate values using interactive Leaflet maps.

2.  **CSV Data Processing**

    -   Upload CSV data exported from SMART.

    -   Process and visualize findings per quarter.

    -   Interactive tables with sorting, filtering, and summarization.

3.  **CPUE Plot**

    -   Upload patrol track shapefiles.

    -   Analyze Catch Per Unit Effort (CPUE) to assess patrol effectiveness.

    -   Generate plots to visualize the relationship between patrol effort and illegal activities.

4.  **Occupancy Modeling**

    -   Set up and run occupancy models to estimate detection probabilities and occupancy rates.

    -   Customize model formulas and parameters.

    -   Visualize model summaries, predicted maps, and response effects.
