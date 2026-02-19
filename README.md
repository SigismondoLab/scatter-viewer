# Coordinate Viewer

A simple Shiny app for visualizing 2D scatter plots (e.g., UMAP, t-SNE) with metadata coloring and interactive hover information.

## Features

- Load X,Y coordinate data from a TSV or parquet file
- Load metadata from a TSV or parquet file
- Color points by metadata columns
- Customizable hover information: Select which metadata fields to display on hover
- Interactive plotly visualization with zoom, pan, and hover tooltip

## Installation

### Option 1: Install with conda

Create a conda environment with R and required packages:

```bash
conda create -n umap_viewer -c nodefaults -c conda-forge r-essentials r-base r-shinyfiles r-shiny r-plotly r-rcolorbrewer r-viridis r-arrow r-polychrome

```

### Option 2: Install in R/RStudio

Open R or RStudio and run:

```r
install.packages(c("shiny", "shinyFiles", "plotly", "RColorBrewer", "viridis", "arrow", "polychrome"))
```

## Usage

### Running the App

**From R/RStudio:**
```r
# Navigate to the project directory
setwd("/path/to/umap_viewer")

# Run the app
shiny::runApp()
```

**From command line:**
```bash
conda activate umap_viewer
cd /path/to/directory # replace with the actual path to the folder containing app.R
R -e "shiny::runApp()"
```

The app will open in your default web browser.

### Input File Format

**Coordinate file (.tsv or .parquet):**
- Tab-separated or `.parquet` file with columns `X` and `Y`
- Example:
```
X	Y
-5.2	3.1
-4.8	2.9
1.2	-0.5
```

**Metadata file (.tsv or .parquet):**
- Tab-separated  or `.parquet` file with any number of columns
- Must have the same number of rows as the coordinate file
- Example:
```
cell_type	expression_level	log_fold_change
T_cell	125.3	2.4
B_cell	89.7	-1.2
NK_cell	156.2	0.8
```

### Test Files

Sample test files are included:
- `test_scatter.tsv` - 40 points with X,Y coordinates
- `test_meta.tsv` - 40 rows with 6 metadata columns (categorical, numerical positive, and numerical (positive and negative) to illustrate the different color maps applied for each type)

## Workflow

1. Click **"Load coordinate"** and select your coordinate file
2. Click **"Load meta)"** and select your metadata file
3. Use the **"Color By"** dropdown to select a metadata column for coloring
4. Use **"Hover Info"** checkboxes to customize which metadata fields appear on hover
5. Interact with the plot (zoom, pan, hover over points)

