
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Temporal-spatial Data <a href="https://luckykanlei.github.io/TimeSpatData/index.html"><img src="man/figures/logo.png" align="right" height="138" style="float:right; height:200px;"></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/tidyverse/ggplot2/actions/workflows/R-CMD-check.yaml/badge.svg)]()
<!-- badges: end -->

# Data structures

## Macro dimensions (`Dim`)

From a macro perspective, `TimeSpatData` is always defined in three
dimensions:

-   `Time`: timestamps
-   `Spat`: spatial
-   `Vari`: multiple variables

However, the Spat dimension actually has a different and complex form,
as it is based on geological data **raster** and **vector**.

In the vertical direction, we only support the *layer dimension* in this
version. This is not the physical vertical dimension like `x` and `y`,
but rather an extracted layer as a homogeneous layer.

## Data (array) dimensions (`dim`)

During the data-processing and -storage, the data is always given in
array form, the macro dimensions cannot be clearly defined in an array
dimension, therefore the array dimensions are redefined:

### `time`

Dimension `time` is defined for time and has backward consistency. It
always stands in the first dimension, with two possible options:

-   `continuous`: With a fixed time step, defined by `units` and
    `original_time`
-   `discrete`: Without the concept of time step, but always giving the
    exact timestamps

### `spat` in horizontal

Dimension `spat` is defined for vector spatial data (`points`, `lines`,
and `polygons`). It is not ordinal, but is linked to the `Spat_ID` and
`Spat_Data` variables. The coordinate reference system (CRS) is defined
using an [EPSG-code](https://epsg.io/).

### `x` und `y` in horizontal

Die beide Dimensionen sind für Raster (einzeln Schichten) horizontal
Koordinate definiert mit die Koordinaten-Werten von zentralen Punkten
des Zells. Das CRS ist mit **EPSG-code** definiert. Deshalb ist es nicht
erlaubt die Einheit oder andere zusätzliche Sachen von den beiden
Dimensionen zu definieren.

The two dimensions `x` and `y` are defined for raster data in single
layers. They represent the horizontal coordinates of the central points
of cells in the raster. The coordinate reference system (CRS) is defined
using an **EPSG-code**. It is not allowed to define units or other
additional information for these dimensions.

### `layer` in vertical

Dimension `layer` is defined as a conceptual layer without an exact
vertical position.

### `vari`

Dimension `vari` is defined for multiple corresponding variables with
`Name` and `Unit`. The dimension always stands last.

## Data type (name)

`Spat`

Due to the variety of spatial data forms, there are four `Spat-Form`
listed below:

-   `Vect [spat]`: Vector data (points, lines, polygons)
-   `VectLayer [spat, layer]`: Vector data with multiple layers
-   `Rast [x, y]`: Raster data in a single layer
-   `RastLayer [x, y, layer]`: Raster data with multiple layers

`Vari`

Due to the different properties of variables, the following four
`Vari-Form` are defined:

-   `Vari [vari = 1] / []`: single variable
-   `Array [vari]`: multiple variables with the same data-structure and
    -size

In summary, a complete `TimeSpatData` must define the three macro
dimensions (not data dimensions).

## `TimeSpatData` in NetCDF

The definition of dimensions in NetCDF remains the same as the data
structure dimensions described above. Due to the limits of NetCDF, it is
also necessary to further define other additional dimensions, variables,
and attributes.

# Processing Tools

Under the TSD structure, there are four main tools: read, write, crop,
and extract. For more details, see the section `read_tsd()`,
`write_tsd()`, `crop_tsd()` and `extract_tsd()`.
