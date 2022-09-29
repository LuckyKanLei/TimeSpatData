#' create `TimeRastLayerVariable` and `TimeRastLayerArray` data
#' @description
#' - `TimeRastLayerVariable` is a data class that based on 4D-array. There are
#' time, spatial two dimensions in "macro",
#' but actually due to the spatial dimension in 2D-raster and also **layer** in vertical,
#' so there are four dimension in data: **(time, x, y, layer)**.
#' - `TimeRastLayerArray` is a data class that based on 4D-array. There are
#' time, spatial and variable three dimensions in "macro",
#' but actually due to the spatial dimension in 2D-raster and also **layer** in vertical,
#' so there are five dimension in data: **(time, x, y, layer, variable)**.
#' @param data_ (num-array or terra::SpatRaster)
#' @name TimeRastLayerData
#' @param data_ (num-array or terra::SpatRaster)
#' - 4D (array) for [new_TimeRastLayerVariable.array()], it must in the **(time, x, y, layer)** dimension-order, use the function [aperm()] to adjust the dimension-order
#' - 5D (array) for [new_TimeRastLayerArray.array()], it must in the **(time, x, y, layer, variable)** dimension-order, use the function [aperm()] to adjust the dimension-order
#' - (terra::SpatRaster) (multilayer) for [new_TimeRastVariable.SpatRaster()], created by [terra::rast()]
#' @param Name_,Unit_ (char) name and unit of Variable, `Unit_` should be  [units::as_units()]
#' @param Time_ (vector of lubridate::timepoint) time dimension, created by [lubridate::as_date()] or [lubridate::as_datetime()]
#' @param n_Layer (integer) number of the data layers
#' @param Spat_EPSG (integer) EPSG code of the CRS (Coordinate Reference System)
#' @param Spat_extent (vector of numric), extension of the Raster in c(xmin, xmax, ymin, ymax),
#' it can be projected east- and north-meter or global longitude- and latitude-degree
#' @param dim_Time_Layer the order of time- and variable-dimenion in the layer of `terra::rast`,
#' it must be
#' - `c(1, 2)`: first time then layer or
#' - `c(2, 1)`: first layer then time,
#' - default is `c(1, 2)`
#' @param dim_Time_Layer_Vari the order of time- and variable-dimenion in the layer of `terra::rast`,
#' it must be
#' - `c(1, 2, 3)`, `c(1, 3, 2)`: first time then layer or variable,
#' - `c(2, 1, 3)`, `c(2, 3, 1)`: first layer then time or variable,
#' - `c(3, 1, 2)`, `c(3, 2, 1)`: first variable then time or layer,
#' - default is `c(1, 2, 3)`
#' @param ... other parameters
#' @return `TimeRastLayerVariable` and `TimeRastLayerArray` data
#' @export
new_TimeRastLayerVariable <- function(data_, Name_, Unit_, Time_, ...) UseMethod("new_TimeRastLayerVariable", data_)

#' @rdname TimeRastLayerData
#' @importFrom terra crs nlyr ext as.array
#' @export new_TimeRastLayerVariable.SpatRaster
#' @export
new_TimeRastLayerVariable.SpatRaster <- function(data_, Name_, Unit_, Time_, n_Layer, dim_Time_Layer = c(1,2), ...) { #

  ## data_
  check_SpatRaster(data_, length(Time_) * n_Layer)

  if (!all(dim_Time_Layer |> sort() == c(1,2))) {
    stop("`dim_Time_Layer` must be `c(1, 2)` or `c(2, 1)`, default is `c(1, 2)`.")
  }

  num_Time_Layer <- c(length(Time_), n_Layer)
  ary_Data <- as.array(data_)
  dim(ary_Data) <- c(dim(ary_Data)[1:2], num_Time_Layer[dim_Time_Layer])
  ary_Data <- aperm(ary_Data, c(dim_Time_Layer[1] + 2, 1, 2, dim_Time_Layer[2] + 2))


  ## time
  check_dim_time(ary_Data, Time_)

  ## vari
  check_unit(Unit_)




  TimeRastLayerVariable <- structure(
    ary_Data,
    class = c("TimeRastLayerVariable", "array"),
    dimnames = list(as.character(Time_), NULL, NULL, NULL),
    Name = Name_,
    Unit = as_units(Unit_),
    Time = Time_,
    Spat_crs = crs(data_) |> epsg(),
    Spat_extent = ext(data_),
    n_Layer = n_Layer
  )

}

#' @rdname TimeRastLayerData
#' @export new_TimeRastLayerVariable.array
#' @export
new_TimeRastLayerVariable.array <- function(data_, Name_, Unit_, Time_, Spat_EPSG, Spat_extent, n_Layer, ...) { #

  ## dim
  check_dim_n_ary(data_, 4)


  ## time
  check_dim_time(data_, Time_)

  ## spat
  check_extent_rast(data_, Spat_EPSG, Spat_extent)
  Spat_EPSG <- paste0("EPSG:", Spat_EPSG)

  ## vari
  check_unit(Unit_)


  ## layer
  stopifnot(dim(data_)[4] == n_Layer)



  TimeRastLayerVariable <- structure(
    data_,
    class = c("TimeRastLayerVariable", "array"),
    dimnames = list(as.character(Time_), NULL, NULL, NULL),
    Name = Name_,
    Unit = as_units(Unit_),
    Time = Time_,
    Spat_crs = Spat_EPSG,
    Spat_extent = ext(Spat_extent),
    n_Layer = n_Layer
  )

}

#' @rdname TimeRastLayerData
#' @export
new_TimeRastLayerArray <- function(data_, Name_, Unit_, Time_, ...) UseMethod("new_TimeRastLayerArray", data_)

#' @rdname TimeRastLayerData
#' @importFrom terra crs nlyr ext as.array
#' @export new_TimeRastLayerArray.SpatRaster
#' @export
new_TimeRastLayerArray.SpatRaster <- function(data_, Name_, Unit_, Time_, n_Layer, dim_Time_Layer_Vari = c(1,2,3), ...) { #


  ## data_
  check_SpatRaster(data_, length(Time_) * n_Layer * length(Name_))

  if (!all(dim_Time_Layer_Vari |> sort() == c(1,2,3))) {
    stop("`dim_Time_Layer_Vari` must be `c(1, 2, 3)`, `c(1, 3, 2)`, `c(2, 1, 3)`, `c(2, 3, 1)`, `c(3, 1, 2)`, `c(3, 2, 1)`, default is `c(1, 2)`.")
  }

  num_Time_Layer_Vari <- c(length(Time_), n_Layer, length(Name_))
  ary_Data <- as.array(data_)
  dim(ary_Data) <- c(dim(ary_Data)[1:2], num_Time_Layer_Vari[dim_Time_Layer_Vari])
  ary_Data <- aperm(ary_Data, c(dim_Time_Layer_Vari[1] + 2, 1, 2, dim_Time_Layer_Vari[2] + 2, dim_Time_Layer_Vari[3] + 2))


  ## time
  check_dim_time(ary_Data, Time_)

  ## vari
  check_dim_vari(ary_Data, Name_, Unit_)



  names(Unit_) <- Name_

  TimeRastLayerArray <- structure(
    ary_Data,
    class = c("TimeRastLayerArray", "array"),
    dimnames = list(as.character(Time_), NULL, NULL, NULL, Name_),
    Unit = Unit_,
    Time = Time_,
    Spat_crs = crs(data_) |> epsg(),
    Spat_extent = ext(data_),
    n_Layer = n_Layer
  )

}




#' @rdname TimeRastLayerData
#' @export new_TimeRastLayerVariable.array
#' @export
new_TimeRastLayerArray.array <- function(data_, Name_, Unit_, Time_, Spat_EPSG, Spat_extent, n_Layer, ...) {
  ## dim
  check_dim_n_ary(data_, 5)


  ## time
  check_dim_time(data_, Time_)

  ## spat
  check_extent_rast(data_, Spat_EPSG, Spat_extent)
  Spat_EPSG <- paste0("EPSG:", Spat_EPSG)

  ## vari
  check_dim_vari(data_, Name_, Unit_)

  ## layer
  stopifnot(dim(data_)[4] == n_Layer)

  names(Unit_) <- Name_

  TimeRastLayerArray <- structure(
    data_,
    class = c("TimeRastLayerArray", "array"),
    dimnames = list(as.character(Time_), NULL, NULL, NULL, Name_),
    Name = Name_,
    Unit = Unit_,
    Time = Time_,
    Spat_crs = Spat_EPSG,
    Spat_extent = ext(Spat_extent),
    n_Layer = n_Layer
  )

}
