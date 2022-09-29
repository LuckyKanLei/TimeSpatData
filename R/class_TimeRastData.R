#' create `TimeRastVariable` and `TimeRastArray` data
#' @description
#' - `TimeRastVariable` is a data class that based on 3D-array. There are
#' time, spatial two dimensions in "macro",
#' but actually due to the spatial dimension in 2D-raster,
#' so there are there dimension in data: **(time, x, y)**.
#' - `TimeRastArray` is a data class that based on 4D-array. There are
#' time, spatial and variable three dimensions in "macro",
#' but actually due to the spatial dimension in 2D-raster,
#' so there are four dimension in data: **(time, x, y, variable)**.
#' @name TimeRastData
#' @param data_ (num-array or terra::SpatRaster)
#' - 3D (array) for [new_TimeRastVariable.array()], it must in the **(time, x, y)** dimension-order, use the function [aperm()] to adjust the dimension-order
#' - 4D (array) for [new_TimeRastArray.array()], it must in the **(time, x, y, variable)** dimension-order, use the function [aperm()] to adjust the dimension-order
#' - (terra::SpatRaster) (multilayer) for [new_TimeRastVariable.SpatRaster()] and [new_TimeRastArray.SpatRaster()], created by [terra::rast()]
#' @param dim_Time_Vari the order of time- and variable-dimenion in the layer of `terra::rast`,
#' it must be
#' - `c(1, 2)`: first time then variable or
#' - `c(2, 1)`: first variable then time,
#' - default is `c(1, 2)`
#' @param Name_,Unit_ (char or vector of char) name and unit of Variable, `Unit_` should be  [units::as_units()]
#' @param Time_ (vector of lubridate::timepoint) time dimension, created by [lubridate::as_date()] or [lubridate::as_datetime()]
#' @param Spat_EPSG (integer) EPSG code of the CRS (Coordinate Reference System)
#' @param Spat_extent (vector of numric), extension of the Raster in c(xmin, xmax, ymin, ymax),
#' it can be projected east- and north-meter or global longitude- and latitude-degree
#' @param ... other parameters
#' @return `TimeRastVariable`, `TimeRastArray` data
#' @export
new_TimeRastVariable <- function(data_, Name_, Unit_, Time_, ...) UseMethod("new_TimeRastVariable", data_)

#' @rdname TimeRastData
#' @importFrom terra crs nlyr ext as.array
#' @importFrom units as_units
#' @export new_TimeRastVariable.SpatRaster
#' @export
new_TimeRastVariable.SpatRaster <- function(data_, Name_, Unit_, Time_, ...) { #

  ## data_
  check_SpatRaster(data_, length(Time_))
  ary_Data <- as.array(data_) |> aperm(c(3, 2, 1))

  ## time
  check_dim_time(ary_Data, Time_)

  ## vari
  check_unit(Unit_)

  ## create
  TimeRastVariable <- structure(
    ary_Data,
    class = c("TimeRastVariable", "array"),
    dimnames = list(as.character(Time_), NULL, NULL),
    Name = Name_,
    Unit = as_units(Unit_),
    Time = Time_,
    Spat_crs = crs(data_) |> epsg(),
    Spat_extent = ext(data_)
  )

}

#' @rdname TimeRastData
#' @importFrom units as_units
#' @export new_TimeRastVariable.array
#' @export
new_TimeRastVariable.array <- function(data_, Name_, Unit_, Time_, Spat_EPSG, Spat_extent, ...) {

  ## dim
  check_dim_n_ary(data_, 3)


  ## time
  check_dim_time(data_, Time_)

  ## spat
  check_extent_rast(data_, Spat_EPSG, Spat_extent)
  Spat_EPSG <- paste0("EPSG:", Spat_EPSG)

  ## vari
  check_unit(Unit_)




  ## create
  TimeRastVariable <- structure(
    data_,
    class = c("TimeRastVariable", "array"),
    dimnames = list(as.character(Time_), NULL, NULL),
    Name = Name_,
    Unit = as_units(Unit_),
    Time = Time_,
    Spat_crs = Spat_EPSG,
    Spat_extent = ext(Spat_extent)
  )

}


#' @rdname TimeRastData
#' @export
new_TimeRastArray <- function(data_, Name_, Unit_, Time_, ...) UseMethod("new_TimeRastArray", data_)



#' @rdname TimeRastData
#' @importFrom terra crs nlyr ext as.array
#' @export new_TimeRastArray.SpatRaster
#' @export
new_TimeRastArray.SpatRaster <- function(data_, Name_, Unit_, Time_, dim_Time_Vari = c(1,2), ...) { #
  ## data_
  check_SpatRaster(data_, length(Time_) * length(Name_))

  if (!all(dim_Time_Vari |> sort() == c(1,2))) {
    stop("`dim_Time_Vari` must be `c(1, 2)` or `c(2, 1)`, default is `c(1, 2)`.")
  }

  num_Time_Vari <- c(length(Time_), length(Name_))
  ary_Data <- as.array(data_)
  dim(ary_Data) <- c(dim(ary_Data)[1:2], num_Time_Vari[dim_Time_Vari])
  ary_Data <- aperm(ary_Data, c(dim_Time_Vari[1] + 2, 2, 1, dim_Time_Vari[2] + 2))


  ## time
  check_dim_time(ary_Data, Time_)

  ## vari
  check_dim_vari(ary_Data, Name_, Unit_)

  names(Unit_) <- Name_


  TimeRastArray <- structure(
    ary_Data,
    class = c("TimeRastArray", "array"),
    dimnames = list(as.character(Time_), NULL, NULL, Name_),
    Name = Name_,
    Unit = Unit_,
    Time = Time_,
    Spat_crs = crs(data_) |> epsg(),
    Spat_extent = ext(data_)
  )

}



#' @rdname TimeRastData
#' @export new_TimeRastArray.array
#' @export
new_TimeRastArray.array <- function(data_, Name_, Unit_, Time_, Spat_EPSG, Spat_extent, ...) { #
  ## dim
  check_dim_n_ary(data_, 4)


  ## time
  check_dim_time(data_, Time_)

  ## spat
  check_extent_rast(data_, Spat_EPSG, Spat_extent)
  Spat_EPSG <- paste0("EPSG:", Spat_EPSG)

  ## vari
  check_dim_vari(data_, Name_, Unit_)


  names(Unit_) <- Name_


  TimeRastArray <- structure(
    data_,
    class = c("TimeRastArray", "array"),
    dimnames = list(as.character(Time_), NULL, NULL, Name_),
    Name = Name_,
    Unit = Unit_,
    Time = Time_,
    Spat_crs = Spat_EPSG,
    Spat_extent = ext(Spat_extent)
  )

}




