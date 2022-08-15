#' create TimeRastVariable
#' @description `TimeRastVariable` have time, spatial two dimensions in "macro",
#' but actually due to the spatial dimension in 2D-raster, so there are there dimension in data.
#' @name TimeRastVariable
#' @param data_ (num-array or terra::SpatRaster)
#' - 3D (array) for [new_TimeRastVariable.array()], it must in the **(time, x, y)** dimension-order, use the function [aperm()] to adjust the dimension-order
#' - (terra::SpatRaster) (multilayer) for [new_TimeRastVariable.SpatRaster()], created by [terra::rast()]
#' @param Name_,Unit_ (char) name and unit of Variable, `Unit_` should be  [units::as_units()]
#' @param Time_ (vector of lubridate::timepoint) time dimension, created by [lubridate::as_date()] or [lubridate::as_datetime()]
#' @param ... other parameters
#' @return `TimeRastVariable` data
#' @export
new_TimeRastVariable <- function(data_, Name_, Unit_, Time_, ...) UseMethod("new_TimeRastVariable", data_)

#' @rdname TimeRastVariable
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

#' @rdname TimeRastVariable
#' @param Spat_EPSG (integer) EPSG code of the CRS (Coordinate Reference System)
#' @param Spat_extent (vector of numric), extension of the Raster in c(xmin, xmax, ymin, ymax),
#' it can be projected east- and north-meter or global longitude- and latitude-degree
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
    Spat_EPSG = Spat_EPSG,
    Spat_extent = ext(Spat_extent)
  )

}


#' create TimeRastArray
#' @description `TimeRastArray` have time, spatial, variable three dimensions in "macro",
#' but actually due to the spatial dimension in 2D-raster, so there are four dimension in data.
#' @name TimeRastArray
#' @param data_ (num-array or terra::SpatRaster)
#' - 4D (array) for [new_TimeRastArray.array()], it must in the **(time, x, y, variable)** dimension-order, use the function [aperm()] to adjust the dimension-order
#' - (terra::SpatRaster) (multilayer) for [new_TimeRastArray.SpatRaster()], created by [terra::rast()]
#' @param Name_,Unit_ (char) name and unit of Variable, `Unit_` should be  [units::as_units()]
#' @param Time_ (vector of lubridate::timepoint) time dimension, created by [lubridate::as_date()] or [lubridate::as_datetime()]
#' @param ... other parameters
#' @return `TimeRastArray` data
#' @export
new_TimeRastArray <- function(data_, Name_, Unit_, Time_, ...) UseMethod("new_TimeRastArray", data_)



#' @rdname TimeRastArray
#' @param dim_Time_Vari the order of time- and variable-dimenion in the layer of `terra::rast`,
#' it must be
#' - `c(1, 2)`: first time then variable or
#' - `c(2, 1)`: first variable then time,
#' - default is `c(1, 2)`
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



#' @rdname TimeRastArray
#' @param Spat_EPSG (integer) EPSG code of the CRS (Coordinate Reference System)
#' @param Spat_extent (vector of numric), extension of the Raster in c(xmin, xmax, ymin, ymax),
#' it can be projected east- and north-meter or global longitude- and latitude-degree
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


#' create new_TimeRastLayerVariable
#' @description `new_TimeRastLayerVariable` have time, spatial two dimensions in "macro",
#' but actually due to the spatial dimension in 2D-raster, so there are there dimension in data.
#' @name TimeRastLayerVariable
#' @param data_ (num-array or terra::SpatRaster)
#' - 3D (array) for [new_TimeRastVariable.array()], it must in the **(time, x, y)** dimension-order, use the function [aperm()] to adjust the dimension-order
#' - (terra::SpatRaster) (multilayer) for [new_TimeRastVariable.SpatRaster()], created by [terra::rast()]
#' @param Name_,Unit_ (char) name and unit of Variable, `Unit_` should be  [units::as_units()]
#' @param Time_ (vector of lubridate::timepoint) time dimension, created by [lubridate::as_date()] or [lubridate::as_datetime()]
#' @param n_Layer (integer) number of the data layers
#' @param ... other parameters
#' @return `new_TimeRastLayerVariable` data
#' @export
new_TimeRastLayerVariable <- function(data_, Name_, Unit_, Time_, ...) UseMethod("new_TimeRastLayerVariable", data_)

#' @rdname TimeRastLayerVariable
#' @param dim_Time_Layer the order of time- and variable-dimenion in the layer of `terra::rast`,
#' it must be
#' - `c(1, 2)`: first time then layer or
#' - `c(2, 1)`: first layer then time,
#' - default is `c(1, 2)`
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

#' @rdname TimeRastLayerVariable
#' @param Spat_EPSG (integer) EPSG code of the CRS (Coordinate Reference System)
#' @param Spat_extent (vector of numric), extension of the Raster in c(xmin, xmax, ymin, ymax),
#' it can be projected east- and north-meter or global longitude- and latitude-degree
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
    Spat_EPSG = Spat_EPSG,
    Spat_extent = ext(Spat_extent),
    n_Layer = n_Layer
  )

}

#' create TimeRastLayerArray
#' @description `TimeRastLayerArray` have time, spatial two dimensions in "macro",
#' but actually due to the spatial dimension in 2D-raster, so there are there dimension in data.
#' @name TimeRastLayerArray
#' @param data_ (num-array or terra::SpatRaster)
#' - 3D (array) for [new_TimeRastVariable.array()], it must in the **(time, x, y)** dimension-order, use the function [aperm()] to adjust the dimension-order
#' - (terra::SpatRaster) (multilayer) for [new_TimeRastVariable.SpatRaster()], created by [terra::rast()]
#' @param Name_,Unit_ (char) name and unit of Variable, `Unit_` should be  [units::as_units()]
#' @param Time_ (vector of lubridate::timepoint) time dimension, created by [lubridate::as_date()] or [lubridate::as_datetime()]
#' @param n_Layer (integer) number of the data layers
#' @param ... other parameters
#' @return `TimeRastLayerArray` data
#' @export
new_TimeRastLayerArray <- function(data_, Name_, Unit_, Time_, ...) UseMethod("new_TimeRastLayerArray", data_)

#' @rdname TimeRastLayerArray
#' @param dim_Time_Layer_Vari the order of time- and variable-dimenion in the layer of `terra::rast`,
#' it must be
#' - `c(1, 2, 3)`, `c(1, 3, 2)`: first time then layer or variable,
#' - `c(2, 1, 3)`, `c(2, 3, 1)`: first layer then time or variable,
#' - `c(3, 1, 2)`, `c(3, 2, 1)`: first variable then time or layer,
#' - default is `c(1, 2, 3)`
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




#' @rdname TimeRastLayerArray
#' @param Spat_EPSG (integer) EPSG code of the CRS (Coordinate Reference System)
#' @param Spat_extent (vector of numric), extension of the Raster in c(xmin, xmax, ymin, ymax),
#' it can be projected east- and north-meter or global longitude- and latitude-degree
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
    Spat_EPSG = Spat_EPSG,
    Spat_extent = ext(Spat_extent),
    n_Layer = n_Layer
  )

}


