#' get the `TimeSpatVariable` from `TimeSpatArray`
#' @description get the variable from `TimeSpatArray` to `TimeSpatVariable` by the `name`:
#' - `TimeVectArray` to `TimeVectVariable`
#' - `TimeRastArray` to `TimeRastVariable`
#' - `TimeRastLayerArray` to `TimeRastLayerVariable`
#' @name subset
#' @param data_ (TimeSpatArray) data in
#' - `TimeVectArray`,
#' - `TimeRastArray` or
#' - `TimeRastLayerArray`
#' @param name (char) name od the variable
#' @importFrom  units as_units
#' @return `TimeSpatVariable` data
#' @export
`[[.TimeVectArray` <- function(data_, name){
  unclass_Data <- unclass(data_)
  Variable_ <- unclass_Data[,,name]
  TimeVectVariable <- structure(
    Variable_,
    class = c("TimeVectVariable", "matrix", "array"),
    Name = name,
    Unit = as_units(attr(data_, "Unit")[name]),
    Time = attr(data_, "Time"),
    Spat_ID = attr(data_, "Spat_ID"),
    Spat_Data = attr(data_, "Spat_Data")
  )

}

#' @name subset
#' @export
`[[.TimeRastArray` <- function(data_, name){
  unclass_Data <- unclass(data_)
  Variable_ <- unclass_Data[,,,name]
  TimeVectVariable <- structure(
    Variable_,
    class = c("TimeRastVariable", "array"),
    Name = name,
    Unit = as_units(attr(data_, "Unit")[name]),
    Time = attr(data_, "Time"),
    Spat_EPSG = attr(data_, "Spat_EPSG"),
    Spat_extent = attr(data_, "Spat_extent")
  )

}

#' @name subset
#' @export
`[[.TimeRastLayerArray` <- function(data_, name){
  unclass_Data <- unclass(data_)
  Variable_ <- unclass_Data[,,,,name]
  TimeVectVariable <- structure(
    Variable_,
    class = c("TimeRastLayerVariable", "array"),
    Name = name,
    Unit = as_units(attr(data_, "Unit")[name]),
    Time = attr(data_, "Time"),
    Spat_EPSG = attr(data_, "Spat_EPSG"),
    Spat_extent = attr(data_, "Spat_extent"),
    n_Layer = attr(data_, "n_Layer")
  )

}
