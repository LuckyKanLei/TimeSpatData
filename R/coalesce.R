#' coalesce TimeSpatData (Layer-Dimension)
#' @description
#' Finds the first non-missing value at each position (Raster grid, Polygon, Line or Point) in Layer-Dimension.
#' **NOTE**: this function can only be used for `TimeVectLayerVariable` and `TimeRastLayerVariable`.
#'
#' @name coalesce_tsd
#' @param tsd_Data `TimeSpatData` data
#' - `TimeVectLayerVariable`
#' - `TimeRastLayerVariable`
#' @importFrom dplyr coalesce
#' @export
coalesce_tsd <- function(tsd_Data) UseMethod("coalesce_tsd", tsd_Data)

#' @rdname coalesce_tsd
#' @export
coalesce_tsd.TimeVectLayerVariable <- function(tsd_Data) {
  n_Layer <- attr(tsd_Data, "n_Layer")
  dim_n <- dim(tsd_Data)
  ary_New <- tsd_Data[,,1] |> as.vector()

  for (i in 2:n_Layer) {
    ary_New <- coalesce(ary_New, tsd_Data[,,i] |> as.vector())
  }
  ary_New <- ary_New |> array(dim_n[1:2])
  tsd_Attr <- attributes(tsd_Data)
  tsd_Attr$dim <- dim(ary_New)
  tsd_Attr$dimnames[[3]] <- NULL
  tsd_Attr$n_Layer <- NULL
  tsd_Attr$class = c("TimeVectVariable", "array")
  attributes(ary_New) <- tsd_Attr
  ary_New

}


#' @rdname coalesce_tsd
#' @export
coalesce_tsd.TimeRastLayerVariable <- function(tsd_Data) {
  n_Layer <- attr(tsd_Data, "n_Layer")
  dim_n <- dim(tsd_Data)
  ary_New <- tsd_Data[,,,1] |> as.vector()

  for (i in 2:n_Layer) {
    ary_New <- coalesce(ary_New, tsd_Data[,,,i] |> as.vector())
  }
  ary_New <- ary_New |> array(dim_n[1:3])
  tsd_Attr <- attributes(tsd_Data)
  tsd_Attr$dim <- dim(ary_New)
  tsd_Attr$dimnames[[4]] <- NULL
  tsd_Attr$n_Layer <- NULL
  tsd_Attr$class = c("TimeRastVariable", "array")
  attributes(ary_New) <- tsd_Attr
  ary_New

}


