#' crop TimeSpatData (Spat-Dimension)
#' @description Crop (mask) `TimeSpatData` new `TimeSpatData`.
#' Every time can only **one Polygon** as the `mask_Area`.
#' - for `TimeVectData` the polygons, lines oder points will mit `mask_Area` first intersect [terra::intersect]
#' then only the polygons, lines oder points which in the `mask_Area` will be exported.
#' - for `TimeRastData` or `TimeRastLayerData` the spatial dimension will first crop [terra::crop] than [terra::mask]
#' the raster-cells will be in NA setted, when they are not in the `mask_Area`.
#'
#' Crop (mask) `TimeSpatData` to create a new `TimeSpatData`.
#' Every time the function is used, **only one polygon** can be used as the mask_Area.
#' - For `TimeVectData`, the polygons, lines, or points will be intersected with the mask_Area using [terra::intersect],
#' and only the polygons, lines, or points within the `mask_Area` will be exported.
#' - For `TimeRastData`, the spatial dimension will be cropped using [terra::crop] and then masked using [terra::mask].
#' Raster cells that are not within the `mask_Area` will be set to NA.
#' @name crop_tsd
#' @param tsd_Data `TimeSpatData` data
#' - `TimeVectVariable`
#' - `TimeVectArray`
#' - `TimeRastVariable`
#' - `TimeRastArray`
#' - `TimeRastLayerVariable`
#' - `TimeRastLayerArray`
#' @param mask_Area (SpatVector) **one** Polygon, from [terra::vect()]
#' @importFrom terra intersect rast crop res mask expanse
#' @importFrom stringr str_remove
#' @importFrom purrr map
#' @export
crop_tsd <- function(tsd_Data, mask_Area) UseMethod("crop_tsd", tsd_Data)

#' @rdname crop_tsd
#' @export
crop_tsd.TimeVectVariable <- function(tsd_Data, mask_Area) {
  check_epsg(tsd_Data, mask_Area)
  vect_tvd <- attr(tsd_Data, "Spat_Data")

  intersect_tvd <- intersect(vect_tvd, mask_Area)

  Spat_ID <- intersect_tvd$Spat_ID

  tvd_new <- tsd_Data[,Spat_ID] |> unclass()

  new_TimeVectVariable(tvd_new,
                       attr(tsd_Data, "Name"),
                       attr(tsd_Data, "Unit"),
                       attr(tsd_Data, "Time"),
                       Spat_ID,
                       vect_tvd[match(Spat_ID, vect_tvd$Spat_ID),])
}
#' @rdname crop_tsd
#' @export
crop_tsd.TimeVectArray <- function(tsd_Data, mask_Area) {
  check_epsg(tsd_Data, mask_Area)
  vect_tvd <- attr(tsd_Data, "Spat_Data")

  intersect_tvd <- intersect(vect_tvd, mask_Area)

  Spat_ID <- intersect_tvd$Spat_ID

  tvd_new <- tsd_Data[,Spat_ID,] |> unclass()

  new_TimeVectArray(tvd_new,
                    attr(tsd_Data, "Name"),
                    attr(tsd_Data, "Unit"),
                    attr(tsd_Data, "Time"),
                    Spat_ID,
                    vect_tvd[match(Spat_ID, vect_tvd$Spat_ID),])
}

#' @rdname crop_tsd
#' @export
crop_tsd.TimeVectLayerVariable <- function(tsd_Data, mask_Area) {
  check_epsg(tsd_Data, mask_Area)
  vect_tvd <- attr(tsd_Data, "Spat_Data")

  intersect_tvd <- intersect(vect_tvd, mask_Area)

  Spat_ID <- intersect_tvd$Spat_ID

  tvd_new <- tsd_Data[,Spat_ID,] |> unclass()

  new_TimeVectLayerVariable(tvd_new,
                       attr(tsd_Data, "Name"),
                       attr(tsd_Data, "Unit"),
                       attr(tsd_Data, "Time"),
                       Spat_ID,
                       vect_tvd[match(Spat_ID, vect_tvd$Spat_ID),])
}
#' @rdname crop_tsd
#' @export
crop_tsd.TimeVectLayerArray <- function(tsd_Data, mask_Area) {
  check_epsg(tsd_Data, mask_Area)
  vect_tvd <- attr(tsd_Data, "Spat_Data")

  intersect_tvd <- intersect(vect_tvd, mask_Area)

  Spat_ID <- intersect_tvd$Spat_ID

  tvd_new <- tsd_Data[,Spat_ID,,] |> unclass()

  new_TimeVectLayerArray(tvd_new,
                    attr(tsd_Data, "Name"),
                    attr(tsd_Data, "Unit"),
                    attr(tsd_Data, "Time"),
                    Spat_ID,
                    vect_tvd[match(Spat_ID, vect_tvd$Spat_ID),])
}

#' @rdname crop_tsd
#' @export
crop_tsd.TimeRastVariable <- function(tsd_Data, mask_Area) {
  check_epsg(tsd_Data, mask_Area)
  extent_ori <- attr(tsd_Data, "Spat_extent")
  crs_ori <- attr(tsd_Data, "Spat_crs")
  rast_trd <- rast(nrows = dim(tsd_Data)[3], ncols = dim(tsd_Data)[2], crs = paste0("EPSG:", crs_ori), extent = extent_ori)

  rast_crop <- crop(rast_trd, mask_Area)
  extent_crop <- ext(rast_crop)
  res_crop <- res(rast_crop)
  x_count <- (extent_crop[2] - extent_crop[1]) / res_crop[1]
  y_count <- (extent_crop[4] - extent_crop[3]) / res_crop[2]
  x_star <- (extent_crop[1] - extent_ori[1]) / res_crop[1] + 1
  y_star <- (extent_crop[3] - extent_ori[3]) / res_crop[1] + 1
  rast_crop[] <- 1
  rast_mask <- mask(rast_crop, mask_Area)
  mat_mask <- rast_mask |> as.array()
  idx_cell_NA <- which(is.na(mat_mask))

  data_crop <- tsd_Data[, x_star:(x_star + x_count - 1), y_star:(y_star + y_count - 1)]
  dim_n_crop <- dim(data_crop)
  ary_crop <- array(data_crop, c(dim_n_crop[1], dim_n_crop[2] * dim_n_crop[3]))
  ary_crop[, idx_cell_NA] <- NA

  new_TimeRastArray(array(ary_crop, dim_n_crop),
                    attr(tsd_Data, "Name"),
                    attr(tsd_Data, "Unit"),
                    attr(tsd_Data, "Time"),
                    crs_ori |> str_remove("EPSG:") |> as.numeric(),
                    extent_crop)
}

#' @rdname crop_tsd
#' @export
crop_tsd.TimeRastArray <- function(tsd_Data, mask_Area) {
  check_epsg(tsd_Data, mask_Area)
  extent_ori <- attr(tsd_Data, "Spat_extent")
  crs_ori <- attr(tsd_Data, "Spat_crs")
  rast_trd <- rast(nrows = dim(tsd_Data)[3], ncols = dim(tsd_Data)[2], crs = paste0("EPSG:", crs_ori), extent = extent_ori)

  rast_crop <- crop(rast_trd, mask_Area)
  extent_crop <- ext(rast_crop)
  res_crop <- res(rast_crop)
  x_count <- (extent_crop[2] - extent_crop[1]) / res_crop[1]
  y_count <- (extent_crop[4] - extent_crop[3]) / res_crop[2]
  x_star <- (extent_crop[1] - extent_ori[1]) / res_crop[1] + 1
  y_star <- (extent_crop[3] - extent_ori[3]) / res_crop[1] + 1
  rast_crop[] <- 1
  rast_mask <- mask(rast_crop, mask_Area)
  mat_mask <- rast_mask |> as.array()
  idx_cell_NA <- which(is.na(mat_mask))

  data_crop <- tsd_Data[, x_star:(x_star + x_count - 1), y_star:(y_star + y_count - 1),]
  dim_n_crop <- dim(data_crop)
  ary_crop <- array(data_crop, c(dim_n_crop[1], dim_n_crop[2] * dim_n_crop[3], dim_n_crop[4]))
  ary_crop[, idx_cell_NA, ] <- NA

  new_TimeRastArray(array(ary_crop, dim_n_crop),
                                    attr(tsd_Data, "Name"),
                                    attr(tsd_Data, "Unit"),
                                    attr(tsd_Data, "Time"),
                                    crs_ori |> str_remove("EPSG:") |> as.numeric(),
                                    extent_crop)
}



#' @rdname crop_tsd
#' @export
crop_tsd.TimeRastLayerVariable <- function(tsd_Data, mask_Area) {
  check_epsg(tsd_Data, mask_Area)
  extent_ori <- attr(tsd_Data, "Spat_extent")
  crs_ori <- attr(tsd_Data, "Spat_crs")
  rast_trd <- rast(nrows = dim(tsd_Data)[3], ncols = dim(tsd_Data)[2], crs = paste0("EPSG:", crs_ori), extent = extent_ori)

  rast_crop <- crop(rast_trd, mask_Area)
  extent_crop <- ext(rast_crop)
  res_crop <- res(rast_crop)
  x_count <- (extent_crop[2] - extent_crop[1]) / res_crop[1]
  y_count <- (extent_crop[4] - extent_crop[3]) / res_crop[2]
  x_star <- (extent_crop[1] - extent_ori[1]) / res_crop[1] + 1
  y_star <- (extent_crop[3] - extent_ori[3]) / res_crop[1] + 1
  rast_crop[] <- 1
  rast_mask <- mask(rast_crop, mask_Area)
  mat_mask <- rast_mask |> as.array()
  idx_cell_NA <- which(is.na(mat_mask))

  data_crop <- tsd_Data[, x_star:(x_star + x_count - 1), y_star:(y_star + y_count - 1),]
  dim_n_crop <- dim(data_crop)
  ary_crop <- array(data_crop, c(dim_n_crop[1], dim_n_crop[2] * dim_n_crop[3], dim_n_crop[4]))
  ary_crop[, idx_cell_NA, ] <- NA

  new_TimeRastLayerVariable(array(ary_crop, dim_n_crop),
                    attr(tsd_Data, "Name"),
                    attr(tsd_Data, "Unit"),
                    attr(tsd_Data, "Time"),
                    crs_ori |> str_remove("EPSG:") |> as.numeric(),
                    extent_crop)
}

#' @rdname crop_tsd
#' @export
crop_tsd.TimeRastLayerArray <- function(tsd_Data, mask_Area) {
  check_epsg(tsd_Data, mask_Area)
  extent_ori <- attr(tsd_Data, "Spat_extent")
  crs_ori <- attr(tsd_Data, "Spat_crs")
  rast_trd <- rast(nrows = dim(tsd_Data)[3], ncols = dim(tsd_Data)[2], crs = paste0("EPSG:", crs_ori), extent = extent_ori)

  rast_crop <- crop(rast_trd, mask_Area)
  extent_crop <- ext(rast_crop)
  res_crop <- res(rast_crop)
  x_count <- (extent_crop[2] - extent_crop[1]) / res_crop[1]
  y_count <- (extent_crop[4] - extent_crop[3]) / res_crop[2]
  x_star <- (extent_crop[1] - extent_ori[1]) / res_crop[1] + 1
  y_star <- (extent_crop[3] - extent_ori[3]) / res_crop[1] + 1
  rast_crop[] <- 1
  rast_mask <- mask(rast_crop, mask_Area)
  mat_mask <- rast_mask |> as.array()
  idx_cell_NA <- which(is.na(mat_mask))

  data_crop <- tsd_Data[, x_star:(x_star + x_count - 1), y_star:(y_star + y_count - 1),]
  dim_n_crop <- dim(data_crop)
  ary_crop <- array(data_crop, c(dim_n_crop[1], dim_n_crop[2] * dim_n_crop[3], dim_n_crop[4], dim_n_crop[5]))
  ary_crop[, idx_cell_NA, ,] <- NA

  new_TimeRastLayerArray(array(ary_crop, dim_n_crop),
                    attr(tsd_Data, "Name"),
                    attr(tsd_Data, "Unit"),
                    attr(tsd_Data, "Time"),
                    crs_ori |> str_remove("EPSG:") |> as.numeric(),
                    extent_crop)
}

