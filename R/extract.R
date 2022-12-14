#' extract TimeSpatData (Spat-Dimension)
#' @description Extract `TimeSpatData` to `TimeVectData`.
#' The theory is same as [terra::extract], `TimeVectData` or `TimeRastData` will be extracted using a weighted average in the given polygons (`mask_area`).
#' The weight will be calculated based on the area of the polygons or raster cells within the `mask_area`.
#' **NOTE**: this function can only be used for **values that make sense in weighted average**.
#' @name extract_tsd
#' @param tsd_data `TimeSpatData` data
#' - `TimeVectVariable`
#' - `TimeVectArray`
#' - `TimeRastVariable`
#' - `TimeRastArray`
#' @param mask_area (SpatVector) Polygons, from [terra::vect()]
#' @importFrom terra intersect rast crop res mask expanse size as.polygons values
#' @export
extract_tsd <- function(tsd_data, mask_area) UseMethod("extract_tsd", tsd_data)

#' @rdname extract_tsd
#' @export
extract_tsd.TimeVectVariable <- function(tsd_data, mask_area) {
  check_epsg(tsd_data, mask_area)

  dim_n <- dim(tsd_data)
  n_ori <- dim_n[2]
  n_region <- length(mask_area)

  ## spatial data
  vect_tvd <- attr(tsd_data, "Spat_Data")
  vect_tvd$Spat_ID_ori <- vect_tvd$Spat_ID
  Spat_ID <- mask_area$Spat_ID
  ## intersect
  intersect_tvd <- intersect(vect_tvd[, "Spat_ID_ori"], mask_area[, "Spat_ID"])
  intersect_tvd$intersect_area <- expanse(intersect_tvd)

  ## matric of weight
  df_weight <- as.data.frame(intersect_tvd)
  mat_weight <- matrix(0, n_ori, n_region)

  for (i in 1:n_region) {
    area_intersect <- df_weight$intersect_area[df_weight$Spat_ID == mask_area$Spat_ID[i]]
    mat_weight[match(df_weight$Spat_ID_ori[df_weight$Spat_ID == mask_area$Spat_ID[i]], vect_tvd$Spat_ID_ori), i] <- area_intersect / sum(area_intersect)
  }


  ## scala product
  mat_value <- tsd_data |> unclass()
  extract_data <- mat_value %*% mat_weight
  new_TimeVectVariable(extract_data,
                       attr(tsd_data, "Name"),
                       attr(tsd_data, "Unit"),
                       attr(tsd_data, "Time"),
                       Spat_ID,
                       mask_area)
}

#' @rdname extract_tsd
#' @export
extract_tsd.TimeVectArray <- function(tsd_data, mask_area) {
  check_epsg(tsd_data, mask_area)

  dim_n <- dim(tsd_data)
  n_ori <- dim_n[2]
  n_region <- length(mask_area)

  ## spatial data
  vect_tvd <- attr(tsd_data, "Spat_Data")
  vect_tvd$Spat_ID_ori <- vect_tvd$Spat_ID
  Spat_ID <- mask_area$Spat_ID

  ## intersect
  intersect_tvd <- intersect(vect_tvd[, "Spat_ID_ori"], mask_area[, "Spat_ID"])
  intersect_tvd$intersect_area <- expanse(intersect_tvd)

  ## matric of weight
  df_weight <- as.data.frame(intersect_tvd)
  mat_weight <- matrix(0, n_ori, n_region)

  for (i in 1:n_region) {
    area_intersect <- df_weight$intersect_area[df_weight$Spat_ID == mask_area$Spat_ID[i]]
    mat_weight[match(df_weight$Spat_ID_ori[df_weight$Spat_ID == mask_area$Spat_ID[i]], vect_tvd$Spat_ID_ori), i] <- area_intersect / sum(area_intersect)
  }


  ## scala product
  mat_value <- aperm(tsd_data, c(1,3,2)) |> array(c(dim_n[1] * dim_n[3], dim_n[2]))
  extract_data <- mat_value %*% mat_weight |> array(c(dim_n[1], dim_n[3], n_region)) |> aperm(c(1,3,2))
  new_TimeVectArray(extract_data,
                       attr(tsd_data, "Name"),
                       attr(tsd_data, "Unit"),
                       attr(tsd_data, "Time"),
                       Spat_ID,
                       mask_area)
}

#' @rdname extract_tsd
#' @export
extract_tsd.TimeVectLayerVariable <- function(tsd_data, mask_area) {
  check_epsg(tsd_data, mask_area)

  dim_n <- dim(tsd_data)
  n_ori <- dim_n[2]
  n_region <- length(mask_area)

  ## spatial data
  vect_tvd <- attr(tsd_data, "Spat_Data")
  vect_tvd$Spat_ID_ori <- vect_tvd$Spat_ID
  Spat_ID <- mask_area$Spat_ID
  ## intersect
  intersect_tvd <- intersect(vect_tvd[, "Spat_ID_ori"], mask_area[, "Spat_ID"])
  intersect_tvd$intersect_area <- expanse(intersect_tvd)

  ## matric of weight
  df_weight <- as.data.frame(intersect_tvd)
  mat_weight <- matrix(0, n_ori, n_region)

  for (i in 1:n_region) {
    area_intersect <- df_weight$intersect_area[df_weight$Spat_ID == mask_area$Spat_ID[i]]
    mat_weight[match(df_weight$Spat_ID_ori[df_weight$Spat_ID == mask_area$Spat_ID[i]], vect_tvd$Spat_ID_ori), i] <- area_intersect / sum(area_intersect)
  }


  ## scala product
  mat_value <- aperm(tsd_data, c(1,3,2)) |> array(c(dim_n[1] * dim_n[3], dim_n[2]))
  extract_data <- mat_value %*% mat_weight |> array(c(dim_n[1], dim_n[3], n_region)) |> aperm(c(1,3,2))
  new_TimeVectLayerVariable(extract_data,
                       attr(tsd_data, "Name"),
                       attr(tsd_data, "Unit"),
                       attr(tsd_data, "Time"),
                       Spat_ID,
                       mask_area)
}

#' @rdname extract_tsd
#' @export
extract_tsd.TimeVectLayerArray <- function(tsd_data, mask_area) {
  check_epsg(tsd_data, mask_area)

  dim_n <- dim(tsd_data)
  n_ori <- dim_n[2]
  n_region <- length(mask_area)

  ## spatial data
  vect_tvd <- attr(tsd_data, "Spat_Data")
  vect_tvd$Spat_ID_ori <- vect_tvd$Spat_ID
  Spat_ID <- mask_area$Spat_ID

  ## intersect
  intersect_tvd <- intersect(vect_tvd[, "Spat_ID_ori"], mask_area[, "Spat_ID"])
  intersect_tvd$intersect_area <- expanse(intersect_tvd)

  ## matric of weight
  df_weight <- as.data.frame(intersect_tvd)
  mat_weight <- matrix(0, n_ori, n_region)

  for (i in 1:n_region) {
    area_intersect <- df_weight$intersect_area[df_weight$Spat_ID == mask_area$Spat_ID[i]]
    mat_weight[match(df_weight$Spat_ID_ori[df_weight$Spat_ID == mask_area$Spat_ID[i]], vect_tvd$Spat_ID_ori), i] <- area_intersect / sum(area_intersect)
  }


  ## scala product
  mat_value <- aperm(tsd_data, c(1,3,4,2)) |> array(c(dim_n[1] * dim_n[3] * dim_n[4], dim_n[2]))
  extract_data <- mat_value %*% mat_weight |> array(c(dim_n[1], dim_n[3], dim_n[4], n_region)) |> aperm(c(1,4,2,3))
  new_TimeVectLayerArray(extract_data,
                    attr(tsd_data, "Name"),
                    attr(tsd_data, "Unit"),
                    attr(tsd_data, "Time"),
                    Spat_ID,
                    mask_area)
}

#' @rdname extract_tsd
#' @export
extract_tsd.TimeRastVariable <- function(tsd_data, mask_area) {
  check_epsg(tsd_data, mask_area)
  dim_n <- dim(tsd_data)
  n_region <- length(mask_area)

  extent_ori <- attr(tsd_data, "Spat_extent")
  crs_ori <- attr(tsd_data, "Spat_crs")
  rast_trd <- rast(nrows = dim(tsd_data)[3], ncols = dim(tsd_data)[2], crs = paste0("EPSG:", crs_ori), extent = extent_ori)
  Spat_ID <- mask_area$Spat_ID

  ## matrix of weight
  mat_weight <- weight.mat(rast_trd, mask_area)
  ## fix the NA cell
  mat_value_1 <- tsd_data[1,,]
  idx_value <- which(!is.na(mat_value_1))
  mat_weight <- mat_weight[idx_value,]
  if(length(mask_area) == 1) {
    mat_weight <- mat_weight / sum((mat_weight))
  } else {
    mat_weight <- mat_weight / rep(colSums(mat_weight), each = nrow(mat_weight))
  }

  mat_value <- tsd_data |> array(c(dim_n[1], dim_n[2] * dim_n[3]))

  extract_data <- mat_value %*% mat_weight
  new_TimeVectVariable(extract_data,
                       attr(tsd_data, "Name"),
                       attr(tsd_data, "Unit"),
                       attr(tsd_data, "Time"),
                       Spat_ID,
                       mask_area)
}

#' @rdname extract_tsd
#' @export
extract_tsd.TimeRastArray <- function(tsd_data, mask_area) {
  check_epsg(tsd_data, mask_area)
  dim_n <- dim(tsd_data)
  n_region <- length(mask_area)

  extent_ori <- attr(tsd_data, "Spat_extent")
  crs_ori <- attr(tsd_data, "Spat_crs")
  rast_trd <- rast(nrows = dim(tsd_data)[3], ncols = dim(tsd_data)[2], crs = paste0("EPSG:", crs_ori), extent = extent_ori)
  Spat_ID <- mask_area$Spat_ID

  ## matrix of weight
  mat_weight <- weight.mat(rast_trd, mask_area)
  ## fix the NA cell
  mat_value_1 <- tsd_data[1,,,1]
  idx_value <- which(!is.na(mat_value_1))
  mat_weight <- mat_weight[idx_value,]

  if(length(mask_area) == 1) {
    mat_weight <- mat_weight / sum((mat_weight))
  } else {
    mat_weight <- mat_weight / rep(colSums(mat_weight), each = nrow(mat_weight))
  }



  mat_value <- aperm(tsd_data, c(1,4,2,3)) |> array(c(dim_n[1] * dim_n[4], dim_n[2] * dim_n[3]))

  extract_data <- mat_value %*% mat_weight |> array(c(dim_n[1], dim_n[4], n_region)) |> aperm(c(1,3,2))
  new_TimeVectArray(extract_data,
                    attr(tsd_data, "Name"),
                    attr(tsd_data, "Unit"),
                    attr(tsd_data, "Time"),
                    Spat_ID,
                    mask_area)
}

#' @rdname extract_tsd
#' @export
extract_tsd.TimeRastLayerVariable <- function(tsd_data, mask_area) {
  check_epsg(tsd_data, mask_area)
  dim_n <- dim(tsd_data)
  n_region <- length(mask_area)

  extent_ori <- attr(tsd_data, "Spat_extent")
  crs_ori <- attr(tsd_data, "Spat_crs")
  rast_trd <- rast(nrows = dim(tsd_data)[3], ncols = dim(tsd_data)[2], crs = paste0("EPSG:", crs_ori), extent = extent_ori)
  Spat_ID <- mask_area$Spat_ID

  ## matrix of weight
  mat_weight <- weight.mat(rast_trd, mask_area)
  ## fix the NA cell
  mat_value_1 <- tsd_data[1,,]
  idx_value <- which(!is.na(mat_value_1))
  mat_weight <- mat_weight[idx_value,]
  if(length(mask_area) == 1) {
    mat_weight <- mat_weight / sum((mat_weight))
  } else {
    mat_weight <- mat_weight / rep(colSums(mat_weight), each = nrow(mat_weight))
  }

  mat_value <- aperm(tsd_data, c(1,4,2,3)) |> array(c(dim_n[1] * dim_n[4], dim_n[2] * dim_n[3]))

  extract_data <- mat_value %*% mat_weight |> array(c(dim_n[1], dim_n[4], n_region)) |> aperm(c(1,3,2))
  new_TimeVectLayerVariable(extract_data,
                       attr(tsd_data, "Name"),
                       attr(tsd_data, "Unit"),
                       attr(tsd_data, "Time"),
                       Spat_ID,
                       mask_area)
}

#' @rdname extract_tsd
#' @export
extract_tsd.TimeRastLayerArray <- function(tsd_data, mask_area) {
  check_epsg(tsd_data, mask_area)
  dim_n <- dim(tsd_data)
  n_region <- length(mask_area)

  extent_ori <- attr(tsd_data, "Spat_extent")
  crs_ori <- attr(tsd_data, "Spat_crs")
  rast_trd <- rast(nrows = dim(tsd_data)[3], ncols = dim(tsd_data)[2], crs = paste0("EPSG:", crs_ori), extent = extent_ori)
  Spat_ID <- mask_area$Spat_ID

  ## matrix of weight
  mat_weight <- weight.mat(rast_trd, mask_area)
  ## fix the NA cell
  mat_value_1 <- tsd_data[1,,,1]
  idx_value <- which(!is.na(mat_value_1))
  mat_weight <- mat_weight[idx_value,]

  if(length(mask_area) == 1) {
    mat_weight <- mat_weight / sum((mat_weight))
  } else {
    mat_weight <- mat_weight / rep(colSums(mat_weight), each = nrow(mat_weight))
  }



  mat_value <- aperm(tsd_data, c(1,4,5,2,3)) |> array(c(dim_n[1] * dim_n[4] * dim_n[5], dim_n[2] * dim_n[3]))

  extract_data <- mat_value %*% mat_weight |> array(c(dim_n[1], dim_n[4], dim_n[5], n_region)) |> aperm(c(1,4,2,3))
  new_TimeVectLayerArray(extract_data,
                    attr(tsd_data, "Name"),
                    attr(tsd_data, "Unit"),
                    attr(tsd_data, "Time"),
                    Spat_ID,
                    mask_area)
}





