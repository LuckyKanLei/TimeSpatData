# OTHERS ####
new_Variable <- function(Variable_, Name_, Unit_) { #
  attributes(Variable_) <- list(Name = Name_, Unit = as_units(Unit_))
  return(Variable_)
}

find_time_idx <- function(Time_, Time_Line) {

  if (Time_Line[1] < Time_[1]) {
    Time_[1] <- Time_Line[1]
  }

  idx_ <- map(Time_Line, \(x, Time_) {
    max(which(Time_ <= x))
  }, Time_ = Time_) |> unlist()

  return(idx_)
}

epsg <- function(crs_) {
  str_extract(crs_, "(?<=\\n    ID\\[\\\"EPSG\\\",)[:digit:]+(?=\\]\\]$)")
  # num_epsg <- str_extract(crs_, "(?<=\\n    ID\\[\\\"EPSG\\\",)[:digit:]+(?=\\]\\]$)")
  # return(paste0("EPSG:", num_epsg))
}
#' @importFrom utils read.csv
#' @importFrom stringr str_extract
num_ext <- function(ext_) {
  num_csv <- as.character(ext_) |> str_extract("(?<=ext\\().+(?=\\))")
  return(read.csv(text = num_csv, header = F) |> as.numeric())
}

#' @importFrom ncdf4 ncatt_put
write_nc_global_attr <- function(nc_, global_attr) {
  n_attr <- length(global_attr)
  name_attr <- names(global_attr)
  for (i in 1:n_attr) {
    ncatt_put(nc_, 0, name_attr[i], global_attr[i])
  }
}

weight.mat <- function(rast_, polygon_region) {
  rast_ <- rast_[[1]]
  n_grid <- size(rast_)
  id_grid <- array(1:n_grid, dim(rast_)[1:2])
  id_grid[,] <- id_grid[,dim(rast_)[2]:1]
  values(rast_) <- id_grid
  polygon_grid <- as.polygons(rast_)
  names(polygon_grid) <- "ID_grid"

  n_region <- nrow(polygon_region)
  polygon_region$region_area <- expanse(polygon_region)
  polygon_region$ID_region <- 1:n_region

  intersect_grid_region <- terra::intersect(polygon_grid, polygon_region)
  intersect_grid_region$intersect_area <- expanse(intersect_grid_region)

  df_weight <- as.data.frame(intersect_grid_region)
  mat_weight <- matrix(0, n_grid, n_region)
  for (i in 1:n_region) {
    area_intersect <- df_weight$intersect_area[df_weight$ID_region == i]
    mat_weight[df_weight$ID_grid[df_weight$ID_region == i], i] <- area_intersect / sum(area_intersect)
  }

  mat_weight
}
