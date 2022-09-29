#' write TimeSpatData
#' @description Write `TimeSpatData` to nc-file.
#' @name write_tsd
#' @param data_ `TimeSpatData` data
#' - `TimeVectVariable`
#' - `TimeVectArray`
#' - `TimeRastVariable`
#' - `TimeRastArray`
#' - `TimeRastLayerVariable`
#' - `TimeRastLayerArray`
#' @param fn_ (char) filenames
#' @param other_Attr (vector of named char) other global attributes
#' @import terra ncdf4
#' @importFrom stringr str_length
#' @importFrom purrr map
#' @export
write_tsd <- function(data_, fn_, other_Attr = NULL) UseMethod("write_tsd", data_)

#' @rdname write_tsd
#' @export
write_tsd.TimeVectVariable <- function(data_, fn_, other_Attr = NULL) {
  data_tvv <- data_
  dim_n <- dim(data_tvv)
  vect_data <- attr(data_tvv, "Spat_Data")
  geom_data <- geom(vect_data)
  attr_data <- values(vect_data) |> as.data.frame()
  width_Spat_ID <- map(attr_data$Spat_ID, str_length) |> unlist() |> max()

  n_geom <- length(unique(geom_data[,1]))
  width_Time <- map(attr(data_tvv, "Time"), str_length) |> unlist() |> max()

  ## define the Dimensions
  dim_time <- ncdim_def("time", "",
                        1:dim_n[1], create_dimvar = FALSE)
  dim_spat <- ncdim_def("spat", "",
                        1:dim_n[2], create_dimvar = FALSE)
  dim_time_width <- ncdim_def("Y-M-D h:m:s", "",
                              1:width_Time, create_dimvar = FALSE)
  dim_spat_ID_width <- ncdim_def("spat_ID_width", "",
                                 1:width_Spat_ID, create_dimvar = FALSE)
  dim_geom <- ncdim_def("geometry", "",
                        1:dim(geom_data)[1], create_dimvar = FALSE)
  dim_geom_width <- ncdim_def("geom part x y hole", "",
                              1:5, create_dimvar = FALSE)
  dim_geom_ID <- ncdim_def("geometry_ID", "",
                           1:n_geom, create_dimvar = FALSE)
  ## define the data
  var_Data <- ncvar_def("Data", "", list(dim_time, dim_spat), -9999)
  var_time <- ncvar_def("Time", "", list(dim_time_width, dim_time), " ", prec = "char")
  var_spat <- ncvar_def("Spat_ID", "", list(dim_spat_ID_width, dim_spat), " ", prec = "char")
  var_geom <- ncvar_def("Spat_Geom", "", list(dim_geom, dim_geom_width), -9999)
  var_geom_ID <- ncvar_def("Spat_Geom_ID", "", list(dim_spat_ID_width, dim_geom_ID), " ", prec = "char")
  vars <- list(var_Data, var_time, var_spat, var_geom, var_geom_ID)

  ## Create a new empty netcdf file
  nc_ <- nc_create(fn_, vars, TRUE)

  ## put the data
  ncvar_put(nc_, var_Data, unclass(data_tvv))
  # ncvar_put(nc_, var_time, mt_Datetime)
  ncvar_put(nc_, var_time, attr(data_tvv, "Time"))
  ncvar_put(nc_, var_spat, attr(data_tvv, "Spat_ID"))
  ncvar_put(nc_, var_geom, geom_data)
  ncvar_put(nc_, var_geom_ID, attr_data$Spat_ID)

  ## Varible Attributes
  ncatt_put(nc_, var_Data, "Name", attr(data_tvv, "Name"))
  ncatt_put(nc_, var_Data, "Unit", attr(data_tvv, "Unit") |> attr("units") |> as.character())
  ncatt_put(nc_, var_time, "Time_Zone", tz(attr(data_tvv, "Time")[1]))
  ncatt_put(nc_, var_geom, "Shape", geomtype(vect_data))
  ncatt_put(nc_, var_geom, "EPSG", crs(vect_data) |> epsg())
  ## global Attributes
  ncatt_put(nc_, 0, "Data_Type", "TimeVectVariable")
  ncatt_put(nc_, 0, "creat_time", Sys.time() |> as.character())

  if(!is.null(other_Attr)) write_nc_global_attr(nc_, other_Attr)

  nc_close(nc_)

}

#' @rdname write_tsd
#' @export
write_tsd.TimeVectArray <- function(data_, fn_, other_Attr = NULL) {
  data_tva <- data_
  dim_n <- dim(data_tva)
  vect_data <- attr(data_tva, "Spat_Data")
  geom_data <- geom(vect_data)
  attr_data <- values(vect_data) |> as.data.frame()
  width_Spat_ID <- map(attr_data$Spat_ID, str_length) |> unlist() |> max()

  n_geom <- length(unique(geom_data[,1]))
  width_Time <- map(attr(data_tva, "Time"), str_length) |> unlist() |> max()

  ## define the Dimensions
  dim_time <- ncdim_def("time", "",
                        1:dim_n[1], create_dimvar = FALSE)
  dim_spat <- ncdim_def("spat", "",
                        1:dim_n[2], create_dimvar = FALSE)
  dim_vari <- ncdim_def("vari", "",
                        1:dim_n[3], create_dimvar = FALSE,
                        longname = "Name of variables")
  dim_time_width <- ncdim_def("Y-M-D h:m:s", "",
                              1:width_Time, create_dimvar = FALSE)
  dim_spat_ID_width <- ncdim_def("spat_ID_width", "",
                                 1:width_Spat_ID, create_dimvar = FALSE)
  dim_geom <- ncdim_def("geometry", "",
                        1:dim(geom_data)[1], create_dimvar = FALSE)
  dim_geom_width <- ncdim_def("geom part x y hole", "",
                              1:5, create_dimvar = FALSE)
  dim_geom_ID <- ncdim_def("geometry_ID", "",
                           1:n_geom, create_dimvar = FALSE)
  ## define the data
  var_Data <- ncvar_def("Data", "", list(dim_time, dim_spat, dim_vari), -9999)
  # var_time <- ncvar_def("Time", "", list(dim_time, dim_time_width), -9999, prec = "integer")
  var_time <- ncvar_def("Time", "", list(dim_time_width, dim_time), " ", prec = "char")
  var_spat <- ncvar_def("Spat_ID", "", list(dim_spat_ID_width, dim_spat), " ", prec = "char")
  var_geom <- ncvar_def("Spat_Geom", "", list(dim_geom, dim_geom_width), -9999)
  var_geom_ID <- ncvar_def("Spat_Geom_ID", "", list(dim_spat_ID_width, dim_geom_ID), " ", prec = "char")
  vars <- list(var_Data, var_time, var_spat, var_geom, var_geom_ID) #, var_name, var_unit)

  ## Create a new empty netcdf file
  nc_ <- nc_create(fn_, vars, TRUE)

  ## put the data
  data_unclass <- unclass(data_tva)

  for (i in 1:dim_n[3]) {
    ncvar_put(nc_, var_Data, data_unclass[,,i], c(1,1,i), c(dim_n[1:2],1))
  }

  # ncvar_put(nc_, var_Data, unclass(data_tva))
  # ncvar_put(nc_, var_time, mt_Datetime)
  ncvar_put(nc_, var_time, attr(data_tva, "Time"))
  ncvar_put(nc_, var_spat, attr(data_tva, "Spat_ID"))
  ncvar_put(nc_, var_geom, geom_data)
  ncvar_put(nc_, var_geom_ID, attr_data$Spat_ID)

  ## Varible Attributes
  ncatt_put(nc_, var_Data, "Name", paste0(attr(data_tva, "Name"), collapse = ","))
  ncatt_put(nc_, var_Data, "Unit", paste0(attr(data_tva, "Unit"), collapse = ","))
  ncatt_put(nc_, var_time, "Time_Zone", tz(attr(data_tva, "Time")[1]))
  ncatt_put(nc_, var_geom, "Shape", geomtype(vect_data))
  ncatt_put(nc_, var_geom, "EPSG", crs(vect_data) |> epsg())

  ## global Attributes
  ncatt_put(nc_, 0, "Data_Type", "TimeVectArray")
  ncatt_put(nc_, 0, "creat_time", Sys.time() |> as.character())

  if(!is.null(other_Attr)) write_nc_global_attr(nc_, other_Attr)

  nc_close(nc_)

}

#' @rdname write_tsd
#' @export
write_tsd.TimeRastVariable <- function(data_, fn_, other_Attr = NULL) {
  data_trv <- data_
  dim_n <- dim(data_trv)
  rast_demo <- rast(data_trv[1,,] |> t(), crs = attr(data_trv, "Spat_crs"), extent = attr(data_trv, "Spat_extent"))
  x_rast <- xFromCol(rast_demo, 1:dim_n[2])
  y_rast <- yFromRow(rast_demo, 1:dim_n[3])
  width_Time <- map(attr(data_trv, "Time"), str_length) |> unlist() |> max()

  ## define the Dimensions
  dim_time <- ncdim_def("time", "",
                        1:dim_n[1], create_dimvar = FALSE,
                        longname = "Date or Datetime")
  dim_x <- ncdim_def("x", "",
                     x_rast,
                     longname = "[X] in projections or [Longitude] in geographic, define by the CRS (EPSG).")
  dim_y <- ncdim_def("y", "",
                     y_rast,
                     longname = "[Y] in projections or [Latitude] in geographic, define by the CRS (EPSG).")

  dim_time_width <- ncdim_def("time_width", "",
                              1:width_Time, create_dimvar = FALSE,
                              longname = "Year, Month, Day, Hour, Minute, Second")
  dim_extent <- ncdim_def("crs_extent", "",
                          1:4, create_dimvar = FALSE)
  ## define the data
  var_Data <- ncvar_def("Data", "", list(dim_time, dim_x, dim_y), -9999)
  var_time <- ncvar_def("Time", "", list(dim_time_width, dim_time), " ", prec = "char")
  var_geometry<- ncvar_def("Geometry", "", list(dim_extent), -9999, "Geometry with EPSG (CRS) and Extent")

  vars <- list(var_Data, var_time, var_geometry) #, var_name, var_unit)

  ## Create a new empty netcdf file
  nc_ <- nc_create(fn_, vars, TRUE)

  ## put the data
  ncvar_put(nc_, var_Data, unclass(data_trv))
  ncvar_put(nc_, var_time, attr(data_trv, "Time"))
  ncvar_put(nc_, var_geometry, attr(data_trv, "Spat_extent") |> num_ext())

  ## Varible Attributes
  ncatt_put(nc_, var_Data, "Name", attr(data_trv, "Name"))
  ncatt_put(nc_, var_Data, "Unit", attr(data_trv, "Unit") |> attr("units") |> as.character())
  ncatt_put(nc_, var_time, "Time_Zone", tz(attr(data_trv, "Time")[1]))
  ncatt_put(nc_, var_geometry, "Extent", "x_min, x_max, y_min, y_max")
  ncatt_put(nc_, var_geometry, "EPSG", attr(data_trv, "Spat_crs"))
  ## global Attributes
  ncatt_put(nc_, 0, "Data_Type", "TimeRastVariable")
  ncatt_put(nc_, 0, "creat_time", Sys.time() |> as.character())

  if(!is.null(other_Attr)) write_nc_global_attr(nc_, other_Attr)

  nc_close(nc_)

}

#' @rdname write_tsd
#' @export
write_tsd.TimeRastArray <- function(data_, fn_, other_Attr = NULL) {
  data_tra <- data_
  dim_n <- dim(data_tra)
  rast_demo <- rast(data_tra[1,,,1] |> t(), crs = attr(data_tra, "Spat_crs"), extent = attr(data_tra, "Spat_extent"))
  x_rast <- xFromCol(rast_demo, 1:dim_n[2])
  y_rast <- yFromRow(rast_demo, 1:dim_n[3])
  width_Time <- map(attr(data_tra, "Time"), str_length) |> unlist() |> max()

  ## define the Dimensions
  dim_time <- ncdim_def("time", "",
                        1:dim_n[1], create_dimvar = FALSE,
                        longname = "Date or Datetime")
  dim_x <- ncdim_def("x", "",
                     x_rast,
                     longname = "[X] in projections or [Longitude] in geographic, define by the CRS (EPSG).")
  dim_y <- ncdim_def("y", "",
                     y_rast,
                     longname = "[Y] in projections or [Latitude] in geographic, define by the CRS (EPSG).")
  dim_vari <- ncdim_def("vari", "",
                        1:dim_n[4], create_dimvar = FALSE,
                        longname = "Name of variables")

  dim_time_width <- ncdim_def("time_width", "",
                              1:width_Time, create_dimvar = FALSE,
                              longname = "Year, Month, Day, Hour, Minute, Second")
  dim_extent <- ncdim_def("crs_extent", "",
                          1:4, create_dimvar = FALSE)

  ## define the data
  var_Data <- ncvar_def("Data", "", list(dim_time, dim_x, dim_y, dim_vari), -9999)
  var_time <- ncvar_def("Time", "", list(dim_time_width, dim_time), " ", prec = "char")
  var_geometry<- ncvar_def("Geometry", "", list(dim_extent), -9999, "Geometry with EPSG (CRS) and Extent")

  vars <- list(var_Data, var_time, var_geometry) #, var_name, var_unit)

  ## Create a new empty netcdf file
  nc_ <- nc_create(fn_, vars, TRUE)

  ## put the data
  data_unclass <- unclass(data_tra)

  for (i in 1:dim_n[4]) {
    ncvar_put(nc_, var_Data, data_unclass[,,,i], c(1,1,1,i), c(dim_n[1:3],1))
  }


  ncvar_put(nc_, var_time, attr(data_tra, "Time"))
  ncvar_put(nc_, var_geometry, attr(data_tra, "Spat_extent") |> num_ext())
  ## Varible Attributes
  ncatt_put(nc_, var_Data, "Name", paste0(attr(data_tra, "Name"), collapse = ","))
  ncatt_put(nc_, var_Data, "Unit", paste0(attr(data_tra, "Unit"), collapse = ","))
  ncatt_put(nc_, var_time, "Time_Zone", tz(attr(data_tra, "Time")[1]))
  ncatt_put(nc_, var_geometry, "EPSG", attr(data_tra, "Spat_crs"))
  ncatt_put(nc_, var_geometry, "Extent", "x_min, x_max, y_min, y_max")
  ## global Attributes
  ncatt_put(nc_, 0, "Data_Type", "TimeRastArray")
  ncatt_put(nc_, 0, "creat_time", Sys.time() |> as.character())

  if(!is.null(other_Attr)) write_nc_global_attr(nc_, other_Attr)

  nc_close(nc_)

}

#' @rdname write_tsd
#' @export
write_tsd.TimeRastLayerVariable <- function(data_, fn_, other_Attr = NULL) {
  data_trv <- data_
  dim_n <- dim(data_trv)
  rast_demo <- rast(data_trv[1,,] |> t(), crs = attr(data_trv, "Spat_crs"), extent = attr(data_trv, "Spat_extent"))
  x_rast <- xFromCol(rast_demo, 1:dim_n[2])
  y_rast <- yFromRow(rast_demo, 1:dim_n[3])
  width_Time <- map(attr(data_trv, "Time"), str_length) |> unlist() |> max()

  ## define the Dimensions
  dim_time <- ncdim_def("time", "",
                        1:dim_n[1], create_dimvar = FALSE,
                        longname = "Date or Datetime")
  dim_x <- ncdim_def("x", "",
                     x_rast,
                     longname = "[X] in projections or [Longitude] in geographic, define by the CRS (EPSG).")
  dim_y <- ncdim_def("y", "",
                     y_rast,
                     longname = "[Y] in projections or [Latitude] in geographic, define by the CRS (EPSG).")
  dim_layer <- ncdim_def("layer", "",
                         1:dim_n[4], create_dimvar = FALSE,
                         longname = "Layer")


  dim_time_width <- ncdim_def("time_width", "",
                              1:width_Time, create_dimvar = FALSE,
                              longname = "Year, Month, Day, Hour, Minute, Second")
  dim_extent <- ncdim_def("crs_extent", "",
                          1:4, create_dimvar = FALSE)
  ## define the data
  var_Data <- ncvar_def("Data", "", list(dim_time, dim_x, dim_y, dim_layer), -9999)
  var_time <- ncvar_def("Time", "", list(dim_time_width, dim_time), " ", prec = "char")
  var_geometry<- ncvar_def("Geometry", "", list(dim_extent), -9999, "Geometry with EPSG (CRS) and Extent")

  vars <- list(var_Data, var_time, var_geometry) #, var_name, var_unit)

  ## Create a new empty netcdf file
  nc_ <- nc_create(fn_, vars, TRUE)

  ## put the data
  ncvar_put(nc_, var_Data, unclass(data_trv))
  ncvar_put(nc_, var_time, attr(data_trv, "Time"))
  ncvar_put(nc_, var_geometry, attr(data_trv, "Spat_extent") |> num_ext())

  ## Varible Attributes
  ncatt_put(nc_, var_Data, "Name", attr(data_trv, "Name"))
  ncatt_put(nc_, var_Data, "Unit", attr(data_trv, "Unit") |> attr("units") |> as.character())
  ncatt_put(nc_, var_time, "Time_Zone", tz(attr(data_trv, "Time")[1]))
  ncatt_put(nc_, var_geometry, "Extent", "x_min, x_max, y_min, y_max")
  ncatt_put(nc_, var_geometry, "EPSG", attr(data_trv, "Spat_crs"))
  ## global Attributes
  ncatt_put(nc_, 0, "Data_Type", "TimeRastLayerVariable")
  ncatt_put(nc_, 0, "creat_time", Sys.time() |> as.character())

  if(!is.null(other_Attr)) write_nc_global_attr(nc_, other_Attr)

  nc_close(nc_)

}

#' @rdname write_tsd
#' @export
write_tsd.TimeRastLayerArray <- function(data_, fn_, other_Attr = NULL) {
  data_tra <- data_
  dim_n <- dim(data_tra)
  rast_demo <- rast(data_tra[1,,,1] |> t(), crs = attr(data_tra, "Spat_crs"), extent = attr(data_tra, "Spat_extent"))
  x_rast <- xFromCol(rast_demo, 1:dim_n[2])
  y_rast <- yFromRow(rast_demo, 1:dim_n[3])
  width_Time <- map(attr(data_tra, "Time"), str_length) |> unlist() |> max()

  ## define the Dimensions
  dim_time <- ncdim_def("time", "",
                        1:dim_n[1], create_dimvar = FALSE,
                        longname = "Date or Datetime")
  dim_x <- ncdim_def("x", "",
                     x_rast,
                     longname = "[X] in projections or [Longitude] in geographic, define by the CRS (EPSG).")
  dim_y <- ncdim_def("y", "",
                     y_rast,
                     longname = "[Y] in projections or [Latitude] in geographic, define by the CRS (EPSG).")
  dim_layer <- ncdim_def("layer", "",
                         1:dim_n[4], create_dimvar = FALSE,
                         longname = "Layer")
  dim_vari <- ncdim_def("vari", "",
                        1:dim_n[5], create_dimvar = FALSE,
                        longname = "Name of variables")

  dim_time_width <- ncdim_def("time_width", "",
                              1:width_Time, create_dimvar = FALSE,
                              longname = "Year, Month, Day, Hour, Minute, Second")
  dim_extent <- ncdim_def("crs_extent", "",
                          1:4, create_dimvar = FALSE)

  ## define the data
  var_Data <- ncvar_def("Data", "", list(dim_time, dim_x, dim_y, dim_layer, dim_vari), -9999)
  var_time <- ncvar_def("Time", "", list(dim_time_width, dim_time), " ", prec = "char")
  var_geometry<- ncvar_def("Geometry", "", list(dim_extent), -9999, "Geometry with EPSG (CRS) and Extent")

  vars <- list(var_Data, var_time, var_geometry) #, var_name, var_unit)

  ## Create a new empty netcdf file
  nc_ <- nc_create(fn_, vars, TRUE)

  ## put the data
  data_unclass <- unclass(data_tra)

  for (i in 1:dim_n[5]) {
    ncvar_put(nc_, var_Data, data_unclass[,,,,i], c(1,1,1,1,i), c(dim_n[1:4],1))
  }

  # ncvar_put(nc_, var_Data, unclass(data_tra))
  ncvar_put(nc_, var_time, attr(data_tra, "Time"))
  ncvar_put(nc_, var_geometry, attr(data_tra, "Spat_extent") |> num_ext())
  ## Varible Attributes
  ncatt_put(nc_, var_Data, "Name", paste0(attr(data_tra, "Name"), collapse = ","))
  ncatt_put(nc_, var_Data, "Unit", paste0(attr(data_tra, "Unit"), collapse = ","))
  ncatt_put(nc_, var_time, "Time_Zone", tz(attr(data_tra, "Time")[1]))
  ncatt_put(nc_, var_geometry, "EPSG", attr(data_tra, "Spat_crs"))
  ncatt_put(nc_, var_geometry, "Extent", "x_min, x_max, y_min, y_max")
  ## global Attributes
  ncatt_put(nc_, 0, "Data_Type", "TimeRastLayerArray")
  ncatt_put(nc_, 0, "creat_time", Sys.time() |> as.character())

  if(!is.null(other_Attr)) write_nc_global_attr(nc_, other_Attr)

  nc_close(nc_)

}

