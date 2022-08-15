#' read TimeSpatData
#' @description read `TimeSpatData` from nc-file, that writed by [write_tsd()],
#' or write by other tools that match the `TimeSpatData` convention.
#' @param fn_ (char) filenames
#' @param start,count (num) start and count od the data block, more see function [ncdf4::ncvar_get()]
#' @import ncdf4
#' @importFrom terra vect
#' @importFrom stringr str_split
#' @return `TimeSpatData` data
#' - TimeVectVariable
#' - TimeVectArray
#' - TimeRastVariable
#' - TimeRastArray
#' - TimeRastLayerVariable
#' - TimeRastLayerArray
#' @export
read_tsd <- function(fn_, start = NA, count = NA) {

  nc_ <- nc_open(fn_)
  # nc_
  data_nc <- ncvar_get(nc_, "Data", start, count)
  data_type <- ncatt_get(nc_, 0, "Data_Type")$value
  Time_nc <- ncvar_get(nc_, "Time") |> as_datetime()
  switch(data_type,
         TimeVectVariable = {
           Spat_Data <- vect(ncvar_get(nc_, "Spat_Geom"),
                             ncatt_get(nc_, "Spat_Geom", "Shape")$value,
                             data.frame(Spat_ID = ncvar_get(nc_, "Spat_Geom_ID")),
                             ncatt_get(nc_, "Spat_Geom", "EPSG")$value)
           data_ <- new_TimeVectVariable(data_nc,
                                         ncatt_get(nc_, "Data", "Name")$value,
                                         ncatt_get(nc_, "Data", "Unit")$value,
                                         Time_nc,
                                         ncvar_get(nc_, "Spat_ID"),
                                         Spat_Data)
         },
         TimeVectArray = {
           Spat_Data <- vect(ncvar_get(nc_, "Spat_Geom"),
                             ncatt_get(nc_, "Spat_Geom", "Shape")$value,
                             data.frame(Spat_ID = ncvar_get(nc_, "Spat_Geom_ID")),
                             ncatt_get(nc_, "Spat_Geom", "EPSG")$value)
           data_ <- new_TimeVectArray(data_nc,
                                      ncatt_get(nc_, "Data", "Name")$value |> str_split(",") |> unlist(),
                                      ncatt_get(nc_, "Data", "Unit")$value |> str_split(",") |> unlist(),
                                      Time_nc,
                                      ncvar_get(nc_, "Spat_ID"),
                                      Spat_Data)
         },
         TimeRastVariable = {
           data_ <- new_TimeRastVariable(data_nc,
                                         ncatt_get(nc_, "Data", "Name")$value,
                                         ncatt_get(nc_, "Data", "Unit")$value,
                                         Time_nc,
                                         ncatt_get(nc_, "Geometry", "EPSG")$value,
                                         ncvar_get(nc_, "Geometry") |> as.numeric())

         },
         TimeRastArray = {
           data_ <- new_TimeRastArray(data_nc,
                                      ncatt_get(nc_, "Data", "Name")$value |> str_split(",") |> unlist(),
                                      ncatt_get(nc_, "Data", "Unit")$value |> str_split(",") |> unlist(),
                                      Time_nc,
                                      ncatt_get(nc_, "Geometry", "EPSG")$value,
                                      ncvar_get(nc_, "Geometry") |> as.numeric())

         },
         TimeRastLayerVariable = {
           data_ <- new_TimeRastLayerVariable(data_nc,
                                         ncatt_get(nc_, "Data", "Name")$value,
                                         ncatt_get(nc_, "Data", "Unit")$value,
                                         Time_nc,
                                         ncatt_get(nc_, "Geometry", "EPSG")$value,
                                         ncvar_get(nc_, "Geometry") |> as.numeric())

         },
         TimeRastLayerArray = {
           data_ <- new_TimeRastLayerArray(data_nc,
                                      ncatt_get(nc_, "Data", "Name")$value |> str_split(",") |> unlist(),
                                      ncatt_get(nc_, "Data", "Unit")$value |> str_split(",") |> unlist(),
                                      Time_nc,
                                      ncatt_get(nc_, "Geometry", "EPSG")$value,
                                      ncvar_get(nc_, "Geometry") |> as.numeric())

         })

  nc_close(nc_)
  data_
}


