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
  judge_count <- (!is.na(start) && !is.na(count))
  if(judge_count) Time_nc <- Time_nc[start[1]:count[1]]
  switch(data_type,
         TimeVectVariable = {
           Spat_Data <- vect(ncvar_get(nc_, "Spat_Geom"),
                             ncatt_get(nc_, "Spat_Geom", "Shape")$value,
                             data.frame(Spat_ID = ncvar_get(nc_, "Spat_Geom_ID")),
                             ncatt_get(nc_, "Spat_Geom", "EPSG")$value)
           if(judge_count) {
             Spat_ID <- ncvar_get(nc_, "Spat_ID")[start[2]:count[2]]
             data_ <- new_TimeVectVariable(data_nc,
                                           ncatt_get(nc_, "Data", "Name")$value,
                                           ncatt_get(nc_, "Data", "Unit")$value,
                                           Time_nc,
                                           Spat_ID,
                                           Spat_Data[match(Spat_ID, Spat_Data$Spat_ID),])
           } else {
             data_ <- new_TimeVectVariable(data_nc,
                                           ncatt_get(nc_, "Data", "Name")$value,
                                           ncatt_get(nc_, "Data", "Unit")$value,
                                           Time_nc,
                                           ncvar_get(nc_, "Spat_ID"),
                                           Spat_Data)
           }
         },
         TimeVectArray = {
           Spat_Data <- vect(ncvar_get(nc_, "Spat_Geom"),
                             ncatt_get(nc_, "Spat_Geom", "Shape")$value,
                             data.frame(Spat_ID = ncvar_get(nc_, "Spat_Geom_ID")),
                             ncatt_get(nc_, "Spat_Geom", "EPSG")$value)
           if(judge_count) {
             Spat_ID <- ncvar_get(nc_, "Spat_ID")[start[2]:count[2]]
             data_ <- new_TimeVectArray(data_nc,
                                        (ncatt_get(nc_, "Data", "Name")$value |> str_split(",") |> unlist())[start[3]:count[3]],
                                        (ncatt_get(nc_, "Data", "Unit")$value |> str_split(",") |> unlist())[start[3]:count[3]],
                                        Time_nc,
                                        Spat_ID,
                                        Spat_Data[match(Spat_ID, Spat_Data$Spat_ID),])
           } else {
             data_ <- new_TimeVectArray(data_nc,
                                        ncatt_get(nc_, "Data", "Name")$value |> str_split(",") |> unlist(),
                                        ncatt_get(nc_, "Data", "Unit")$value |> str_split(",") |> unlist(),
                                        Time_nc,
                                        ncvar_get(nc_, "Spat_ID"),
                                        Spat_Data)
           }

         },
         TimeRastVariable = {
           if(judge_count) {
             ori_extent <- ncvar_get(nc_, "Geometry") |> as.numeric()
             x_res <- (ori_extent[2] - ori_extent[1]) / nc_$dim$x$len
             y_res <- (ori_extent[4] - ori_extent[3]) / nc_$dim$y$len
             new_extent <- c(ori_extent[1] + start[2] * x_res,
                             ori_extent[1] + (count[2] + start[2]) * x_res,
                             ori_extent[3] + start[3] * y_res,
                             ori_extent[3] + (count[3] + start[3]) * y_res)
             data_ <- new_TimeRastVariable(data_nc,
                                           ncatt_get(nc_, "Data", "Name")$value,
                                           ncatt_get(nc_, "Data", "Unit")$value,
                                           Time_nc,
                                           ncatt_get(nc_, "Geometry", "EPSG")$value,
                                           new_extent)
           }else {
             data_ <- new_TimeRastVariable(data_nc,
                                           ncatt_get(nc_, "Data", "Name")$value,
                                           ncatt_get(nc_, "Data", "Unit")$value,
                                           Time_nc,
                                           ncatt_get(nc_, "Geometry", "EPSG")$value,
                                           ncvar_get(nc_, "Geometry") |> as.numeric())
           }


         },
         TimeRastArray = {

           if(judge_count) {
             ori_extent <- ncvar_get(nc_, "Geometry") |> as.numeric()
             x_res <- (ori_extent[2] - ori_extent[1]) / nc_$dim$x$len
             y_res <- (ori_extent[4] - ori_extent[3]) / nc_$dim$y$len
             new_extent <- c(ori_extent[1] + start[2] * x_res,
                             ori_extent[1] + (count[2] + start[2]) * x_res,
                             ori_extent[3] + start[3] * y_res,
                             ori_extent[3] + (count[3] + start[3]) * y_res)
             data_ <- new_TimeRastArray(data_nc,
                                        (ncatt_get(nc_, "Data", "Name")$value |> str_split(",") |> unlist())[start[4]:count[4]],
                                        (ncatt_get(nc_, "Data", "Unit")$value |> str_split(",") |> unlist())[start[4]:count[4]],
                                        Time_nc,
                                        ncatt_get(nc_, "Geometry", "EPSG")$value,
                                        new_extent)
           }else {
             data_ <- new_TimeRastArray(data_nc,
                                        ncatt_get(nc_, "Data", "Name")$value |> str_split(",") |> unlist(),
                                        ncatt_get(nc_, "Data", "Unit")$value |> str_split(",") |> unlist(),
                                        Time_nc,
                                        ncatt_get(nc_, "Geometry", "EPSG")$value,
                                        ncvar_get(nc_, "Geometry") |> as.numeric())
           }





         },
         TimeRastLayerVariable = {
           if(judge_count) {
             ori_extent <- ncvar_get(nc_, "Geometry") |> as.numeric()
             x_res <- (ori_extent[2] - ori_extent[1]) / nc_$dim$x$len
             y_res <- (ori_extent[4] - ori_extent[3]) / nc_$dim$y$len
             new_extent <- c(ori_extent[1] + start[2] * x_res,
                             ori_extent[1] + (count[2] + start[2]) * x_res,
                             ori_extent[3] + start[3] * y_res,
                             ori_extent[3] + (count[3] + start[3]) * y_res)

             data_ <- new_TimeRastLayerVariable(data_nc,
                                                ncatt_get(nc_, "Data", "Name")$value,
                                                ncatt_get(nc_, "Data", "Unit")$value,
                                                Time_nc,
                                                ncatt_get(nc_, "Geometry", "EPSG")$value,
                                                new_extent)
           } else {
             data_ <- new_TimeRastLayerVariable(data_nc,
                                                ncatt_get(nc_, "Data", "Name")$value,
                                                ncatt_get(nc_, "Data", "Unit")$value,
                                                Time_nc,
                                                ncatt_get(nc_, "Geometry", "EPSG")$value,
                                                ncvar_get(nc_, "Geometry") |> as.numeric())
           }


         },
         TimeRastLayerArray = {
           if(judge_count) {
             ori_extent <- ncvar_get(nc_, "Geometry") |> as.numeric()
             x_res <- (ori_extent[2] - ori_extent[1]) / nc_$dim$x$len
             y_res <- (ori_extent[4] - ori_extent[3]) / nc_$dim$y$len
             new_extent <- c(ori_extent[1] + start[2] * x_res,
                             ori_extent[1] + (count[2] + start[2]) * x_res,
                             ori_extent[3] + start[3] * y_res,
                             ori_extent[3] + (count[3] + start[3]) * y_res)

             data_ <- new_TimeRastLayerArray(data_nc,
                                             ncatt_get(nc_, "Data", "Name")$value |> str_split(",") |> unlist(),
                                             ncatt_get(nc_, "Data", "Unit")$value |> str_split(",") |> unlist(),
                                             Time_nc,
                                             ncatt_get(nc_, "Geometry", "EPSG")$value,
                                             new_extent)
           } else {
             data_ <- new_TimeRastLayerArray(data_nc,
                                             ncatt_get(nc_, "Data", "Name")$value |> str_split(",") |> unlist(),
                                             ncatt_get(nc_, "Data", "Unit")$value |> str_split(",") |> unlist(),
                                             Time_nc,
                                             ncatt_get(nc_, "Geometry", "EPSG")$value,
                                             ncvar_get(nc_, "Geometry") |> as.numeric())
           }


         })

  nc_close(nc_)
  data_
}


