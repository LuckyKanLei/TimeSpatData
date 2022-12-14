% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_TimeRastData.R
\name{TimeRastData}
\alias{TimeRastData}
\alias{new_TimeRastVariable}
\alias{new_TimeRastVariable.SpatRaster}
\alias{new_TimeRastVariable.array}
\alias{new_TimeRastArray}
\alias{new_TimeRastArray.SpatRaster}
\alias{new_TimeRastArray.array}
\title{create \code{TimeRastVariable} and \code{TimeRastArray} data}
\usage{
new_TimeRastVariable(data_, Name_, Unit_, Time_, ...)

\method{new_TimeRastVariable}{SpatRaster}(data_, Name_, Unit_, Time_, ...)

\method{new_TimeRastVariable}{array}(data_, Name_, Unit_, Time_, Spat_EPSG, Spat_extent, ...)

new_TimeRastArray(data_, Name_, Unit_, Time_, ...)

\method{new_TimeRastArray}{SpatRaster}(data_, Name_, Unit_, Time_, dim_Time_Vari = c(1, 2), ...)

\method{new_TimeRastArray}{array}(data_, Name_, Unit_, Time_, Spat_EPSG, Spat_extent, ...)
}
\arguments{
\item{data_}{(num-array or terra::SpatRaster)
\itemize{
\item 3D (array) for \code{\link[=new_TimeRastVariable.array]{new_TimeRastVariable.array()}}, it must in the \strong{(time, x, y)} dimension-order, use the function \code{\link[=aperm]{aperm()}} to adjust the dimension-order
\item 4D (array) for \code{\link[=new_TimeRastArray.array]{new_TimeRastArray.array()}}, it must in the \strong{(time, x, y, variable)} dimension-order, use the function \code{\link[=aperm]{aperm()}} to adjust the dimension-order
\item (terra::SpatRaster) (multilayer) for \code{\link[=new_TimeRastVariable.SpatRaster]{new_TimeRastVariable.SpatRaster()}} and \code{\link[=new_TimeRastArray.SpatRaster]{new_TimeRastArray.SpatRaster()}}, created by \code{\link[terra:rast]{terra::rast()}}
}}

\item{Name_, Unit_}{(char or vector of char) name and unit of Variable, \code{Unit_} should be  \code{\link[units:units]{units::as_units()}}}

\item{Time_}{(vector of lubridate::timepoint) time dimension, created by \code{\link[lubridate:as_date]{lubridate::as_date()}} or \code{\link[lubridate:as_date]{lubridate::as_datetime()}}}

\item{...}{other parameters}

\item{Spat_EPSG}{(integer) EPSG code of the CRS (Coordinate Reference System)}

\item{Spat_extent}{(vector of numric), extension of the Raster in c(xmin, xmax, ymin, ymax),
it can be projected east- and north-meter or global longitude- and latitude-degree}

\item{dim_Time_Vari}{the order of time- and variable-dimenion in the layer of \code{terra::rast},
it must be
\itemize{
\item \code{c(1, 2)}: first time then variable or
\item \code{c(2, 1)}: first variable then time,
\item default is \code{c(1, 2)}
}}
}
\value{
\code{TimeRastVariable}, \code{TimeRastArray} data
}
\description{
\itemize{
\item \code{TimeRastVariable} is a data class that is based on a 3D array.
It has two "macro" dimensions, time and spatial.
But due to the raster spatial has two dimension,
the data class actually has three data-dimensions: \strong{(time, x, y)}.
\item \code{TimeRastArray} is a data class that is based on a 4D array.
It has three "macro" dimensions: time, spatial, and variable.
But due to the raster spatial has two dimension,
the data class actually has four data-dimensions: \strong{(time, x, y, variable)}.
}
}
