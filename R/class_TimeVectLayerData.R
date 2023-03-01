#' create `TimeVectLayerVariable` and `TimeVectLayerArray` data
#' @name TimeVectLayerData
#' @description
#' - `TimeVectLayerVariable` is a data class that is based on a 3D array. It has two "macro" dimensions: time and spatial.
#' The spatial dimension also holds the geological vector data (points, lines, polygons) that is saved in the `Spat_Data`.
#' But due to the vertical **layer** in spatial dimension,
#' the data class actually has three data-dimensions: **(time, spat, layer)**.
#'
#' - `TimeVectLayerArray` is a data class that is based on a 4D array. It has three dimensions: time, spatial, and variable.
#' The spatial dimension also holds the geological vector data (points, lines, polygons) that is saved in the `Spat_Data`.
#' But due to the vertical **layer** in spatial dimension,
#' the data class actually has four data-dimensions: **(time, spat, layer, variable)**.
#' @param data_ (num-array) 3D for `TimeVectLayerVariable` and 4D for `TimeVectLayerArray`
#' @param Name_,Unit_ (char or vector of char) name and unit of Variable, `Unit_` should be converted by [units::as_units()]
#' @param Time_ (vector of lubridate::timepoint) time dimension, created by [lubridate::as_date()] or [lubridate::as_datetime()]
#' @param Spat_ID (vector of char) the identifying of the spatial-dimension, they must be contained in the `Spat_Data`
#' @param Spat_Data (terra::SpatVector) geological data, create by [terra::vect()]
#' @param na_check (bool) if check the NAs
#' @importFrom units as_units
#' @importFrom terra wrap
#' @export
new_TimeVectLayerVariable <- function(data_, Name_, Unit_, Time_, Spat_ID, Spat_Data, na_check = FALSE) { #


  ## dim
  check_dim_n_ary(data_, 3)

  ## time
  check_dim_time(data_, Time_)

  ## spat
  check_dim_spat(Spat_ID, Spat_Data)

  ## vari
  check_unit(Unit_)


  ## NAs
  check_na(data_, na_check)



  TimeVectLayerVariable <- structure(
    data_,
    class = c("TimeVectLayerVariable", "array"),
    dimnames = list(as.character(Time_), Spat_ID, NULL),
    Name = Name_,
    Unit = as_units(Unit_),
    Time = Time_,
    Spat_ID = Spat_ID,
    Spat_Data = Spat_Data |> wrap()
  )
}

#' @rdname TimeVectLayerData
#' @export
new_TimeVectLayerArray <- function(data_, Name_, Unit_, Time_, Spat_ID, Spat_Data, na_check = FALSE) { #
  ## dim
  check_dim_n_ary(data_, 4)

  ## time
  check_dim_time(data_, Time_)

  ## spat
  check_dim_spat(Spat_ID, Spat_Data)

  ## vari
  check_dim_vari(data_, Name_, Unit_)

  ## NA
  check_na(data_, na_check)

  names(Unit_) <- Name_

  TimeVectLayerArray <- structure(
    data_,
    class = c("TimeVectLayerArray", "array"),
    dimnames = list(as.character(Time_), Spat_ID, NULL, Name_),
    Name = Name_,
    Unit = Unit_,
    Time = Time_,
    Spat_ID = Spat_ID,
    Spat_Data = Spat_Data |> wrap()
  )
}




