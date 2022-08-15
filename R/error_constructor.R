#' @import rlang glue
check_dim_n_ary <- function(data_, n_dim_must) {
  dim_Data <- dim(data_)
  n_dim <- length(dim_Data)
  if (n_dim != n_dim_must) {
    abort(glue_col("`data_` must have {red {n_dim_must}} dimensions, not {blue {n_dim}}."), .literal = TRUE)
  }
}
#' @import rlang glue lubridate
check_dim_time <- function(data_, Time_) {
  if (!is.timepoint(Time_)) {
    abort(glue_col("The class of `Time_` must be {red 'Date'} or {red 'POSIXct'}. \nTry to use the functions {green lubridate::as_date()} or {green lubridate::as_datetime}."), .literal = TRUE)
  }
  dim_Data <- dim(data_)
  dim_time_ary <- dim_Data[1]
  n_time <- length(Time_)
  if (dim_time_ary != n_time) {
    abort(glue_col("Can't match the time-scale of `data_` [{red n = {dim_time_ary}}] with `Time_` [{red n = {n_time}}]."), .literal = TRUE)
  }
}

#' @import rlang glue units
check_unit <- function(Unit_) {
  tryCatch(
    as_units(Unit_),
    abort(
      glue_col(
        "{red '{Unit_}'} (in `Unit_`) can't be  convernt to a {blue units::unit}, try to check unit with the function {green units::as_unit()} and show more pre-defined units from UDUNITS2 with {green units::valid_udunits()} and {green units::valid_udunits_prefixes()}."
      ),
      .literal = TRUE
    )
  )

}
#' @import rlang glue units
check_dim_vari <- function(data_, Name_, Unit_) {

  if (any(duplicated(Name_))) {
    dp_name <- paste(Name_[duplicated(Name_)], collapse = ", ")
    abort(glue_col("`Name_` must be {blue uniqued}, but these {red {dp_name}} are repeated."), .literal = TRUE)
  }
  dim_Data <- dim(data_)
  n_dim <- length(dim_Data)
  dim_vari_ary <- dim_Data[n_dim]
  n_name <- length(Name_)
  n_unit <- length(Unit_)
  if (dim_vari_ary != n_name | dim_vari_ary != n_unit) {
    abort(glue_col("Can't match the variable-scale of `data_` [{red n = {dim_vari_ary}}] with `Name_` [{red n = {n_name}}] or `Unit_` [{red n = {n_unit}}]."), .literal = TRUE)
  }
  for (i in 1:n_unit) {
    unit_check <- Unit_[i]
    tryCatch(as_units(unit_check),
             abort(glue_col("{red '{unit_check}'} (in `Unit_`) can't be  convernt to a {blue units::unit}, try to check unit with the function {green units::as_unit()} and show more pre-defined units from UDUNITS2 with {green units::valid_udunits()} and {green units::valid_udunits_prefixes()}."), .literal = TRUE)
    )
  }
}
#' @import rlang glue
#' @importFrom methods isClass
check_dim_spat <- function(data_, Spat_ID, Spat_Data) {

  if(!(isClass(Spat_Data, "SpatVector") && "Spat_ID" %in% names(Spat_Data))) {
    abort(glue_col("The class of `Spat_Data` must be {red 'terra::SpatVector'}, please use the function {green terra::vect()}. \nAnd the `Spat_Data` must contian one Attribute named as {red 'Spat_ID'}, which match with `Spat_ID`."), .literal = TRUE)
  }
  if (any(duplicated(Spat_ID))) {
    dp_name <- paste(Spat_ID[duplicated(Spat_ID)], collapse = ", ")
    abort(glue_col("`Spat_ID` must be {blue uniqued}, but these {red {dp_name}} are repeated."), .literal = TRUE)
  }
  if (any(duplicated(Spat_Data$Spat_ID))) {
    dp_name <- paste(Spat_Data$Spat_ID[duplicated(Spat_Data$Spat_ID)], collapse = ", ")
    abort(glue_col("`Spat_Data$Spat_ID` must be {blue uniqued}, but these {red {dp_name}} are repeated."), .literal = TRUE)
  }

  if(!all(Spat_ID %in% Spat_Data$Spat_ID)) {
    abort(glue_col("All of the names in `Spat_ID` must be contianed in 'Spat_ID' of `Spat_Data`."), .literal = TRUE)
  }

}





#' @import rlang glue
check_extent_rast <- function(data_, Spat_EPSG, Spat_extent) {

  if(!is.numeric(Spat_EPSG)) {
    abort(glue_col("`Spat_EPSG` must be a {blue integer, e.g. 31468}."))
  }

  dim_Data <- dim(data_)
  res_x <- (Spat_extent[2] - Spat_extent[1]) / dim_Data[2]
  res_y <- (Spat_extent[4] - Spat_extent[3]) / dim_Data[3]
  inform(glue_col("The reselutions are:\n{blue x: {res_x}}\n{blue y: {res_y}}\n{red They will not be checked, please make sure they are correct}."))

}

#' @import rlang glue
check_SpatRaster <- function(data_, n_layer_should) {
  if(crs(data_) == "") {
    abort(glue_col("`data_` must have the {red CRS} (Coordinate Reference System)."))
  }
  ## dim
  n_layer <- nlyr(data_)
  if (n_layer != n_layer_should) {
    abort(
      glue_col("Please make sure that the number of {read `Time_` * `Layer` * `Variable`: {n_layer_should}} \nis matching the number of layers of {blue raster data `data_`: {n_layer}}.")
    )
  }

}
#' @import rlang glue
check_na <- function(data_, na_check) {
  if (any(is.na(data_))) {
    if (na_check) {
      abort(glue_col("NA founded in `data_`."))
    } else {
      warn("There are some NAs in the `data_`, if nessay please check and fix them.")
    }
  }

}
