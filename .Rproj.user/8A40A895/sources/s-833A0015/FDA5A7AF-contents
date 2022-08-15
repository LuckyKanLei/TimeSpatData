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
  num_epsg <- str_extract(crs_, "(?<=\\n    ID\\[\\\"EPSG\\\",)[:digit:]+(?=\\]\\]$)")
  return(paste0("EPSG:", num_epsg))
}
#' @importFrom utils read.csv
#' @importFrom stringr str_extract
num_ext <- function(ext_) {
  num_csv <- as.character(ext_) |> str_extract("(?<=ext\\().+(?=\\))")
  return(read.csv(text = num_csv, header = F) |> as.numeric())
}

