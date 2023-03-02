#' summarize TimeSpatData in Time-Dimension
#' @description
#'
#' - [summary_time_tsd] summarize the Time-Dimension in fixed time interval like yearly, monthly and so on
#' - [summary_index_tsd] summarize the Time-Dimension in flexibel time interval, use the parameter `idx_End` to divide the interval
#'
#' @name summary_time_tsd
#' @param tsd_Data `TimeSpatData` data
#' - `TimeVectVariable`
#' - `TimeVectArray`
#' - `TimeRastVariable`
#' - `TimeRastArray`
#' @param str_Interval (string) <“microseconds”, “milliseconds”, “seconds”, “minutes”, “hours”, “days”, “weeks”, “months”, “quarters” and “years”> time interval
#' @param smry_Funct (function) function that used for summary, e.g. [mean], [max] and [min], it can also be the self-defined function
#' @param ... other parameters for `smry_Funct`
#' @importFrom terra intersect rast crop res mask expanse size as.polygons values
#' @importFrom xts endpoints
#' @return summarized time series with the same data structure like `tsd_Data`
#' @export
summary_time_tsd <- function(tsd_Data, str_Interval = "years", smry_Funct = mean, ...) {

  dim_n <- dim(tsd_Data)
  n_Dim <- length(dim_n)
  Time_ <- attr(tsd_Data, "Time")
  INDEX <- endpoints(Time_, str_Interval)

  switch (paste0("C", n_Dim),
    C2 = {  ary_New <- sapply(1:(length(INDEX) - 1), function(y) {
      apply(tsd_Data[(INDEX[y] + 1):INDEX[y + 1],], 2:n_Dim, smry_Funct, ...)
    }, simplify  = "array")
    },
    C3 = {  ary_New <- sapply(1:(length(INDEX) - 1), function(y) {
      apply(tsd_Data[(INDEX[y] + 1):INDEX[y + 1],,], 2:n_Dim, smry_Funct, ...)
    }, simplify  = "array")
    },
    C4 = {  ary_New <- sapply(1:(length(INDEX) - 1), function(y) {
      apply(tsd_Data[(INDEX[y] + 1):INDEX[y + 1],,,], 2:n_Dim, smry_Funct, ...)
    }, simplify  = "array")
    },
    C5 = {  ary_New <- sapply(1:(length(INDEX) - 1), function(y) {
      apply(tsd_Data[(INDEX[y] + 1):INDEX[y + 1],,,,], 2:n_Dim, smry_Funct, ...)
    }, simplify  = "array")
    },
    stop("Enter something that switches me!")
  )


  ary_New <- aperm(ary_New, c(n_Dim, 1:(n_Dim - 1)))

  tsd_Attr <- attributes(tsd_Data)
  tsd_Attr$dim <- dim(ary_New)
  tsd_Attr$dimnames[[1]] <- tsd_Attr$dimnames[[1]][INDEX[-1]]
  tsd_Attr$Time <- tsd_Attr$Time[INDEX[-1]]
  attributes(ary_New) <- tsd_Attr
  ary_New
}

#' @rdname summary_time_tsd
#' @param idx_End (integer) index of the end points every interval
#' @export
summary_index_tsd <- function(tsd_Data, idx_End, smry_Funct = mean, ...) {

  dim_n <- dim(tsd_Data)
  n_Dim <- length(dim_n)
  INDEX <- c(0, idx_End)

  switch (paste0("C", n_Dim),
          C2 = {  ary_New <- sapply(1:(length(INDEX) - 1), function(y) {
            apply(tsd_Data[(INDEX[y] + 1):INDEX[y + 1],], 2:n_Dim, smry_Funct, ...)
          }, simplify  = "array")
          },
          C3 = {  ary_New <- sapply(1:(length(INDEX) - 1), function(y) {
            apply(tsd_Data[(INDEX[y] + 1):INDEX[y + 1],,], 2:n_Dim, smry_Funct, ...)
          }, simplify  = "array")
          },
          C4 = {  ary_New <- sapply(1:(length(INDEX) - 1), function(y) {
            apply(tsd_Data[(INDEX[y] + 1):INDEX[y + 1],,,], 2:n_Dim, smry_Funct, ...)
          }, simplify  = "array")
          },
          C5 = {  ary_New <- sapply(1:(length(INDEX) - 1), function(y) {
            apply(tsd_Data[(INDEX[y] + 1):INDEX[y + 1],,,,], 2:n_Dim, smry_Funct, ...)
          }, simplify  = "array")
          },
          stop("Enter something that switches me!")
  )


  ary_New <- aperm(ary_New, c(n_Dim, 1:(n_Dim - 1)))

  tsd_Attr <- attributes(tsd_Data)
  tsd_Attr$dim <- dim(ary_New)
  tsd_Attr$dimnames[[1]] <- tsd_Attr$dimnames[[1]][INDEX[-1]]
  tsd_Attr$Time <- tsd_Attr$Time[INDEX[-1]]
  attributes(ary_New) <- tsd_Attr
  ary_New
}









