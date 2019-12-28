

#' Discover which values are coerced to NA in as.numeric
#'
#' @name whichNA
#'
#' @description Helps track down which values are introducing NA's in the as.numeric
#' function. Can be run on a vector or on a dataframe. In the later case it can
#' only check one variable at a time, and will return all columns for the rows
#' that have the na coercion warning. I find this dataframe view helpful in diagnosing the
#' problem.
#'
#' When to use it? After seeing, "Warning message: NAs introduced by coercion"
#' Run this function on your data and variable to find out exactly which rows are
#' causing the issue. This is a warning that should not be ignored. Nothing is worse
#' than discovering that all the rows greater than 999 had commas (1,000) and are now
#' NA after doing a bunch of analysis.
#'
#' @param x a vector or a variable name in quotes
#' @param df optional, a data frame
#'
#'
#' @return a data frame with all rows containing non valid numeric values
#' @export
#'
#' @examples
#' x <- c("a", 2, 3, "d")
#' whichNA(x)
#'
#' df <- data.frame(x = c("a", 2, 3, "d"),
#'                  y = c(6, 7, 8, 9),
#'                  z = c("f", "g", "h", "i"),
#'                  stringsAsFactors = FALSE)
#' whichNA("x", df)

whichNA <- function(x, df = NULL) {
  # vector
  if(is.null(df)) {
    na <- which(is.na(as.numeric(x)) != is.na(x))
    return(x[na])
    # dataframe
  } else {
    na <- which(is.na(as.numeric(df[[x]])) != is.na(df[[x]]))
    return(df[na,])
  }
}
