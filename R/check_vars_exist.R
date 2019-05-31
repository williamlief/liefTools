
#' check variable existence in data set
#'
#' @description given a list of variables and a data set, checks if those variables
#' exist and throws an error stating 'variable x not found' for whichever ones
#' don't. I got fed up with opaque error messages caused by mistyped variable
#' names. If you are nesting this inside another function and using tidy style
#' remember to unquote (!!) the variables that you pass in. The function returns
#' nothing if the variables exist.
#'
#' @param df a tibble
#' @param ... list of variables to confirm existence of.
#'
#' @return Silent or Error
#' @export
#'
#' @examples
#' check_vars_exist(df = airquality, Ozone, Month)
check_vars_exist <- function(df, ...) {

  vars <- rlang::enquos(...)

  for(var in vars) {
    var <- rlang::as_name(var)
    if (!(var %in% colnames(df))) {
      stop(paste0("variable ", var, " not found"), call. = FALSE)
    }
  }
}

