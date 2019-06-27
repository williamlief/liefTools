#' Turn variable into vector of dummy variables
#'
#' @name dummify
#'
#' @description Takes any variable and splits into separate variables by unique
#' value, with a binary indicator (0/1) for each. Uses stats::model.matrix to
#' perform the dummifying. By default, all new dummy variables have the name
#' var_'unique_value' where var_ is the supplied variable name and unique_value
#' is the value for which the indicator is 1. Set 'prefix = FALSE' to remove
#' var_ from the new variable name. By default, this function errors when var
#' has more than 20 distinct values. Set 'force = TRUE' to dummify anyway.
#' TODO: Check how missing values are handled. Allow ... list of variable input
#'
#' @param df a tbl
#' @param var variable to dummify (will be coerced to character before dummifying)
#' @param prefix (default = TRUE) prepend var name to dummy variable names?
#' @param force (default = FALSE) allow creation of more than 20 dummies?
#'
#' @return the original tbl with dummy variables added on the end.
#' @export
#'
#' @examples
#' dummify(iris, Species)
dummify <- function(df, var, prefix = TRUE, force = FALSE) {
  var <- rlang::enquo(var)

  # Create prefix to prepend to dummy vars
  prefix <- ifelse(prefix,
                   paste0(rlang::quo_name(var), "_"),
                   "")

  # Pull out the variable we want to dummify
  tmp_ <- df %>% dplyr::pull(!!var) %>% as.character()

  # Check that there won't be a ridiculous number of dummies created
  n_level <- dplyr::n_distinct(tmp_)
  if(!force & n_level > 20) {
    stop(paste("The variable you are trying to dummify has", n_level, "distinct values.
         Are you sure you want to create", n_level, "dummy variables?
         Set `force = TRUE` to proceed."))
  }


  dums <- stats::model.matrix(~tmp_ -1) %>%
    dplyr::as_tibble() %>%
             dplyr::rename_all(., .funs = list(~sub("tmp_", prefix, .)))

  return(dplyr::bind_cols(df, dums))
}


