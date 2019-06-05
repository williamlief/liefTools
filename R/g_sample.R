
#' Sample groups from a table
#'
#'@name g_sample
#' @description using group_by with sample returns a sample of the observations
#' within a group. This function instead returns all observations for a sample
#' of groups.
#'
#' @param tbl tbl of data
#' @param group grouping variable
#' @param ...
#'  @inheritParams dplyr::sample_frac
#' @seealso See \code{\link[dplyr]{sample}} for details on sample function.
#' @examples
#' g_sample_frac(airquality, Month, .3)
#' g_sample_n(airquality, Month, 3)
NULL

## NULL

#' @rdname g_sample
#' @export
g_sample_frac <- function(tbl, group, ...) {

    group <- rlang::enquo(group)
    liefTools::check_vars_exist(tbl, !!group)


    sample <-
      dplyr::select(tbl, !!group) %>%
      dplyr::distinct() %>%
      dplyr::sample_frac(...) %>%
      dplyr::left_join(tbl)

    return(sample)
}



#' @rdname g_sample
#' @export
g_sample_n <- function(tbl, group, ...) {

  group <- rlang::enquo(group)
  liefTools::check_vars_exist(tbl, !!group)

  sample <-
    dplyr::select(tbl, !!group) %>%
    dplyr::distinct() %>%
    dplyr::sample_n(...) %>%
    dplyr::left_join(tbl)

  return(sample)
}
