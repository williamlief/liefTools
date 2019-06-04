
#' Sample fraction of groups
#'
#'@name sample_frac_g
#' @description using group_by with sample_frac returns a sample of the observations
#' within a group. This function instead returns all observations for a fraction
#' of groups.
#'
#' @param tbl tbl of data
#' @param group grouping variable
#' @param size the fraction of groups to select
#' @param replace sample with or without replacement
#' @param weight sampling weights // NOT SUPPORTED
#'
#' @export
#'
#' @examples
#' sample_frac_g(airquality, Month, .3)
sample_frac_g <- function(tbl, group,
                          size = 1, replace = FALSE, weight = NULL) {

    group <- rlang::enquo(group)
    liefTools::check_vars_exist(tbl, !!group)


    sample <-
      dplyr::select(tbl, !!group) %>%
      dplyr::distinct() %>%
      dplyr::sample_frac(size = !!size, replace = !!replace, weight = !!weight) %>%
      dplyr::left_join(tbl)

    return(sample)
}
