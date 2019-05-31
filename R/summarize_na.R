#' Summary of NA by variable
#'
#' @name summarize_na
#' @description reports percent of missing data for all variables in data frame
#'
#' @param df a tibble
#' @param group (optional) single grouping variable. If used, pct missing by
#' group will be spread across columns.
#' @param round (optional) specify number of digits to round na pcts to. Default is
#' no rounding.
#'
#' @return a tibble with one row per variable in original datset, values are pct na.
#' @export
#'
#' @examples
#'  summarize_na(df = airquality)
#'  summarize_na(df = airquality, group = Month)
#'  summarize_na(df = airquality, group = Month, round = 3)
summarize_na <- function(df, group = NULL, round = NULL) {

  if(rlang::quo_is_null(rlang::enquo(group))) {

    miss <- df %>%
      dplyr::summarize_all(list(~mean(is.na(.)))) %>%
      tidyr::gather(key = "var", value = "pct_na")

  } else {

    group <- rlang::enquo(group)
    var <- rlang::as_name(group)
    if (!(var %in% colnames(df))) {
      stop(paste0("variable ", var, " not found"))
    }

    miss <- df %>%
      dplyr::group_by(!!group) %>%
      dplyr::summarize_all(list(~mean(is.na(.)))) %>%
      tidyr::gather(key = "var", value = "pct_na", -!!group) %>%
      tidyr::spread(key = !!group, value = pct_na)

  }

  if(!is.null(round)) {
    miss <- miss %>%
      dplyr::mutate_if(is.numeric, base::round, round)
  }

  return(miss)
}
utils::globalVariables(c("pct_na"))
