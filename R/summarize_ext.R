# extenstions to summarize for looking at whole datasets quickly
utils::globalVariables(c("pct_na", "mean"))

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
    liefTools::check_vars_exist(df, !!group)

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



#' Summary of mean by variable
#'
#' @name summarize_mean
#' @description reports mean value for all numeric variables in a data frame
#'
#' @param df a tibble
#' @param group (optional) single grouping variable. If used, mean by
#' group will be spread across columns.
#' @param round (optional) specify number of digits to round means to. Default is
#' no rounding.
#' @param na.rm (optional) a logical value indicating whether NA values should be
#'  stripped before the computation proceeds. Unlike base, here I default to TRUE
#'
#' @return a tibble with one row per variable in original datset, values are mean
#' @export
#'
#' @examples
#'  summarize_mean(df = airquality)
#'  summarize_mean(df = airquality, group = Month)
#'  summarize_mean(df = airquality, group = Month, round = 3)
#'  summarize_mean(df = airquality, na.rm = FALSE)
summarize_mean <- function(df, group = NULL, round = NULL, na.rm = TRUE) {

  if(rlang::quo_is_null(rlang::enquo(group))) {

    mean <- df %>%
      dplyr::summarize_if(is.numeric, list(~mean(., na.rm = na.rm))) %>%
      tidyr::gather(key = "var", value = "mean")

  } else {

    group <- rlang::enquo(group)
    liefTools::check_vars_exist(df, !!group)

    mean <- df %>%
      dplyr::group_by(!!group) %>%
      dplyr::summarize_if(is.numeric, list(~mean(., na.rm = na.rm))) %>%
      tidyr::gather(key = "var", value = "mean", -!!group) %>%
      tidyr::spread(key = !!group, value = mean)

  }

  if(!is.null(round)) {
    mean <- mean %>%
      dplyr::mutate_if(is.numeric, base::round, round)
  }

  return(mean)
}
