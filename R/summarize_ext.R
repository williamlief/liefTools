# extenstions to summarize for looking at whole datasets quickly
utils::globalVariables(c("pct_na", "mean", "value"))

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
#' @description reports mean value for all numeric and logical variables in a
#' data frame
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
      dplyr::summarize_if(function(col) {is.numeric(col) | is.logical(col)},
                          list(~mean(., na.rm = na.rm))) %>%
      tidyr::gather(key = "var", value = "mean")

  } else {

    group <- rlang::enquo(group)
    liefTools::check_vars_exist(df, !!group)

    mean <- df %>%
      dplyr::group_by(!!group) %>%
      dplyr::summarize_if(function(col) {is.numeric(col) | is.logical(col)},
                          list(~mean(., na.rm = na.rm))) %>%
      tidyr::gather(key = "var", value = "mean", -!!group) %>%
      tidyr::spread(key = !!group, value = mean)

  }

  if(!is.null(round)) {
    mean <- mean %>%
      dplyr::mutate_if(is.numeric, base::round, round)
  }

  return(mean)
}


#' mean, sd, min, max of variables
#'
#' @name summarize_stats
#' @description returns the mean, sd, min and max of a list of variables. By default
#' passes na.rm = TRUE. Respects grouping of input data.
#'
#' @param tbl A tbl
#' @param ... variables to summarize
#' @param na.rm  logical value indicating whether NA values should be stripped
#' before the computation proceeds. Default is TRUE
#' @return a long table with one row per variable. Columns are variable, mean, sd,
#' min and max.
#' @details This function respects grouping. If the tbl is grouped, the output
#' will be as well.
#' @export
#'
#' @examples
#' summarize_stats(airquality, Ozone, Solar.R, Wind, Temp)
#' summarize_stats(airquality %>% dplyr::group_by(Month), Ozone, Solar.R, Wind, Temp)
summarize_stats <- function(tbl, ..., na.rm = TRUE) {
  vars <- rlang::enquos(...)

  if (dplyr::is_grouped_df(tbl)) {
    groups <- dplyr::group_vars(tbl)
    groups <- c("variable", groups)
  } else groups <- "variable"

  tbl %>%
    tidyr::gather(key = "variable", value = "value", !!! vars) %>%
    dplyr::group_by(.dots = groups) %>%
    dplyr::summarize(
      mean = base::mean(value, na.rm = !!na.rm),
      sd   =  stats::sd(value, na.rm = !!na.rm),
      min  =  base::min(value, na.rm = !!na.rm),
      max  =  base::max(value, na.rm = !!na.rm))
}

#' multi-level summarize function
#'
#' @name summarize_multi
#' @description This function is used identically to dplyr::summarize. However,
#'   it applies the grouping variables of .data additively and returns summary
#'   values at each level. The returned dataset contains an additional variable
#'   'level' that tracks the number of grouping variables applied. Note that the
#'   function additively applies grouping variables in the order that they were
#'   originally specified. It does not do combinations of levels. In other
#'   words, a dataset with three grouping variables, 'a', 'b', and 'c', would
#'   return level 0: ungroup(), level 1: group_by('a'), level 2: group_by ('a',
#'   'b') and level 3: group_by('a', 'b', 'c')
#'
#' @note by default this function creates a variable called "level_" that tracks
#'   the levels of the groupings. If this conflicts with your dataset it can be
#'   renamed using the level parameter.
#'
#' @param .data the data frame to be summarised
#' @param ... further arguments of the form var = value
#' @param level name of created variable tracking summary levels. Default is 'level_'
#'
#' @export
#'
#' @examples
#' summarize_multi(iris, mean_sepal = mean(Sepal.Length))
#' summarize_multi(iris %>% dplyr::group_by(Species), mean_sepal = mean(Sepal.Length))
#' summarize_multi(iris %>% dplyr::group_by(Species, Petal.Width), mean_sepal = mean(Sepal.Length))
#' @importFrom rlang :=
summarize_multi <- function(.data, ... , level = "level_") {

  level = rlang::enquo(level)

  df <- .data %>%
    dplyr::ungroup() %>%
    dplyr::summarize(...) %>%
    dplyr::mutate(!!level := 0) %>%
    dplyr::select(!!level, dplyr::everything())

  if (dplyr::is_grouped_df(.data)) {

    groups <- dplyr::group_vars(.data)
    list <- c()

    for(i in 1:length(groups)) {
      list[[i]] <- .data %>%
        dplyr::group_by_at(dplyr::vars(dplyr::one_of(groups[1:i]))) %>%
        dplyr::summarize(...) %>%
        dplyr::mutate(!!level := i) %>%
        dplyr::ungroup()
    }

    df <- df %>%
      dplyr::bind_rows(list) %>%
      dplyr::select(!!level, groups, dplyr::everything())
  }

  return(df)
}



