#' Summary of NA by variable
#'
#' @name summarize_na
#' @description reports percent of missing data for all variables in data frame
#' @usage missing_summary(diamonds)
#'
#' @param df : a tibble
#' @param group : optional single grouping variable. If used, pct missing by
#' group will be spread across columns.
#'
#' @return a tibble with one row per variable in original datset, values are pct na.
#' @export
#'
#' @examples
#'  missing_summary(diamonds)
#'  missing_summary(diamonds, color)
summarize_na <- function(df, group = NULL) {

  if(rlang::quo_is_null(enquo(group))) {
    miss <- df %>%
      summarize_all(list(~mean(is.na(.)))) %>%
      gather(key = "var", value = "pct_na")

    return(miss)
  }

  group <- enquo(group)
  var <- rlang::as_name(group)
  if (!(var %in% colnames(df))) {
    stop(paste0("variable ", var, " not found"))
  }

  miss <- df %>%
    group_by(!!group) %>%
    summarize_all(list(~mean(is.na(.)))) %>%
    gather(key = "var", value = "pct_na", -!!group) %>%
    spread(key = !!group, value = pct_na)

  return(miss)
}
