#' balance a time series panel
#'
#' @name balance_panel
#' @description Balances a panel of unit-time data. Filters out units that are
#' missing observations.
#'
#' @details Give a panel of observations, this function subsets the data frame
#' to only include those units that are observed in every time period.
#' Optional `start` and `stop` times can be designated, if specified,
#' observations before the start and stop times will be dropped.
#' Set `complete_obs = FALSE` to keep units that are missing some time
#' observations during the panel.
#'
#' @param df a tibble
#' @param unit (optional) the unit variable, default is unit
#' @param time (optional) the time variable, default is time
#' @param start (optional) the first year in the panel (int), default is
#' minimum observed
#' @param stop (optional) the last year in the panel (int), default is the
#'  maximum observed
#' @param complete_obs (optional) require units to be observed in all time
#' periods? default is true.
#'
#' @examples
#'  balance_panel(df = airquality, unit = Month, time = Day)
#'
#' @return input tibble filtered to balanced panel
#' @export

balance_panel <- function(df, unit=unit, time=time,
                          start=NULL, stop=NULL, complete_obs = TRUE) {

  unit  <- rlang::enquo(unit)
  time <- rlang::enquo(time)

  liefTools::check_vars_exist(df, !!unit, !!time)

  if (is.null(start)) { start <- min(dplyr::pull(df, !!time)) }
  if (is.null(stop))  { stop  <- max(dplyr::pull(df, !!time)) }

  balance <- df %>%
    dplyr::group_by(!!unit) %>%
    dplyr::filter(!!time >= !!start,
                  !!time <= !!stop) %>%
                  {if(complete_obs == TRUE) dplyr::filter(., dplyr::n() == !!stop - !!start + 1) else . } %>%
    dplyr::ungroup()

  return(balance)
}
