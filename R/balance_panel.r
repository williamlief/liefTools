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
#' @param panel_start (optional) the first year in the panel (int), default is
#' minimum observed
#' @param panel_stop (optional) the last year in the panel (int), default is the
#'  maximum observed
#' @param complete_obs (optional) require units to be observed in all time
#' periods? default is true.
#'
#' @return input tibble filtered to balanced panel
#' @export

balance_panel <- function(df, unit=unit, time=time,
                          start=NULL, stop=NULL, complete.obs = TRUE) {

  unit  <- enquo(unit)
  time <- enquo(time)

  vars <- c(rlang::as_name(unit), rlang::as_name(time))
  for (var in vars) {
    if (!(var %in% colnames(df))) {
      stop(paste0("variable ", var, " not found"))
    }
  }

  if (is.null(start)) { start <- min(pull(df, !!time)) }
  if (is.null(stop))  { stop  <- max(pull(df, !!time)) }

  balance <- df %>%
    group_by(!!unit) %>%
    filter(!!time >= !!start,
           !!time <= !!stop) %>%
    {if(complete.obs == TRUE) filter(., n() == !!stop - !!start + 1) else . } %>%
    ungroup()

  return(balance)
}
