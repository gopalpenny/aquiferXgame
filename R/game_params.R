# params_valid

#' Check for valid parameters
#'
#' Check for valid parameters
#' @param params \code{data.frame} or \code{list} of parameters
#' @details
#' To evaluate the game, a \code{data.frame} or \code{list} of paramers must be supplied to
#' \code{evaluate_treaty} or \code{evaluate_treaty_cases}. These must include, for all scenarios:
#' \describe{
#' \item{Demand}{\code{Qf, Qs}}
#' \item{Unit cost of alternative supply}{\code{p0f, p0s}}
#' \item{Unit cost of pumping}{\code{B}}
#' \item{Unit cost of recharge, Swiss}{\code{crs}}
#' \item{Volume of recharge, with (T) and without (N) a treaty}{\code{rmT, rmN}}
#' \item{Recharge relationship with a treaty}{\code{rsT, rfT}}
#' \item{Recharge relationship without a treaty}{\code{rsN, rfN}}
#' \item{Trust between players}{\code{gs, gf}}
#' \item{Cost of the treaty}{\code{es, ef}}
#' }
#' And additional parameters depending on the type of aquifer. For confined aquifers:
#' \describe{
#' \item{Drawdown relationships}{\code{Dff, Dss, Dsf, Dfs}}
#' \item{Groundwater depth without pumping}{\code{d0s, d0f}}
#' }
#' And for unconfined aquifers:
#' \describe{
#' \item{Drawdown relationships}{\code{PHIff, PHIss, PHIsf, PHIfs}}
#' \item{Groundwater depth without pumping}{\code{dBs, dBf, h0s, h0f}}
#' }
#' @return
#' Returns the aquifer type, depending on whether Dxx is specified ("confined") or PHIxx is specified ("unconfined")
#' @examples
#' library(genevoisgame)
#' params <- default_params %>% dplyr::select()
#' params_valid(params)
#' params_valid(params %>% select())
check_params <- function(params) {
  drawdown_confined_params <- c('Dff','Dss','Dsf','Dfs','d0s','d0f')
  drawdown_unconfined_params <- c('PHIff','PHIss','PHIsf','PHIfs','dBs','dBf','h0s','h0f')
  # initial_depth_confined_params <- c('d0s','d0f')
  # initial_depth_unconfined_params <- c('dBs','dBf','h0s','h0f')
  additional_params <- c('Qf','Qs','p0f','p0s','B','rmN','rmT','rsN','rsT','rfN','rfT','crs','gs','gf','es','ef')

  param_names <- names(params)
  missing_params <- c()

  if (!all(additional_params %in% param_names)) {
    missing_add_params <- additional_params[!(additional_params %in% param_names)]
    # warning(paste("missing",paste(missing_add_params,collapse=", "),"in params"))
    missing_params <- c(missing_params,missing_add_params)
  }

  # Drawdown -- specify either drawdown_confined_params OR drawdown_unconfined_params
  # Drawdown --
  # 1. ensure parameters are specified as D or PHI
  # 2a. if D, ensure all D parameters are present along with d0s, d0f
  # 2b. if PHI, ensure all PHI parameters are present along with dBs, dBf, hs, hf

  # 1. ensure parameters are specified as D or PHI
  if (any(grepl("D[sfij][sfij]",param_names)) & any(grepl("PHI[sfij][sfij]",param_names))) {
    stop("params should contain either Dix or PHIix, not both")

    # 2a. if D, ensure all D parameters are present along with d0s, d0f
  } else if (any(grepl("D[sfij][sfij]",param_names))) {
    params$aquifer_type <- "confined"
    if (!all(drawdown_confined_params %in% param_names)) {
      missing_conf_params <- drawdown_confined_params[!(drawdown_confined_params %in% param_names)]
      # warning(paste("missing",paste(missing_conf_params,collapse=", "),"in params"))
      missing_params <- c(missing_params,missing_conf_params)
    }

    # 2b. if PHI, ensure all PHI parameters are present along with dBs, dBf, hs, hf
  } else if (any(grepl("PHI[sfij][sfij]",param_names))) {
    params$aquifer_type <- "unconfined"
    if (!all(drawdown_unconfined_params %in% param_names)) {
      missing_unconf_params <- drawdown_unconfined_params[!(drawdown_unconfined_params %in% param_names)]
      # warning(paste("missing",paste(missing_unconf_params,collapse=", "),"in params"))
      missing_params <- c(missing_params,missing_unconf_params)
    }
  } else {
    stop("Missing drawdown parameters (Dxx or PHIxx) in params.")
  }

  if (length(missing_params) > 0) {
    warning(paste("missing",paste(missing_params,collapse=", "),"in params"))
  }

  # return aquifer type
  return(params$aquifer_type)
}
