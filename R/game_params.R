# params_valid

#' Check for valid parameters
#'
#' Check for valid parameters
#' @param params \code{data.frame} or \code{list} of parameters
#' @details
#' To evaluate the game, a \code{data.frame} or \code{list} of paramers must be supplied to
#' \code{evaluate_treaty} or \code{evaluate_treaty_cases}. Generally speaking, these parameters
#' should be non-negative. The parameters must include, for all scenarios:
#' \describe{
#' \item{Demand}{\code{Qf, Qs}}
#' \item{Unit cost of alternative supply}{\code{p0f, p0s}}
#' \item{Unit cost of pumping}{\code{B}}
#' \item{Unit cost of recharge, Swiss}{\code{crs}}
#' \item{Volume of recharge, with (T) and without (N) a treaty}{\code{rmT, rmN}}
#' \item{Trust between players}{\code{gs, gf}}
#' \item{Cost of the treaty}{\code{es, ef}}
#' }
#' And additional parameters depending on the type of aquifer. For confined aquifers:
#' \describe{
#' \item{Drawdown relationships}{\code{Dff, Dss, Dsf, Dfs}}
#' \item{Groundwater depth without pumping}{\code{d0s, d0f}}
#' \item{Recharge relationship with a treaty}{\code{DsrT, DfrT}}
#' \item{Recharge relationship without a treaty}{\code{DsrN, DfrN}}
#' }
#' And for unconfined aquifers:
#' \describe{
#' \item{Drawdown relationships}{\code{PHIff, PHIss, PHIsf, PHIfs}}
#' \item{Groundwater depth without pumping}{\code{dBs, dBf, h0s, h0f}}
#' \item{Recharge relationship with a treaty}{\code{PHIsrT, PHIfrT}}
#' \item{Recharge relationship without a treaty}{\code{PHIsrN, PHIfrN}}
#' \item{Nonlinearity of the cost function}{\code{l}}
#' }
#' @export
#' @return
#' Returns the aquifer type, depending on whether Dxx is specified ("confined") or PHIxx is specified ("unconfined")
#' @examples
#' library(genevoisgame)
#' check_params(example_params_confined)
#' check_params(example_params_unconfined)
#' \dontrun{check_params(example_params_confined[,-ncol(example_params_confined):-1])}
#'
#' params <- data.frame(example_params_confined,gs=c(0.2,0.8))
#' check_params(params)
#' \dontrun{check_params(params[,-ncol(example_params_confined):-1])}
check_params <- function(params) {
  drawdown_confined_params <- c('Dff','Dss','Dsf','Dfs','d0s','d0f','DsrN','DsrT','DfrN','DfrT')
  drawdown_unconfined_params <- c('PHIff','PHIss','PHIsf','PHIfs','dBs','dBf','h0s','h0f','PHIsrN','PHIsrT','PHIfrN','PHIfrT','l')
  # initial_depth_confined_params <- c('d0s','d0f')
  # initial_depth_unconfined_params <- c('dBs','dBf','h0s','h0f')
  additional_params <- c('Qf','Qs','p0f','p0s','B','rmN','rmT','crs','c0rs','gs','gf','es','ef')
  all_params <- unique(c(drawdown_confined_params,drawdown_unconfined_params,additional_params))
  additional_warnings <- NULL

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
  # 3. Check for negative values

  # 1. ensure parameters are specified as D or PHI
  if (any(grepl("D[sfij][sfij]",param_names)) & any(grepl("PHI[sfij][sfij]",param_names))) {
    stop("params should contain either Dix or PHIix, not both")

    # 2a. if D, ensure all D parameters are present along with d0s, d0f
  } else if (any(grepl("D[sfij][sfij]",param_names))) {
    aquifer_type <- "confined"
    if (!all(drawdown_confined_params %in% param_names)) {
      missing_conf_params <- drawdown_confined_params[!(drawdown_confined_params %in% param_names)]
      # warning(paste("missing",paste(missing_conf_params,collapse=", "),"in params"))
      missing_params <- c(missing_params,missing_conf_params)
    }

    # 2b. if PHI, ensure all PHI parameters are present along with dBs, dBf, hs, hf
  } else if (any(grepl("PHI[sfij][sfij]",param_names))) {
    aquifer_type <- "unconfined"
    if (!all(drawdown_unconfined_params %in% param_names)) {
      missing_unconf_params <- drawdown_unconfined_params[!(drawdown_unconfined_params %in% param_names)]
      # warning(paste("missing",paste(missing_unconf_params,collapse=", "),"in params"))
      missing_params <- c(missing_params,missing_unconf_params)
    }
    if ("l" %in% param_names) {
      if (any(params$l < 0) | any(params$l > 1)) {
        additional_warnings <- c(additional_warnings,"Column l contains values not in the range [0,1)")
      }
      if (any(params$l == 1)) {
        additional_warnings <- c(additional_warnings,"Column l contains values equal to 1 (one). This triggers linear cost solution for the unconfined game.")
      }
    }
  } else {
    stop("Missing drawdown parameters (Dxx or PHIxx) in params.")
  }

  # 3. Check for negative values in the parameters specified above
  game_params <- params[,names(params) %in% all_params] # remove parameters that are not contained in one of the above lists
  neg_vals_df <- game_params[,sapply(game_params,function(x) any(x<0))]
  if (ncol(neg_vals_df) > 0) {
    warning(paste0("param column(s), ",paste(names(neg_vals_df),collapse=", "),", contain negative values."))
  }

  if (length(missing_params) > 0) {
    warning(paste("missing",paste(missing_params,collapse=", "),"in params"))
  }

  if (length(additional_warnings) > 0) {
    for (i in 1:length(additional_warnings)) {
      warning(additional_warnings[i])
    }
  }

  # return aquifer type
  return(aquifer_type)
}



#' Check game dynamics (NOT WORKING)
#'
#' Check the basic game dynamics, particularly in cases where zRange=0 and the first best
#' and nash outcomes are the same.
#' @param params game parameters
#' @param text_results logical value determining if results are returned as text or a tibble
#' @param aquifer_type "confined" or "unconfined", or \code{NULL} -- in which case \code{check_params} will determine the type
#' @details
#' In a number of situations, the genevois game does nothing interesting: both players pump at their
#' entire demand, the first best is equivalent to the nash equilibrium, and there are no
#' advantages or disadvantages to signing a treaty. This function helps us better understand
#' why this is the case.
#'
#' For any interesting dynamics to arise from the game, the First Best has to be different (better)
#' than the Nash Equilibrium. This occurs when players forgo pumping and supply their water from another
#' source, which can only occur if the other source is less expensive than pumping. In other words,
#' pumping for at least one of the players has to be great enough that the cost of pumping is greater
#' than the cost of alternative supply. If this is not the case, then the players cannot do any better
#' working together than they could on their own.
#' @return
#' Either (1) a text description of the dynamics (if \code{text_results==TRUE}) or (2) a table
#' containing the following values:
#' \itemize{
#' \item: \code{ds_max, df_max}: The maximum depth for each player, if both players pump at their demand (\code{Qs, Qf}).
#' \item: \code{ds_threshold, df_threshold}: The depth for each player where the cost of pumping equals the cost of alternative supply (\code{Qi=B*di}).
#' }
#' @keywords internal
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # Table output
#' check_game_dynamics(example_params_unconfined,text_results = FALSE)
#' example_params_unconfined %>% dplyr::mutate(Qs=1,Qf=1) %>% check_game_dynamics(text_results = FALSE)
#'
#' # Text output
#' check_game_dynamics(example_params_unconfined) %>% cat()
#' example_params_unconfined %>% dplyr::mutate(Qs=1,Qf=1) %>% check_game_dynamics() %>% cat()
#' }
check_game_dynamics <- function(params, text_results=TRUE, aquifer_type=NULL) {
  # # this function calculates water table depth, given parameters and abstraction
  # if(dim(params)[1]!=1){
  #   stop("This is an error message because params not 1 dimension")
  # }
  # if (is.null(aquifer_type)) {
  #   aquifer_type <- check_params(params)
  # }
  #
  # # get depths
  # if (aquifer_type == "confined") {
  #   get_ds <- conA_ds
  #   get_df <- conA_df
  #   params_treaty <- params %>% dplyr::rename(rm=rmT,Dsr=DsrT,Dfr=DfrT)
  #   params_notreaty <- params %>% dplyr::rename(rm=rmN,Dsr=DsrN,Dfr=DfrN)
  #   ds_threshold <- params$p0s / params$B
  #   df_threshold <- params$p0f / params$B
  # } else {
  #   params$Bs <- params$B
  #   params$Bf <- params$B
  #   get_ds <- unconA_ds
  #   get_df <- unconA_df
  #   params_treaty <- params %>% dplyr::rename(rm=rmT,PHIsr=PHIsrT,PHIfr=PHIfrT)
  #   params_notreaty <- params %>% dplyr::rename(rm=rmN,PHIsr=PHIsrN,PHIfr=PHIfrN)
  #   ds_threshold <- params$p0s / params$Bs
  #   df_threshold <- params$p0f / params$Bf
  # }
  #
  # ds_max <- get_ds(qs=params$Qs,qf=params$Qf,params_notreaty)
  # df_max <- get_df(qs=params$Qs,qf=params$Qf,params_notreaty)
  #
  # if (ds_max > ds_threshold | df_max > df_threshold) {
  #   dynamics <- "possibly interesting"
  # } else {
  #   dynamics <- "no interesting"
  # }
  #
  # if (text_results) {
  #   results <- paste0("If both players pump at their maximum (qi=Qi), the water table depth will be ds = ",ds_max,", df = ",df_max,".\n",
  #                     "For the cost of pumping to equal the cost of alternative supply, the depths would be ds = ",ds_threshold,", df = ",df_threshold,".\n",
  #                     "In other words, you would expect ",dynamics," dynamics from this game.\n")
  # } else {
  #   results <- tibble::tibble(ds_max=ds_max,df_max=df_max,
  #                             ds_threshold=ds_threshold,df_threshold=df_threshold,
  #                             dynamics=dynamics)
  # }

  return(NULL)
}
