# evaluate_treaty.R
# 2 Functions to evaluate the agreement
# 2a Single parameter set
# 2b Multiple parameter sets
# 2 Functions to evaluate the agreement
# 2a Single parameter set

#' Evaluate the treaty scenario
#'
#' Evaluate whether or not the treaty will be made in a confined or unconfined aquifer
#' @param params Parameter list (or data.frame with 1 row) containing
#' necessary parameters to evaluate the agreement. See \code{?check_params} for details.
#' @details
#' Evaluate the treaty given social, economic, and geophysical parameters.
#' @return
#' Returns a 1-row tibble containing pumping, utility ranges needed for the treaty,
#' and whether or not there is a treaty (i.e., if zRange > 0)
#' @importFrom magrittr %>%
#' @export
#' @examples
#' evaluate_treaty(default_params)
evaluate_treaty <- function(params) {
  # (eval_out <- evaluate_treaty(params_default()))
  # this function calculates abstraction from the game,
  # and determines whether or not a treaty is signed
  if(nrow(params)!=1){
    stop("This is an error message because params not 1 dimension")
  }

  aquifer_type <- check_params(params)

  if (aquifer_type == "confined") {
    treaty_results <- evaluate_treaty_confined(params)
  } else if (aquifer_type == "confined") {
    treaty_results <- evaluate_treaty_unconfined(params)
  } else {
    stop("aquifer must be confined or unconfined, as specified by Dxx or PHIxx parameters.")
  }

  return(treaty_results)
}

#' Evaluate treaty utility
#'
#' Evaluate utility given (single) treaty parameters
evaluate_treaty_utility <- function(params,q_vals) {
  # this function calculates utilities, given parameters and abstraction
  if(dim(params)[1]!=1){
    stop("This is an error message because params not 1 dimension")
  }
  for (v in 1:dim(q_vals)[2]) {assign(names(q_vals)[v], q_vals[[v]])}
  # get utilities
  Us_hat <- conA_Us(qs=qshat,qf=qfhat,params %>% dplyr::mutate(rm=rmT,rs=rsT),z=0)
  Uf_hat <- conA_Uf(qs=qshat,qf=qfhat,params %>% dplyr::mutate(rm=rmT,rf=rfT),z=0)
  Us_star <- conA_Us(qs=qsstar,qf=qfstar,params %>% dplyr::mutate(rm=rmN,rs=rsN),z=0)
  Uf_star <- conA_Uf(qs=qsstar,qf=qfstar,params %>% dplyr::mutate(rm=rmN,rf=rfN),z=0)
  Us_double <- conA_Us(qs=qsdouble,qf=qfhat,params %>% dplyr::mutate(rm=rmT,rs=rsT),z=0)
  Uf_double <- conA_Uf(qs=qshat,qf=qfdouble,params %>% dplyr::mutate(rm=rmT,rf=rfT),z=0)
  Us_hat_double <- conA_Us(qs=qshat,qf=qfdouble,params %>% dplyr::mutate(rm=rmT,rs=rsT),z=0)
  Uf_hat_double <- conA_Uf(qs=qsdouble,qf=qfhat,params %>% dplyr::mutate(rm=rmT,rf=rfT),z=0)
  u_vals <- tibble::tibble(Us_hat=Us_hat,Uf_hat=Uf_hat,
                           Us_star=Us_star,Uf_star=Uf_star,
                           Us_double=Us_double,Uf_double=Uf_double,
                           Us_hat_double=Us_hat_double,Uf_hat_double=Uf_hat_double)
  return(u_vals)
}

#' Evaluate treaty depths
#'
#' Evaluate water table depth given (single) treaty parameters
evaluate_treaty_depths <- function(params,q_vals,aquifer_type) {
  # this function calculates water table depth, given parameters and abstraction
  if(dim(params)[1]!=1){
    stop("This is an error message because params not 1 dimension")
  }
  for (v in 1:dim(q_vals)[2]) {assign(names(q_vals)[v], q_vals[[v]])} # assign q_values to variables with appropriate names
  # get depths
  if (aquifer_type == "confined") {
    get_ds <- conA_ds
    get_df <- conA_df
  } else {
    get_ds <- unconA_ds
    get_df <- unconA_df
  }
  ds_hat <- get_ds(qs=qshat,qf=qfhat,params %>% dplyr::mutate(rm=rmT,rs=rsT))
  df_hat <- get_df(qs=qshat,qf=qfhat,params %>% dplyr::mutate(rm=rmT,rf=rfT))
  ds_star <- get_ds(qs=qsstar,qf=qfstar,params %>% dplyr::mutate(rm=rmN,rs=rsN))
  df_star <- get_df(qs=qsstar,qf=qfstar,params %>% dplyr::mutate(rm=rmN,rf=rfN))
  ds_double <- get_ds(qs=qsdouble,qf=qfhat,params %>% dplyr::mutate(rm=rmT,rs=rsT))
  df_double <- get_df(qs=qshat,qf=qfdouble,params %>% dplyr::mutate(rm=rmT,rf=rfT))
  ds_hat_double <- get_ds(qs=qsdouble,qf=qfhat,params %>% dplyr::mutate(rm=rmT,rs=rsT))
  df_hat_double <- get_df(qs=qshat,qf=qfdouble,params %>% dplyr::mutate(rm=rmT,rf=rfT))
  d_vals <- tibble::tibble(ds_hat=ds_hat,df_hat=df_hat,
                           ds_star=ds_star,df_star=df_star,
                           ds_double=ds_double,df_double=df_double,
                           ds_hat_double=ds_hat_double,df_hat_double=df_hat_double)
  return(d_vals)
}


#' Evaluate multiple treaty scenarios
#'
#' Evaluate whether or not the treaty will be made under multiple scenarios
#' @param params_df Data.frame of parameters with each row sent to \code{evaluate_treaty()}.
#' @param return_criteria Character string containing letters that indicate output variables. See details.
#' @details
#' Evaluate the treaty given multiple combinations of social, economic,
#' and geophysical parameters.
#' This function takes a data.frame of parameters,
#' evaluates each row to see if a treaty is signed,
#' and returns a tibble with the results and original params.
#'
#' The parameter \code{return_criteria} can contain the following letters:
#' \itemize{
#' \item q - will return abstraction rates
#' \item p - will return only parameters different from default. Otherwise all parameters returned
#' \item a - return all parameters (i.e., it's redundant to include a AND p)
#' \item u - return utilities of each player
#' \item d - return depth to water table for each player
#' }
#' @return
#' Returns a \code{tibble} containing values utility ranges needed for the treaty,
#' and whether or not there is a treaty (i.e., if zRange > 0), as well as output specified by \code{return_criteria}.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' library(genevoisgame)
#' params_df <- genevoisgame::default_params
#' evaluate_treaty_cases(df)
#' evaluate_treaty_cases(default_params,'')
evaluate_treaty_cases <- function(params_df,return_criteria="qp") {
  aquifer_type <- check_params(params_df)
  params_list <- split(params_df,1:dim(params_df)[1])
  eval_results <- do.call(rbind,lapply(params_list,evaluate_treaty))
  q_vals_list <- split(eval_results %>% dplyr::select(dplyr::starts_with("q")),1:dim(eval_results)[1])
  # eval_results_treat <- eval_results %>% select(treaty,zRange,zMinSwiss,zMaxFrench)
  eval_return <- eval_results %>% dplyr::select(treaty,zRange,zMinSwiss,zMaxFrench)
  if (max(grepl("p",return_criteria))==1) { # identify parameters that do not vary AND are equal to default value
    vars_stable <- params_df %>% tidyr::gather(variable,value) %>% dplyr::group_by(variable) %>%
      dplyr::summarize(n_unique=length(unique(value))) %>%
      # dplyr::left_join(params_default() %>% tidyr::gather(variable,default),by="variable") %>%
      dplyr::filter(n_unique==1) %>%
      dplyr::pull(variable)
    params_cases <- params_df %>% dplyr::select(-match(vars_stable,names(params_df))) # remove identified parameters
    eval_return <- eval_return %>% dplyr::bind_cols(params_cases)
  }
  if (max(grepl("a",return_criteria))==1) { # return all parameters
    eval_return <- eval_return %>% dplyr::bind_cols(params_df)
  }
  if (max(grepl("q",return_criteria))==1) { # return abstraction rates
    eval_return <- eval_return %>% dplyr::bind_cols(eval_results%>% dplyr::select(starts_with("q")))
  }
  if (max(grepl("u",return_criteria))==1) { # return utilities
    u_vals <- do.call(rbind,mapply(evaluate_treaty_utility,params=params_list,q_vals=q_vals_list,SIMPLIFY=FALSE))
    eval_return <- eval_return %>% dplyr::bind_cols(u_vals)
  }
  if (max(grepl("d",return_criteria))==1) { # return depth to water table
    d_vals <- do.call(rbind,mapply(evaluate_treaty_depths,params=params_list,q_vals=q_vals_list,aquifer_type=aquifer_type,SIMPLIFY=FALSE))
    eval_return <- eval_return %>% dplyr::bind_cols(d_vals)
  }
  return(eval_return)
}
