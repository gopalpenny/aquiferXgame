# evaluate_treaty.R
# 2 Functions to evaluate the agreement
# 2a Single parameter set
# 2b Multiple parameter sets
# 2 Functions to evaluate the agreement
# 2a Single parameter set

#' Evaluate the treaty scenario
#'
#' Evaluate whether or not the treaty will be made.
#' @param params Parameter list (or data.frame with 1 row) containing
#' necessary parameters to evaluate the agreement.
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
    q_hat <- conA_qeval(params,conAf_qs0=conA_qshat0,conA_qfhat0,conA_qshat2,conA_qfhat2)
    q_star <- conA_qeval(params,conAf_qs0=conA_qsstar0,conA_qfstar0,conA_qsstar2,conA_qfstar2)
    q_double <- conA_qeval(params %>% dplyr::mutate(qshat=q_hat$qs,qfhat=q_hat$qf),
                         conAf_qs0=conA_qsdouble0,conA_qfdouble0,conA_qsdouble2,conA_qfdouble2)
  } else {
    first_best <- with(params,function(x) {
      F1 <- p0s+1/2*B*(-((Dfs*x[2])/sqrt(d0f-rmT*rsT+Dfs*x[1]+Dff*x[2]))+(-2*d0s+2*rmT*rsT-3*Dss*x[1]-2*Dsf*x[2])/sqrt(d0s-rmT*rsT+Dss*x[1]+Dsf*x[2]))
      F2 <- p0f+1/2*B*((-2*d0f+2*rmT*rsT-2*Dfs*x[1]-3*Dff*x[2])/sqrt(d0f-rmT*rsT+Dfs*x[1]+Dff*x[2])-(Dsf*x[1])/sqrt(d0s-rmT*rsT+Dss*x[1]+Dsf*x[2]))
      return(list(F1=F1,F2=F2))
    })
    rootSolve::multiroot(f=firstbest,start=c(params$Qs*0.9,params$Qf*0.9))
  }

  q_vals <- tibble::tibble(qshat=q_hat$qs,qfhat=q_hat$qf,
                   qsstar=q_star$qs,qfstar=q_star$qf,
                   qsdouble=q_double$qs,qfdouble=q_double$qf)

  # get z constraints
  zMaxFrench_calc <- conA_zMaxFrench(params,q_vals)
  zMinSwiss_calc <- conA_zMinSwiss(params,q_vals)
  zRange_calc <- zMaxFrench_calc - zMinSwiss_calc
  treaty <- ifelse(zRange_calc>0,"Y","N")
  return(tibble::tibble(treaty=treaty,zRange=zRange_calc,
                zMinSwiss=zMinSwiss_calc,zMaxFrench=zMaxFrench_calc) %>%
           dplyr::bind_cols(q_vals))
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
  u_vals <- tibble(Us_hat=Us_hat,Uf_hat=Uf_hat,
                   Us_star=Us_star,Uf_star=Uf_star,
                   Us_double=Us_double,Uf_double=Uf_double,
                   Us_hat_double=Us_hat_double,Uf_hat_double=Uf_hat_double)
  return(u_vals)
}

#' Evaluate treaty depth
#'
#' Evaluate water table depth given (single) treaty parameters
evaluate_treaty_depths <- function(params,q_vals) {
  # this function calculates water table depth, given parameters and abstraction
  if(dim(params)[1]!=1){
    stop("This is an error message because params not 1 dimension")
  }
  for (v in 1:dim(q_vals)[2]) {assign(names(q_vals)[v], q_vals[[v]])}
  # get depths
  ds_hat <- conA_ds(qs=qshat,qf=qfhat,params %>% dplyr::mutate(rm=rmT,rs=rsT))
  df_hat <- conA_df(qs=qshat,qf=qfhat,params %>% dplyr::mutate(rm=rmT,rf=rfT))
  ds_star <- conA_ds(qs=qsstar,qf=qfstar,params %>% dplyr::mutate(rm=rmN,rs=rsN))
  df_star <- conA_df(qs=qsstar,qf=qfstar,params %>% dplyr::mutate(rm=rmN,rf=rfN))
  ds_double <- conA_ds(qs=qsdouble,qf=qfhat,params %>% dplyr::mutate(rm=rmT,rs=rsT))
  df_double <- conA_df(qs=qshat,qf=qfdouble,params %>% dplyr::mutate(rm=rmT,rf=rfT))
  ds_hat_double <- conA_ds(qs=qsdouble,qf=qfhat,params %>% dplyr::mutate(rm=rmT,rs=rsT))
  df_hat_double <- conA_df(qs=qshat,qf=qfdouble,params %>% dplyr::mutate(rm=rmT,rf=rfT))
  d_vals <- tibble(ds_hat=ds_hat,df_hat=df_hat,
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
    d_vals <- do.call(rbind,mapply(evaluate_treaty_depths,params=params_list,q_vals=q_vals_list,SIMPLIFY=FALSE))
    eval_return <- eval_return %>% dplyr::bind_cols(d_vals)
  }
  return(eval_return)
}
