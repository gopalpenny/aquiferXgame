# evaluate_treaty.R

# 2 Functions to evaluate the agreement
# 2a Single parameter set
# 2b Multiple parameter sets

# 2 Functions to evaluate the agreement
# 2a Single parameter set
evaluate_agreement <- function(params) {
  # (eval_out <- evaluate_agreement(params_default()))
  # this function calculates abstraction from the game,
  # and determines whether or not a treaty is signed
  if(dim(params)[1]!=1){
    stop("This is an error message because params not 1 dimension")
  }

  param_names <- names(params)
  # get abstraction rates
  if (any(grepl("PHI[sfij][sfij]",param_names)) & any(grepl("PHI[sfij][sfij]",param_names))) {
    stop("params should contain either Dix or PHIix, not both")
  } else if (any(grepl("D[sfij][sfij]",param_names))) {
    aquifer_type <- "confined"
  } else if (any(grepl("PHI[sfij][sfij]",param_names))) {
    aquifer_type <- "unconfined"
  } else {
    stop("Missing drawdown parameters (Dix or PHIix)")
  }

  if (aquifer_type == "confined") {
    q_hat <- MM$qeval(params,MM_qs0=MM$qshat0,MM$qfhat0,MM$qshat2,MM$qfhat2)
    q_star <- MM$qeval(params,MM_qs0=MM$qsstar0,MM$qfstar0,MM$qsstar2,MM$qfstar2)
    q_double <- MM$qeval(params %>% mutate(qshat=q_hat$qs,qfhat=q_hat$qf),
                         MM_qs0=MM$qsdouble0,MM$qfdouble0,MM$qsdouble2,MM$qfdouble2)
  } else {
    first_best <- with(params,function(x) {
      F1 <- p0s+1/2*B*(-((Dfs*x[2])/sqrt(d0f-rmT*rsT+Dfs*x[1]+Dff*x[2]))+(-2*d0s+2*rmT*rsT-3*Dss*x[1]-2*Dsf*x[2])/sqrt(d0s-rmT*rsT+Dss*x[1]+Dsf*x[2]))
      F2 <- p0f+1/2*B*((-2*d0f+2*rmT*rsT-2*Dfs*x[1]-3*Dff*x[2])/sqrt(d0f-rmT*rsT+Dfs*x[1]+Dff*x[2])-(Dsf*x[1])/sqrt(d0s-rmT*rsT+Dss*x[1]+Dsf*x[2]))
      return(list(F1=F1,F2=F2))
    })
    rootSolve::multiroot(f=firstbest,start=c(params$Qs*0.9,params$Qf*0.9))
  }

  q_vals <- tibble(qshat=q_hat$qs,qfhat=q_hat$qf,
                   qsstar=q_star$qs,qfstar=q_star$qf,
                   qsdouble=q_double$qs,qfdouble=q_double$qf)

  # get z constraints
  zMaxFrench_calc <- MM$zMaxFrench(params,q_vals)
  zMinSwiss_calc <- MM$zMinSwiss(params,q_vals)
  zRange_calc <- zMaxFrench_calc - zMinSwiss_calc
  treaty <- ifelse(zRange_calc>0,"Y","N")
  return(tibble(treaty=treaty,zRange=zRange_calc,
                zMinSwiss=zMinSwiss_calc,zMaxFrench=zMaxFrench_calc) %>%
           bind_cols(q_vals))
}

evaluate_agreement_utility <- function(params,q_vals) {
  # this function calculates utilities, given parameters and abstraction
  if(dim(params)[1]!=1){
    stop("This is an error message because params not 1 dimension")
  }
  for (v in 1:dim(q_vals)[2]) {assign(names(q_vals)[v], q_vals[[v]])}
  # get utilities
  Us_hat <- MM$Us(qs=qshat,qf=qfhat,params %>% mutate(rm=rmT,rs=rsT),z=0)
  Uf_hat <- MM$Uf(qs=qshat,qf=qfhat,params %>% mutate(rm=rmT,rf=rfT),z=0)
  Us_star <- MM$Us(qs=qsstar,qf=qfstar,params %>% mutate(rm=rmN,rs=rsN),z=0)
  Uf_star <- MM$Uf(qs=qsstar,qf=qfstar,params %>% mutate(rm=rmN,rf=rfN),z=0)
  Us_double <- MM$Us(qs=qsdouble,qf=qfhat,params %>% mutate(rm=rmT,rs=rsT),z=0)
  Uf_double <- MM$Uf(qs=qshat,qf=qfdouble,params %>% mutate(rm=rmT,rf=rfT),z=0)
  Us_hat_double <- MM$Us(qs=qshat,qf=qfdouble,params %>% mutate(rm=rmT,rs=rsT),z=0)
  Uf_hat_double <- MM$Uf(qs=qsdouble,qf=qfhat,params %>% mutate(rm=rmT,rf=rfT),z=0)
  u_vals <- tibble(Us_hat=Us_hat,Uf_hat=Uf_hat,
                   Us_star=Us_star,Uf_star=Uf_star,
                   Us_double=Us_double,Uf_double=Uf_double,
                   Us_hat_double=Us_hat_double,Uf_hat_double=Uf_hat_double)
  return(u_vals)
}

evaluate_agreement_depths <- function(params,q_vals) {
  # this function calculates water table depth, given parameters and abstraction
  if(dim(params)[1]!=1){
    stop("This is an error message because params not 1 dimension")
  }
  for (v in 1:dim(q_vals)[2]) {assign(names(q_vals)[v], q_vals[[v]])}
  # get depths
  ds_hat <- MM$ds(qs=qshat,qf=qfhat,params %>% mutate(rm=rmT,rs=rsT))
  df_hat <- MM$df(qs=qshat,qf=qfhat,params %>% mutate(rm=rmT,rf=rfT))
  ds_star <- MM$ds(qs=qsstar,qf=qfstar,params %>% mutate(rm=rmN,rs=rsN))
  df_star <- MM$df(qs=qsstar,qf=qfstar,params %>% mutate(rm=rmN,rf=rfN))
  ds_double <- MM$ds(qs=qsdouble,qf=qfhat,params %>% mutate(rm=rmT,rs=rsT))
  df_double <- MM$df(qs=qshat,qf=qfdouble,params %>% mutate(rm=rmT,rf=rfT))
  ds_hat_double <- MM$ds(qs=qsdouble,qf=qfhat,params %>% mutate(rm=rmT,rs=rsT))
  df_hat_double <- MM$df(qs=qshat,qf=qfdouble,params %>% mutate(rm=rmT,rf=rfT))
  d_vals <- tibble(ds_hat=ds_hat,df_hat=df_hat,
                   ds_star=ds_star,df_star=df_star,
                   ds_double=ds_double,df_double=df_double,
                   ds_hat_double=ds_hat_double,df_hat_double=df_hat_double)
  return(d_vals)
}


# 2b Multiple parameter sets
evaluate_agreement_cases <- function(params_df,return_criteria="tqp") {
  # this function take a data.frame of parameters,
  # evaluates each row to see if a treaty is signed,
  # and returns a tibble with the results and original params
  # t - not operational
  # q - will return abstraction rates
  # p - will return only parameters different from default. Otherwise all parameters returned
  # a - returns all parameters. Redundant to include a AND p
  params_list <- split(params_df,1:dim(params_df)[1])
  eval_results <- do.call(rbind,lapply(params_list,evaluate_agreement))
  q_vals_list <- split(eval_results %>% select(starts_with("q")),1:dim(eval_results)[1])
  # eval_results_treat <- eval_results %>% select(treaty,zRange,zMinSwiss,zMaxFrench)
  eval_return <- eval_results %>% select(treaty,zRange,zMinSwiss,zMaxFrench)
  if (max(grepl("p",return_criteria))==1) { # identify parameters that do not vary AND are equal to default value
    vars_stable <- params_df %>% gather(variable,value) %>% group_by(variable) %>%
      summarize(value_mean=mean(value),
                value_max=max(value)) %>%
      left_join(params_default() %>% gather(variable,default),by="variable") %>%
      filter(value_mean==value_max,value_mean==default) %>%
      pull(variable)
    params_cases <- params_df %>% select(-match(vars_stable,names(params_df))) # remove identified parameters
    eval_return <- eval_return %>% bind_cols(params_cases)
  }
  if (max(grepl("a",return_criteria))==1) { # return all parameters
    eval_return <- eval_return %>% bind_cols(params_df)
  }
  if (max(grepl("q",return_criteria))==1) { # return abstraction rates
    eval_return <- eval_return %>% bind_cols(eval_results%>% select(starts_with("q")))
  }
  if (max(grepl("u",return_criteria))==1) { # return utilities
    u_vals <- do.call(rbind,mapply(evaluate_agreement_utility,params=params_list,q_vals=q_vals_list,SIMPLIFY=FALSE))
    eval_return <- eval_return %>% bind_cols(u_vals)
  }
  if (max(grepl("d",return_criteria))==1) { # return depth to water table
    d_vals <- do.call(rbind,mapply(evaluate_agreement_depths,params=params_list,q_vals=q_vals_list,SIMPLIFY=FALSE))
    eval_return <- eval_return %>% bind_cols(d_vals)
  }
  return(eval_return)
}
