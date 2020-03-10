# game_functions_confined.R
# 4 Mathematica functions and game parameter evaluations
# 4x function to evaluate contraints
# 4a function to evaluate abstraction rates
# 4b Mathematica functions for depth, utility, and abstraction
# 4c Mathematica functions for z constraints



#' Evaluate the treaty in a confined aquifer
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
evaluate_treaty_confined <- function(params) {
  # (eval_out <- evaluate_treaty(params_default()))
  # this function calculates abstraction from the game,
  # and determines whether or not a treaty is signed

  q_hat <- conA_qeval(params,conAf_qs0=conA_qshat0,conA_qfhat0,conA_qshat2,conA_qfhat2)
  q_star <- conA_qeval(params,conAf_qs0=conA_qsstar0,conA_qfstar0,conA_qsstar2,conA_qfstar2)
  q_double <- conA_qeval(params %>% dplyr::mutate(qshat=q_hat$qs,qfhat=q_hat$qf),
                         conAf_qs0=conA_qsdouble0,conA_qfdouble0,conA_qsdouble2,conA_qfdouble2)

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


#
# Evaluate treaty depth in a confined aquifer
#
# Evaluate water table depth given (single) treaty parameters
# evaluate_treaty_depths_confined <- function(params,q_vals) {
#   # this function calculates water table depth, given parameters and abstraction
#   if(dim(params)[1]!=1){
#     stop("This is an error message because params not 1 dimension")
#   }
#   for (v in 1:dim(q_vals)[2]) {assign(names(q_vals)[v], q_vals[[v]])}
#   # get depths
#   ds_hat <- conA_ds(qs=qshat,qf=qfhat,params %>% dplyr::mutate(rm=rmT,rs=rsT))
#   df_hat <- conA_df(qs=qshat,qf=qfhat,params %>% dplyr::mutate(rm=rmT,rf=rfT))
#   ds_star <- conA_ds(qs=qsstar,qf=qfstar,params %>% dplyr::mutate(rm=rmN,rs=rsN))
#   df_star <- conA_df(qs=qsstar,qf=qfstar,params %>% dplyr::mutate(rm=rmN,rf=rfN))
#   ds_double <- conA_ds(qs=qsdouble,qf=qfhat,params %>% dplyr::mutate(rm=rmT,rs=rsT))
#   df_double <- conA_df(qs=qshat,qf=qfdouble,params %>% dplyr::mutate(rm=rmT,rf=rfT))
#   ds_hat_double <- conA_ds(qs=qsdouble,qf=qfhat,params %>% dplyr::mutate(rm=rmT,rs=rsT))
#   df_hat_double <- conA_df(qs=qshat,qf=qfdouble,params %>% dplyr::mutate(rm=rmT,rf=rfT))
#   d_vals <- tibble(ds_hat=ds_hat,df_hat=df_hat,
#                    ds_star=ds_star,df_star=df_star,
#                    ds_double=ds_double,df_double=df_double,
#                    ds_hat_double=ds_hat_double,df_hat_double=df_hat_double)
#   return(d_vals)
# }


# 4a function to evaluate abstraction rates
conA_qeval <- function(params,conAf_qs0,conAf_qf0,conAf_qs2,conAf_qf2) {
  qs0 <- conAf_qs0(params) # get initial q_hat
  qf0 <- conAf_qf0(params)
  qs1 <- qs0
  qf1 <- qf0
  qs2 <- apply_constraints(qs1,c(0,params$Qs)) # contrain q_hat to bounds
  qf2 <- apply_constraints(qf1,c(0,params$Qf))
  for (i in 1:500) {
    if (qs1 != qs2 | qf1 != qf2) { # if constrained q_hat != initial q_hat
      # cat('qs1 =',qs1,'\nqs2 =',qs2,'\nqf1 =',qf1,'\nqf2 =',qf2,"\n")
      qs1 <- qs2 # store old
      qf1 <- qf2
      qs2 <- apply_constraints(conAf_qs2(params,qf1),c(0,params$Qs)) # update qshat with constrained qfhat
      qf2 <- apply_constraints(conAf_qf2(params,qs1),c(0,params$Qf)) # update qfhat with constrained qshat
    } else {
      return(list(qs=qs2,qf=qf2))
    }
  }
  cat("q values not converging given params:\n")
  print(as.data.frame(params[1,]))
  return(list(qs=qs2,qf=qf2))
}

# 4b Mathematica functions for depth, utility, and abstraction

conA_Us <- function(qs,qf,params,z) {
  with(params,
       -es-p0s*(-qs+Qs)-crs*rm-B*qs*(d0s+Dsf*qf+Dss*qs-rm*rs)+z
  )}
conA_Uf <- function(qs,qf,params,z) {
  with(params,
       -ef-p0f*(-qf+Qf)-B*qf*(d0f+Dff*qf+Dfs*qs-rf*rm)-z
  )}
conA_ds <- function(qs,qf,params) {
  with(params,
       d0s+Dsf*qf+Dss*qs-rm*rs
  )}
conA_df <- function(qs,qf,params) {
  with(params,
       d0f+Dff*qf+Dfs*qs-rf*rm
  )}

conA_qshat0 <- function(params) {
  with(params,
       (Dfs*p0f+Dsf*p0f-2*Dff*p0s+B*(2*d0s*Dff-d0f*(Dfs+Dsf)+rmT*(Dfs*rfT+Dsf*rfT-2*Dff*rsT)))/(B*(Dfs^2+2*Dfs*Dsf+Dsf^2-4*Dff*Dss))
  )}
conA_qfhat0 <- function(params) {
  with(params,
       (-2*Dss*p0f+(Dfs+Dsf)*p0s+B*(-d0s*(Dfs+Dsf)+2*d0f*Dss-2*Dss*rfT*rmT+(Dfs+Dsf)*rmT*rsT))/(B*(Dfs^2+2*Dfs*Dsf+Dsf^2-4*Dff*Dss))
  )}
conA_qshat2 <- function(params,qf1) {
  with(params,
       (p0s-B*(d0s+Dfs*qf1+Dsf*qf1-rmT*rsT))/(2*B*Dss)
  )}
conA_qfhat2 <- function(params,qs1) {
  with(params,
       (p0f-B*(d0f+Dfs*qs1+Dsf*qs1-rfT*rmT))/(2*B*Dff)
  )}
# qi_star
conA_qsstar0 <- function(params) {
  with(params,
       (2*B*d0s*Dff-B*d0f*Dsf+Dsf*p0f-2*Dff*p0s+B*Dsf*rfN*rmN-2*B*Dff*rmN*rsN)/(B*Dfs*Dsf-4*B*Dff*Dss)
  )}
conA_qfstar0 <- function(params) {
  with(params,
       (-2*Dss*p0f+Dfs*p0s+B*(-d0s*Dfs+2*d0f*Dss-2*Dss*rfN*rmN+Dfs*rmN*rsN))/(B*(Dfs*Dsf-4*Dff*Dss))
  )}
conA_qsstar2 <- function(params,qf1) {
  with(params,
       (p0s-B*(d0s+Dsf*qf1-rmN*rsN))/(2*B*Dss)
  )}
conA_qfstar2 <- function(params,qs1) {
  with(params,
       (p0f-B*(d0f+Dfs*qs1-rfN*rmN))/(2*B*Dff)
  )}
# qi_double
conA_qsdouble0 <- function(params) { # in call: include params %>% mutate(qfhat=qfhat,qshat=qshat)
  with(params,
       (Dsf*(p0f-gs*p0f)-2*Dff*p0s+B*(2*d0s*Dff+d0f*Dsf*(-1+gs)+2*Dff*Dsf*gs*qfhat-Dfs*Dsf*gf*qshat+Dfs*Dsf*gf*gs*qshat+Dsf*rfT*rmT-Dsf*gs*rfT*rmT-2*Dff*rmT*rsT))/(B*(-4*Dff*Dss+Dfs*Dsf*(-1+gf)*(-1+gs)))
  )}
conA_qfdouble0 <- function(params) { # in call: include params %>% mutate(qfhat=qfhat,qshat=qshat)
  with(params,
       (-2*Dss*p0f-Dfs*(-1+gf)*p0s+B*(2*d0f*Dss+d0s*Dfs*(-1+gf)-Dfs*Dsf*gs*qfhat+Dfs*Dsf*gf*gs*qfhat+2*Dfs*Dss*gf*qshat-2*Dss*rfT*rmT+Dfs*rmT*rsT-Dfs*gf*rmT*rsT))/(B*(-4*Dff*Dss+Dfs*Dsf*(-1+gf)*(-1+gs)))
  )}
conA_qsdouble2 <- function(params,qf1) {
  with(params,
       (p0s-B*(d0s+Dsf*(qf1-gs*qf1+gs*qfhat)-rmT*rsT))/(2*B*Dss)
  )}
conA_qfdouble2 <- function(params,qs1) {
  with(params,
       (p0f-B*(d0f+Dfs*(qs1-gf*qs1+gf*qshat)-rfT*rmT))/(2*B*Dff)
  )}

# 4c Mathematica functions for z constraints
# z constraints
conA_zMinSwiss <- function(params,q_vals) {
  with(q_vals, # q_vals should include qsstar,qfstar,qshat,qfhat,qsdouble,qfdouble
       with(params,
            es+p0s*(Qs-qshat)-p0s*(Qs-qsstar)-crs*rmN+crs*rmT-B*qsstar*(d0s+Dsf*qfstar+Dss*qsstar-rmN*rsN)+B*qshat*(d0s+Dsf*qfdouble+Dss*qshat-rmT*rsT)-gs*(B*qshat*(d0s+Dsf*qfdouble+Dss*qshat-rmT*rsT)-B*qshat*(d0s+Dsf*qfhat+Dss*qshat-rmT*rsT))
       ))}
conA_zMaxFrench <- function(params,q_vals) {
  with(q_vals,
       with(params,
            -ef-p0f*(Qf-qfhat)+p0f*(Qf-qfstar)+B*qfstar*(d0f+Dff*qfstar+Dfs*qsstar-rfN*rmN)-B*qfhat*(d0f+Dff*qfhat+Dfs*qsdouble-rfT*rmT)+gf*(B*qfhat*(d0f+Dff*qfhat+Dfs*qsdouble-rfT*rmT)-B*qfhat*(d0f+Dff*qfhat+Dfs*qshat-rfT*rmT))
       ))}
conA_zRange <- function(params,q_vals) {
  zRange <- conA_zMaxFrench(params,q_vals) - conA_zMinSwiss(params,q_vals)
  return(zRange)
}
