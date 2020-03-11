# game_functions_unconfined.R


#' Evaluate the treaty in unconfined aquifer
#'
#' Evaluate whether or not the treaty will be made.
#' @param params Parameter list (or data.frame with 1 row) containing
#' necessary parameters to evaluate the agreement in an unconfined case.
#' @details
#' Evaluate the treaty given social, economic, and geophysical parameters.
#' @return
#' Returns a 1-row tibble containing pumping, utility ranges needed for the treaty,
#' and whether or not there is a treaty (i.e., if zRange > 0)
#' @importFrom magrittr %>%
#' @export
#' @examples
#' evaluate_treaty(default_params)
evaluate_treaty_unconfined <- function(params) {
  # (eval_out <- evaluate_treaty(params_default()))
  # this function calculates abstraction from the game,
  # and determines whether or not a treaty is signed
  if(nrow(params)!=1){
    stop("This is an error message because params not 1 dimension")
  }
  params$Bs <- params$B
  params$Bf <- params$B
  params$phi0s <- params$h0s^2
  params$phi0f <- params$h0f^2

  q_hat <- unconA_qhat(params)
  q_star <- unconA_qstar(params)
  q_double <- unconA_qdouble(params)

  q_vals <- tibble::tibble(qshat=q_hat[1],qfhat=q_hat[2],
                           qsstar=q_star[1],qfstar=q_star[2],
                           qsdouble=q_double[1],qfdouble=q_double[2])

  # # get z constraints
  zMaxFrench_calc <- unconA_zMaxFrench(params,q_vals)
  zMinSwiss_calc <- unconA_zMinSwiss(params,q_vals)
  zRange_calc <- zMaxFrench_calc - zMinSwiss_calc
  treaty <- ifelse(zRange_calc>0,"Y","N")
  return(tibble::tibble(treaty=treaty,zRange=zRange_calc,
                zMinSwiss=zMinSwiss_calc,zMaxFrench=zMaxFrench_calc) %>%
           dplyr::bind_cols(q_vals))
}


# 4b Mathematica functions for depth, utility, and abstraction

unconA_Us <- function(qs,qf,params,z) {
  with(params,
       -es-p0s*(-qs+Qs)-crs*rm-Bs*qs*(dBs-sqrt[h0s^2-PHIsf*qf-PHIss*qs+PHIrs*rm])+z
  )}
unconA_Uf <- function(qs,qf,params,z) {
  with(params,
       -ef-p0f*(-qf+Qf)-Bf*qf*(dBf-sqrt[h0f^2-PHIff*qf-PHIfs*qs+PHIrf*rm])-z
  )}
unconA_ds <- function(qs,qf,params) {
  with(params,
       dBs-sqrt[h0s^2-PHIsf*qf-PHIss*qs+PHIrs*rm]
  )}
unconA_df <- function(qs,qf,params) {
  with(params,
       dBf-sqrt[h0f^2-PHIff*qf-PHIfs*qs+PHIrf*rm]
  )}

# q values

unconA_qhat <- function(params) {
  # get roots for F1 = F2 = 0, solving for Qs, Qf
  first_best_equations <- with(params,function(x) {
    F1<--Bs*dBs+p0s-(Bf*PHIfs*x[2])/(2*sqrt[phi0f+PHIrf*rm-PHIfs*x[1]-PHIff*x[2]])+(Bs*(2*phi0s+2*PHIrs*rm-3*PHIss*x[1]-2*PHIsf*x[2]))/(2*sqrt[phi0s+PHIrs*rm-PHIss*x[1]-PHIsf*x[2]])
    F2<--Bf*dBf+p0f+(Bf*(2*phi0f+2*PHIrf*rm-2*PHIfs*x[1]-3*PHIff*x[2]))/(2*sqrt[phi0f+PHIrf*rm-PHIfs*x[1]-PHIff*x[2]])-(Bs*PHIsf*x[1])/(2*sqrt[phi0s+PHIrs*rm-PHIss*x[1]-PHIsf*x[2]])
    return(list(F1=F1,F2=F2))
  })
  q_hat <- rootSolve::multiroot(f=first_best_equations,start=c(params$Qs*0.9,params$Qf*0.9))
  return(q_hat)
}

unconA_qstar <- function(params) {
  # get roots for N1 = N2 = 0, solving for Qs, Qf
  nash_equations <- with(params,function(x) {
    N1<--Bs*dBs+p0s+(Bs*(2*phi0s+2*PHIrs*rm-3*PHIss*x[1]-2*PHIsf*x[2]))/(2*sqrt[phi0s+PHIrs*rm-PHIss*x[1]-PHIsf*x[2]])
    N2<--Bf*dBf+p0f+(Bf*(2*phi0f+2*PHIrf*rm-2*PHIfs*x[1]-3*PHIff*x[2]))/(2*sqrt[phi0f+PHIrf*rm-PHIfs*x[1]-PHIff*x[2]])
    return(list(N1=N1, N2=N2))
  })
  q_star <- rootSolve::multiroot(f=nash_equations,start=c(params$Qs*0.9,params$Qf*0.9))
  return(q_star)
}

unconA_qdouble <- function(params) {
  # get roots for D1 = D2 = 0, solving for Qs, Qf
  cheat_equations <- with(params,function(x) {
    D1<-gs*(p0s-(Bs*PHIss*x[1])/(2*sqrt[phi0s-PHIsf*qfhat+PHIrs*rmT-PHIss*x[1]])-Bs*(dBs-sqrt[phi0s-PHIsf*qfhat+PHIrs*rmT-PHIss*x[1]]))+(1-gs)*(p0s-(Bs*PHIss*x[1])/(2*sqrt[phi0s+PHIrs*rmT-PHIss*x[1]-PHIsf*x[2]])-Bs*(dBs-sqrt[phi0s+PHIrs*rmT-PHIss*x[1]-PHIsf*x[2]]))
    D2<-gf*(p0f-(Bf*PHIff*x[2])/(2*sqrt[phi0f-PHIfs*qshat+PHIrf*rmT-PHIff*x[2]])-Bf*(dBf-sqrt[phi0f-PHIfs*qshat+PHIrf*rmT-PHIff*x[2]]))+(1-gf)*(p0f-(Bf*PHIff*x[2])/(2*sqrt[phi0f+PHIrf*rmT-PHIfs*x[1]-PHIff*x[2]])-Bf*(dBf-sqrt[phi0f+PHIrf*rmT-PHIfs*x[1]-PHIff*x[2]]))
    return(list(D1=D1, D2=D2))
  })
  q_double <- rootSolve::multiroot(f=cheat_equations,start=c(params$Qs*0.9,params$Qf*0.9))
  return(q_double)
}


# z constraints
unconA_zMinSwiss <- function(params,q_vals) {
  with(q_vals, # q_vals should include qsstar,qfstar,qshat,qfhat,qsdouble,qfdouble
       with(params,
            es+p0s*(Qs-qshat)-p0s*(Qs-qsstar)-crs*rmN-Bs*qsstar*(dBs-sqrt[h0s^2-PHIsf*qfstar-PHIss*qsstar+PHIrs*rmN])+crs*rmT+Bs*qshat*(dBs-sqrt[h0s^2-PHIsf*qfdouble-PHIss*qshat+PHIrs*rmT])-gs*(Bs*qshat*(dBs-sqrt[h0s^2-PHIsf*qfdouble-PHIss*qshat+PHIrs*rmT])-Bs*qshat*(dBs-sqrt[h0s^2-PHIsf*qfhat-PHIss*qshat+PHIrs*rmT]))
       ))}
unconA_zMaxFrench <- function(params,q_vals) {
  with(q_vals,
       with(params,
            -ef-p0f*(Qf-qfhat)+p0f*(Qf-qfstar)+Bf*qfstar*(dBf-sqrt[h0f^2-PHIff*qfstar-PHIfs*qsstar+PHIrf*rmN])-Bf*qfhat*(dBf-sqrt[h0f^2-PHIff*qfhat-PHIfs*qsdouble+PHIrf*rmT])+gf*(Bf*qfhat*(dBf-sqrt[h0f^2-PHIff*qfhat-PHIfs*qsdouble+PHIrf*rmT])-Bf*qfhat*(dBf-sqrt[h0f^2-PHIff*qfhat-PHIfs*qshat+PHIrf*rmT]))
       ))}
unconA_zRange <- function(params,q_vals) {
  zRange <- unconA_zMaxFrench(params,q_vals) - unconA_zMinSwiss(params,q_vals)
  return(zRange)
}
