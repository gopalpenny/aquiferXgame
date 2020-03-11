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
  q_double <- unconA_qdouble(params,qshat=q_hat[1],qfhat=q_hat[2])

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
  first_best_equations <- function(x,params) {
    F1<-with(params,
             -Bs*dBs+p0s-(Bf*PHIfs*x[2])/(2*sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2]))+(Bs*(2*h0s^2+2*PHIrsT*rmT-3*PHIss*x[1]-2*PHIsf*x[2]))/(2*sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2]))
    )
    F2<-with(params,
             -Bf*dBf+p0f+(Bf*(2*h0f^2+2*PHIrfT*rmT-2*PHIfs*x[1]-3*PHIff*x[2]))/(2*sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2]))-(Bs*PHIsf*x[1])/(2*sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2]))
    )
    return(c(F1,F2))
  }
  q_hat <- rootSolve::multiroot(f=first_best_equations,start=c(params$Qs*0.9,params$Qf*0.9),params=params)$root
  return(q_hat)
}

unconA_qstar <- function(params) {
  # get roots for N1 = N2 = 0, solving for Qs, Qf
  nash_equations <- function(x,params) {
    N1<-with(params,
             -Bs*dBs+p0s+(Bs*(2*h0s^2+2*PHIrsN*rmN-3*PHIss*x[1]-2*PHIsf*x[2]))/(2*sqrt(h0s^2+PHIrsN*rmN-PHIss*x[1]-PHIsf*x[2]))
    )
    N2<-with(params,
             -Bf*dBf+p0f+(Bf*(2*h0f^2+2*PHIrfN*rmN-2*PHIfs*x[1]-3*PHIff*x[2]))/(2*sqrt(h0f^2+PHIrfN*rmN-PHIfs*x[1]-PHIff*x[2]))
    )
    return(c(N1, N2))
  }
  q_star <- rootSolve::multiroot(f=nash_equations,start=c(params$Qs*0.9,params$Qf*0.9),params=params)$root
  return(q_star)
}

unconA_qdouble <- function(params,qshat,qfhat,max_vals) {
  # get roots for D1 = D2 = 0, solving for Qs, Qf
  cheat_equations <- function(x,params,qshat,qfhat,max_vals) {
    if (in_range(x[1],c(0,max_vals$qs)) & in_range(x[2],c(0,max_vals$qf))) {
      # Continue with root finding if proposed values are in the valid range
      D1<-with(params,
               gs*(p0s-(Bs*PHIss*x[1])/(2*sqrt(h0s^2-PHIsf*qfhat+PHIrsT*rmT-PHIss*x[1]))-Bs*(dBs-sqrt(h0s^2-PHIsf*qfhat+PHIrsT*rmT-PHIss*x[1])))+(1-gs)*(p0s-(Bs*PHIss*x[1])/(2*sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2]))-Bs*(dBs-sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2])))
      )
      D2<-with(params,
               gf*(p0f-(Bf*PHIff*x[2])/(2*sqrt(h0f^2-PHIfs*qshat+PHIrfT*rmT-PHIff*x[2]))-Bf*(dBf-sqrt(h0f^2-PHIfs*qshat+PHIrfT*rmT-PHIff*x[2])))+(1-gf)*(p0f-(Bf*PHIff*x[2])/(2*sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2]))-Bf*(dBf-sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2])))
      )
    } else {
      # Stop root finding if either value is outside the acceptable range
      D1 <- 0
      D2 <- 0
    }
    return(c(D1, D2))
  }
  q_double <- rootSolve::multiroot(f=cheat_equations,start=c(params$Qs*0.9,params$Qf*0.9),params=params,qshat=qshat,qfhat=qfhat,max_vals=max_vals)$root
  return(q_double)
}


# z constraints
unconA_zMinSwiss <- function(params,q_vals) {
  with(q_vals, # q_vals should include qsstar,qfstar,qshat,qfhat,qsdouble,qfdouble
       with(params,
            es+p0s*(Qs-qshat)-p0s*(Qs-qsstar)-crs*rmN-Bs*qsstar*(dBs-sqrt(h0s^2-PHIsf*qfstar-PHIss*qsstar+PHIrsN*rmN))+crs*rmT+Bs*qshat*(dBs-sqrt(h0s^2-PHIsf*qfdouble-PHIss*qshat+PHIrsT*rmT))-gs*(Bs*qshat*(dBs-sqrt(h0s^2-PHIsf*qfdouble-PHIss*qshat+PHIrsT*rmT))-Bs*qshat*(dBs-sqrt(h0s^2-PHIsf*qfhat-PHIss*qshat+PHIrsT*rmT)))
       ))}
unconA_zMaxFrench <- function(params,q_vals) {
  with(q_vals,
       with(params,
            -ef-p0f*(Qf-qfhat)+p0f*(Qf-qfstar)+Bf*qfstar*(dBf-sqrt(h0f^2-PHIff*qfstar-PHIfs*qsstar+PHIrfN*rmN))-Bf*qfhat*(dBf-sqrt(h0f^2-PHIff*qfhat-PHIfs*qsdouble+PHIrfT*rmT))+gf*(Bf*qfhat*(dBf-sqrt(h0f^2-PHIff*qfhat-PHIfs*qsdouble+PHIrfT*rmT))-Bf*qfhat*(dBf-sqrt(h0f^2-PHIff*qfhat-PHIfs*qshat+PHIrfT*rmT)))
       ))}
unconA_zRange <- function(params,q_vals) {
  zRange <- unconA_zMaxFrench(params,q_vals) - unconA_zMinSwiss(params,q_vals)
  return(zRange)
}


# maximum pumping gvalues





