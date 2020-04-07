# game_functions_unconfined_nonlinear.R
# all functions are called by game_functions_unconfined()

# 4b Mathematica functions for depth, utility, and abstraction

unconA_nl_Us <- function(qs,qf,params,z) {
  with(params,
       p0s-Bs*qs*(((1-l)*PHIss)/(2*(h0s^2-PHIsf*qf-PHIss*qs+rm*rs))+(l*PHIss)/(2*dBs*sqrt(h0s^2-PHIsf*qf-PHIss*qs+rm*rs)))*(dBs-sqrt(h0s^2-PHIsf*qf-PHIss*qs+rm*rs))-(Bs*PHIss*qs*(l*(1-sqrt(h0s^2-PHIsf*qf-PHIss*qs+rm*rs)/dBs)+(1-l)*(log(dBs)-log(sqrt(h0s^2-PHIsf*qf-PHIss*qs+rm*rs)))))/(2*sqrt(h0s^2-PHIsf*qf-PHIss*qs+rm*rs))-Bs*(dBs-sqrt(h0s^2-PHIsf*qf-PHIss*qs+rm*rs))*(l*(1-sqrt(h0s^2-PHIsf*qf-PHIss*qs+rm*rs)/dBs)+(1-l)*(log(dBs)-log(sqrt(h0s^2-PHIsf*qf-PHIss*qs+rm*rs))))
  )}
unconA_nl_Uf <- function(qs,qf,params,z) {
  with(params,
       -Bf*qf*(((1-l)*PHIfs)/(2*(h0f^2-PHIff*qf-PHIfs*qs+rf*rm))+(l*PHIfs)/(2*dBf*sqrt(h0f^2-PHIff*qf-PHIfs*qs+rf*rm)))*(dBf-sqrt(h0f^2-PHIff*qf-PHIfs*qs+rf*rm))-(Bf*PHIfs*qf*(l*(1-sqrt(h0f^2-PHIff*qf-PHIfs*qs+rf*rm)/dBf)+(1-l)*(log(dBf)-log(sqrt(h0f^2-PHIff*qf-PHIfs*qs+rf*rm)))))/(2*sqrt(h0f^2-PHIff*qf-PHIfs*qs+rf*rm))
  )}
unconA_nl_ds <- function(qs,qf,params) {
  with(params,
       dBs-sqrt(h0s^2-PHIsf*qf-PHIss*qs+PHIrs*rm)
  )}
unconA_nl_df <- function(qs,qf,params) {
  with(params,
       dBf-sqrt(h0f^2-PHIff*qf-PHIfs*qs+PHIrf*rm)
  )}

# q values

unconA_nl_qeval <- function(params,unconAf_nl_q0,unconAf_nl_q2,qshat=NULL,qfhat=NULL) {
  # uconAf is for...??? function!
  qdouble <- !is.null(qshat) | !is.null(qfhat) # if qdouble, then need to input qshat and qfhat to functions
  if (!qdouble) { # need to supply qshat, qfhat for q_double
    q0 <- unconAf_nl_q0(params) # get initial q_hat
  } else {
    q0 <- unconAf_nl_q0(params,qshat,qfhat) # get initial q_hat
  }
  qs0 <- q0[1]
  qf0 <- q0[2]
  qs1 <- qs0
  qf1 <- qf0
  qs2 <- apply_constraints(qs1,c(0,params$Qs)) # contrain q_hat to bounds
  qf2 <- apply_constraints(qf1,c(0,params$Qf))
  for (i in 1:500) {
    if (qs1 != qs2 | qf1 != qf2) { # if constrained q_hat != initial q_hat
      # cat('qs1 =',qs1,'\nqs2 =',qs2,'\nqf1 =',qf1,'\nqf2 =',qf2,"\n")
      qs1 <- qs2 # store old
      qf1 <- qf2
      if (!qdouble) { # need to supply qshat, qfhat for q_double
        q2 <- unconAf_nl_q2(params,qs1,qf1)
      } else {
        q2 <- unconAf_nl_q2(params,qs1,qf1,qshat,qfhat)
      }
      qs2 <- apply_constraints(q2[1],c(0,params$Qs)) # update qshat with constrained qfhat
      qf2 <- apply_constraints(q2[2],c(0,params$Qf)) # update qfhat with constrained qshat
    } else {
      return(list(qs=qs2,qf=qf2))
    }
  }
  cat("q values not converging given params:\n")
  print(as.data.frame(params[1,]))
  return(list(qs=qs2,qf=qf2))
}

unconA_nl_qhat0 <- function(params) {
  # unconA_nl_qhat0
  # get roots for F1 = F2 = 0, solving for Qs, Qf
  first_best_equations <- function(x,params) {
    if (!check_aquifer_depleted(x[1],x[2],params,TRUE)) {
      # Continue with root finding if aquifer is not depleted
      F1<-with(params,
               p0s-Bf*x[2]*(((1-l)*PHIfs)/(2*(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2]))+(l*PHIfs)/(2*dBf*sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2])))*(dBf-sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2]))-Bs*x[1]*(((1-l)*PHIss)/(2*(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2]))+(l*PHIss)/(2*dBs*sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2])))*(dBs-sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2]))-(Bf*PHIfs*x[2]*(l*(1-sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2])/dBf)+(1-l)*(log(dBf)-log(sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2])))))/(2*sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2]))-(Bs*PHIss*x[1]*(l*(1-sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2])/dBs)+(1-l)*(log(dBs)-log(sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2])))))/(2*sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2]))-Bs*(dBs-sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2]))*(l*(1-sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2])/dBs)+(1-l)*(log(dBs)-log(sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2]))))
      )
      F2<-with(params,
               p0f-Bf*x[2]*(((1-l)*PHIff)/(2*(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2]))+(l*PHIff)/(2*dBf*sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2])))*(dBf-sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2]))-Bs*x[1]*(((1-l)*PHIsf)/(2*(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2]))+(l*PHIsf)/(2*dBs*sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2])))*(dBs-sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2]))-(Bf*PHIff*x[2]*(l*(1-sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2])/dBf)+(1-l)*(log(dBf)-log(sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2])))))/(2*sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2]))-Bf*(dBf-sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2]))*(l*(1-sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2])/dBf)+(1-l)*(log(dBf)-log(sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2]))))-(Bs*PHIsf*x[1]*(l*(1-sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2])/dBs)+(1-l)*(log(dBs)-log(sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2])))))/(2*sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2]))
      )
    } else {
      # Stop root finding if aquifer depleted or pumping is negative for either player
      F1 <- 0
      F2 <- 0
    }
    return(c(F1,F2))
  }
  q_hat <- rootSolve::multiroot(f=first_best_equations,start=c(params$Qs*0.9,params$Qf*0.9),params=params)$root
  return(q_hat)
}

unconA_nl_qhat2 <- function(params,qs1,qf1) {
  # unconA_nl_qhat2
  # this function calculates estimates pumping rates on the boundary points for first best -- ie, qs = 0 | qf = 0
  # get roots for Fqf0 = 0
  first_best_equations_qs2 <- function(x,params,qf1) {
    if (!check_aquifer_depleted(x,qf1,params,TRUE) & x >= 0) { # Continue with root finding if aquifer is not depleted, and pumping is positive for both players
      F1<-with(params,
               p0s+1/2*Bs*(-((2*l*(h0s^2+PHIrsT*rmT-2*PHIss*x[1]))/dBs)+(4*h0s^2*l+4*l*PHIrsT*rmT+PHIss*x[1]-7*l*PHIss*x[1])/sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1])+(-2*dBs*l*(h0s^2+PHIrsT*rmT)+dBs*(-1+3*l)*PHIss*x[1])/(h0s^2+PHIrsT*rmT-PHIss*x[1]))+(Bs*(-1+l)*(-2*h0s^2-2*PHIrsT*rmT+3*PHIss*x[1]+2*dBs*sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]))*(2*log(dBs)-log(h0s^2+PHIrsT*rmT-PHIss*x[1])))/(4*sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]))
      )
    } else { # Stop root finding if aquifer depleted or pumping is negative for either player
      F1 <- 0
    }
    return(c(F1))
  }
  qs2_hat <- rootSolve::multiroot(f=first_best_equations_qs2,start=qs1,params=params,qf1=qf1)$root

  # get roots for Fqs0 = 0
  first_best_equations_qf2 <- function(x,params,qs1) {
    if (!check_aquifer_depleted(qs1,x,params,TRUE) & x >= 0) { # Continue with root finding if aquifer is not depleted, and pumping is positive for both players
      F2<-with(params,
               p0f+1/2*Bf*(-((2*l*(h0f^2+PHIrfT*rmT-2*PHIff*x[1]))/dBf)+(4*h0f^2*l+4*l*PHIrfT*rmT+PHIff*x[1]-7*l*PHIff*x[1])/sqrt(h0f^2+PHIrfT*rmT-PHIff*x[1])+(-2*dBf*l*(h0f^2+PHIrfT*rmT)+dBf*(-1+3*l)*PHIff*x[1])/(h0f^2+PHIrfT*rmT-PHIff*x[1]))+(Bf*(-1+l)*(-2*h0f^2-2*PHIrfT*rmT+3*PHIff*x[1]+2*dBf*sqrt(h0f^2+PHIrfT*rmT-PHIff*x[1]))*(2*log(dBf)-log(h0f^2+PHIrfT*rmT-PHIff*x[1])))/(4*sqrt(h0f^2+PHIrfT*rmT-PHIff*x[1]))
      )
    } else { # Stop root finding if aquifer depleted or pumping is negative for either player
      F2 <- 0
    }
    return(c(F2))
  }
  qf2_hat <- rootSolve::multiroot(f=first_best_equations_qf2,start=qf1,params=params,qs1=qs1)$root

  possible_max <- data.frame(qs=c(qs1,qs2_hat,0,0),
                             qf=c(qf1,0,qf2_hat,0))
  Us <- mapply(unconA_nl_Us,qs=possible_max$qs,qf=possible_max$qf,params=list(params),z=0)
  Uf <- mapply(unconA_nl_Uf,qs=possible_max$qs,qf=possible_max$qf,params=list(params),z=0)
  idx <- which.max(Us+Uf)

  return(c(possible_max$qs[idx],possible_max$qf[idx]))
}

unconA_nl_qstar0 <- function(params) {
  # unconA_nl_qstar0
  # get roots for N1 = N2 = 0, solving for Qs, Qf
  nash_equations <- function(x,params) {
    if (!check_aquifer_depleted(x[1],x[2],params,FALSE) & x[1] >= 0 & x[2] >= 0) {
      # Continue with root finding if aquifer is not depleted, and pumping is positive for both players
      N1<-with(params,
               p0s+1/2*Bs*(-2*dBs*l-(2*l*(h0s^2+PHIrsN*rmN-2*PHIss*x[1]-PHIsf*x[2]))/dBs+(dBs*(-1+l)*PHIss*x[1])/(h0s^2+PHIrsN*rmN-PHIss*x[1]-PHIsf*x[2])+(PHIss*x[1]+l*(4*h0s^2+4*PHIrsN*rmN-7*PHIss*x[1]-4*PHIsf*x[2]))/sqrt(h0s^2+PHIrsN*rmN-PHIss*x[1]-PHIsf*x[2]))+(Bs*(-1+l)*(-2*h0s^2-2*PHIrsN*rmN+3*PHIss*x[1]+2*PHIsf*x[2]+2*dBs*sqrt(h0s^2+PHIrsN*rmN-PHIss*x[1]-PHIsf*x[2]))*(2*log(dBs)-log(h0s^2+PHIrsN*rmN-PHIss*x[1]-PHIsf*x[2])))/(4*sqrt(h0s^2+PHIrsN*rmN-PHIss*x[1]-PHIsf*x[2]))
      )
      N2<-with(params,
               p0f+1/2*Bf*(-((2*l*(h0f^2+PHIrfN*rmN-PHIfs*x[1]-2*PHIff*x[2]))/dBf)+(-2*dBf*l*(h0f^2+PHIrfN*rmN-PHIfs*x[1])+dBf*(-1+3*l)*PHIff*x[2])/(h0f^2+PHIrfN*rmN-PHIfs*x[1]-PHIff*x[2])+(PHIff*x[2]+l*(4*h0f^2+4*PHIrfN*rmN-4*PHIfs*x[1]-7*PHIff*x[2]))/sqrt(h0f^2+PHIrfN*rmN-PHIfs*x[1]-PHIff*x[2]))+(Bf*(-1+l)*(-2*h0f^2-2*PHIrfN*rmN+2*PHIfs*x[1]+3*PHIff*x[2]+2*dBf*sqrt(h0f^2+PHIrfN*rmN-PHIfs*x[1]-PHIff*x[2]))*(2*log(dBf)-log(h0f^2+PHIrfN*rmN-PHIfs*x[1]-PHIff*x[2])))/(4*sqrt(h0f^2+PHIrfN*rmN-PHIfs*x[1]-PHIff*x[2]))
      )
    } else {
      # Stop root finding if aquifer depleted or pumping is negative for either player
      N1 <- 0
      N2 <- 0
    }
    return(c(N1, N2))
  }
  q_star <- rootSolve::multiroot(f=nash_equations,start=c(params$Qs*0.9,params$Qf*0.9),params=params)$root
  return(q_star)
}

unconA_nl_qstar2 <- function(params,qs1,qf1) {
  # unconA_nl_qstar2
  # get roots for N1_q2 = 0, solving for Qs
  nash_equations_qs2 <- function(x,params,qf1) {
    if (!check_aquifer_depleted(x,qf1,params,FALSE) & x >= 0) { # Continue with root finding if aquifer is not depleted, and pumping is positive for both players
      N1<-with(params,
               p0s+1/2*Bs*(-((2*l*(h0s^2-PHIsf*qf1+PHIrsN*rmN-2*PHIss*x[1]))/dBs)+(-2*dBs*l*(h0s^2-PHIsf*qf1+PHIrsN*rmN)+dBs*(-1+3*l)*PHIss*x[1])/(h0s^2-PHIsf*qf1+PHIrsN*rmN-PHIss*x[1])+(PHIss*x[1]+l*(4*h0s^2-4*PHIsf*qf1+4*PHIrsN*rmN-7*PHIss*x[1]))/sqrt(h0s^2-PHIsf*qf1+PHIrsN*rmN-PHIss*x[1]))+(Bs*(-1+l)*(-2*h0s^2+2*PHIsf*qf1-2*PHIrsN*rmN+3*PHIss*x[1]+2*dBs*sqrt(h0s^2-PHIsf*qf1+PHIrsN*rmN-PHIss*x[1]))*(2*log(dBs)-log(h0s^2-PHIsf*qf1+PHIrsN*rmN-PHIss*x[1])))/(4*sqrt(h0s^2-PHIsf*qf1+PHIrsN*rmN-PHIss*x[1]))
      )
    } else { # Stop root finding if aquifer depleted or pumping is negative for either player
      N1 <- 0
    }
    return(c(N1))
  }
  qs2_star <- rootSolve::multiroot(f=nash_equations_qs2,start=qs1,params=params,qf1=qf1)$root

  # get roots for N2_q2 = 0, solving for Qf
  nash_equations_qf2 <- function(x,params,qs1) {
    if (!check_aquifer_depleted(qs1,x,params,FALSE) & x >= 0) { # Continue with root finding if aquifer is not depleted, and pumping is nonnegative for both players
      N2<-with(params,
               p0f+1/2*Bf*(-((2*l*(h0f^2-PHIfs*qs1+PHIrfN*rmN-2*PHIff*x[1]))/dBf)+(-2*dBf*l*(h0f^2-PHIfs*qs1+PHIrfN*rmN)+dBf*(-1+3*l)*PHIff*x[1])/(h0f^2-PHIfs*qs1+PHIrfN*rmN-PHIff*x[1])+(PHIff*x[1]+l*(4*h0f^2-4*PHIfs*qs1+4*PHIrfN*rmN-7*PHIff*x[1]))/sqrt(h0f^2-PHIfs*qs1+PHIrfN*rmN-PHIff*x[1]))+(Bf*(-1+l)*(-2*h0f^2+2*PHIfs*qs1-2*PHIrfN*rmN+3*PHIff*x[1]+2*dBf*sqrt(h0f^2-PHIfs*qs1+PHIrfN*rmN-PHIff*x[1]))*(2*log(dBf)-log(h0f^2-PHIfs*qs1+PHIrfN*rmN-PHIff*x[1])))/(4*sqrt(h0f^2-PHIfs*qs1+PHIrfN*rmN-PHIff*x[1]))
      )
    } else { # Stop root finding if aquifer depleted or pumping is negative for either player
      N2 <- 0
    }
    return(c(N2))
  }
  qf2_star <- rootSolve::multiroot(f=nash_equations_qf2,start=qf1,params=params,qs1=qs1)$root

  return(c(qs2_star,qf2_star))
}

unconA_nl_qdouble0 <- function(params,qshat,qfhat) {
  # unconA_nl_qdouble0
  # get roots for D1 = D2 = 0, solving for Qs, Qf
  cheat_equations <- function(x,params,qshat,qfhat) {# need to ensure aquifer is not fully depleted for any combination of qs, qf, qshat, qfhat, and pumping is nonnegative for both players
    if (!check_aquifer_depleted(x[1],x[2],params,TRUE) & # qs, qf
        !check_aquifer_depleted(qshat,x[2],params,TRUE) & # qshat, qf
        !check_aquifer_depleted(x[1],qfhat,params,TRUE) & # qf, qshat
        x[1] >= 0 & x[2] >= 0) {
      # Continue with root finding if aquifer is not depleted, and pumping is positive for both players
      D1<-with(params,
               gs*(p0s+1/2*Bs*(-((2*l*(h0s^2-PHIsf*qfhat+PHIrsT*rmT-2*PHIss*x[1]))/dBs)+(-2*dBs*l*(h0s^2-PHIsf*qfhat+PHIrsT*rmT)+dBs*(-1+3*l)*PHIss*x[1])/(h0s^2-PHIsf*qfhat+PHIrsT*rmT-PHIss*x[1])+(PHIss*x[1]+l*(4*h0s^2-4*PHIsf*qfhat+4*PHIrsT*rmT-7*PHIss*x[1]))/sqrt(h0s^2-PHIsf*qfhat+PHIrsT*rmT-PHIss*x[1]))+(Bs*(-1+l)*(-2*h0s^2+2*PHIsf*qfhat-2*PHIrsT*rmT+3*PHIss*x[1]+2*dBs*sqrt(h0s^2-PHIsf*qfhat+PHIrsT*rmT-PHIss*x[1]))*(2*log(dBs)-log(h0s^2-PHIsf*qfhat+PHIrsT*rmT-PHIss*x[1])))/(4*sqrt(h0s^2-PHIsf*qfhat+PHIrsT*rmT-PHIss*x[1])))+(1-gs)*(p0s+1/2*Bs*(-2*dBs*l-(2*l*(h0s^2+PHIrsT*rmT-2*PHIss*x[1]-PHIsf*x[2]))/dBs+(dBs*(-1+l)*PHIss*x[1])/(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2])+(PHIss*x[1]+l*(4*h0s^2+4*PHIrsT*rmT-7*PHIss*x[1]-4*PHIsf*x[2]))/sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2]))+(Bs*(-1+l)*(-2*h0s^2-2*PHIrsT*rmT+3*PHIss*x[1]+2*PHIsf*x[2]+2*dBs*sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2]))*(2*log(dBs)-log(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2])))/(4*sqrt(h0s^2+PHIrsT*rmT-PHIss*x[1]-PHIsf*x[2])))
      )
      D2<-with(params,
               gf*(p0f+Bf*l*(-dBf-(h0f^2-PHIfs*qshat+PHIrfT*rmT-2*PHIff*x[2])/dBf+(2*h0f^2-2*PHIfs*qshat+2*PHIrfT*rmT-3*PHIff*x[2])/sqrt(h0f^2-PHIfs*qshat+PHIrfT*rmT-PHIff*x[2]))+(Bf*(-1+l)*(-2*h0f^2+2*PHIfs*qshat-2*PHIrfT*rmT+3*PHIff*x[2]+2*dBf*sqrt(h0f^2-PHIfs*qshat+PHIrfT*rmT-PHIff*x[2]))*(log(dBf)-log(h)))/(2*sqrt(h0f^2-PHIfs*qshat+PHIrfT*rmT-PHIff*x[2])))+(1-gf)*(p0f+Bf*l*(-dBf-(h0f^2+PHIrfT*rmT-PHIfs*x[1]-2*PHIff*x[2])/dBf+(2*h0f^2+2*PHIrfT*rmT-2*PHIfs*x[1]-3*PHIff*x[2])/sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2]))+(Bf*(-1+l)*(-2*h0f^2-2*PHIrfT*rmT+2*PHIfs*x[1]+3*PHIff*x[2]+2*dBf*sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2]))*(log(dBf)-log(h)))/(2*sqrt(h0f^2+PHIrfT*rmT-PHIfs*x[1]-PHIff*x[2])))
      )
    } else {
      # Stop root finding if aquifer depleted or pumping is negative for either player
      D1 <- 0
      D2 <- 0
    }
    return(c(D1, D2))
  }
  q_double <- rootSolve::multiroot(f=cheat_equations,start=c(params$Qs*0.9,params$Qf*0.9),params=params,qshat=qshat,qfhat=qfhat)$root
  return(q_double)
}

unconA_nl_qdouble2 <- function(params,qs1,qf1,qshat,qfhat) {
  # unconA_nl_qdouble2
  # get roots for D1_q2 = 0, solving for Qs
  cheat_equations_qs2 <- function(x,params,qshat,qfhat,qf1) {
    if (!check_aquifer_depleted(x,qf1,params,TRUE) &
        !check_aquifer_depleted(x,qfhat,params,TRUE) &
        x >= 0) { # Continue with root finding if aquifer is not depleted, and pumping is positive for both players
      D1<-with(params,
               (1-gs)*(p0s-1/2*Bs*PHIss*x[1]*((1-l)/(h0s^2-PHIsf*qf1+PHIrsT*rmT-PHIss*x[1])+l/(dBs*sqrt(h0s^2-PHIsf*qf1+PHIrsT*rmT-PHIss*x[1])))*(dBs-sqrt(h0s^2-PHIsf*qf1+PHIrsT*rmT-PHIss*x[1]))-(Bs*PHIss*x[1]*(l-(l*sqrt(h0s^2-PHIsf*qf1+PHIrsT*rmT-PHIss*x[1]))/dBs-(-1+l)*(log(dBs)-1/2*log(h0s^2-PHIsf*qf1+PHIrsT*rmT-PHIss*x[1]))))/(2*sqrt(h0s^2-PHIsf*qf1+PHIrsT*rmT-PHIss*x[1]))-Bs*(dBs-sqrt(h0s^2-PHIsf*qf1+PHIrsT*rmT-PHIss*x[1]))*(l-(l*sqrt(h0s^2-PHIsf*qf1+PHIrsT*rmT-PHIss*x[1]))/dBs-(-1+l)*(log(dBs)-1/2*log(h0s^2-PHIsf*qf1+PHIrsT*rmT-PHIss*x[1]))))+gs*(p0s-1/2*Bs*PHIss*x[1]*((1-l)/(h0s^2-PHIsf*qfhat+PHIrsT*rmT-PHIss*x[1])+l/(dBs*sqrt(h0s^2-PHIsf*qfhat+PHIrsT*rmT-PHIss*x[1])))*(dBs-sqrt(h0s^2-PHIsf*qfhat+PHIrsT*rmT-PHIss*x[1]))-(Bs*PHIss*x[1]*(l-(l*sqrt(h0s^2-PHIsf*qfhat+PHIrsT*rmT-PHIss*x[1]))/dBs-(-1+l)*(log(dBs)-1/2*log(h0s^2-PHIsf*qfhat+PHIrsT*rmT-PHIss*x[1]))))/(2*sqrt(h0s^2-PHIsf*qfhat+PHIrsT*rmT-PHIss*x[1]))-Bs*(dBs-sqrt(h0s^2-PHIsf*qfhat+PHIrsT*rmT-PHIss*x[1]))*(l-(l*sqrt(h0s^2-PHIsf*qfhat+PHIrsT*rmT-PHIss*x[1]))/dBs-(-1+l)*(log(dBs)-1/2*log(h0s^2-PHIsf*qfhat+PHIrsT*rmT-PHIss*x[1]))))
      )
    } else { # Stop root finding if aquifer depleted or pumping is negative for either player
      D1 <- 0
    }
    return(c(D1))
  }
  qs2_double <- rootSolve::multiroot(f=cheat_equations_qs2,start=qs1,params=params,qshat=qshat,qfhat=qfhat,qf1=qf1)$root

  # get roots for D2_q2 = 0, solving for Qf
  cheat_equations_qf2 <- function(x,params,qshat,qfhat,qs1) {
    if (!check_aquifer_depleted(qs1,x,params,TRUE) &
        !check_aquifer_depleted(qshat,x,params,TRUE) &
        x >= 0) { # Continue with root finding if aquifer is not depleted, and pumping is positive for both players
      D2<-with(params,
               (1-gf)*(p0f-1/2*Bf*PHIff*x[1]*((1-l)/(h0f^2-PHIfs*qs1+PHIrfT*rmT-PHIff*x[1])+l/(dBf*sqrt(h0f^2-PHIfs*qs1+PHIrfT*rmT-PHIff*x[1])))*(dBf-sqrt(h0f^2-PHIfs*qs1+PHIrfT*rmT-PHIff*x[1]))-(Bf*PHIff*x[1]*(l-(l*sqrt(h0f^2-PHIfs*qs1+PHIrfT*rmT-PHIff*x[1]))/dBf-(-1+l)*(log(dBf)-1/2*log(h0f^2-PHIfs*qs1+PHIrfT*rmT-PHIff*x[1]))))/(2*sqrt(h0f^2-PHIfs*qs1+PHIrfT*rmT-PHIff*x[1]))-Bf*(dBf-sqrt(h0f^2-PHIfs*qs1+PHIrfT*rmT-PHIff*x[1]))*(l-(l*sqrt(h0f^2-PHIfs*qs1+PHIrfT*rmT-PHIff*x[1]))/dBf-(-1+l)*(log(dBf)-1/2*log(h0f^2-PHIfs*qs1+PHIrfT*rmT-PHIff*x[1]))))+gf*(p0f-1/2*Bf*PHIff*x[1]*((1-l)/(h0f^2-PHIfs*qshat+PHIrfT*rmT-PHIff*x[1])+l/(dBf*sqrt(h0f^2-PHIfs*qshat+PHIrfT*rmT-PHIff*x[1])))*(dBf-sqrt(h0f^2-PHIfs*qshat+PHIrfT*rmT-PHIff*x[1]))-(Bf*PHIff*x[1]*(l-(l*sqrt(h0f^2-PHIfs*qshat+PHIrfT*rmT-PHIff*x[1]))/dBf-(-1+l)*(log(dBf)-1/2*log(h0f^2-PHIfs*qshat+PHIrfT*rmT-PHIff*x[1]))))/(2*sqrt(h0f^2-PHIfs*qshat+PHIrfT*rmT-PHIff*x[1]))-Bf*(dBf-sqrt(h0f^2-PHIfs*qshat+PHIrfT*rmT-PHIff*x[1]))*(l-(l*sqrt(h0f^2-PHIfs*qshat+PHIrfT*rmT-PHIff*x[1]))/dBf-(-1+l)*(log(dBf)-1/2*log(h0f^2-PHIfs*qshat+PHIrfT*rmT-PHIff*x[1]))))
      )
    } else { # Stop root finding if aquifer depleted or pumping is negative for either player
      D2 <- 0
    }
    return(c(D2))
  }
  qf2_double <- rootSolve::multiroot(f=cheat_equations_qf2,start=qf1,params=params,qshat=qshat,qfhat=qfhat,qs1=qs1)$root

  return(c(qs2_double,qf2_double))
}


# z constraints
unconA_nl_zMinSwiss <- function(params,q_vals) {
  with(q_vals, # q_vals should include qsstar,qfstar,qshat,qfhat,qsdouble,qfdouble
       with(params,
            es+p0s*(Qs-qshat)-p0s*(Qs-qsstar)-crs*rmN-Bs*qsstar*(dBs-sqrt(h0s^2-PHIsf*qfstar-PHIss*qsstar+PHIsrN*rmN))+crs*rmT+Bs*qshat*(dBs-sqrt(h0s^2-PHIsf*qfdouble-PHIss*qshat+PHIsrT*rmT))-gs*(Bs*qshat*(dBs-sqrt(h0s^2-PHIsf*qfdouble-PHIss*qshat+PHIsrT*rmT))-Bs*qshat*(dBs-sqrt(h0s^2-PHIsf*qfhat-PHIss*qshat+PHIsrT*rmT)))
       ))}
unconA_nl_zMaxFrench <- function(params,q_vals) {
  with(q_vals,
       with(params,
            -ef-p0f*(Qf-qfhat)+p0f*(Qf-qfstar)+Bf*qfstar*(dBf-sqrt(h0f^2-PHIff*qfstar-PHIfs*qsstar+PHIfrN*rmN))-Bf*qfhat*(dBf-sqrt(h0f^2-PHIff*qfhat-PHIfs*qsdouble+PHIfrT*rmT))+gf*(Bf*qfhat*(dBf-sqrt(h0f^2-PHIff*qfhat-PHIfs*qsdouble+PHIfrT*rmT))-Bf*qfhat*(dBf-sqrt(h0f^2-PHIff*qfhat-PHIfs*qshat+PHIfrT*rmT)))
       ))}
unconA_nl_zRange <- function(params,q_vals) {
  zRange <- unconA_nl_zMaxFrench(params,q_vals) - unconA_nl_zMinSwiss(params,q_vals)
  return(zRange)
}


#' Check if unconfined aquifer is fully depleted
#'
#' Is pumping enough to fully deplete the aquifer for either player?
#' @param params parameter data.frame with single row
#' @param treaty boolean value that determines the context for evaluation (e.g., for Nash should be \code{F}, for First Best should be \code{T})
#' @param qs pumping from player S
#' @param qf pumping from player F
#' @keywords internal
#' @return
#' Returns boolean value, TRUE if the aquifer has been fully depleted for some amount of pumping.
check_aquifer_depleted <- function(qs,qf,params,treaty) {
  if (treaty) {
    names(params)[match(c("rmT","PHIsrT","PHIfrT"),names(params))] <- c("rm","PHIsr","PHIfr")
  } else {
    names(params)[match(c("rmN","PHIsrN","PHIfrN"),names(params))] <- c("rm","PHIsr","PHIfr")
  }
  phis<-with(params,
             h0s^2-PHIsf*qf-PHIss*qs+PHIsr*rm
  )

  phif<-with(params,
             h0f^2-PHIff*qf-PHIfs*qs+PHIfr*rm
  )

  if (phis < 0 | phif < 0) {
    aquifer_depleted <- TRUE
  } else {
    aquifer_depleted <- FALSE
  }
  return(aquifer_depleted)
}




