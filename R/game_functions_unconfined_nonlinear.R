# game_functions_unconfined_nonlinear.R
# all functions are called by game_functions_unconfined()

# 4b Mathematica functions for depth, utility, and abstraction

unconA_nl_Us <- function(qs,qf,params,z) {
  with(params,
       -c0rs-es+p0s*(qs-Qs)-crs*rm+z+Bs*qs*(l*(-dBs+sqrt(h0s^2-PHIsf*qf-PHIss*qs+PHIsr*rm))+dBs*(-1+l)*(log(dBs)-1/2*log(h0s^2-PHIsf*qf-PHIss*qs+PHIsr*rm)))
  )}
unconA_nl_Uf <- function(qs,qf,params,z) {
  with(params,
       -ef+p0f*(qf-Qf)-z+Bf*qf*(l*(-dBf+sqrt(h0f^2-PHIff*qf-PHIfs*qs+PHIfr*rm))+dBf*(-1+l)*(log(dBf)-1/2*log(h0f^2-PHIff*qf-PHIfs*qs+PHIfr*rm)))
  )}
unconA_nl_ds <- function(qs,qf,params) {
  with(params,
       dBs-sqrt(h0s^2-PHIsf*qf-PHIss*qs+PHIsr*rm)
  )}
unconA_nl_df <- function(qs,qf,params) {
  with(params,
       dBf-sqrt(h0f^2-PHIff*qf-PHIfs*qs+PHIfr*rm)
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
               1/2*(2*p0s-(Bf*l*PHIfs*x[2])/sqrt(h0f^2+PHIfrT*rmT-PHIfs*x[1]-PHIff*x[2])-(Bf*dBf*(-1+l)*PHIfs*x[2])/(-h0f^2-PHIfrT*rmT+PHIfs*x[1]+PHIff*x[2])-(Bs*l*PHIss*x[1])/sqrt(h0s^2+PHIsrT*rmT-PHIss*x[1]-PHIsf*x[2])-(Bs*dBs*(-1+l)*PHIss*x[1])/(-h0s^2-PHIsrT*rmT+PHIss*x[1]+PHIsf*x[2])+2*Bs*l*(-dBs+sqrt(h0s^2+PHIsrT*rmT-PHIss*x[1]-PHIsf*x[2]))+Bs*dBs*(-1+l)*(2*log(dBs)-log(h0s^2+PHIsrT*rmT-PHIss*x[1]-PHIsf*x[2])))
      )
      F2<-with(params,
               1/2*(2*p0f-(Bf*l*PHIff*x[2])/sqrt(h0f^2+PHIfrT*rmT-PHIfs*x[1]-PHIff*x[2])-(Bf*dBf*(-1+l)*PHIff*x[2])/(-h0f^2-PHIfrT*rmT+PHIfs*x[1]+PHIff*x[2])-(Bs*l*PHIsf*x[1])/sqrt(h0s^2+PHIsrT*rmT-PHIss*x[1]-PHIsf*x[2])-(Bs*dBs*(-1+l)*PHIsf*x[1])/(-h0s^2-PHIsrT*rmT+PHIss*x[1]+PHIsf*x[2])+2*Bf*l*(-dBf+sqrt(h0f^2+PHIfrT*rmT-PHIfs*x[1]-PHIff*x[2]))+Bf*dBf*(-1+l)*(2*log(dBf)-log(h0f^2+PHIfrT*rmT-PHIfs*x[1]-PHIff*x[2])))
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
  # get roots for F1_q2 = 0
  first_best_equations_qs2 <- function(x,params,qf1) {
    if (!check_aquifer_depleted(x,qf1,params,TRUE) & x >= 0) { # Continue with root finding if aquifer is not depleted, and pumping is positive for both players
      F1<-with(params,
               1/2*(2*p0s-(Bf*l*PHIfs*qf1)/sqrt(h0f^2-PHIff*qf1+PHIfrT*rmT-PHIfs*x[1])-(Bf*dBf*(-1+l)*PHIfs*qf1)/(-h0f^2+PHIff*qf1-PHIfrT*rmT+PHIfs*x[1])-(Bs*l*PHIss*x[1])/sqrt(h0s^2-PHIsf*qf1+PHIsrT*rmT-PHIss*x[1])-(Bs*dBs*(-1+l)*PHIss*x[1])/(-h0s^2+PHIsf*qf1-PHIsrT*rmT+PHIss*x[1])+2*Bs*l*(-dBs+sqrt(h0s^2-PHIsf*qf1+PHIsrT*rmT-PHIss*x[1]))+Bs*dBs*(-1+l)*(2*log(dBs)-log(h0s^2-PHIsf*qf1+PHIsrT*rmT-PHIss*x[1])))
      )
    } else { # Stop root finding if aquifer depleted or pumping is negative for either player
      F1 <- 0
    }
    return(c(F1))
  }
  qs2_hat_qf0 <- rootSolve::multiroot(f=first_best_equations_qs2,start=qs1,params=params,qf1=0)$root # estimate qs when qf=0
  qs2_hat_qfQf <- rootSolve::multiroot(f=first_best_equations_qs2,start=qs1,params=params,qf1=params$Qf)$root # estimate qs when qf=Qf

  # get roots for F2_q2 = 0
  first_best_equations_qf2 <- function(x,params,qs1) {
    if (!check_aquifer_depleted(qs1,x,params,TRUE) & x >= 0) { # Continue with root finding if aquifer is not depleted, and pumping is positive for both players
      F2<-with(params,
               1/2*(2*p0f-(Bf*l*PHIff*x[1])/sqrt(h0f^2-PHIfs*qs1+PHIfrT*rmT-PHIff*x[1])-(Bf*dBf*(-1+l)*PHIff*x[1])/(-h0f^2+PHIfs*qs1-PHIfrT*rmT+PHIff*x[1])-(Bs*l*PHIsf*qs1)/sqrt(h0s^2-PHIss*qs1+PHIsrT*rmT-PHIsf*x[1])-(Bs*dBs*(-1+l)*PHIsf*qs1)/(-h0s^2+PHIss*qs1-PHIsrT*rmT+PHIsf*x[1])+2*Bf*l*(-dBf+sqrt(h0f^2-PHIfs*qs1+PHIfrT*rmT-PHIff*x[1]))+Bf*dBf*(-1+l)*(2*log(dBf)-log(h0f^2-PHIfs*qs1+PHIfrT*rmT-PHIff*x[1])))
      )
    } else { # Stop root finding if aquifer depleted or pumping is negative for either player
      F2 <- 0
    }
    return(c(F2))
  }
  qf2_hat_qs0 <- rootSolve::multiroot(f=first_best_equations_qf2,start=qf1,params=params,qs1=0)$root # estimate qf when qs=0
  qf2_hat_qsQs <- rootSolve::multiroot(f=first_best_equations_qf2,start=qf1,params=params,qs1=params$Qs)$root # estimate qf when qs=Qs

  q_FB_matrix <- rbind(c(qs1,qf1),
                       c(qs2_hat_qf0,0),
                       c(qs2_hat_qfQf,params$Qf),
                       c(0,qf2_hat_qs0),
                       c(params$Qs,qf2_hat_qsQs),
                       c(0,0))
  possible_max <- data.frame(qs=sapply(q_FB_matrix[,1],apply_constraints,interval=c(0,params$Qs)),
                             qf=sapply(q_FB_matrix[,2],apply_constraints,interval=c(0,params$Qf)))
  possible_max <- possible_max[!duplicated(possible_max),]

  # fix parameters to represent the treaty scenario
  params_treaty <- params
  names(params_treaty)[match(c("rmT","PHIsrT","PHIfrT"),names(params_treaty))] <- c("rm","PHIsr","PHIfr")
  # get utility for each of the scenarios for both players
  possible_max$Us <- mapply(unconA_nl_Us,qs=possible_max$qs,qf=possible_max$qf,params=list(params_treaty),z=0)
  possible_max$Uf <- mapply(unconA_nl_Uf,qs=possible_max$qs,qf=possible_max$qf,params=list(params_treaty),z=0)
  possible_max$joint <- possible_max$Us+possible_max$Uf
  idx <- which.max(possible_max$joint)[1]

  return(c(possible_max$qs[idx],possible_max$qf[idx]))
}

unconA_nl_qstar0 <- function(params) {
  # unconA_nl_qstar0
  # get roots for N1 = N2 = 0, solving for Qs, Qf
  nash_equations <- function(x,params) {
    if (!check_aquifer_depleted(x[1],x[2],params,FALSE) & x[1] >= 0 & x[2] >= 0) {
      # Continue with root finding if aquifer is not depleted, and pumping is positive for both players
      N1<-with(params,
               p0s+1/2*Bs*(-2*dBs*l+(dBs*(-1+l)*PHIss*x[1])/(h0s^2+PHIsrN*rmN-PHIss*x[1]-PHIsf*x[2])+(l*(2*h0s^2+2*PHIsrN*rmN-3*PHIss*x[1]-2*PHIsf*x[2]))/sqrt(h0s^2+PHIsrN*rmN-PHIss*x[1]-PHIsf*x[2]))+1/2*Bs*dBs*(-1+l)*(2*log(dBs)-log(h0s^2+PHIsrN*rmN-PHIss*x[1]-PHIsf*x[2]))
      )
      N2<-with(params,
               p0f+(Bf*dBf*(-1+l)*PHIff*x[2])/(2*(h0f^2+PHIfrN*rmN-PHIfs*x[1]-PHIff*x[2]))-(Bf*l*PHIff*x[2])/(2*sqrt(h0f^2+PHIfrN*rmN-PHIfs*x[1]-PHIff*x[2]))+Bf*l*(-dBf+sqrt(h0f^2+PHIfrN*rmN-PHIfs*x[1]-PHIff*x[2]))+Bf*dBf*(-1+l)*(log(dBf)-1/2*log(h0f^2+PHIfrN*rmN-PHIfs*x[1]-PHIff*x[2]))
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
               p0s+(Bs*dBs*(-1+l)*PHIss*x[1])/(2*(h0s^2-PHIsf*qf1+PHIsrN*rmN-PHIss*x[1]))-(Bs*l*PHIss*x[1])/(2*sqrt(h0s^2-PHIsf*qf1+PHIsrN*rmN-PHIss*x[1]))+Bs*l*(-dBs+sqrt(h0s^2-PHIsf*qf1+PHIsrN*rmN-PHIss*x[1]))+Bs*dBs*(-1+l)*(log(dBs)-1/2*log(h0s^2-PHIsf*qf1+PHIsrN*rmN-PHIss*x[1]))
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
               p0f+(Bf*dBf*(-1+l)*PHIff*x[1])/(2*(h0f^2-PHIfs*qs1+PHIfrN*rmN-PHIff*x[1]))-(Bf*l*PHIff*x[1])/(2*sqrt(h0f^2-PHIfs*qs1+PHIfrN*rmN-PHIff*x[1]))+Bf*l*(-dBf+sqrt(h0f^2-PHIfs*qs1+PHIfrN*rmN-PHIff*x[1]))+Bf*dBf*(-1+l)*(log(dBf)-1/2*log(h0f^2-PHIfs*qs1+PHIfrN*rmN-PHIff*x[1]))
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
               gs*(p0s+(Bs*dBs*(-1+l)*PHIss*x[1])/(2*(h0s^2-PHIsf*qfhat+PHIsrT*rmT-PHIss*x[1]))-(Bs*l*PHIss*x[1])/(2*sqrt(h0s^2-PHIsf*qfhat+PHIsrT*rmT-PHIss*x[1]))+Bs*l*(-dBs+sqrt(h0s^2-PHIsf*qfhat+PHIsrT*rmT-PHIss*x[1]))+Bs*dBs*(-1+l)*(log(dBs)-1/2*log(h0s^2-PHIsf*qfhat+PHIsrT*rmT-PHIss*x[1])))+(1-gs)*(p0s+(Bs*dBs*(-1+l)*PHIss*x[1])/(2*(h0s^2+PHIsrT*rmT-PHIss*x[1]-PHIsf*x[2]))-(Bs*l*PHIss*x[1])/(2*sqrt(h0s^2+PHIsrT*rmT-PHIss*x[1]-PHIsf*x[2]))+Bs*l*(-dBs+sqrt(h0s^2+PHIsrT*rmT-PHIss*x[1]-PHIsf*x[2]))+Bs*dBs*(-1+l)*(log(dBs)-1/2*log(h0s^2+PHIsrT*rmT-PHIss*x[1]-PHIsf*x[2])))
      )
      D2<-with(params,
               gf*(p0f+(Bf*dBf*(-1+l)*PHIff*x[2])/(2*(h0f^2-PHIfs*qshat+PHIfrT*rmT-PHIff*x[2]))-(Bf*l*PHIff*x[2])/(2*sqrt(h0f^2-PHIfs*qshat+PHIfrT*rmT-PHIff*x[2]))+Bf*l*(-dBf+sqrt(h0f^2-PHIfs*qshat+PHIfrT*rmT-PHIff*x[2]))+Bf*dBf*(-1+l)*(log(dBf)-1/2*log(h0f^2-PHIfs*qshat+PHIfrT*rmT-PHIff*x[2])))+(1-gf)*(p0f+(Bf*dBf*(-1+l)*PHIff*x[2])/(2*(h0f^2+PHIfrT*rmT-PHIfs*x[1]-PHIff*x[2]))-(Bf*l*PHIff*x[2])/(2*sqrt(h0f^2+PHIfrT*rmT-PHIfs*x[1]-PHIff*x[2]))+Bf*l*(-dBf+sqrt(h0f^2+PHIfrT*rmT-PHIfs*x[1]-PHIff*x[2]))+Bf*dBf*(-1+l)*(log(dBf)-1/2*log(h0f^2+PHIfrT*rmT-PHIfs*x[1]-PHIff*x[2])))
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
               (1-gs)*(p0s+(Bs*dBs*(-1+l)*PHIss*x[1])/(2*(h0s^2-PHIsf*qf1+PHIsrT*rmT-PHIss*x[1]))-(Bs*l*PHIss*x[1])/(2*sqrt(h0s^2-PHIsf*qf1+PHIsrT*rmT-PHIss*x[1]))+Bs*l*(-dBs+sqrt(h0s^2-PHIsf*qf1+PHIsrT*rmT-PHIss*x[1]))+Bs*dBs*(-1+l)*(log(dBs)-1/2*log(h0s^2-PHIsf*qf1+PHIsrT*rmT-PHIss*x[1])))+gs*(p0s+(Bs*dBs*(-1+l)*PHIss*x[1])/(2*(h0s^2-PHIsf*qfhat+PHIsrT*rmT-PHIss*x[1]))-(Bs*l*PHIss*x[1])/(2*sqrt(h0s^2-PHIsf*qfhat+PHIsrT*rmT-PHIss*x[1]))+Bs*l*(-dBs+sqrt(h0s^2-PHIsf*qfhat+PHIsrT*rmT-PHIss*x[1]))+Bs*dBs*(-1+l)*(log(dBs)-1/2*log(h0s^2-PHIsf*qfhat+PHIsrT*rmT-PHIss*x[1])))
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
               (1-gf)*(p0f+(Bf*dBf*(-1+l)*PHIff*x[1])/(2*(h0f^2-PHIfs*qs1+PHIfrT*rmT-PHIff*x[1]))-(Bf*l*PHIff*x[1])/(2*sqrt(h0f^2-PHIfs*qs1+PHIfrT*rmT-PHIff*x[1]))+Bf*l*(-dBf+sqrt(h0f^2-PHIfs*qs1+PHIfrT*rmT-PHIff*x[1]))+Bf*dBf*(-1+l)*(log(dBf)-1/2*log(h0f^2-PHIfs*qs1+PHIfrT*rmT-PHIff*x[1])))+gf*(p0f+(Bf*dBf*(-1+l)*PHIff*x[1])/(2*(h0f^2-PHIfs*qshat+PHIfrT*rmT-PHIff*x[1]))-(Bf*l*PHIff*x[1])/(2*sqrt(h0f^2-PHIfs*qshat+PHIfrT*rmT-PHIff*x[1]))+Bf*l*(-dBf+sqrt(h0f^2-PHIfs*qshat+PHIfrT*rmT-PHIff*x[1]))+Bf*dBf*(-1+l)*(log(dBf)-1/2*log(h0f^2-PHIfs*qshat+PHIfrT*rmT-PHIff*x[1])))
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
            es-p0s*qshat+p0s*qsstar-crs*rmN+crs*rmT+Bs*l*(dBs*(qshat-qsstar)+qsstar*sqrt(h0s^2-PHIsf*qfstar-PHIss*qsstar+PHIsrN*rmN)+qshat*(-sqrt(h0s^2-PHIsf*qfdouble-PHIss*qshat+PHIsrT*rmT)+gs*sqrt(h0s^2-PHIsf*qfdouble-PHIss*qshat+PHIsrT*rmT)-gs*sqrt(h0s^2-PHIsf*qfhat-PHIss*qshat+PHIsrT*rmT)))-1/2*Bs*dBs*(-1+l)*(2*(qshat-qsstar)*log(dBs)+qsstar*log(h0s^2-PHIsf*qfstar-PHIss*qsstar+PHIsrN*rmN)+(-1+gs)*qshat*log(h0s^2-PHIsf*qfdouble-PHIss*qshat+PHIsrT*rmT)-gs*qshat*log(h0s^2-PHIsf*qfhat-PHIss*qshat+PHIsrT*rmT))
       )
  )
}
unconA_nl_zMaxFrench <- function(params,q_vals) {
  with(q_vals,
       with(params,
            -ef+p0f*(qfhat-qfstar)+Bf*l*(dBf*(-qfhat+qfstar)-qfstar*sqrt(h0f^2-PHIff*qfstar-PHIfs*qsstar+PHIfrN*rmN)+qfhat*(sqrt(h0f^2-PHIff*qfhat-PHIfs*qsdouble+PHIfrT*rmT)-gf*sqrt(h0f^2-PHIff*qfhat-PHIfs*qsdouble+PHIfrT*rmT)+gf*sqrt(h0f^2-PHIff*qfhat-PHIfs*qshat+PHIfrT*rmT)))+1/2*Bf*dBf*(-1+l)*(2*(qfhat-qfstar)*log(dBf)+qfstar*log(h0f^2-PHIff*qfstar-PHIfs*qsstar+PHIfrN*rmN)+(-1+gf)*qfhat*log(h0f^2-PHIff*qfhat-PHIfs*qsdouble+PHIfrT*rmT)-gf*qfhat*log(h0f^2-PHIff*qfhat-PHIfs*qshat+PHIfrT*rmT))
       )
  )
}
unconA_nl_zRange <- function(params,q_vals) {
  zRange <- unconA_nl_zMaxFrench(params,q_vals) - unconA_nl_zMinSwiss(params,q_vals)
  return(zRange)
}




