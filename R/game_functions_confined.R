# game_functions_confined.R
# 4 Mathematica functions and game parameter evaluations
# 4x function to evaluate contraints
# 4a function to evaluate abstraction rates
# 4b Mathematica functions for depth, utility, and abstraction
# 4c Mathematica functions for z constraints

#' Apply interval contraints to numeric value
apply_constraints <- function(x,interval) {
  if (x<interval[1]) {
    x <- interval[1]
  } else if (x>interval[2]) {
    x <- interval[2]
  }
  return(x)
}

MM <- list()
#
# 4a function to evaluate abstraction rates
MM_qeval <- function(params,MMf_qs0,MMf_qf0,MMf_qs2,MMf_qf2) {
  qs0 <- MMf_qs0(params) # get initial q_hat
  qf0 <- MMf_qf0(params)
  qs1 <- qs0
  qf1 <- qf0
  qs2 <- apply_constraints(qs1,c(0,params$Qs)) # contrain q_hat to bounds
  qf2 <- apply_constraints(qf1,c(0,params$Qf))
  for (i in 1:500) {
    if (qs1 != qs2 | qf1 != qf2) { # if constrained q_hat != initial q_hat
      # cat('qs1 =',qs1,'\nqs2 =',qs2,'\nqf1 =',qf1,'\nqf2 =',qf2,"\n")
      qs1 <- qs2 # store old
      qf1 <- qf2
      qs2 <- apply_constraints(MMf_qs2(params,qf1),c(0,params$Qs)) # update qshat with constrained qfhat
      qf2 <- apply_constraints(MMf_qf2(params,qs1),c(0,params$Qf)) # update qfhat with constrained qshat
    } else {
      return(list(qs=qs2,qf=qf2))
    }
  }
  cat("q values not converging given params:\n")
  print(as.data.frame(params[1,]))
  return(list(qs=qs2,qf=qf2))
}

# 4b Mathematica functions for depth, utility, and abstraction

MM_Us <- function(qs,qf,params,z) {
  with(params,
       -es-p0s*(-qs+Qs)-crs*rm-B*qs*(d0s+Dsf*qf+Dss*qs-rm*rs)+z
  )}
MM_Uf <- function(qs,qf,params,z) {
  with(params,
       -ef-p0f*(-qf+Qf)-B*qf*(d0f+Dff*qf+Dfs*qs-rf*rm)-z
  )}

MM_ds <- function(qs,qf,params) {
  with(params,
       d0s+Dsf*qf+Dss*qs-rm*rs
  )}
MM_df <- function(qs,qf,params) {
  with(params,
       d0f+Dff*qf+Dfs*qs-rf*rm
  )}

MM_qshat0 <- function(params) {
  with(params,
       (Dfs*p0f+Dsf*p0f-2*Dff*p0s+B*(2*d0s*Dff-d0f*(Dfs+Dsf)+rmT*(Dfs*rfT+Dsf*rfT-2*Dff*rsT)))/(B*(Dfs^2+2*Dfs*Dsf+Dsf^2-4*Dff*Dss))
  )}
MM_qfhat0 <- function(params) {
  with(params,
       (-2*Dss*p0f+(Dfs+Dsf)*p0s+B*(-d0s*(Dfs+Dsf)+2*d0f*Dss-2*Dss*rfT*rmT+(Dfs+Dsf)*rmT*rsT))/(B*(Dfs^2+2*Dfs*Dsf+Dsf^2-4*Dff*Dss))
  )}
MM_qshat2 <- function(params,qf1) {
  with(params,
       (p0s-B*(d0s+Dfs*qf1+Dsf*qf1-rmT*rsT))/(2*B*Dss)
  )}
MM_qfhat2 <- function(params,qs1) {
  with(params,
       (p0f-B*(d0f+Dfs*qs1+Dsf*qs1-rfT*rmT))/(2*B*Dff)
  )}
# qi_star
MM_qsstar0 <- function(params) {
  with(params,
       (2*B*d0s*Dff-B*d0f*Dsf+Dsf*p0f-2*Dff*p0s+B*Dsf*rfN*rmN-2*B*Dff*rmN*rsN)/(B*Dfs*Dsf-4*B*Dff*Dss)
  )}
MM_qfstar0 <- function(params) {
  with(params,
       (-2*Dss*p0f+Dfs*p0s+B*(-d0s*Dfs+2*d0f*Dss-2*Dss*rfN*rmN+Dfs*rmN*rsN))/(B*(Dfs*Dsf-4*Dff*Dss))
  )}
MM_qsstar2 <- function(params,qf1) {
  with(params,
       (p0s-B*(d0s+Dsf*qf1-rmN*rsN))/(2*B*Dss)
  )}
MM_qfstar2 <- function(params,qs1) {
  with(params,
       (p0f-B*(d0f+Dfs*qs1-rfN*rmN))/(2*B*Dff)
  )}
# qi_double
MM_qsdouble0 <- function(params) { # in call: include params %>% mutate(qfhat=qfhat,qshat=qshat)
  with(params,
       (Dsf*(p0f-gs*p0f)-2*Dff*p0s+B*(2*d0s*Dff+d0f*Dsf*(-1+gs)+2*Dff*Dsf*gs*qfhat-Dfs*Dsf*gf*qshat+Dfs*Dsf*gf*gs*qshat+Dsf*rfT*rmT-Dsf*gs*rfT*rmT-2*Dff*rmT*rsT))/(B*(-4*Dff*Dss+Dfs*Dsf*(-1+gf)*(-1+gs)))
  )}
MM_qfdouble0 <- function(params) { # in call: include params %>% mutate(qfhat=qfhat,qshat=qshat)
  with(params,
       (-2*Dss*p0f-Dfs*(-1+gf)*p0s+B*(2*d0f*Dss+d0s*Dfs*(-1+gf)-Dfs*Dsf*gs*qfhat+Dfs*Dsf*gf*gs*qfhat+2*Dfs*Dss*gf*qshat-2*Dss*rfT*rmT+Dfs*rmT*rsT-Dfs*gf*rmT*rsT))/(B*(-4*Dff*Dss+Dfs*Dsf*(-1+gf)*(-1+gs)))
  )}
MM_qsdouble2 <- function(params,qf1) {
  with(params,
       (p0s-B*(d0s+Dsf*(qf1-gs*qf1+gs*qfhat)-rmT*rsT))/(2*B*Dss)
  )}
MM_qfdouble2 <- function(params,qs1) {
  with(params,
       (p0f-B*(d0f+Dfs*(qs1-gf*qs1+gf*qshat)-rfT*rmT))/(2*B*Dff)
  )}

# 4c Mathematica functions for z constraints
# z constraints
MM_zMinSwiss <- function(params,q_vals) {
  with(q_vals, # q_vals should include qsstar,qfstar,qshat,qfhat,qsdouble,qfdouble
       with(params,
            es+p0s*(Qs-qshat)-p0s*(Qs-qsstar)-crs*rmN+crs*rmT-B*qsstar*(d0s+Dsf*qfstar+Dss*qsstar-rmN*rsN)+B*qshat*(d0s+Dsf*qfdouble+Dss*qshat-rmT*rsT)-gs*(B*qshat*(d0s+Dsf*qfdouble+Dss*qshat-rmT*rsT)-B*qshat*(d0s+Dsf*qfhat+Dss*qshat-rmT*rsT))
       ))}
MM_zMaxFrench <- function(params,q_vals) {
  with(q_vals,
       with(params,
            -ef-p0f*(Qf-qfhat)+p0f*(Qf-qfstar)+B*qfstar*(d0f+Dff*qfstar+Dfs*qsstar-rfN*rmN)-B*qfhat*(d0f+Dff*qfhat+Dfs*qsdouble-rfT*rmT)+gf*(B*qfhat*(d0f+Dff*qfhat+Dfs*qsdouble-rfT*rmT)-B*qfhat*(d0f+Dff*qfhat+Dfs*qshat-rfT*rmT))
       ))}
MM_zRange <- function(params,q_vals) {
  zRange <- MM_zMaxFrench(params,q_vals) - MM_zMinSwiss(params,q_vals)
  return(zRange)
}
