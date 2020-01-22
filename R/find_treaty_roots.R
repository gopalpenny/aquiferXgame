# find_treaty_roots.R

# 3 Root finding
# 3a Find roots for data frame of parameters
# 3b-i Define fixed point root finding algorithm
# 3b-i Define newton_raphson root finding algorithm

# 3 Root finding
# 3a Find roots for data frame of parameters
## ROOT FINDING
get_roots <- function(params_df,treaty_cases,x_var,y_var,root_init,eps_val) {
  roots0 <- NULL

  root_init <- range(params_df[,y_var[2]])

  x_vals_contain_0 <- treaty_cases %>% rename_("x"=x_var[1]) %>%
    select_("x",y_var[1],"zRange") %>% group_by(x) %>%
    mutate(zRange_eps=zRange-eps_val,
           min_zRange_eps=min(zRange_eps),
           max_zRange_eps=max(zRange_eps),
           contains_zero=min_zRange_eps < 0 & max_zRange_eps >0) %>%
    filter(contains_zero) %>% pull(x) %>% unique()
  p_idx <- match(y_var,names(params_df))
  params_df[,y_var] <- 0
  params_df[,c("es","ef")] <- eps_val


  # params_subset <- distinct(params_df) %>% filter(demand_ratio %in% x_vals_contain_0)
  params_subset <- distinct(params_df) %>% filter(eval(rlang::parse_expr(paste0(x_var[1],"%in% x_vals_contain_0"))))
  params_list <- split(params_subset,1:dim(params_subset)[1])
  # function() {
  roots0 <-tryCatch({
    cat("Running Newton-Raphson root solver... ")
    tibble(sapply(params_list,newton_raphson,interval=root_init,y_var=y_var[-1])) %>%
      set_names(y_var[1]) %>% mutate(sfc=paste0("eps=",eps_val))
  },error=function(cond){
    cat("failed.\nUsing Fixed Point root solver... ")
    tibble(sapply(params_list,fixedpoint,fun=fp_fun,x0=mean(root_init),y_var=y_var[-1])) %>%
      set_names(y_var[1]) %>% mutate(sfc=paste0("eps=",eps_val))
  },warning=function(cond){

  },finally={
    cat("done.\n")
  })
  #  if (root_solver=="newton_raphson") {
  #   roots0 <- tibble(sapply(params_list,newton_raphson,interval=root_init,y_var=y_var)) %>%
  #     set_names(y_var[1]) %>% mutate(sfc=paste0("eps=",eps_val))
  # }
  # if (is.null(roots0)) {
  #   roots0 <- tibble(sapply(params_list,fixedpoint,fun=fp_fun,x0=mean(root_init),y_var=y_var)) %>%
  #     set_names(y_var[1]) %>% mutate(sfc=paste0("eps=",eps_val))
  # } else {stop("must correctly specify root_solver as fixed_point or newton_raphson")}
  return(cbind(params_subset %>% select(match(x_var,names(params_subset))),roots0))
}

# 3b-i Define fixed point root finding algorithm
fixedpoint <- function(fun,x0,y_var, params,tol=1e-07, niter=500){
  ## fixed-point algorithm to find x such that fun(x) == x
  ## assume that fun is a function of a single variable
  ## x0 is the initial guess of the fixed point
  ## y_var is the parameters to vary (they must be equal)
  xold <- x0
  xnew <- fun(xold,y_var,params)
  for (i in 1:niter) {
    xold <- xnew
    xnew <- fun(xold,y_var,params)
    if ( abs((xnew-xold)) < tol | is.nan(xnew))
      return(xnew)
  }
  return(xnew)
  cat("exceeded allowed number of iterations")
}

fp_fun <- function(x,y_var,params) {
  p_idx <- match(y_var,names(params))
  params[1,p_idx] <- x
  return(x-0.5*evaluate_agreement(params)$zRange)
}

# 3b-i Define newton_raphson root finding algorithm
# newton_raphson(interval=c(0,1),params_list[[1]],y_var)
# newton_raphson(interval=interval,params=params_list[[2]],y_var=y_var)
newton_raphson <- function(interval,params,y_var) {
  return(uniroot(nr_fun,interval,params=params,y_var=y_var)$root)
}

nr_fun <- function(x,params,y_var) {
  p_idx <- match(y_var,names(params))
  params[p_idx] <- x
  return(evaluate_agreement(params)$zRange)
}
