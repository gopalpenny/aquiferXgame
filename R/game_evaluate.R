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
#' @param aquifer_type Aquifer type as "confined" or "unconfined". If not specified
#' (default) \code{check_params} is run and used to determine the aquifer type.
#' @details
#' Evaluate the treaty given social, economic, and geophysical parameters
#' @return
#' Returns a 1-row tibble containing pumping, utility ranges needed for the treaty,
#' and whether or not there is a treaty (i.e., if zRange > 0).
#'
#' If the aquifer in the
#' game is confined (see \code{?check_params} for how aquifer type is determined)  the solution is
#' exact. If the aquifer is unconfined, the solution is calculated numerically using \code{multiroot}
#' from the \code{rootSolve} package, using default tolerance parameters. This means that there
#' could be small errors in the results for \code{zMinSwiss} and \code{zMaxFrench}. For unconfined aquifers,
#' \code{zRange} is rounded to the 6th decimal place to minimize the effect of numerical errors on
#' the treaty outcome. If necessary, it can be recalculated as \code{zRange = zMaxFrench - zMinSwiss}.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' evaluate_treaty(example_params_confined)
#' evaluate_treaty(example_params_unconfined)
evaluate_treaty <- function(params, aquifer_type = NULL) {
  # (eval_out <- evaluate_treaty(params_default()))
  # this function calculates abstraction from the game,
  # and determines whether or not a treaty is signed
  if(nrow(params)!=1){
    stop("This is an error message because params not 1 dimension")
  }

  if (is.null(aquifer_type)) {
    aq_null <- TRUE
    aquifer_type <- check_params(params)
  } else {
    aq_null <- FALSE
  }

  if (aquifer_type == "confined") {
    treaty_results <- evaluate_treaty_confined(params)
  } else if (aquifer_type == "unconfined") {
    treaty_results <- evaluate_treaty_unconfined(params)
    # check for aquifer depletion ONLY if aquifer was not specified (if aquifer is specified, assume call comes from evaluate_treaty_cases)
    if (aq_null) {
      if (all(c("AD_fb","AD_nash","AD_cheat") %in% names(treaty_results))) {
        warning(paste("The aquifer was fully depleted for at least one player in the",
                      with(treaty_results,paste(c("First Best","Nash","Cheat")[c(AD_fb,AD_nash,AD_cheat)],collapse=", ")),"scenario(s)"))
      }
    }
  } else {
    stop("aquifer must be confined or unconfined, as specified by Dxx or PHIxx parameters.")
  }

  return(treaty_results)
}

#' Evaluate multiple treaty scenarios
#'
#' Evaluate whether or not the treaty will be made under multiple scenarios
#' @param params_df Data.frame of parameters with each row sent to \code{evaluate_treaty()}.
#' @param return_criteria Character string containing letters that indicate output variables. See details.
#' @param progress_bar Show a progress bar. Useful in unconfined aquifers for large N
#' @details
#' Evaluate the treaty given multiple combinations of social, economic,
#' and geophysical parameters.
#' This function takes a data.frame of parameters,
#' evaluates each row to see if a treaty is signed,
#' and returns a tibble with the results and original params.
#'
#' The parameter \code{return_criteria} can contain the following letters:
#' \itemize{
#' \item p - will return only parameters different from default. Otherwise all parameters returned
#' \item a - return all parameters (i.e., it's redundant to include a AND p)
#' \item u - return utilities of each player
#' \item d - return depth to water table for each player
#' }
#' @return
#' Returns a \code{tibble} containing z-values needed for the treaty
#' and whether or not there is a treaty (i.e., if zRange > 0), as well as output specified by \code{return_criteria}.
#'
#' If the aquifer in the game is confined (see \code{?check_params} for how aquifer type is determined)  the solution is
#' exact. If the aquifer is unconfined, the solution is calculated numerically using \code{multiroot}
#' from the \code{rootSolve} package, using default tolerance parameters. This means that there
#' could be small errors in the results for \code{zMinSwiss} and \code{zMaxFrench}. For unconfined aquifers,
#' \code{zRange} is rounded to the 6th decimal place to minimize the effect of numerical errors on
#' the treaty outcome. If necessary, it can be recalculated as \code{zRange = zMaxFrench - zMinSwiss}.
#'
#' In unconfined aquifers, it is possible that the game converges on a result that allows the aquifer
#' to be fully depleted for as least one of the players (hi < 0). In this case, the results will included
#' three additional columns:
#' \code{AD_fb,AD_nash,AD_cheat}, representing logical values that indicate in which scenario the aquifer was depleted
#' (first best, nash, or cheat). Unfortunately, this model is unable to resolve such a scenario which represents
#' another type of game: a game of chicken.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' library(genevoisgame)
#' evaluate_treaty_cases(rbind(example_params_confined,example_params_confined))
#' evaluate_treaty_cases(rbind(example_params_unconfined,example_params_unconfined),"qudp")
evaluate_treaty_cases <- function(params_df,return_criteria="qp",progress_bar = FALSE) {
  aquifer_type <- check_params(params_df)
  params_list <- split(params_df,1:dim(params_df)[1])
  eval_results_list <- list()
  if (progress_bar) pb <- utils::txtProgressBar(min = 0, max = nrow(params_df), style = 3)
  for (i in 1:nrow(params_df)) {
    eval_results_list[[i]] <- evaluate_treaty(params_list[[i]],aquifer_type=aquifer_type) #lapply(params_list,evaluate_treaty,aquifer_type=aquifer_type)
    if (progress_bar) utils::setTxtProgressBar(pb, i)
  }
  if (progress_bar) close(pb)
  eval_results <- dplyr::bind_rows(eval_results_list)
  q_vals_list <- split(eval_results %>% dplyr::select(dplyr::starts_with("q")),1:dim(eval_results)[1])
  # eval_results_treat <- eval_results %>% select(treaty,zRange,zMinSwiss,zMaxFrench)
  eval_return <- eval_results[,c("treaty","zRange","zMinSwiss","zMaxFrench")] %>%
    dplyr::bind_cols(eval_results %>% dplyr::select(dplyr::starts_with("AD_")))
  if (any(grepl("q",return_criteria))) { # return abstraction rates
    eval_return <- eval_return %>% dplyr::bind_cols(eval_results%>% dplyr::select(dplyr::starts_with("qs"),dplyr::starts_with("qf")))
  }
  if (any(grepl("u",return_criteria))) { # return utilities
    u_vals <- do.call(rbind,mapply(evaluate_treaty_utility,params=params_list,
                                   q_vals=q_vals_list,aquifer_type=aquifer_type,SIMPLIFY=FALSE))
    eval_return <- eval_return %>% dplyr::bind_cols(u_vals %>% dplyr::select(dplyr::starts_with("Us"),dplyr::starts_with("Uf"),dplyr::everything()))
  }
  if (any(grepl("d",return_criteria))) { # return depth to water table
    d_vals <- do.call(rbind,mapply(evaluate_treaty_depths,params=params_list,
                                   q_vals=q_vals_list,aquifer_type=aquifer_type,SIMPLIFY=FALSE))
    eval_return <- eval_return %>% dplyr::bind_cols(d_vals %>% dplyr::select(dplyr::starts_with("ds"),dplyr::starts_with("df"),dplyr::everything()))
  }
  if (any(grepl("a",return_criteria))) { # return all parameters
    eval_return <- eval_return %>% dplyr::bind_cols(params_df)
  } else if (max(grepl("p",return_criteria))==1) { # identify parameters that do not vary AND are equal to default value
    name_table <- sapply(params_df,function(x) length(unique(x)))
    vars_stable <- names(name_table[name_table == 1])
    params_cases <- params_df[,-match(vars_stable,names(params_df))] # remove identified parameters
    eval_return <- eval_return %>% dplyr::bind_cols(params_cases)
  }
  # check for aquifer depletion
  if (all(c("AD_fb","AD_nash","AD_cheat") %in% names(eval_return))) {
    warning(paste("The aquifer was fully depleted for at least one player in some parameter sets in the",
                  with(eval_return,paste(c("First Best","Nash","Cheat")[c(any(AD_fb),any(AD_nash),any(AD_cheat))],collapse=", ")),"scenario(s)"))
  }
  return(eval_return)
}

#' Evaluate treaty utility
#'
#' Evaluate utility given (single) treaty parameters
#' @inheritParams evaluate_treaty_depths
#' @keywords internal
evaluate_treaty_utility <- function(params, q_vals, aquifer_type) {
  DsrN <- DsrT <- DfrN <- DfrT <- PHIsrN <- PHIsrT <- PHIfrN <- PHIfrT <- NULL
  # this function calculates utilities, given parameters and abstraction
  if(dim(params)[1]!=1){
    stop("This is an error message because params not 1 dimension")
  }
  for (v in 1:ncol(q_vals)) {assign(names(q_vals)[v], q_vals[[v]])}
  # get utilities
  if (aquifer_type == "confined") {
    get_Us <- conA_Us
    get_Uf <- conA_Uf
    params_treaty <- params
    names(params_treaty)[match(c("rmT","DsrT","DfrT"),names(params_treaty))] <- c("rm","Dsr","Dfr")
    params_notreaty <- params
    names(params_notreaty)[match(c("rmN","DsrN","DfrN"),names(params_notreaty))] <- c("rm","Dsr","Dfr")
  } else {
    params$Bs <- params$B
    params$Bf <- params$B
    if (params$l == 1) {
      get_Us <- unconA_lin_Us
      get_Uf <- unconA_lin_Uf
    } else {
      get_Us <- unconA_nl_Us
      get_Uf <- unconA_nl_Uf
    }
    params_treaty <- params
    names(params_treaty)[match(c("rmT","PHIsrT","PHIfrT"),names(params_treaty))] <- c("rm","PHIsr","PHIfr")
    params_notreaty <- params
    names(params_notreaty)[match(c("rmN","PHIsrN","PHIfrN"),names(params_notreaty))] <- c("rm","PHIsr","PHIfr")
  }
  Us_hat <- get_Us(qs=q_vals$qshat,qf=q_vals$qfhat,params_treaty,z=0)
  Uf_hat <- get_Uf(qs=q_vals$qshat,qf=q_vals$qfhat,params_treaty,z=0)
  Us_star <- get_Us(qs=q_vals$qsstar,qf=q_vals$qfstar,params_notreaty,z=0)
  Uf_star <- get_Uf(qs=q_vals$qsstar,qf=q_vals$qfstar,params_notreaty,z=0)
  Us_double <- get_Us(qs=q_vals$qsdouble,qf=q_vals$qfhat,params_treaty,z=0)
  Uf_double <- get_Uf(qs=q_vals$qshat,qf=q_vals$qfdouble,params_treaty,z=0)
  Us_hat_double <- get_Us(qs=q_vals$qshat,qf=q_vals$qfdouble,params_treaty,z=0)
  Uf_hat_double <- get_Uf(qs=q_vals$qsdouble,qf=q_vals$qfhat,params_treaty,z=0)
  u_vals <- tibble::tibble(Us_hat=Us_hat,Uf_hat=Uf_hat,
                           Us_star=Us_star,Uf_star=Uf_star,
                           Us_double=Us_double,Uf_double=Uf_double,
                           Us_hat_double=Us_hat_double,Uf_hat_double=Uf_hat_double)
  return(u_vals)
}

#' Evaluate treaty depths
#'
#' Evaluate water table depth given (single) treaty parameters
#' @inheritParams evaluate_treaty
#' @param q_vals list of pumping rates
#' @keywords internal
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
    params_treaty <- params
    names(params_treaty)[match(c("rmT","DsrT","DfrT"),names(params_treaty))] <- c("rm","Dsr","Dfr")
    params_notreaty <- params
    names(params_notreaty)[match(c("rmN","DsrN","DfrN"),names(params_notreaty))] <- c("rm","Dsr","Dfr")
  } else {
    params$Bs <- params$B
    params$Bf <- params$B
    if (params$l == 1) {
      get_ds <- unconA_lin_ds
      get_df <- unconA_lin_df
    } else {
      get_ds <- unconA_nl_ds
      get_df <- unconA_nl_df
    }
    params_treaty <- params
    names(params_treaty)[match(c("rmT","PHIsrT","PHIfrT"),names(params_treaty))] <- c("rm","PHIsr","PHIfr")
    params_notreaty <- params
    names(params_notreaty)[match(c("rmN","PHIsrN","PHIfrN"),names(params_notreaty))] <- c("rm","PHIsr","PHIfr")
  }
  ds_hat <- get_ds(qs=q_vals$qshat,qf=q_vals$qfhat,params_treaty)
  df_hat <- get_df(qs=q_vals$qshat,qf=q_vals$qfhat,params_treaty)
  ds_star <- get_ds(qs=q_vals$qsstar,qf=q_vals$qfstar,params_notreaty)
  df_star <- get_df(qs=q_vals$qsstar,qf=q_vals$qfstar,params_notreaty)
  ds_double <- get_ds(qs=q_vals$qsdouble,qf=q_vals$qfhat,params_treaty)
  df_double <- get_df(qs=q_vals$qshat,qf=q_vals$qfdouble,params_treaty)
  ds_hat_double <- get_ds(qs=q_vals$qsdouble,qf=q_vals$qfhat,params_treaty)
  df_hat_double <- get_df(qs=q_vals$qshat,qf=q_vals$qfdouble,params_treaty)
  d_vals <- tibble::tibble(ds_hat=ds_hat,df_hat=df_hat,
                           ds_star=ds_star,df_star=df_star,
                           ds_double=ds_double,df_double=df_double,
                           ds_hat_double=ds_hat_double,df_hat_double=df_hat_double)
  return(d_vals)
}

#' Get contour lines
#'
#' Get contour lines, using a wrapper for contourLines
#' @param df A data.frame containing x, y, and z columns. x and y must form a raster,
#'     meaning every x must be represented for each y, and vice versa.
#' @param levels A vector of values at which contours should be plotted. If used, nlevels is ignored.
#' @param ... If x, y, and z are directly specified here, df will be ignored.
#' @importFrom magrittr %>%
#' @importFrom grDevices contourLines
#' @export
#' @examples
#' library(ggplot2)
#'
#' df <- expand.grid(x=-10:10,y=-10:10)
#' df$z <- df$x^2
#' cl <- get_contours(df,levels=5)
#' unique(cl$level)
#' ggplot() +
#'   geom_raster(data=df,aes(x,y,fill=z)) +
#'   geom_path(data=cl,aes(x,y,group=line))
#'
#' cl <- get_contours(df,levels=c(15,20,60))
#' unique(cl$level)
#'
#' df <- expand.grid(x=seq(-5,5,length.out=20),y=seq(-5,5,length.out=20))
#' df$z <- sqrt(df$x^2+df$y^2)
#' cl <- get_contours(df,levels=seq(2,10,by=2))
#' ggplot() +
#'   geom_raster(data=df,aes(x,y,fill=z)) +
#'   geom_path(data=cl,aes(x,y,group=line)) + coord_equal()
get_contours <- function(df = NULL, levels = 0, ...) {
  params <- list(...)
  if (all(c("x","y","z") %in% names(params))) {
    df$x <- params$x
    df$y <- params$y
    df$z <- params$z
  } else if (!all(c("x","y","z") %in% names(df))) {
    stop("df must contain columns for x, y, and z")
  }

  grid_points_all <- merge(expand.grid(x=unique(df$x),y=unique(df$y)), df,by=c("x","y"))
  z_values_check <- grid_points_all$z
  df_error <-  any(is.na(z_values_check) & !is.nan(z_values_check))
  if (df_error) {
    stop("in df, every x, y combination must have a z value.")
  }
  nx <- length(unique(df$x))
  ny <- length(unique(df$y))
  # if (nlevels > nx | nlevels > ny) {
  #   stop("nlevels (",nlevels,") must be less than nx (",nx,") and ny (",ny,").")
  # }
  df <- df[order(df$y,df$x),]
  xmat <- matrix(df$x,ncol=ny)
  ymat <- matrix(df$y,ncol=ny)
  zmat <- matrix(df$z,ncol=ny)
  x_seq <- xmat[,1]
  y_seq <- ymat[1,]

  # get contour lines
  cl_list <- contourLines(x_seq,y_seq,zmat,levels=levels)
  if (length(cl_list) > 0 ) {
    for (i in 1:length(cl_list)) {
      cl_list[[i]]$line <- i
    }

    # bind contour lines
    cl <- do.call(rbind,lapply(cl_list,function(l) data.frame(x=l$x,y=l$y,level=l$level, line=l$line)))
    cl$i <- 1:nrow(cl)
    cl$level_factor <- as.factor(as.character(cl$level))

  } else {
    warning(paste0("No contours found. Level range: (",min(levels),",",max(levels()),"). ",
                   "z range: (",min(df$z,na.rm=TRUE),",",max(df$z,na.rm=TRUE),")"))
    cl <- NULL
  }
  return(cl)
}
