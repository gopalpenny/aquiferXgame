# define_parameters.R

# 1. Define default parameter set


get_params <- function(scenario,params_path="../../data/format/game_params",area_km2=50) {
  # call as: params_get("genevois_parameter_estimates")
  params_filepath <- file.path(params_path,paste0(scenario,".csv"))
  if (file.exists(params_filepath)) {
    params <- read_csv(params_filepath) %>% dplyr::select(parameter,value) %>% spread(parameter,value)
  } else if (scenario == "symmetric1") {
    params <- default_params
  } else if (scenario == "genevese1") {
    params <- default_params %>%
      mutate(Qf=4,#p0f=0.7,
             rsN=(1/area_km2)*0.55, #m/MCM
             rsT=(1/area_km2)*0.5, #m/MCM
             rfN=(1/area_km2)*0.45, #m/MCM
             rfT=(1/area_km2)*0.5) #m/MCM)
  } else{
    stop("scenario doesn't match file or other parameter set\n")
  }
  return(params)
}
